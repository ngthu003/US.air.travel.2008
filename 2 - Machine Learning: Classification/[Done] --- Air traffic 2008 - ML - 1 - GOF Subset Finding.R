libs <- c('tidyverse', 'ggplot2', 'caret', 'e1071', 'pROC', 'rpart', 'kernlab', 'klaR')
lapply(libs, require, character.only = TRUE)
rm(libs)


# Load data
load("D:/Kaggle Datasets/Airport/airtraffic08-airportinfo-MachineLearning.RData")


# 1. Narrow down relevant predictors --------------------------------
data <- na.omit(data)
data <- data %>%
  dplyr::select(-CRSDepTime, -CRSArrTime, -Cancelled,
                -Origin, -Dest,
                -Origin.Area.sq.m, -Dest.Area.sq.m,
                -Origin.state, -Dest.state,
                -Origin.Population, -Dest.Population,
                -Origin.Pop.Density, -Dest.Pop.Density,
                -DayofMonth)



# 2. Classify Airlines ----------------------------------------------
# All airlines with < 4% shares are grouped together into 'OtherCarriers'
otherCarriers <- data %>%
  group_by(UniqueCarrier) %>%
  count() %>%
  mutate(Share = n/nrow(data) * 100) %>%
  arrange(desc(Share)) %>%
  filter(Share < 4) %>%
  dplyr::select(UniqueCarrier)
otherCarriers <- otherCarriers[[1]]
data$UniqueCarrier <- as.character(data$UniqueCarrier)
data$UniqueCarrier[data$UniqueCarrier %in% otherCarriers] <- 'OtherCarriers'
# sort(table(data$UniqueCarrier), decreasing = T)
rm(otherCarriers)


# Convert to factors relevant predictors
data$UniqueCarrier <- as.factor(data$UniqueCarrier)
data$Origin.Capital <- as.factor(data$Origin.Capital)
data$Dest.Capital <- as.factor(data$Dest.Capital)
str(data)


# Fix a Test set & Other set ----------------------------------------
set.seed(1)
split <- sample(nrow(data), nrow(data)/100, replace = FALSE)
test  <- data[ split,]
other <- data[-split,]

rm(test, split, otherCarriers)


# Goodness-of-Fit test for Subset fn --------------------------------
GOF.test.fn <- function(df, subset) {
  Chi.sq.test <- data.frame(colnames(df))
  colnames(Chi.sq.test) <- 'Variables'
  for (i in 1:ncol(df)) {
    actual <- table(subset[,i])
    expected <- table(df[,i])
    p.exp <- expected / sum(expected)
    p.val <- chisq.test(x = actual, p = p.exp)$p.value
    Chi.sq.test$`Chi.sq p-val`[i] <- round(p.val,4)
  }
  Chi.sq.test <- Chi.sq.test %>%
    mutate(Goodness.of.Fit = ifelse(`Chi.sq p-val` > .05, 'Good', 'Bad'),
           Good.Subset = ifelse(Goodness.of.Fit == 'Good', 1, 0))
  return(Chi.sq.test)
}

data.X2 <- data %>%
  dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
test.X2 <- test %>%
  dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)

# GOF.on.test <- GOF.test.fn(data.X2, test.X2)
# To see if all variables are representative of the entire dataset
# sum(GOF.on.test$Good.Subset) == nrow(GOF.on.test)
# rm(test.X2, data.X2)






# Data split into 10 sets of train: 100k & evaluation: 50k ----------
dataset.list.100k <- list()
nk <- 10
for (k in 1:nk) {
  
  data.X2 <- data %>% 
    dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
  # By default: flag == TRUE <=> subset not representative of entire dataset
  flag <- TRUE
  # Randomization Seed parameter
  i <- 1
  
  while (flag == TRUE) {
    # Split the current dataset into train & evaluation
    set.seed(i)
    split1 <- sample(nrow(other), 150000, replace = FALSE)
    dataset <- other[split1,]
    
    set.seed(i)
    split2 <- sample(nrow(dataset), 100000, replace = FALSE)
    train <- dataset[split2,]
    evaluation <- dataset[-split2,]
    
    # Filter out the numerical Distance variable
    dataset.X2 <- dataset %>% 
      dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
    train.X2 <- train %>% 
      dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
    evaluation.X2 <- evaluation %>% 
      dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
    
    dataset.GOF <- GOF.test.fn(data.X2, dataset.X2)
    train.GOF <- GOF.test.fn(data.X2, train.X2)
    evaluation.GOF <- GOF.test.fn(data.X2, evaluation.X2)
    
    counter.GOF <- sum(train.GOF$Good.Subset) + sum(evaluation.GOF$Good.Subset)
    
    if (counter.GOF == 2*ncol(data.X2)) {
      other <- other[-split1,]
      dataset.list.100k[[2*k - 1]] <- train
      names(dataset.list.100k)[2*k-1] <- paste('train', k)
      dataset.list.100k[[2*k]] <- evaluation
      names(dataset.list.100k)[2*k] <- paste('evaluation', k)
      flag <- FALSE
    } else {
      i <- i + 1
    }
  }
}

View(dataset.list.100k)
print(object.size(dataset.list.100k), units = 'Mb')






















# Data split into 10 sets of train: 20k & evaluation: 10k -----------
dataset.list.10k <- list()
nk <- 10
for (k in 1:nk) {
  
  data.X2 <- data %>% 
    dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
  # By default: flag == TRUE <=> subset not representative of entire dataset
  flag <- TRUE
  # Randomization Seed parameter
  i <- 1
  
  while (flag == TRUE) {
    # Split the current dataset into train & evaluation
    set.seed(i)
    split1 <- sample(nrow(other), 15000, replace = FALSE)
    dataset <- other[split1,]
    
    set.seed(i)
    split2 <- sample(nrow(dataset), 10000, replace = FALSE)
    train <- dataset[split2,]
    evaluation <- dataset[-split2,]
    
    # Filter out the numerical Distance variable
    dataset.X2 <- dataset %>% 
      dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
    train.X2 <- train %>% 
      dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
    evaluation.X2 <- evaluation %>% 
      dplyr::select(-Distance, -CRSElapsedTime, -Origin.Capital, -Dest.Capital)
    
    dataset.GOF <- GOF.test.fn(data.X2, dataset.X2)
    train.GOF <- GOF.test.fn(data.X2, train.X2)
    evaluation.GOF <- GOF.test.fn(data.X2, evaluation.X2)
    
    counter.GOF <- sum(train.GOF$Good.Subset) + sum(evaluation.GOF$Good.Subset)
    
    if (counter.GOF == 2*ncol(data.X2)) {
      other <- other[-split1,]
      dataset.list.10k[[2*k - 1]] <- train
      names(dataset.list.10k)[2*k-1] <- paste('train', k)
      dataset.list.10k[[2*k]] <- evaluation
      names(dataset.list.10k)[2*k] <- paste('evaluation', k)
      flag <- FALSE
    } else {
      i <- i + 1
    }
  }
}

View(dataset.list.10k)
print(object.size(dataset.list.10k), units = 'Mb')
rm(k, nk, split, split1, split2, data.X2, test.X2, train.X2, evaluation.X2, flag, counter.GOF)
rm(dataset.X2, evaluation.GOF, train.GOF, i)
rm(dataset.GOF, dataset, evaluation, train)
rm(data, other, test, GOF.test.fn)



save.image(file = 'Dataset - Alternate - 10 each of 100k and 10k-training sets.RData')

















