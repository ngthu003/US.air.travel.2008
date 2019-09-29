libs <- c('tidyverse', 'caret', 'e1071', 'pROC', 'rpart', 'kernlab', 'klaR', 
          'lubridate', 'ggplot2', 'ggthemes', 'ggpubr', 'xtable', 'reshape2', 'ggrepel')
lapply(libs, require, character.only = TRUE)
rm(libs)



# Load prepared dataset ---------------------------------------------
load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 2 - ML - Classification/airtraffic08-airportinfo-MachineLearning.RData")




# ' ' ' trainControl ------------------------------------------------
# Wrapper fns for performance measures
# All measures
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = 'cv',
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = fiveStats,
                     savePredictions = TRUE,
                     verboseIter = FALSE)


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



# *** Down-sample other dataset ----------------------------------------
predictors <- names(other)[-11]
set.seed(1)
downSampledData <- downSample(x = other[, predictors],
                              y = other$CancelledF,
                              yname = 'CancelledF')
# str(downSampledData)
# table(downSampledData$CancelledF)
rm(predictors)

# After down-sampling:
# -------------------- 271,616 x 13
# -------------------- of which 135,808 Cancelled & NotCancelled





################################################################Begin
# ' ' ' Less expensive Algorithm ####################################
##################################################################End


mod.glm <- function(train, evaluation, approach) {  #------------------ 1-A. Logistic Regression
  # Fit Model
  set.seed(1)
  lr.Fit <- train(CancelledF ~ ., data = train,
                  method = 'glm', preProc = c('center', 'scale'),
                  trControl = ctrl, metric = 'ROC')
  # Prediction
  lr.Pred <- predict(lr.Fit, newdata = evaluation, type = 'prob')[,1]
  lr.ROC <- roc(evaluation$CancelledF, lr.Pred,
                levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    lr.Thres <- .5
  } else if (approach == 'alternate') {
    lr.Thres <- coords(lr.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  lr.PredF <- factor(ifelse(lr.Pred > lr.Thres, 'cancelled', 'notcancelled'),
                     levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(lr.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- lr.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         lr.ROC$auc, lr.Thres), 4)
  return(metric)
} 



mod.lda <- function(train, evaluation, approach) {  #------------------ 1-B. Linear Discriminant Analysis
  # Fit Model
  set.seed(1)
  lda.Fit <- train(CancelledF ~ . -CRSArrT.Block -CRSDepT.Block, data = train, 
                   method = 'lda', preProc = c('center', 'scale'),
                   trControl = ctrl, metric = 'ROC')
  # Prediction
  lda.Pred <- predict(lda.Fit, newdata = evaluation, type = 'prob')[,1]
  lda.ROC <- roc(evaluation$CancelledF, lda.Pred,
                 levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    lda.Thres <- .5
  } else if (approach == 'alternate') {
    lda.Thres <- coords(lda.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  lda.PredF <- factor(ifelse(lda.Pred > lda.Thres, 'cancelled', 'notcancelled'),
                      levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(lda.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- lda.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         lda.ROC$auc, lda.Thres), 4)
  return(metric)
} 



mod.qda <- function(train, evaluation, approach) {  #------------------ 1-C. Quadratic Discriminant Analysis
  # Fit Model
  set.seed(1)
  qda.Fit <- train(CancelledF ~ . -CRSArrT.Block -CRSDepT.Block, data = train, 
                   method = 'qda', preProc = c('center', 'scale'),
                   trControl = ctrl, metric = 'ROC')
  # Prediction
  qda.Pred <- predict(qda.Fit, newdata = evaluation, type = 'prob')[,1]
  qda.ROC <- roc(evaluation$CancelledF, qda.Pred,
                 levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    qda.Thres <- .5
  } else if (approach == 'alternate') {
    qda.Thres <- coords(qda.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  qda.PredF <- factor(ifelse(qda.Pred > qda.Thres, 'cancelled', 'notcancelled'),
                      levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(qda.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- qda.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         qda.ROC$auc, qda.Thres), 4)
  return(metric)
} 



mod.fda <- function(train, evaluation, approach) {  #------------------ 1-D. Flexible Discriminant Analysis
  # Fit Model
  set.seed(1)
  fda.Fit <- train(CancelledF ~ ., data = train, 
                   method = 'fda', preProc = c('center', 'scale'),
                   tuneGrid = expand.grid(.degree = 1, .nprune = seq(2,10,2)),
                   trControl = ctrl, metric = 'ROC')
  # Prediction
  fda.Pred <- predict(fda.Fit, newdata = evaluation, type = 'prob')[,1]
  fda.ROC <- roc(evaluation$CancelledF, fda.Pred,
                 levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    fda.Thres <- .5
  } else if (approach == 'alternate') {
    fda.Thres <- coords(fda.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  fda.PredF <- factor(ifelse(fda.Pred > fda.Thres, 'cancelled', 'notcancelled'),
                      levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(fda.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- fda.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         fda.ROC$auc, fda.Thres), 4)
  return(metric)
} 



















# 2-A. Split into train & evaluation set ----------------------------
set.seed(1)
split <- sample(nrow(downSampledData), nrow(downSampledData)*2/3, replace = FALSE)
train <- downSampledData[split,]
evaluation <- downSampledData[-split,]
rm(split)
# rm(split, downSampledData)

# Train size: 181,077
# Evaluation:  90,539



# 2-B. DF to store Models & Performace Measures ---------------------
# DF to store model evaluation metric:
modEval.DownSampling.Regular <- as.data.frame(matrix(ncol = 13))
colnames(modEval.DownSampling.Regular) <- c('Approach', 'Method', 'k',
                                            'Train size', 'Evaluation size',
                                            'Accuracy', 'Kappa', 'Sensitivity', 'Specificity', 
                                            'F1', 'AUC', 'Threshold', 'Runtime (mins)')

# List of Models for each Method
models.DownSampling.Regular <- list()


# 2-C. Model training: Down-Sampling --------------------------------
# List of functions for each Method
models.list <- list(mod.glm, mod.lda, mod.qda, mod.fda)
models.name <- c('Logistic Regression', 'Linear Discriminant Analysis', 
                 'Quadratic Discriminant Analysis', 'Flexible Discriminant Analysis')



# Train the models for each method: Approach: Down-Sampling
for (i in 1:length(models.list)) {
  
  print(models.name[i])
  
  start.time <- Sys.time()
  model <- models.list[[i]]
  metric <- model(train, evaluation, 'sampling')
  end.time <- Sys.time()
  run.time <- round(as.numeric(end.time - start.time, units = 'mins'),2)
  # Update Model list
  models.DownSampling.Regular[[i]] <- metric[[1]]
  names(models.DownSampling.Regular)[i] <- models.name[i]
  # Update Performance measure data frame
  modEval.DownSampling.Regular <- rbind(modEval.DownSampling.Regular,
                                        c('Down-Sampling', models.name[i], 1, 
                                          nrow(train), nrow(evaluation),
                                          metric[[2]], run.time))
  
  print(run.time)
  
}


View(modEval.DownSampling.Regular)
modEval.DownSampling.Regular
View(models.DownSampling.Regular)
print(object.size(modEval.DownSampling.Regular), units = 'Mb')
print(object.size(models.DownSampling.Regular), units = 'Mb')



################################################################Begin
# *** More expensive Algorithm ######################################
##################################################################End






mod.nnet <- function(train, evaluation, approach) {  #------------------ 3-A. Neural nertwork
  # Fit Model
  nnetGrid <- expand.grid(.size = seq(1,10,3),
                          .decay = c(.1, 1, 2))
  set.seed(1)
  nnet.Fit <- train(CancelledF ~ ., data = train, 
                    method = 'nnet', preProc = c('center', 'scale'),
                    tuneGrid = nnetGrid, maxit = 1000, linout = FALSE,
                    trControl = ctrl, metric = 'ROC',
                    trace = FALSE)
  # Prediction
  nnet.Pred <- predict(nnet.Fit, newdata = evaluation, type = 'prob')[,1]
  nnet.ROC <- roc(evaluation$CancelledF, nnet.Pred,
                  levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    nnet.Thres <- .5
  } else if (approach == 'alternate') {
    nnet.Thres <- coords(nnet.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  nnet.PredF <- factor(ifelse(nnet.Pred > nnet.Thres, 'cancelled', 'notcancelled'),
                       levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(nnet.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- nnet.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         nnet.ROC$auc, nnet.Thres), 4)
  return(metric)
} 





mod.rf <- function(train, evaluation, approach) {  #------------------ 3-B. Random Forest
  # Fit Model
  set.seed(1)
  rf.Fit <- train(CancelledF ~ ., data = train, 
                  method = 'rf', preProc = c('center', 'scale'),
                  tuneLength = 5,
                  tuneGrid = expand.grid(.mtry = seq(10, 50, 10)),
                  trControl = ctrl, metric = 'ROC')
  # Prediction
  rf.Pred <- predict(rf.Fit, newdata = evaluation, type = 'prob')[,1]
  rf.ROC <- roc(evaluation$CancelledF, rf.Pred,
                levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    rf.Thres <- .5
  } else if (approach == 'alternate') {
    rf.Thres <- coords(rf.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  rf.PredF <- factor(ifelse(rf.Pred > rf.Thres, 'cancelled', 'notcancelled'),
                     levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(rf.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- rf.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         rf.ROC$auc, rf.Thres), 4)
  return(metric)
} 





mod.gbm <- function(train, evaluation, approach) {  #------------------ 3-C. Stochastic Gradient Boosting
  # Fit Model
  gbmGrid <- expand.grid(.interaction.depth = seq(4, 10, 3),
                         .shrinkage = c(.001, .01, .1),
                         .n.minobsinnode = 10,
                         .n.trees = 1000)
  set.seed(1)
  gbm.Fit <- train(CancelledF ~ ., data = train, 
                   method = 'gbm', preProc = c('center', 'scale'),
                   tuneGrid = gbmGrid, bag.fraction = 0.5,
                   trControl = ctrl, metric = 'ROC',
                   verbose = FALSE)
  # Prediction
  gbm.Pred <- predict(gbm.Fit, newdata = evaluation, type = 'prob')[,1]
  gbm.ROC <- roc(evaluation$CancelledF, gbm.Pred,
                 levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    gbm.Thres <- .5
  } else if (approach == 'alternate') {
    gbm.Thres <- coords(gbm.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  gbm.PredF <- factor(ifelse(gbm.Pred > gbm.Thres, 'cancelled', 'notcancelled'),
                      levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(gbm.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- gbm.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         gbm.ROC$auc, gbm.Thres), 4)
  return(metric)
} 






mod.knn <- function(train, evaluation, approach) {  #------------------ 3-D. k-Nearest Neighbors
  # Fit Model
  set.seed(1)
  knn.Fit <- train(CancelledF ~ ., data = train, 
                   method = 'knn', preProc = c('center', 'scale'),
                   tuneGrid = expand.grid(k = seq(1,20,4)), 
                   trControl = ctrl, metric = 'ROC')
  # Prediction
  knn.Pred <- predict(knn.Fit, newdata = evaluation, type = 'prob')[,1]
  knn.ROC <- roc(evaluation$CancelledF, knn.Pred,
                 levels = levels(evaluation$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    knn.Thres <- .5
  } else if (approach == 'alternate') {
    knn.Thres <- coords(knn.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  knn.PredF <- factor(ifelse(knn.Pred > knn.Thres, 'cancelled', 'notcancelled'),
                      levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(knn.PredF, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- knn.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         knn.ROC$auc, knn.Thres), 4)
  return(metric)
} 






# 4-A. DF to store Models & Performace Measures ---------------------
# DF to store model evaluation metric:
# tmp.EvaluationTable 
modEval.DownSampling.ExpMethod <- as.data.frame(matrix(ncol = 13))
colnames(modEval.DownSampling.ExpMethod) <- c('Approach', 'Method', 'k', 
                                              'Train size', 'Evaluation size',
                                              'Accuracy', 'Kappa', 'Sensitivity', 'Specificity', 
                                              'F1', 'AUC', 'Threshold', 'Runtime (mins)')

# List of Models for each Method
models.DownSampling.ExpMethod <- list()




# 4-B. Model training: Down-Sampling --------------------------------
# List of functions for each Method
models.list <- list(mod.nnet, mod.rf, mod.gbm, mod.knn)
models.name <- c('Neural Network', 'Random Forest',
                 'Gradient Boosting', 'k-NN')


# List of each k train & validation sets
nk <- 10
for (k in 1:nk) {
  # Split data
  set.seed(10*k)
  split <- sample(nrow(downSampledData), 15000, replace = FALSE)
  tmp <- downSampledData[split,]
  set.seed(10*k)
  split <- sample(nrow(tmp), 10000, replace = FALSE)
  train <- tmp[split,]
  evaluation <- tmp[-split,]
  
  # Train the models for each method
  # Train the models for each method: Approach: Down-Sampling with Expensive methods
  for (i in 1:length(models.list)) {
    
    print(paste0('k: ', k))
    print(models.name[i])
    
    start.time <- Sys.time()
    model <- models.list[[i]]
    metric <- model(train, evaluation, 'sampling')
    end.time <- Sys.time()
    run.time <- round(as.numeric(end.time - start.time, units = 'mins'),2)
    # Update Model list
    models.DownSampling.ExpMethod[[nk*(i-1) + k]] <- metric[[1]]
    names(models.DownSampling.ExpMethod)[nk*(i-1) + k] <- paste(models.name[i], k)
    # Update Performance measure data frame
    modEval.DownSampling.ExpMethod <- rbind(modEval.DownSampling.ExpMethod,
                                            c('Down-Sampling', models.name[i], k, 
                                              nrow(train), nrow(evaluation),
                                              metric[[2]], run.time))
    
    print(run.time)
    
  }
}


View(modEval.DownSampling.ExpMethod)
View(models.DownSampling.ExpMethod)
print(object.size(models.DownSampling.ExpMethod), units = 'Mb')

rm(ctrl, fiveStats, train, evaluation,
   models.list, models.name, metric, model,
   start.time, end.time, run.time, k, nk, 
   mod.fda, mod.gbm, mod.glm, mod.knn, mod.lda, mod.nnet, mod.qda, mod.rf)



# ' ' ' Model Evaluation --------------------------------------------
View(modEval.DownSampling.ExpMethod)
View(modEval.DownSampling.Regular)
print(object.size(modEval.DownSampling.Regular), units = 'Mb')
print(object.size(modEval.DownSampling.ExpMethod), units = 'Mb')
print(object.size(models.DownSampling.Regular), units = 'Mb')
print(object.size(models.DownSampling.ExpMethod), units = 'Mb')

modEval.DownSampling.Analysis <- modEval.DownSampling.Regular[-1,]
modEval.DownSampling.Analysis <- rbind(modEval.DownSampling.Analysis,
                                       modEval.DownSampling.ExpMethod[-1,])
rownames(modEval.DownSampling.Analysis) <- 1:nrow(modEval.DownSampling.Analysis)
# View(modEval.DownSampling.Analysis)

str(modEval.DownSampling.Analysis)
for (i in 3:ncol(modEval.DownSampling.Analysis)) {
  modEval.DownSampling.Analysis[,i] <- as.numeric(modEval.DownSampling.Analysis[,i])
}
saveRDS(modEval.DownSampling.Analysis, 
        file = 'Down-Sampling - All Models Statistics.rds')





# 5-A. Statistical Summary of Performance metrics -------------------
t <- qt(.975, 9, lower = TRUE)
tmp <- modEval.DownSampling.Analysis %>%
  group_by(Method) %>%
  mutate(mean_AUC = mean(AUC), sd_AUC = ifelse(is.na(sd(AUC)) == TRUE, 0, sd(AUC)),
         CI_Low_AUC = mean_AUC - t*sd_AUC, CI_Upp_AUC = mean_AUC + t*sd_AUC,
         mean_Acc = mean(Accuracy), sd_Acc = ifelse(is.na(sd(Accuracy)) == TRUE, 0, sd(Accuracy)),
         CI_Low_Acc = mean_Acc - t*sd_Acc, CI_Upp_Acc = mean_Acc + t*sd_Acc,
         mean_Sen = mean(Sensitivity), sd_Sen = ifelse(is.na(sd(Sensitivity)) == TRUE, 0, sd(Sensitivity)),
         CI_Low_Sen = mean_Sen - t*sd_Sen, CI_Upp_Sen = mean_Sen + t*sd_Sen,
         mean_Spe = mean(Specificity), sd_Spe = ifelse(is.na(sd(Specificity)) == TRUE, 0, sd(Specificity)),
         CI_Low_Spe = mean_Spe - t*sd_Spe, CI_Upp_Spe = mean_Spe + t*sd_Spe,
         mean_Kap = mean(Kappa), sd_Kap = ifelse(is.na(sd(Kappa)) == TRUE, 0, sd(Kappa)),
         CI_Low_Kap = mean_Kap - t*sd_Kap, CI_Upp_Kap = mean_Kap + t*sd_Kap,
         mean_F1 = mean(F1), sd_F1 = ifelse(is.na(sd(F1)) == TRUE, 0, sd(F1)),
         CI_Low_F1 = mean_F1 - t*sd_F1, CI_Upp_F1 = mean_F1 + t*sd_F1,
         mean_Thr = mean(Threshold), sd_Thr = ifelse(is.na(sd(Threshold)) == TRUE, 0, sd(Threshold)),
         CI_Low_Thr = mean_Thr - t*sd_Thr, CI_Upp_Thr = mean_Thr + t*sd_Thr) %>%
  filter(row_number()==1)


View(tmp)
names(modEval.DownSampling.Analysis)
rm(i, t)





# ' ' ' Statistical Summary Plot ------------------------------------
tmp$Method.S <- tmp$Method
tmp$Method.S[2:4] <- c('Linear DA', 'Quadratic DA', 'Flexible DA')



# 6-A. AUC ----------------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_AUC) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
                 ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
                        ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
                               ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
p.auc <- tmp %>%
  dplyr::select(Method.S, mean_AUC, CI_Low_AUC, CI_Upp_AUC) %>%
  ggplot(aes(x = reorder(Method.S, mean_AUC), y = mean_AUC)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_AUC, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_AUC, ymax = CI_Upp_AUC), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.5, .8)) +
  labs(subtitle = 'Statistic: AUC') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean AUC') + xlab('')

# p.auc + labs(subtitle = 'Down-Sampling: Method Comparison')


# 6-B. Accuracy -----------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Acc) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
                 ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
                        ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
                               ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
p.acc <- tmp %>%
  dplyr::select(Method.S, mean_Acc, CI_Low_Acc, CI_Upp_Acc) %>%
  ggplot(aes(x = reorder(Method.S, mean_Acc), y = mean_Acc)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Acc, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Acc, ymax = CI_Upp_Acc), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.5, .8)) +
  labs(subtitle = 'Statistic: Accuracy') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Accuracy') + xlab('')

# p.acc + labs(subtitle = 'Down-Sampling: Method Comparison')



# 6-C. Sensitivity --------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Sen) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
                 ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
                        ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
                               ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
p.sen <- tmp %>%
  dplyr::select(Method.S, mean_Sen, CI_Low_Sen, CI_Upp_Sen) %>%
  ggplot(aes(x = reorder(Method.S, mean_Sen), y = mean_Sen)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Sen, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Sen, ymax = CI_Upp_Sen), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.5, .8)) +
  labs(subtitle = 'Statistic: Sensitivity') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Sensitivity') + xlab('')

# p.sen + labs(subtitle = 'Down-Sampling: Method Comparison')



# 6-D. Specificity --------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Spe) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
                 ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
                        ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
                               ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
p.spe <- tmp %>%
  dplyr::select(Method.S, mean_Spe, CI_Low_Spe, CI_Upp_Spe) %>%
  ggplot(aes(x = reorder(Method.S, mean_Spe), y = mean_Spe)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Spe, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Spe, ymax = CI_Upp_Spe), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.5, .8)) +
  labs(subtitle = 'Statistic: Specificity') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Specificity') + xlab('')

# p.spe + labs(subtitle = 'Down-Sampling: Method Comparison')



# 6-E. Kappa --------------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Kap) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
                 ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
                        ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
                               ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
p.kap <- tmp %>%
  dplyr::select(Method.S, mean_Kap, CI_Low_Kap, CI_Upp_Kap) %>%
  ggplot(aes(x = reorder(Method.S, mean_Kap), y = mean_Kap)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Kap, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Kap, ymax = CI_Upp_Kap), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.1, .5)) +
  labs(subtitle = 'Statistic: Kappa') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Kappa') + xlab('')

# p.kap + labs(subtitle = 'Down-Sampling: Method Comparison')



# 6-F. F1 -----------------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_F1) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
                 ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
                        ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
                               ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
p.f1 <- tmp %>%
  dplyr::select(Method.S, mean_F1, CI_Low_F1, CI_Upp_F1) %>%
  ggplot(aes(x = reorder(Method.S, mean_F1), y = mean_F1)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_F1, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_F1, ymax = CI_Upp_F1), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.5, .8)) +
  labs(subtitle = 'Statistic: F1') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean F1') + xlab('')

# p.f1 + labs(subtitle = 'Down-Sampling: Method Comparison')





# 6-G. Arrange all plots --------------------------------------------
figure <- ggarrange(p.auc, p.acc,
                    p.sen, p.spe,
                    p.kap, p.f1,
                    ncol = 2, nrow = 3)  
annotate_figure(figure,
                # top = text_grob('Down-Sampling: Method Comparison',
                #                 color = 'black', face = 'bold', size = 15)
)



# 6-H. Statistics Summary -------------------------------------------
stat.sum <- tmp %>%
  dplyr::select(Approach, Method, mean_AUC, mean_Acc, mean_Sen, mean_Spe, mean_Kap, mean_F1, mean_Thr)
names(stat.sum)

stat.sum[,3:ncol(stat.sum)] <- sapply(stat.sum[,3:ncol(stat.sum)], function(x) round(x,3))

xtable(stat.sum, digits=c(0,0,0,rep(3,7)))
print(xtable(stat.sum, digits=c(0,0,0,rep(3,7)), type = 'latex'), 
      file = "Down-Sampling - Method Summary.tex")



# rm(p.acc, p.auc, p.sen, p.spe, p.kap, p.f1, figure)
# rm(tmp, stat.num, tmp.Col)




# Model Selection ---------------------------------------------------




# 7-A. Compute the 'Closest' models per method ----------------------
t <- qt(.975, 9, lower = TRUE)
tmp <- modEval.DownSampling.Analysis %>%
  group_by(Method) %>%
  mutate(mean_AUC = mean(AUC), sd_AUC = ifelse(is.na(sd(AUC)) == TRUE, 0, sd(AUC)),
         CI_Low_AUC = mean_AUC - t*sd_AUC, CI_Upp_AUC = mean_AUC + t*sd_AUC,
         mean_Acc = mean(Accuracy), sd_Acc = ifelse(is.na(sd(Accuracy)) == TRUE, 0, sd(Accuracy)),
         CI_Low_Acc = mean_Acc - t*sd_Acc, CI_Upp_Acc = mean_Acc + t*sd_Acc,
         mean_Sen = mean(Sensitivity), sd_Sen = ifelse(is.na(sd(Sensitivity)) == TRUE, 0, sd(Sensitivity)),
         CI_Low_Sen = mean_Sen - t*sd_Sen, CI_Upp_Sen = mean_Sen + t*sd_Sen,
         mean_Spe = mean(Specificity), sd_Spe = ifelse(is.na(sd(Specificity)) == TRUE, 0, sd(Specificity)),
         CI_Low_Spe = mean_Spe - t*sd_Spe, CI_Upp_Spe = mean_Spe + t*sd_Spe,
         mean_Kap = mean(Kappa), sd_Kap = ifelse(is.na(sd(Kappa)) == TRUE, 0, sd(Kappa)),
         CI_Low_Kap = mean_Kap - t*sd_Kap, CI_Upp_Kap = mean_Kap + t*sd_Kap,
         mean_F1 = mean(F1), sd_F1 = ifelse(is.na(sd(F1)) == TRUE, 0, sd(F1)),
         CI_Low_F1 = mean_F1 - t*sd_F1, CI_Upp_F1 = mean_F1 + t*sd_F1,
         mean_Thr = mean(Threshold), sd_Thr = ifelse(is.na(sd(Threshold)) == TRUE, 0, sd(Threshold)),
         CI_Low_Thr = mean_Thr - t*sd_Thr, CI_Upp_Thr = mean_Thr + t*sd_Thr)
View(tmp)  


# dist = sqrt( 3|Sen - Ave(Sen)| + 2|AUC - Ave(AUC)| + |F1 - Ave(F1)| )
tmp.Reduced <- tmp %>%
  group_by(Method) %>%
  mutate(distance = sqrt(3*abs(Sensitivity - mean_Sen) + 2*abs(AUC - mean_AUC) + abs(F1 - mean_F1))) %>%
  arrange(distance) %>%
  mutate(Model = paste(Method, k)) %>%
  filter(Method %in% c('Neural Network', 'Random Forest', 'Gradient Boosting', 'k-NN')) %>%
  dplyr::select(Model)
tmp.Reduced <- tmp.Reduced$Model


# models.DownSampling.ExpMethod[tmp.Reduced[1]]
models.DownSampling.BestModels <- list()
for (i in 1:length(models.DownSampling.Regular)) {
  models.DownSampling.BestModels[[i]] <- models.DownSampling.Regular[[i]]
  names(models.DownSampling.BestModels)[i] <- names(models.DownSampling.Regular)[i]
}
k <- length(models.DownSampling.BestModels)
for (i in 1:length(tmp.Reduced)) {
  models.DownSampling.BestModels[[k + i]] <- models.DownSampling.ExpMethod[tmp.Reduced[i]][[1]]
  names(models.DownSampling.BestModels)[k + i] <- tmp.Reduced[i]
}
View(models.DownSampling.BestModels)
rm(i,k,t, tmp.Reduced)


print(object.size(models.DownSampling.BestModels), units = 'Mb')
saveRDS(models.DownSampling.BestModels, 
        file = 'Down-Sampling - Best Models per Algorithms.rds')




# Performance of top 4 methods --------------------------------------
# dist = sqrt( 3|Sen - Ave(Sen)| + 2|AUC - Ave(AUC)| + |F1 - Ave(F1)| )
x <- c('Neural Network', 'Random Forest', 'Gradient Boosting', 'Quadratic Discriminant Analysis')
tmp.Reduced <- tmp %>%
  group_by(Method) %>%
  mutate(distance = sqrt(3*abs(Sensitivity - mean_Sen) + 2*abs(AUC - mean_AUC) + abs(F1 - mean_F1))) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  mutate(Model = paste(Method, k)) %>%
  dplyr::select(Model, k) %>%
  filter(Method %in% x)


# Narrow down the top 3 methods
top4.metric <- merge(tmp.Reduced, modEval.DownSampling.Analysis)
names(top4.metric)



p.1 <- top4.metric %>%
  dplyr::select(Method, AUC, Accuracy, Sensitivity, Specificity, F1) %>%
  melt(id.vars='Method') %>%
  mutate(Method.Ord = factor(Method, levels = x)) %>%
  dplyr::select(Method.Ord, variable, value) %>%
  ggplot(aes(x = Method.Ord, y = value, color = variable, group = variable)) + 
  geom_point(size = 5) + geom_line(size = 1) +
  geom_text_repel(aes(label = round(value, 3)), vjust = -1, hjust = -.5, size = 3, 
                  color = 'black', fontface = 'bold') +
  scale_y_continuous(limits = c(.55, .75)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank()) + 
  ylab('') + xlab('')




p.2 <- top4.metric %>%
  dplyr::select(Method, Kappa) %>%
  melt(id.vars='Method') %>%
  mutate(Method.Ord = factor(Method, levels = x)) %>%
  dplyr::select(Method.Ord, variable, value) %>%
  ggplot(aes(x = Method.Ord, y = value, color = variable, group = variable)) + 
  geom_point(size = 5) + geom_line(size = 1) +
  geom_text_repel(aes(label = round(value, 3)), vjust = -1, hjust = -.5, size = 3, 
                  color = 'black', fontface = 'bold') +
  scale_y_continuous(limits = c(.275, .375)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank()) + 
  ylab('') + xlab('')

figure <- ggarrange(p.1, p.2, ncol = 1, nrow = 2)  
annotate_figure(figure,
                # top = text_grob('Down-Sampling: Top 4 methods',
                #                 color = 'black', face = 'bold', size = 15)
)

ncol(top4.metric)

top4.metric <- top4.metric %>%
  dplyr::select(Method, k, Approach, AUC, Accuracy, Sensitivity, Specificity, Kappa, F1, Threshold)

head(top4.metric)
print(xtable(top4.metric, 
             digits = c(rep(0,4), rep(3,7)), type = 'latex'), 
      file = "Down-Sampling - Top 4 Methods.tex")

View(modEval.DownSampling.Analysis)
ncol(modEval.DownSampling.Analysis)
print(xtable(modEval.DownSampling.Analysis, 
             digits = c(rep(0,6), rep(3,8)), type = 'latex'), 
      file = "[Stock] --- Down-Sampling - All methods and models.tex")



write.csv(modEval.DownSampling.Analysis, 
          file = '[Stock] --- Down-Sampling - All Methods and Models Metrics.csv')








########################## SCRATCH ##################################



# Next time: To load models
`Down-Sampling - Best Models per Algorithms` <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Down-Sampling - Best Models per Algorithms.rds")
models.DownSampling.BestModels <- `Down-Sampling - Best Models per Algorithms`
`Down-Sampling - All Models Statistics` <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Down-Sampling - All Models Statistics.rds")
modEval.DownSampling.Analysis <- `Down-Sampling - All Models Statistics`
rm(`Down-Sampling - All Models Statistics`, `Down-Sampling - Best Models per Algorithms`)






