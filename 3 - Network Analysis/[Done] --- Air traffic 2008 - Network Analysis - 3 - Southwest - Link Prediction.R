libs <- c('statnet', 'degreenet', 'intergraph', 'igraph', 'network', 'sna', 
          'tidyverse', 'ape', 'reshape2',  'expm', 'ROCR', 'caret',
          'xtable', 'latex2exp',
          'ggmap', 'ggrepel', 'ggplot2', 'ggthemes', 'gridExtra', 'directlabels')
lapply(libs, require, character.only = TRUE)
rm(libs)


load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 1 - EDA/airtraffic08-airportinfo-EDA.RData")
airport.info <- read.csv('airport-info-08.csv')



state.rm <- c('VI', 'PR', 'HI', 'AK')
k <- which(data$Origin.state %in% state.rm | data$Dest.state %in% state.rm)
data <- data[-k,]
rm(k, state.rm)




get.airline.graph <- function(airline, df, city.df) {                       # Helper fn 
  # 1-A. Prepare dataset
  airline.data <- df %>%
    filter(UniqueCarrier == airline) %>%
    select(Origin, Dest, Distance, Origin.Hub, Dest.Hub, UniqueCarrier) %>%
    group_by(Origin, Dest) %>%
    mutate(count = n()) %>%
    distinct(Origin, Dest, Distance, count, Origin.Hub, Dest.Hub, UniqueCarrier) %>%
    arrange(desc(count))
  
  # Convert to igraph object
  airline.graph <- graph.edgelist(as.matrix(airline.data[,1:2]))
  
  # 1-B. Vertex Attributes
  # Airport Hub 
  tmp <- airline.data %>%
    group_by(Origin) %>%
    distinct(Origin, Origin.Hub)
  vertex.attr.df <- data.frame(Index = 1:vcount(airline.graph),
                               Airport = V(airline.graph)$name)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'Origin', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  V(airline.graph)$Hub <- vertex.attr.df$Origin.Hub
  
  # Total number of Departing flights
  tmp <- airline.data %>%
    group_by(Origin) %>%
    mutate(Departing.flights = sum(count)) %>%
    distinct(Origin, Departing.flights)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'Origin', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  V(airline.graph)$Departing.flights <- vertex.attr.df$Departing.flights
  
  # Total number of Arriving flights
  tmp <- airline.data %>%
    group_by(Dest) %>%
    mutate(Arriving.flights = sum(count)) %>%
    distinct(Dest, Arriving.flights)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'Dest', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  V(airline.graph)$Arriving.flights <- vertex.attr.df$Arriving.flights
  
  # ------------------- State, Population, Area.sq.m, Pop.Density, Housing.Density, Capital
  # ------------------- Prepare info
  tmp <- city.df %>%
    select(iata, state, Population, Area.sq.m, Pop.Density, Capital)
  vertex.attr.df <- merge(vertex.attr.df, tmp, 
                          by.x = 'Airport', by.y = 'iata', all.x = TRUE)
  vertex.attr.df <- vertex.attr.df[order(vertex.attr.df$Index),]
  # ------------------- Add vertex attributes: City info
  V(airline.graph)$State <- vertex.attr.df$state
  V(airline.graph)$Population <- vertex.attr.df$Population
  V(airline.graph)$Area.sq.m <- vertex.attr.df$Area.sq.m
  V(airline.graph)$Pop.Density <- vertex.attr.df$Pop.Density
  V(airline.graph)$Capital <- vertex.attr.df$Capital
  
  # 1-B. Edge Attributes
  # Number of flights
  E(airline.graph)$Flights <- airline.data$count
  # Route distance
  E(airline.graph)$Distance <- airline.data$Distance
  
  return(airline.graph)
}







# ' ' ' Southwest ----------------------------------------------------------
sw.dig <- get.airline.graph('WN', data, airport.info)
summary(sw.dig)
########################################################### Undirected graph
E(sw.dig)$weight <- E(sw.dig)$Flights                        ############# !
sw.und <- as.undirected(sw.dig, mode = 'collapse',           ############# !
                        edge.attr.comb = 'sum')              ############# !
sw.und <- delete_edge_attr(sw.und, 'weight')                 ############# !
summary(sw.und)                                              ############# !
######################################################################## END





# Link Prediction ----------------------------------------------------------


# ' ' ' Scoring fn ---------------------------------------------------------


# 0. Data preparation ------------------------------------------------------
nv <- vcount(sw.und); ne <- ecount(sw.und)
A <- get.adjacency(sw.und)
Avec <- A[lower.tri(A)]
# ------------------------------------------------------------------------ !
# ------------------------------------------------------------------------ !
# 1. Number of Common Neighbors --------------------------------------------
ncn.Score <- numeric()
# loop to compute NCN
for (i in 1:(nv-1)) {
  ni <- neighborhood(sw.und, 1, i)
  nj <- neighborhood(sw.und, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY = FALSE)
  tmp <- unlist(lapply(nbhd.ij, length)) - 2*A[i, (i+1):nv]
  ncn.Score <- c(ncn.Score, tmp)
}
pred.ncn <- prediction(ncn.Score, Avec)
# ------------------------------------------------------------------------ !
# ------------------------------------------------------------------------ !
# 2. Jaccard Coef. ---------------------------------------------------------
jac.Score <- numeric()
# loop to compute NCN
for (i in 1:(nv-1)) {
  ni <- neighborhood(sw.und, 1, i)
  nj <- neighborhood(sw.und, 1, (i+1):nv)
  inter.ij <- mapply(intersect, ni, nj, SIMPLIFY = FALSE)
  union.ij <- mapply(union, ni, nj, SIMPLIFY = FALSE)
  tmp1 <- unlist(lapply(inter.ij, length)) - 2*A[i, (i+1):nv]
  tmp2 <- unlist(lapply(union.ij, length)) - 2*A[i, (i+1):nv]
  tmp <- tmp1 / tmp2
  jac.Score <- c(jac.Score, tmp)
}
pred.Jac <- prediction(jac.Score, Avec)
# ------------------------------------------------------------------------ !
# ------------------------------------------------------------------------ !
# 3. Preferential Attachment Score -----------------------------------------
pa.Score <- numeric()
# loop to compute Preferential Attachment Score
for (i in 1:(nv-1)) {
  ni.len <- unlist(lapply(neighborhood(sw.und, 1, i), length))
  nj.len <- unlist(lapply(neighborhood(sw.und, 1, (i+1):nv), length))
  pa.Score <- c(pa.Score, ni.len * nj.len)
}
pred.pa <- prediction(pa.Score, Avec)
# ------------------------------------------------------------------------ !
# ------------------------------------------------------------------------ !
# 4. Katz Score ------------------------------------------------------------
A.mat <- as.matrix(A)
################################################### Katz: Unweighted version 
# fn to calculate Katz score: Unweighted ver.
katz.unw.fn <- function(graph, beta) {
  # Get adjacency matrix
  adj.matrix <- as.matrix(get.adjacency(graph))
  nv <- vcount(graph)
  # Initialize Katz score vector
  katzScore <- rep(0, nv*(nv-1)/2)
  for (i in 1:(nv-1)) {
    A.tmp <- adj.matrix %^% i
    A.tmp <- A.tmp[lower.tri(A.tmp)]
    katzScore <- katzScore + (beta^A.tmp * A.tmp)
  }
  return(katzScore)
}
# Scores over different beta
betas <- c(.01, .05, .1, .2, .25, .5, .75, 1)
aucs <- numeric(length(betas))
pred.Katz <- list()
for (i in 1:length(betas)) {
  beta <- betas[i]
  katz.tmp <- katz.unw.fn(sw.und, beta)
  pred <- prediction(katz.tmp, Avec)
  pred.Katz[[i]] <- pred
  perf <- performance(pred, 'auc')
  aucs[i] <- slot(perf, 'y.values')[[1]]
}

data.frame(beta = betas,
           AUC = aucs)    ############################################## End
plot(performance(pred.Katz[[3]], 'acc'), xlim = c(0,.6), col = 'blue')
plot(performance(pred.Katz[[4]], 'acc'), add = TRUE, col = 'green')
plot(performance(pred.Katz[[5]], 'acc'), add = TRUE, col = 'red')
plot(performance(pred.Katz[[5]], 'tpr', 'fpr'), col = 'green')
plot(performance(pred.Katz[[4]], 'tpr', 'fpr'), col = 'blue', add = T)
plot(performance(pred.Katz[[3]], 'tpr', 'fpr'), col = 'red', add = T)
# to get the cut-off giving max accuracy
ind <- which.max(slot(performance(pred.Katz[[1]], 'acc'), 'y.values')[[1]])
acc <- plot(performance(pred.Katz[[1]], 'acc'))[[1]][ind]
cutoff <- (slot(performance(pred.Katz[[1]], 'acc'), "x.values"))[[1]][ind]
print(c(accuracy = acc, cutoff = cutoff))




length(slot(performance(pred.ncn, 'tpr', 'fpr'), 'x.values')[[1]])
length(unlist(slot(performance(pred.ncn, 'tpr', 'fpr'), 'x.values')))
unlist(slot(performance(pred.ncn, 'tpr', 'fpr'), 'x.values'))
length(unlist(slot(auc.ncn, 'x.values')))




# Common neighbors
auc.ncn <- performance(pred.ncn, 'sens', 'spec')
tmp <- data.frame(Sensitivity = unlist(slot(auc.ncn, 'y.values')),
                  FPR = 1 - unlist(slot(auc.ncn, 'x.values')),
                  score.fn = rep('Common Neighbors', length(unlist(slot(auc.ncn, 'x.values')))))
# Jaccard coefficient
auc.Jac <- performance(pred.Jac, 'sens', 'spec')
tmp <- rbind(tmp,
             data.frame(Sensitivity = unlist(slot(auc.Jac, 'y.values')),
                        FPR = 1 - unlist(slot(auc.Jac, 'x.values')),
                        score.fn = rep('Jaccard Coefficient', length(unlist(slot(auc.Jac, 'x.values'))))))
# Perferential attachment
auc.p.a <- performance(pred.pa, 'sens', 'spec')
tmp <- rbind(tmp,
             data.frame(Sensitivity = unlist(slot(auc.p.a, 'y.values')),
                        FPR = 1 - unlist(slot(auc.p.a, 'x.values')),
                        score.fn = rep('Preferential Attachment', length(unlist(slot(auc.p.a, 'x.values'))))))
auc.kat <- performance(pred.Katz[[5]], 'sens', 'spec')
tmp <- rbind(tmp,
             data.frame(Sensitivity = unlist(slot(auc.kat, 'y.values')),
                        FPR = 1 - unlist(slot(auc.kat, 'x.values')),
                        score.fn = rep('Katz score: beta = .25', length(unlist(slot(auc.kat, 'x.values'))))))


# Comparison statistics ----------------------------------------------------
# ROC curves of all scoring functions
tmp %>%
  ggplot(aes(x = FPR, y = Sensitivity, color = score.fn)) +
  geom_point() + geom_line(size = 1) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.direction = 'vertical',
        legend.position = 'right') +
  xlab('1 - Specificity') + ylab('Sensivity') +
  guides(color = guide_legend(title = 'Scoring functions'))




# Scoring functions statistics
all.fn <- list(pred.ncn, pred.Jac, pred.pa)
for (i in 1:length(pred.Katz)) {
  all.fn[[3+i]] <- pred.Katz[[i]]
}
aucs <- c()
for (i in 1:length(all.fn)) {
  aucs[i] <- round(slot(performance(all.fn[[i]], 'auc'), 'y.values')[[1]],3)
}
accs <- c()
for (i in 1:length(all.fn)) {
  accs[i] <- round(max(slot(performance(all.fn[[i]], 'acc'), 'y.values')[[1]]),3)
}
s.names <- c('Common Neighbors', 'Jaccard Coefficient', 'Preferential Attachment', 
             rep('Katz score', length(betas)))
tmp <- data.frame(Functions = s.names, 
                  Beta = c(0,0,0, betas),
                  AUC = aucs, 
                  Max.Accuracy = accs)

# print(xtable(tmp, digits = c(0,0,2,3,3), type = 'latex'),
#       file = 'Southwest - Link Prediction - Scoring Function Summary Table.tex')






















# ' ' ' Probabilistic Classifier -------------------------------------------


# 0. Data preparation ------------------------------------------------------
# All permutations of airports
sw.airports <- V(sw.und)$name
n <- length(sw.airports)
vertex.1 <- c()
vertex.2 <- c()

for (i in 1:(n-1)) {
  tmp.v2 <- sw.airports[(i+1):n]
  vertex.2 <- c(vertex.2, tmp.v2)
  vertex.1 <- c(vertex.1, rep(sw.airports[i], length(tmp.v2)))
}
sw.network.data <- data.frame(Airport.1 = vertex.1,
                              Airport.2 = vertex.2)
sw.network.data$Airport.1 <- as.character(sw.network.data$Airport.1)
sw.network.data$Airport.2 <- as.character(sw.network.data$Airport.2)

# Add airport 1 info
sw.data.1 <- data.frame()
for (i in 1:n) {
  tmp.sw <- sw.network.data
  tmp.airport <- sw.airports[i]
  tmp.info <- airport.info %>%
    filter(iata == tmp.airport) %>%
    select(iata, state, Population, Area.sq.m, Pop.Density, Capital)
  tmp.sw <- merge(tmp.sw, tmp.info, by.x = 'Airport.1', 'iata')
  for (j in 3:7) {
    colnames(tmp.sw)[j] <- paste0('Airport.1.', colnames(tmp.sw)[j])
  }
  sw.data.1 <- rbind(sw.data.1, tmp.sw)
}
# Add airport 2info
sw.data.2 <- data.frame()
for (i in 1:n) {
  tmp.sw <- sw.network.data
  tmp.airport <- sw.airports[i]
  tmp.info <- airport.info %>%
    filter(iata == tmp.airport) %>%
    select(iata, state, Population, Area.sq.m, Pop.Density, Capital)
  tmp.sw <- merge(tmp.sw, tmp.info, by.x = 'Airport.2', 'iata')
  for (j in 3:7) {
    colnames(tmp.sw)[j] <- paste0('Airport.2.', colnames(tmp.sw)[j])
  }
  sw.data.2 <- rbind(sw.data.2, tmp.sw)
}
# Merge 2 df from 2 airports
sw.data <- merge(sw.data.1, sw.data.2)
View(sw.data)
rm(i, j, n, tmp.sw, tmp.info, sw.data.1, sw.data.2, m, sw.airports, 
   tmp.airport, tmp.v2, vertex.1, vertex.2)


# Get the edge list
sw.edge <- as.data.frame(get.edgelist(sw.und))
# Initialize binary variable edge
sw.data$Flight <- rep('no', nrow(sw.data))
for (i in 1:nrow(sw.edge)) {
  k <- which(sw.data$Airport.1 ==  sw.edge$V1[i]
             & sw.data$Airport.2 ==  sw.edge$V2[i])
  sw.data$Flight[k] <- 'yes'
}
str(sw.data)
rm(i, k)


# Convert Captials and Flights to factors
sw.data$Airport.1 <- as.factor(sw.data$Airport.1)
sw.data$Airport.2 <- as.factor(sw.data$Airport.2)
sw.data$Airport.1.Capital <- gsub('0', 'no', sw.data$Airport.1.Capital)
sw.data$Airport.1.Capital <- gsub('1', 'yes', sw.data$Airport.1.Capital)
sw.data$Airport.2.Capital <- gsub('0', 'no', sw.data$Airport.2.Capital)
sw.data$Airport.2.Capital <- gsub('1', 'yes', sw.data$Airport.2.Capital)
sw.data$Airport.1.Capital <- as.factor(sw.data$Airport.1.Capital)
sw.data$Airport.2.Capital <- as.factor(sw.data$Airport.2.Capital)
sw.data$Flight <- as.factor(sw.data$Flight)
str(sw.data)
table(sw.data$Flight)



# trainControl
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = 'cv',
                     number = 5,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     summaryFunction = fiveStats,
                     verboseIter = TRUE)



# k <- sample(nrow(sw.data), nrow(sw.data)*.7)
train <- sw.data
# evaluation <- sw.data[-k,]




# 1. Logistic Regression ---------------------------------------------------
set.seed(1)
glm.Fit <- train(Flight ~ ., data = train, 
                 method = 'glm', preProc = c('center', 'scale'),
                 trControl = ctrl, metric = 'ROC')



# 2. Linear Discriminant Analysis ------------------------------------------
set.seed(1)
lda.Fit <- train(Flight ~ ., data = train, 
                 method = 'lda', preProc = c('center', 'scale'),
                 trControl = ctrl, metric = 'ROC')


# 3. Random Forest ---------------------------------------------------------
set.seed(1)
rf.Fit <- train(Flight ~ ., data = train, 
                method = 'rf', preProc = c('center', 'scale'),
                tuneLength = 5,
                tuneGrid = expand.grid(.mtry = seq(10, 50, 10)),
                trControl = ctrl, metric = 'ROC')
# rf.tune.idx <- rf.Fit$pred$mtry == 20

# 4. Stochastic Gradient Boosting ------------------------------------------
gbmGrid <- expand.grid(.interaction.depth = seq(4, 10, 3),
                       .shrinkage = c(.001, .01, .1),
                       .n.minobsinnode = 10,
                       .n.trees = 1000)
set.seed(1)
gbm.Fit <- train(Flight ~ ., data = train, 
                 method = 'gbm', preProc = c('center', 'scale'),
                 tuneGrid = gbmGrid, bag.fraction = 0.5,
                 trControl = ctrl, metric = 'ROC')
# gbm.tune.idx <- gbm.Fit$pred$mtry == 20




# Comparison statistics ----------------------------------------------------
tmp <- data.frame(yes = glm.Fit$pred$yes,
                  obs = glm.Fit$pred$obs,
                  Model = rep('Logistic Regression', length(glm.Fit$pred$obs)))
tmp <- rbind(tmp,
             data.frame(yes = lda.Fit$pred$yes,
                        obs = lda.Fit$pred$obs,
                        Model = rep('Linear Discriminant Analysis', length(lda.Fit$pred$obs))))
tmp <- rbind(tmp,
             data.frame(yes = rf.Fit$pred$yes[rf.tune.idx],
                        obs = rf.Fit$pred$obs[rf.tune.idx],
                        Model = rep('Random Forest', length(rf.Fit$pred$obs[rf.tune.idx]))))
tmp <- rbind(tmp,
             data.frame(yes = gbm.Fit$pred$yes[gbm.tune.idx],
                        obs = gbm.Fit$pred$obs[gbm.tune.idx],
                        Model = rep('Stochastic Gradient Boosting', length(gbm.Fit$pred$obs[gbm.tune.idx]))))


tmp %>%
  ggplot(aes(m = yes, d = factor(obs, levels = c('yes', 'no')), color = Model)) +
  geom_roc(n.cuts = 0) +
  coord_equal() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.direction = 'vertical',
        legend.position = 'right') +
  xlab('1 - Specificity') + ylab('Sensivity') +
  guides(color = guide_legend(title = 'Scoring functions'))
  




