libs <- c('tidyverse', 'caret', 'e1071', 'pROC', 'rpart', 'kernlab', 'klaR', 
          'lubridate', 'ggplot2', 'ggthemes', 'ggpubr', 'xtable', 'reshape2', 'ggrepel')
lapply(libs, require, character.only = TRUE)
rm(libs)



# Load prepared dataset ---------------------------------------------
load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 2 - ML - Classification/Dataset - Alternate - 10 each of 100k and 10k-training sets.RData")




# ' ' ' trainControl ------------------------------------------------
# Wrapper fns for performance measures
# All measures
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = 'cv',
                     number = 5,
                     classProbs = TRUE,
                     summaryFunction = fiveStats,
                     verboseIter = FALSE)



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






# ' ' ' Alternate Cut-offs ------------------------------------------



# 1-A. DF to store Models & Performace Measures ---------------------
# DF to store model evaluation metric:
modEval.Alternate.Regular <- as.data.frame(matrix(ncol = 13))
colnames(modEval.Alternate.Regular) <- c('Approach', 'Method', 'k',
                                         'Train size', 'Evaluation size',
                                         'Accuracy', 'Kappa', 'Sensitivity', 'Specificity', 
                                         'F1', 'AUC', 'Threshold', 'Runtime (mins)')

# List of Models for each Method
models.Alternate.Regular <- list()


# 1-B. Model training: Alternatve Cut-offs --------------------------
# List of functions for each Method
models.list <- list(mod.glm, mod.lda, mod.qda, mod.fda)
models.name <- c('Logistic Regression', 'Linear Discriminant Analysis',
                 'Quadratic Discriminant Analysis', 'Flexible Discriminant Analysis')


# List of each k train & validation sets
nk <- 10
for (k in 1:nk) {
  # Update train & evaluation sets
  train <- dataset.list.100k[[2*k-1]]
  evaluation <- dataset.list.100k[[2*k]]
  
  # Train the models for each method: Approach: Alternate Cut-offs
  for (i in 1:length(models.list)) {
    
    print(paste0('k: ', k))
    print(models.name[i])
    
    start.time <- Sys.time()
    model <- models.list[[i]]
    metric <- model(train, evaluation, 'alternate')
    end.time <- Sys.time()
    run.time <- round(as.numeric(end.time - start.time, units = 'mins'),2)
    # Update Model list
    models.Alternate.Regular[[nk*(i-1) + k]] <- metric[[1]]
    names(models.Alternate.Regular)[nk*(i-1) + k] <- paste(models.name[i], k)
    # Update Performance measure data frame
    modEval.Alternate.Regular <- rbind(modEval.Alternate.Regular,
                                         c('Alternate Cut-offs', models.name[i], k, 
                                           nrow(train), nrow(evaluation),
                                           metric[[2]], run.time))
    
    print(run.time)
    
  }
}


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









# ' ' ' Alternate Cut-offs ------------------------------------------



# 2-A. DF to store Models & Performace Measures ---------------------
# DF to store model evaluation metric:
modEval.Alternate.ExpMethod <- as.data.frame(matrix(ncol = 13))
colnames(modEval.Alternate.ExpMethod) <- c('Approach', 'Method', 'k',
                                           'Train size', 'Evaluation size',
                                           'Accuracy', 'Kappa', 'Sensitivity', 'Specificity',
                                           'F1', 'AUC', 'Threshold', 'Runtime (mins)')

# List of Models for each Method
models.Alternate.ExpMethod <- list()



# 2-B. Model training: Alternatve Cut-offs --------------------------
# List of functions for each Method
models.list <- list(mod.nnet, mod.rf, mod.gbm, mod.knn)
models.name <- c('Neural Network', 'Random Forest',
                 'Gradient Boosting', 'k-NN')



# List of each k train & validation sets
nk <- 10
for (k in 1:nk) {
  # Update train & evaluation sets
  train <- dataset.list.10k[[2*k-1]]
  evaluation <- dataset.list.10k[[2*k]]
  
  # Train the models for each method: Approach: Alternate Cut-offs
  for (i in 1:length(models.list)) {
    
    print(paste0('k: ', k))
    print(models.name[i])
    
    start.time <- Sys.time()
    model <- models.list[[i]]
    metric <- model(train, evaluation, 'alternate')
    end.time <- Sys.time()
    run.time <- round(as.numeric(end.time - start.time, units = 'mins'),2)
    # Update Model list
    models.Alternate.ExpMethod[[nk*(i-1) + k]] <- metric[[1]]
    names(models.Alternate.ExpMethod)[nk*(i-1) + k] <- paste(models.name[i], k)
    # Update Performance measure data frame
    modEval.Alternate.ExpMethod <- rbind(modEval.Alternate.ExpMethod,
                                         c('Alternate Cut-offs', models.name[i], k, 
                                           nrow(train), nrow(evaluation),
                                           metric[[2]], run.time))
    
    print(run.time)
    
  }
}





View(modEval.Alternate.ExpMethod)
View(models.Alternate.ExpMethod)
print(object.size(models.Alternate.ExpMethod), units = 'Mb')
print(object.size(models.Alternate.Regular), units = 'Mb')

rm(ctrl, fiveStats, dataset.list.100k, dataset.list.10k, train, evaluation,
   models.list, models.name, metric, model,
   start.time, end.time, run.time, k, nk, 
   mod.fda, mod.gbm, mod.glm, mod.knn, mod.lda, mod.nnet, mod.qda, mod.rf,)



# ' ' ' Model Evaluation --------------------------------------------
View(modEval.Alternate.ExpMethod)
View(modEval.Alternate.Regular)
print(object.size(modEval.Alternate.Regular), units = 'Mb')
print(object.size(modEval.Alternate.ExpMethod), units = 'Mb')
print(object.size(models.Alternate.Regular), units = 'Mb')
print(object.size(models.Alternate.ExpMethod), units = 'Mb')

modEval.Alternate.Analysis <- modEval.Alternate.Regular[-1,]
modEval.Alternate.Analysis <- rbind(modEval.Alternate.Analysis,
                                    modEval.Alternate.ExpMethod[-1,])
rownames(modEval.Alternate.Analysis) <- 1:nrow(modEval.Alternate.Analysis)
# View(modEval.Alternate.Analysis)

str(modEval.Alternate.Analysis)
for (i in 3:ncol(modEval.Alternate.Analysis)) {
  modEval.Alternate.Analysis[,i] <- as.numeric(modEval.Alternate.Analysis[,i])
}

View(modEval.Alternate.Analysis)
saveRDS(modEval.Alternate.Analysis, 
        file = 'Alternate Cut-offs - All Models Statistics.rds')



# 5-A. Statistical Summary of Performance metrics -------------------
t <- qt(.975, 9, lower = TRUE)
tmp <- modEval.Alternate.Analysis %>%
  group_by(Method) %>%
  mutate(mean_AUC = mean(AUC), sd_AUC = sd(AUC),
         CI_Low_AUC = mean_AUC - t*sd_AUC, CI_Upp_AUC = mean_AUC + t*sd_AUC,
         mean_Acc = mean(Accuracy), sd_Acc = sd(Accuracy),
         CI_Low_Acc = mean_Acc - t*sd_Acc, CI_Upp_Acc = mean_Acc + t*sd_Acc,
         mean_Sen = mean(Sensitivity), sd_Sen = sd(Sensitivity),
         CI_Low_Sen = mean_Sen - t*sd_Sen, CI_Upp_Sen = mean_Sen + t*sd_Sen,
         mean_Spe = mean(Specificity), sd_Spe = sd(Specificity),
         CI_Low_Spe = mean_Spe - t*sd_Spe, CI_Upp_Spe = mean_Spe + t*sd_Spe,
         mean_Kap = mean(Kappa), sd_Kap = sd(Kappa),
         CI_Low_Kap = mean_Kap - t*sd_Kap, CI_Upp_Kap = mean_Kap + t*sd_Kap,
         mean_F1 = mean(F1), sd_F1 = sd(F1),
         CI_Low_F1 = mean_F1 - t*sd_F1, CI_Upp_F1 = mean_F1 + t*sd_F1,
         mean_Thr = mean(Threshold), sd_Thr = sd(Threshold),
         CI_Low_Thr = mean_Thr - t*sd_Thr, CI_Upp_Thr = mean_Thr + t*sd_Thr) %>%
  filter(row_number()==1)


# View(tmp)
# names(modEval.Alternate.Analysis)
rm(t)







# ' ' ' Statistical Summary Plot ------------------------------------
tmp$Method.S <- tmp$Method
tmp$Method.S[2:4] <- c('Linear DA', 'Quadratic DA', 'Flexible DA')
  


# 6-A. AUC ----------------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_AUC) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Logistic Regression', 'Purple', 
                 ifelse(tmp.Col$Method == 'Linear Discriminant Analysis', 'Blue', 
                        ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Brown', 'Black')))
p.auc <- tmp %>%
  dplyr::select(Method.S, mean_AUC, CI_Low_AUC, CI_Upp_AUC) %>%
  ggplot(aes(x = reorder(Method.S, mean_AUC), y = mean_AUC)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_AUC, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_AUC, ymax = CI_Upp_AUC), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.2, .9)) +
  labs(subtitle = 'Statistic: AUC') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean AUC') + xlab('')

# p.auc + labs(subtitle = 'Alternate Cut-offs')


# 6-B. Accuracy -----------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Acc) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Logistic Regression', 'Purple', 
                 ifelse(tmp.Col$Method == 'Linear Discriminant Analysis', 'Blue', 
                        ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Brown', 'Black')))
p.acc <- tmp %>%
  dplyr::select(Method.S, mean_Acc, CI_Low_Acc, CI_Upp_Acc) %>%
  ggplot(aes(x = reorder(Method.S, mean_Acc), y = mean_Acc)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Acc, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Acc, ymax = CI_Upp_Acc), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.2, .9)) +
  labs(subtitle = 'Statistic: Accuracy') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Accuracy') + xlab('')

# p.acc + labs(subtitle = 'Alternate Cut-offs')



# 6-C. Sensitivity --------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Sen) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Logistic Regression', 'Purple', 
                 ifelse(tmp.Col$Method == 'Linear Discriminant Analysis', 'Blue', 
                        ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Brown', 'Black')))
p.sen <- tmp %>%
  dplyr::select(Method.S, mean_Sen, CI_Low_Sen, CI_Upp_Sen) %>%
  ggplot(aes(x = reorder(Method.S, mean_Sen), y = mean_Sen)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Sen, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Sen, ymax = CI_Upp_Sen), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.2, .9)) +
  labs(subtitle = 'Statistic: Sensitivity') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Sensitivity') + xlab('')

# p.sen + labs(subtitle = 'Alternate Cut-offs')



# 6-D. Specificity --------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Spe) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Logistic Regression', 'Purple', 
                 ifelse(tmp.Col$Method == 'Linear Discriminant Analysis', 'Blue', 
                        ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Brown', 'Black')))
p.spe <- tmp %>%
  dplyr::select(Method.S, mean_Spe, CI_Low_Spe, CI_Upp_Spe) %>%
  ggplot(aes(x = reorder(Method.S, mean_Spe), y = mean_Spe)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Spe, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Spe, ymax = CI_Upp_Spe), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.2, .9)) +
  labs(subtitle = 'Statistic: Specificity') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Specificity') + xlab('')

# p.spe + labs(subtitle = 'Alternate Cut-offs')



# 6-E. Kappa --------------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Kap) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Logistic Regression', 'Purple', 
                 ifelse(tmp.Col$Method == 'Linear Discriminant Analysis', 'Blue', 
                        ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Brown', 'Black')))
p.kap <- tmp %>%
  dplyr::select(Method.S, mean_Kap, CI_Low_Kap, CI_Upp_Kap) %>%
  ggplot(aes(x = reorder(Method.S, mean_Kap), y = mean_Kap)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Kap, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Kap, ymax = CI_Upp_Kap), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(-.01, .05)) +
  labs(subtitle = 'Statistic: Kappa') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean Kappa') + xlab('')

# p.kap + labs(subtitle = 'Alternate Cut-offs')



# 6-F. F1 -----------------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_F1) %>%
  dplyr::select(Method)
tmp.Col = ifelse(tmp.Col$Method == 'Logistic Regression', 'Purple', 
                 ifelse(tmp.Col$Method == 'Linear Discriminant Analysis', 'Blue', 
                        ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Brown', 'Black')))
p.f1 <- tmp %>%
  dplyr::select(Method.S, mean_F1, CI_Low_F1, CI_Upp_F1) %>%
  ggplot(aes(x = reorder(Method.S, mean_F1), y = mean_F1)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_F1, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_F1, ymax = CI_Upp_F1), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.02, .1)) +
  labs(subtitle = 'Statistic: F1') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean F1') + xlab('')

# p.f1 + labs(subtitle = 'Alternate Cut-offs')





# 6-G. Arrange all plots --------------------------------------------
figure <- ggarrange(p.auc, p.acc,
                    p.sen, p.spe,
                    p.kap, p.f1,
                    ncol = 2, nrow = 3)  
annotate_figure(figure,
                # top = text_grob('Alternate Cut-offs: Method Comparison',
                #                 color = 'black', face = 'bold', size = 15)
                )






# 6-H. Statistics Summary -------------------------------------------
stat.sum <- tmp %>%
  dplyr::select(Approach, Method, mean_AUC, mean_Acc, mean_Sen, mean_Spe, mean_Kap, mean_F1, mean_Thr)
names(stat.sum)

stat.sum[,3:ncol(stat.sum)] <- sapply(stat.sum[,3:ncol(stat.sum)], function(x) round(x,3))

xtable(stat.sum, digits=c(0,0,0,rep(3,7)))
print(xtable(stat.sum, digits=c(0,0,0,rep(3,7)), type = 'latex'), 
      file = "Alternate Cut-offs - Method Summary.tex")



rm(p.acc, p.auc, p.sen, p.spe, p.kap, p.f1, figure)
rm(tmp, stat.num, tmp.Col, t, x)


# Model Selection ---------------------------------------------------




# 7-A. Compute the 'Closest' models per method ----------------------
t <- qt(.975, 9, lower = TRUE)
tmp <- modEval.Alternate.Analysis %>%
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
  filter(row_number() == 1) %>%
  mutate(Model = paste(Method, k)) %>%
  dplyr::select(Model)

x <- c('Logistic Regression', 'Linear Discriminant Analysis',
       'Quadratic Discriminant Analysis', 'Flexible Discriminant Analysis', 
       'Neural Network', 'Random Forest', 'Gradient Boosting', 'k-NN')

tmp.Reduced <- tmp.Reduced[match(x, tmp.Reduced$Method),]$Model


models.Alternate.BestModels <- list()
for (i in c(1:4)) {
  models.Alternate.BestModels[[i]] <- models.Alternate.Regular[tmp.Reduced[i]][[1]]
  names(models.Alternate.BestModels)[i] <- tmp.Reduced[i]
}
for (i in c(5:8)) {
  models.Alternate.BestModels[[i]] <- models.Alternate.ExpMethod[tmp.Reduced[i]][[1]]
  names(models.Alternate.BestModels)[i] <- tmp.Reduced[i]
}


View(models.Alternate.BestModels)
rm(i,k,t, tmp.Reduced)


print(object.size(models.Alternate.BestModels), units = 'Mb')
saveRDS(models.Alternate.BestModels, 
        file = 'Alternate Cut-offs - Best Models per Algorithms.rds')





# Performance of top 3 methods --------------------------------------
# dist = sqrt( 3|Sen - Ave(Sen)| + 2|AUC - Ave(AUC)| + |F1 - Ave(F1)| )
x <- c('Logistic Regression', 'Linear Discriminant Analysis', 'Quadratic Discriminant Analysis')
tmp.Reduced <- tmp %>%
  group_by(Method) %>%
  mutate(distance = sqrt(3*abs(Sensitivity - mean_Sen) + 2*abs(AUC - mean_AUC) + abs(F1 - mean_F1))) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  mutate(Model = paste(Method, k)) %>%
  dplyr::select(Model, k) %>%
  filter(Method %in% x)


# Narrow down the top 3 methods
top3.metric <- merge(tmp.Reduced, modEval.Alternate.Analysis)
names(top3.metric)



p.1 <- top3.metric %>%
  dplyr::select(Method, AUC, Accuracy, Sensitivity, Specificity) %>%
  melt(id.vars='Method') %>%
  mutate(Method.Ord = factor(Method, levels = x)) %>%
  dplyr::select(Method.Ord, variable, value) %>%
  ggplot(aes(x = Method.Ord, y = value, color = variable, group = variable)) + 
  geom_point(size = 5) + geom_line(size = 1) +
  geom_text_repel(aes(label = round(value, 3)), vjust = -1, hjust = -.5, size = 3, 
                  color = 'black', fontface = 'bold') +
  scale_y_continuous(limits = c(.6, .75)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank()) + 
  ylab('') + xlab('')




p.2 <- top3.metric %>%
  dplyr::select(Method, Kappa, F1, Threshold) %>%
  melt(id.vars='Method') %>%
  mutate(Method.Ord = factor(Method, levels = x)) %>%
  dplyr::select(Method.Ord, variable, value) %>%
  ggplot(aes(x = Method.Ord, y = value, color = variable, group = variable)) + 
  geom_point(size = 5) + geom_line(size = 1) +
  geom_text_repel(aes(label = round(value, 3)), vjust = -1, hjust = -.5, size = 3, 
                  color = 'black', fontface = 'bold') +
  scale_y_continuous(limits = c(.0, .1)) +
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
                # top = text_grob('Alternate Cut-offs: Top 3 methods',
                #                 color = 'black', face = 'bold', size = 15)
)

ncol(top3.metric)

top3.metric <- top3.metric %>%
  dplyr::select(Method, k, Approach, AUC, Accuracy, Sensitivity, Specificity, Kappa, F1, Threshold)

head(top3.metric)
print(xtable(top3.metric, 
             digits = c(rep(0,4), rep(3,7)), type = 'latex'), 
      file = "Alternate Cut-offs - Top 3 Methods.tex")


print(xtable(modEval.Alternate.Analysis, 
             digits = c(rep(0,6), rep(3,8)), type = 'latex'), 
      file = "[Stock] --- Alternate Cut-offs - All methods and models.tex")



write.csv(modEval.Alternate.Analysis, 
          file = '[Stock] --- Alternate Cut-offs - All Methods and Models Metrics.csv')


# saveRDS(models.Alternate.Regular, 
#         file = '[Stock] --- Alternate Cut-offs - Regular Algorithms.rds')
# saveRDS(models.Alternate.Regular, 
#         file = '[Stock] --- Alternate Cut-offs - Regular Algorithms.rds')

########################## SCRATCH ##################################






# Next time: To load models
`Alternate Cut-offs - Best Models per Algorithms` <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Alternate Cut-offs - Best Models per Algorithms.rds")
models.Alternate.BestModels <- `Alternate Cut-offs - Best Models per Algorithms`
`Alternate Cut-offs - All Models Statistics` <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Alternate Cut-offs - All Models Statistics.rds")
modEval.Alternate.Analysis <- `Alternate Cut-offs - All Models Statistics`
rm(`Alternate Cut-offs - All Models Statistics`, `Alternate Cut-offs - Best Models per Algorithms`)













