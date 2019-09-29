libs <- c('tidyverse', 'caret', 'e1071', 'pROC', 'rpart', 'kernlab', 'klaR', 
          'lubridate', 'ggplot2', 'ggthemes', 'ggpubr', 'xtable', 'reshape2', 'ggrepel')
lapply(libs, require, character.only = TRUE)
rm(libs)



# Load Test set -----------------------------------------------------
load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 2 - ML - Classification/Dataset - Test set.RData")




# 1. Alternate Cut-offs ---------------------------------------------
alternate.alg.all <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Alternate Cut-offs - Best Models per Algorithms.rds")

alternate.alg.top.3 <- list()
tmp.names <- c('Logistic Regression', 'Linear Discriminant Analysis', 'Quadratic Discriminant Analysis')
for (i in 1:3) {
  idx <- which(sapply(tmp.names[i], grepl, names(alternate.alg.all)))
  alternate.alg.top.3[[i]] <- alternate.alg.all[[idx]]
  names(alternate.alg.top.3)[i] <- tmp.names[i]
}
# View(alternate.alg.top.3)





# 2. Down-sampling --------------------------------------------------
downSampling.alg.all <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Down-Sampling - Best Models per Algorithms.rds")

downSampling.alg.top.4 <- list()
tmp.names <- c('Quadratic Discriminant Analysis', 'Neural Network', 'Random Forest', 'Gradient Boosting')
for (i in 1:4) {
  idx <- which(sapply(tmp.names[i], grepl, names(downSampling.alg.all)))
  downSampling.alg.top.4[[i]] <- downSampling.alg.all[[idx]]
  names(downSampling.alg.top.4)[i] <- tmp.names[i]
}
# View(downSampling.alg.top.4)








# 3. Cost-sensitive Training ----------------------------------------
caseWgts.alg.all <- readRDS("D:/Data Science Projects/Airport/Air traffic 2008 - 2 - ML - Classification/Cost-sensitive Training - Best Models per Algorithms.rds")
tmp <- caseWgts.alg.all[1:4]
caseWgts.alg.top.3 <- list()
tmp.names <- c('50', '75', '100')
for (i in 1:3) {
  idx <- which(sapply(tmp.names[i], grepl, names(tmp)))
  caseWgts.alg.top.3[[i]] <- tmp[[idx]]
  names(caseWgts.alg.top.3)[i] <- paste0('SVM: Linear; Weight = ', tmp.names[i])
}
# View(caseWgts.alg.top.3)

rm(tmp, i, idx, tmp.names)
rm(alternate.alg.all, downSampling.alg.all, caseWgts.alg.all)

print(object.size(alternate.alg.top.3), units = 'Mb')
print(object.size(downSampling.alg.top.4), units = 'Mb')
print(object.size(caseWgts.alg.top.3), units = 'Mb')






# Testing -----------------------------------------------------------
testing <- function(model, testset, approach) {
  # Prediction
  tmp.Pred <- predict(model, newdata = testset, type = 'prob')[,1]
  tmp.ROC <- roc(testset$CancelledF, tmp.Pred,
                levels = levels(testset$CancelledF))
  
  ############################### Choose appropriate Cuf-off Threshold
  if (approach == 'sampling') {
    tmp.Thres <- .5
  } else if (approach == 'alternate') {
    tmp.Thres <- coords(tmp.ROC, x = 'best', best.method = 'closest.topleft')[['threshold']]
  } ############################################################## End
  
  tmp.PredF <- factor(ifelse(tmp.Pred > tmp.Thres, 'cancelled', 'notcancelled'),
                      levels = c('cancelled', 'notcancelled'))
  # Confusion matrix for current model
  cfMat <- confusionMatrix(tmp.PredF, testset$CancelledF, positive = 'cancelled', mode = 'everything')
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- model
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']], 
                         tmp.ROC$auc, tmp.Thres), 4)
  metric[[3]] <- cfMat
  return(metric)
}



tmp.model <- alternate.alg.top.3[[1]]
tmp <- testing(tmp.model, test, 'alternate')
tmp.model.2 <- downSampling.alg.top.4[[1]]
tmp.2 <- testing(tmp.model.2, test, 'sampling')



testing.caseWgts <- function(model, testset) {
  # Prediction
  tmp.Pred <- predict(model, newdata = testset)
  cfMat <- confusionMatrix(tmp.Pred, testset$CancelledF, positive = 'cancelled', mode = 'everything')
  # Confusion matrix: Performance matrix
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- model
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']],
                         NA, NA), 4)
  metric[[3]] <- cfMat
  return(metric)
}


tmp.model.3 <- caseWgts.alg.top.3[[1]]
tmp.3 <- testing.caseWgts(tmp.model.3, test)












# Running test set --------------------------------------------------


# prepare df to store results
model.Results <- as.data.frame(matrix(ncol = 9))
colnames(model.Results) <- c('Approach', 'Method', 
                             'Accuracy', 'Kappa', 'Sensitivity', 'Specificity', 
                             'F1', 'AUC', 'Threshold')

allmodel.allstats <- list()


# Prediction results on test set: Alternate Cut-offs
for (i in 1:length(alternate.alg.top.3)) {
  # Get current model
  model <- alternate.alg.top.3[[i]]
  # Evaluate current model on test set
  metric <- testing(model, test, 'alternate')
  # Update Model list
  allmodel.allstats[[i]] <- metric
  names(allmodel.allstats)[i] <- paste0('Alternate Cut-offs; ', names(alternate.alg.top.3)[i])
  # Update Performance measure data frame
  model.Results <- rbind(model.Results,
                         c('Alternate Cut-offs', names(alternate.alg.top.3)[i], metric[[2]]))
}
View(allmodel.allstats)


# Prediction results on test set: Resampling
k <- length(allmodel.allstats)
for (i in 1:length(downSampling.alg.top.4)) {
  # Get current model
  model <- downSampling.alg.top.4[[i]]
  # Evaluate current model on test set
  metric <- testing(model, test, 'sampling')
  # Update Model list
  allmodel.allstats[[k+i]] <- metric
  names(allmodel.allstats)[k+i] <- paste0('Resampling; ', names(downSampling.alg.top.4)[i])
  # Update Performance measure data frame
  model.Results <- rbind(model.Results,
                         c('Resampling', names(downSampling.alg.top.4)[i], metric[[2]]))
}
View(allmodel.allstats)
View(model.Results)



# Prediction results on test set: Cost-sensitive
k <- length(allmodel.allstats)
for (i in 1:length(caseWgts.alg.top.3)) {
  # Get current model
  model <- caseWgts.alg.top.3[[i]]
  # Evaluate current model on test set
  metric <- testing.caseWgts(model, test)
  # Update Model list
  allmodel.allstats[[k+i]] <- metric
  names(allmodel.allstats)[k+i] <- paste0('Cost-sensitive Training; ', names(caseWgts.alg.top.3)[i])
  # Update Performance measure data frame
  model.Results <- rbind(model.Results,
                         c('Cost-sensitive Training', names(caseWgts.alg.top.3)[i], metric[[2]]))
}
View(allmodel.allstats)
View(model.Results)









