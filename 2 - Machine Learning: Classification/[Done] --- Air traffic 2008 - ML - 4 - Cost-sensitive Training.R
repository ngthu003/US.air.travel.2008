libs <- c('tidyverse', 'caret', 'e1071', 'pROC', 'rpart', 'kernlab', 'klaR', 
          'lubridate', 'ggplot2', 'ggthemes', 'ggpubr', 'xtable', 'reshape2', 'ggrepel')
lapply(libs, require, character.only = TRUE)
rm(libs)



# Load prepared dataset ---------------------------------------------
load("D:/Data Science Projects/US Air Travel in 2008/Air traffic 2008 - 2 - ML - Classification/Dataset - Alternate - 10 each of 100k and 10k-training sets.RData")



# ' ' ' trainControl ------------------------------------------------
# Wrapper fns for performance measures
# All measures
stats <- function (data, lev = NULL, model = NULL)  {
  c(postResample(data[, "pred"], data[, "obs"]),
    Sens = sensitivity(data[, "pred"], data[, "obs"]),
    Spec = specificity(data[, "pred"], data[, "obs"]))
}

ctrl <- trainControl(method = "cv", number = 5,
                     summaryFunction = stats)



mod.svmLinear <- function(train, evaluation, weight) {  #------------------ SVM: Linear kernel
  # Fit Model
  svmLinearGrid <- expand.grid(.C = 2^seq(-4, 1, length = 6))
  set.seed(1)
  svmLinearWgt.Fit <- train(CancelledF ~ ., data = train,
                            preProc = c('center', 'scale'),
                            method = 'svmLinear',
                            tuneGrid = svmLinearGrid,
                            metric = 'Kappa',
                            trControl = ctrl,
                            class.weights = weight)
  # Prediction
  svmLinearWgt.Pred <- predict(svmLinearWgt.Fit, newdata = evaluation)
  cfMat <- confusionMatrix(svmLinearWgt.Pred, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  # Confusion matrix: Performance matrix
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- svmLinearWgt.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']]), 4)
  #metric[[3]] <- cfMat
  return(metric)
} 




mod.svmRadial <- function(train, evaluation, weight) {  #------------------ SVM: Radial kernel
  # Fit Model
  sigma <- sigest(CancelledF ~ ., data = train, frac = 1)
  names(sigma) <- NULL
  svmRadialGrid <- expand.grid(.sigma = sigma[[2]],
                               .C = 2^seq(-4, 1, length = 6))
  set.seed(1)
  svmRadialWgt.Fit <- train(CancelledF ~ ., data = train,
                            preProc = c('center', 'scale'),
                            method = 'svmRadial',
                            tuneGrid = svmRadialGrid,
                            metric = 'Kappa',
                            trControl = ctrl,
                            class.weights = weight)
  # Prediction
  svmRadialWgt.Pred <- predict(svmRadialWgt.Fit, newdata = evaluation)
  cfMat <- confusionMatrix(svmRadialWgt.Pred, evaluation$CancelledF, positive = 'cancelled', mode = 'everything')
  # Confusion matrix: Performance matrix
  cfMat.Over <- cfMat$overall
  cfMat.Class <- cfMat$byClass
  # Model Performance Metric
  metric <- list()
  metric[[1]] <- svmRadialWgt.Fit
  metric[[2]] <- round(c(cfMat.Over[['Accuracy']], cfMat.Over[['Kappa']], 
                         cfMat.Class[['Sensitivity']], cfMat.Class[['Specificity']], 
                         cfMat.Class[['F1']]), 4)
  #metric[[3]] <- cfMat
  return(metric)
}





# ' ' ' Cost-sensitive training -------------------------------------


# 2-A. DF to store Models & Performace Measures ---------------------
# DF to store model evaluation metric:
modEval.Case.wgts.Linear <- as.data.frame(matrix(ncol = 12))
colnames(modEval.Case.wgts.Linear) <- c('Approach', 'Method', 'k', 'Case weights',
                                        'Train size', 'Evaluation size',
                                        'Accuracy', 'Kappa', 'Sensitivity', 'Specificity',
                                        'F1', 'Runtime (mins)')

# List of Models for each Method
models.Case.wgts.Linear <- list()



# 2-B. Model training: Alternatve Cut-offs --------------------------
# List of functions for each Method
models.list <- list(mod.svmLinear, mod.svmRadial)
models.name <- c('SVM: Linear', 'SVM: Radial')



# SVM: Linear -------------------------------------------------------
# List of each k train & validation sets
nk <- 10
for (k in 1:nk) {
  # Update train & evaluation sets
  train <- dataset.list.10k[[2*k-1]]
  evaluation <- dataset.list.10k[[2*k]]
  
  weights.Range <- c(1, 10, 25, 50, 75, 100)
  # Loop through different case weights
  for (i in 1:length(weights.Range)) {
    
    model <- models.list[[1]]
    mod.name <- models.name[1]
    
    print(paste0('k: ', k))
    print(mod.name)
    
    tmp.wgt <- c(cancelled = weights.Range[i], notcancelled = 1)
    print(tmp.wgt)
    
    start.time <- Sys.time()
    # model <- models.list[[i]]
    metric <- model(train, evaluation, tmp.wgt)
    end.time <- Sys.time()
    run.time <- round(as.numeric(end.time - start.time, units = 'mins'),2)
    # Update Model list
    models.Case.wgts.Linear[[nk*(i-1) + k]] <- metric[[1]]
    names(models.Case.wgts.Linear)[nk*(i-1) + k] <- paste0(mod.name, ': k = ', k, '; Weight = ', weights.Range[i])
    # Update Performance measure data frame
    modEval.Case.wgts.Linear <- rbind(modEval.Case.wgts.Linear,
                                      c('Cost-sensitive Training', mod.name, k, weights.Range[i],
                                        nrow(train), nrow(evaluation),
                                        metric[[2]], run.time))
    print(run.time)
  }
}

# View(models.Case.wgts.Linear)
# View(modEval.Case.wgts.Linear)



# SVM: Radial -------------------------------------------------------
# DF to store model evaluation metric:
modEval.Case.wgts.Radial <- as.data.frame(matrix(ncol = 12))
colnames(modEval.Case.wgts.Radial) <- c('Approach', 'Method', 'k', 'Case weights',
                                        'Train size', 'Evaluation size',
                                        'Accuracy', 'Kappa', 'Sensitivity', 'Specificity',
                                        'F1', 'Runtime (mins)')

# List of Models for each Method
models.Case.wgts.Radial <- list()



# 2-B. Model training: Alternatve Cut-offs --------------------------
# List of functions for each Method
models.list <- list(mod.svmLinear, mod.svmRadial)
models.name <- c('SVM: Linear', 'SVM: Radial')

# List of each k train & validation sets
nk <- 10
for (k in 1:nk) {
  # Update train & evaluation sets
  train <- dataset.list.10k[[2*k-1]]
  evaluation <- dataset.list.10k[[2*k]]
  
  weights.Range <- c(1, 50, 100)
  # Loop through different case weights
  for (i in 1:length(weights.Range)) {
    
    model <- models.list[[2]]
    mod.name <- models.name[2]
    
    print(paste0('k: ', k))
    print(mod.name)
    
    tmp.wgt <- c(cancelled = weights.Range[i], notcancelled = 1)
    print(tmp.wgt)
    
    start.time <- Sys.time()
    # model <- models.list[[i]]
    metric <- model(train, evaluation, tmp.wgt)
    end.time <- Sys.time()
    run.time <- round(as.numeric(end.time - start.time, units = 'mins'),2)
    # Update Model list
    models.Case.wgts.Radial[[nk*(i-1) + k]] <- metric[[1]]
    names(models.Case.wgts.Radial)[nk*(i-1) + k] <- paste0(mod.name, ': k = ', k, '; Weight = ', weights.Range[i])
    # Update Performance measure data frame
    modEval.Case.wgts.Radial <- rbind(modEval.Case.wgts.Radial,
                                      c('Cost-sensitive Training', mod.name, k, weights.Range[i],
                                        nrow(train), nrow(evaluation),
                                        metric[[2]], run.time))
    print(run.time)
  }
}




# 2-C. Models -------------------------------------------------------
View(models.Case.wgts.Linear)
View(modEval.Case.wgts.Linear)
View(models.Case.wgts.Radial)
View(modEval.Case.wgts.Radial)




# ' ' ' Model Evaluation --------------------------------------------
View(modEval.Case.wgts.Linear)
View(modEval.Case.wgts.Radial)
print(object.size(modEval.Case.wgts.Linear), units = 'Mb')
print(object.size(modEval.Case.wgts.Radial), units = 'Mb')
print(object.size(models.Case.wgts.Linear), units = 'Mb')
print(object.size(models.Case.wgts.Radial), units = 'Mb')

modEval.Case.wgts.Analysis <- modEval.Case.wgts.Linear[-1,]
modEval.Case.wgts.Analysis <- rbind(modEval.Case.wgts.Analysis,
                                    modEval.Case.wgts.Radial[-1,])
rownames(modEval.Case.wgts.Analysis) <- 1:nrow(modEval.Case.wgts.Analysis)
# View(modEval.Case.wgts.Analysis)

str(modEval.Case.wgts.Analysis)
for (i in 3:ncol(modEval.Case.wgts.Analysis)) {
  modEval.Case.wgts.Analysis[,i] <- as.numeric(modEval.Case.wgts.Analysis[,i])
}
# saveRDS(modEval.Case.wgts.Analysis, 
#         file = 'Cost-sensitive Training - All Models Statistics.rds')

modEval.Case.wgts.Analysis
?na.omit

modEval.Case.wgts.Analysis.NA.omit <- na.omit(modEval.Case.wgts.Analysis)


# 5-A. Statistical Summary of Performance metrics -------------------
t <- qt(.975, 9, lower = TRUE)
tmp <- modEval.Case.wgts.Analysis.NA.omit %>%
  mutate(Method.L = paste0(Method, ', wgt: ', `Case weights`)) %>%
  group_by(Method.L) %>%
  mutate(mean_Acc = mean(Accuracy), sd_Acc = ifelse(is.na(sd(Accuracy)) == TRUE, 0, sd(Accuracy)),
         CI_Low_Acc = max(0, mean_Acc - t*sd_Acc), CI_Upp_Acc = min(1, mean_Acc + t*sd_Acc),
         mean_Sen = mean(Sensitivity), sd_Sen = ifelse(is.na(sd(Sensitivity)) == TRUE, 0, sd(Sensitivity)),
         CI_Low_Sen = max(0, mean_Sen - t*sd_Sen), CI_Upp_Sen = min(1, mean_Sen + t*sd_Sen),
         mean_Spe = mean(Specificity), sd_Spe = ifelse(is.na(sd(Specificity)) == TRUE, 0, sd(Specificity)),
         CI_Low_Spe = max(0, mean_Spe - t*sd_Spe), CI_Upp_Spe = min(1, mean_Spe + t*sd_Spe),
         mean_Kap = mean(Kappa), sd_Kap = ifelse(is.na(sd(Kappa)) == TRUE, 0, sd(Kappa)),
         CI_Low_Kap = max(0, mean_Kap - t*sd_Kap), CI_Upp_Kap = min(1, mean_Kap + t*sd_Kap),
         mean_F1 = mean(F1), sd_F1 = ifelse(is.na(sd(F1)) == TRUE, 0, sd(F1)),
         CI_Low_F1 = max(0, mean_F1 - t*sd_F1), CI_Upp_F1 = min(1, mean_F1 + t*sd_F1)) %>%
  filter(row_number()==1)


names(tmp)
View(tmp)
names(modEval.Case.wgts.Analysis)
rm(i, t)





# ' ' ' Statistical Summary Plot ------------------------------------


# 6-A. AUC ----------------------------------------------------------
# tmp.Col <- tmp %>%
#   arrange(mean_AUC) %>%
#   dplyr::select(Method)
# tmp.Col = ifelse(tmp.Col$Method == 'Gradient Boosting', 'Purple', 
#                  ifelse(tmp.Col$Method == 'Neural Network', 'Blue', 
#                         ifelse(tmp.Col$Method == 'Random Forest', 'Brown', 
#                                ifelse(tmp.Col$Method == 'Quadratic Discriminant Analysis', 'Red', 'Black'))))
# p.auc <- tmp %>%
#   dplyr::select(Method.S, mean_AUC, CI_Low_AUC, CI_Upp_AUC) %>%
#   ggplot(aes(x = reorder(Method.S, mean_AUC), y = mean_AUC)) +
#   geom_point(size = 2, color = '#336699') +
#   geom_text(aes(label = round(mean_AUC, 3)), vjust = -1, size = 4, 
#             color = '#336699', fontface = 'bold') +
#   geom_errorbar(aes(ymin = CI_Low_AUC, ymax = CI_Upp_AUC), 
#                 size = 1, width = .2, color = '#336699') +
#   scale_y_continuous(limits = c(.5, .8)) +
#   labs(subtitle = 'Statistic: AUC') +
#   coord_flip() +
#   theme_fivethirtyeight() +
#   theme(axis.text.y = element_text(color = tmp.Col),
#         plot.background = element_rect(fill = 'white'),
#         panel.background = element_rect(fill = 'white')) + 
#   ylab('Mean AUC') + xlab('')

# p.auc + labs(subtitle = 'Down-Sampling: Method Comparison')


# 6-B. Accuracy -----------------------------------------------------
tmp.Col <- tmp %>%
  arrange(mean_Acc) %>%
  dplyr::select(Method.L)
tmp.Col = ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 75', 'Purple', 
                 ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 50', 'Blue', 
                        ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 100', 'Brown', 'Black')))
p.acc <- tmp %>%
  dplyr::select(Method.L, mean_Acc, CI_Low_Acc, CI_Upp_Acc) %>%
  ggplot(aes(x = reorder(Method.L, mean_Acc), y = mean_Acc)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Acc, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Acc, ymax = CI_Upp_Acc), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.0, 1)) +
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
  dplyr::select(Method.L)
tmp.Col = ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 75', 'Purple', 
                 ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 50', 'Blue', 
                        ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 100', 'Brown', 'Black')))
p.sen <- tmp %>%
  dplyr::select(Method.L, mean_Sen, CI_Low_Sen, CI_Upp_Sen) %>%
  ggplot(aes(x = reorder(Method.L, mean_Sen), y = mean_Sen)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Sen, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Sen, ymax = CI_Upp_Sen), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.0, 1)) +
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
  dplyr::select(Method.L)
tmp.Col = ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 75', 'Purple', 
                 ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 50', 'Blue', 
                        ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 100', 'Brown', 'Black')))
p.spe <- tmp %>%
  dplyr::select(Method.L, mean_Spe, CI_Low_Spe, CI_Upp_Spe) %>%
  ggplot(aes(x = reorder(Method.L, mean_Spe), y = mean_Spe)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Spe, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Spe, ymax = CI_Upp_Spe), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.0, 1)) +
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
  dplyr::select(Method.L)
tmp.Col = ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 75', 'Purple', 
                 ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 50', 'Blue', 
                        ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 100', 'Brown', 'Black')))
p.kap <- tmp %>%
  dplyr::select(Method.L, mean_Kap, CI_Low_Kap, CI_Upp_Kap) %>%
  ggplot(aes(x = reorder(Method.L, mean_Kap), y = mean_Kap)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_Kap, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_Kap, ymax = CI_Upp_Kap), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.0, .1)) +
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
  dplyr::select(Method.L)
tmp.Col = ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 75', 'Purple', 
                 ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 50', 'Blue', 
                        ifelse(tmp.Col$Method.L == 'SVM: Linear, wgt: 100', 'Brown', 'Black')))
p.f1 <- tmp %>%
  dplyr::select(Method.L, mean_F1, CI_Low_F1, CI_Upp_F1) %>%
  ggplot(aes(x = reorder(Method.L, mean_F1), y = mean_F1)) +
  geom_point(size = 2, color = '#336699') +
  geom_text(aes(label = round(mean_F1, 3)), vjust = -1, size = 4, 
            color = '#336699', fontface = 'bold') +
  geom_errorbar(aes(ymin = CI_Low_F1, ymax = CI_Upp_F1), 
                size = 1, width = .2, color = '#336699') +
  scale_y_continuous(limits = c(.0, .2)) +
  labs(subtitle = 'Statistic: F1') +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_text(color = tmp.Col),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white')) + 
  ylab('Mean F1') + xlab('')

# p.f1 + labs(subtitle = 'Down-Sampling: Method Comparison')


p.blank <- ggplot() +
  theme_fivethirtyeight() +
  theme_void()
  


# 6-G. Arrange all plots --------------------------------------------
figure <- ggarrange(p.blank, p.acc,
                    p.sen, p.spe,
                    p.kap, p.f1,
                    ncol = 2, nrow = 3)  
annotate_figure(figure,
                # top = text_grob('Down-Sampling: Method Comparison',
                #                 color = 'black', face = 'bold', size = 15)
)



# 6-H. Statistics Summary -------------------------------------------
stat.sum <- tmp %>%
  dplyr::select(Approach, Method, `Case weights`, mean_Acc, mean_Sen, mean_Spe, mean_Kap, mean_F1)
names(stat.sum)

stat.sum[,5:ncol(stat.sum)] <- sapply(stat.sum[,5:ncol(stat.sum)], function(x) round(x,3))

xtable(stat.sum, digits=c(rep(0,5),rep(3,5)))
print(xtable(stat.sum, digits=c(rep(0,5),rep(3,5)), type = 'latex'), 
      file = "Cost-sensitive Training - Method Summary.tex")



# rm(p.acc, p.auc, p.sen, p.spe, p.kap, p.f1, figure)
# rm(tmp, stat.sum, tmp.Col)




# Model Selection ---------------------------------------------------




# 7-A. Compute the 'Closest' models per method ----------------------
# t <- qt(.975, 9, lower = TRUE)
# tmp <- modEval.Case.wgts.Analysis.NA.omit %>%
#   group_by(Method) %>%
#   mutate(mean_Acc = mean(Accuracy), sd_Acc = ifelse(is.na(sd(Accuracy)) == TRUE, 0, sd(Accuracy)),
#          CI_Low_Acc = mean_Acc - t*sd_Acc, CI_Upp_Acc = mean_Acc + t*sd_Acc,
#          mean_Sen = mean(Sensitivity), sd_Sen = ifelse(is.na(sd(Sensitivity)) == TRUE, 0, sd(Sensitivity)),
#          CI_Low_Sen = mean_Sen - t*sd_Sen, CI_Upp_Sen = mean_Sen + t*sd_Sen,
#          mean_Spe = mean(Specificity), sd_Spe = ifelse(is.na(sd(Specificity)) == TRUE, 0, sd(Specificity)),
#          CI_Low_Spe = mean_Spe - t*sd_Spe, CI_Upp_Spe = mean_Spe + t*sd_Spe,
#          mean_Kap = mean(Kappa), sd_Kap = ifelse(is.na(sd(Kappa)) == TRUE, 0, sd(Kappa)),
#          CI_Low_Kap = mean_Kap - t*sd_Kap, CI_Upp_Kap = mean_Kap + t*sd_Kap,
#          mean_F1 = mean(F1), sd_F1 = ifelse(is.na(sd(F1)) == TRUE, 0, sd(F1)),
#          CI_Low_F1 = mean_F1 - t*sd_F1, CI_Upp_F1 = mean_F1 + t*sd_F1)
# View(tmp)  

t <- qt(.975, 9, lower = TRUE)
tmp <- modEval.Case.wgts.Analysis.NA.omit %>%
  mutate(Method.L = paste0(Method, ', wgt: ', `Case weights`),
         Method.F = paste0(Method, ': k = ', k, '; Weight = ', `Case weights`)) %>%
  group_by(Method.L) %>%
  mutate(mean_Acc = mean(Accuracy), sd_Acc = ifelse(is.na(sd(Accuracy)) == TRUE, 0, sd(Accuracy)),
         CI_Low_Acc = max(0, mean_Acc - t*sd_Acc), CI_Upp_Acc = min(1, mean_Acc + t*sd_Acc),
         mean_Sen = mean(Sensitivity), sd_Sen = ifelse(is.na(sd(Sensitivity)) == TRUE, 0, sd(Sensitivity)),
         CI_Low_Sen = max(0, mean_Sen - t*sd_Sen), CI_Upp_Sen = min(1, mean_Sen + t*sd_Sen),
         mean_Spe = mean(Specificity), sd_Spe = ifelse(is.na(sd(Specificity)) == TRUE, 0, sd(Specificity)),
         CI_Low_Spe = max(0, mean_Spe - t*sd_Spe), CI_Upp_Spe = min(1, mean_Spe + t*sd_Spe),
         mean_Kap = mean(Kappa), sd_Kap = ifelse(is.na(sd(Kappa)) == TRUE, 0, sd(Kappa)),
         CI_Low_Kap = max(0, mean_Kap - t*sd_Kap), CI_Upp_Kap = min(1, mean_Kap + t*sd_Kap),
         mean_F1 = mean(F1), sd_F1 = ifelse(is.na(sd(F1)) == TRUE, 0, sd(F1)),
         CI_Low_F1 = max(0, mean_F1 - t*sd_F1), CI_Upp_F1 = min(1, mean_F1 + t*sd_F1))

View(modEval.Case.wgts.Analysis.NA.omit)
View(tmp)

# dist = sqrt( 3|Sen - Ave(Sen)| + 2|AUC - Ave(AUC)| + |F1 - Ave(F1)| )
tmp.Reduced <- tmp %>%
  group_by(Method.L) %>%
  mutate(distance = sqrt(3*abs(Sensitivity - mean_Sen) + abs(Specificity - mean_Spe) + abs(Kappa - mean_Kap) + abs(F1 - mean_F1))) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  arrange(Method.F) %>%
  # mutate(Model = paste(Method.L, k)) %>%
  # filter(Method %in% c('Neural Network', 'Random Forest', 'Gradient Boosting', 'k-NN')) %>%
  dplyr::select(Method.F)
tmp.Reduced <- tmp.Reduced$Method.F

(tmp.Reduced)

# models.DownSampling.ExpMethod[tmp.Reduced[1]]
models.Case.wgts.BestModels <- list()
for (i in (1:4)) {
  models.Case.wgts.BestModels[[i]] <- models.Case.wgts.Linear[tmp.Reduced[i]][[1]]
  names(models.Case.wgts.BestModels)[i] <- tmp.Reduced[i]
}
for (i in c(5:7)) {
  models.Case.wgts.BestModels[[i]] <- models.Case.wgts.Radial[tmp.Reduced[i]][[1]]
  names(models.Case.wgts.BestModels)[i] <- tmp.Reduced[i]
}

View(models.Case.wgts.BestModels)
rm(i,k,t, tmp.Reduced)



print(object.size(models.Case.wgts.BestModels), units = 'Mb')
saveRDS(models.Case.wgts.BestModels, 
        file = 'Cost-sensitive Training - Best Models per Algorithms.rds')




# Performance of top 4 methods --------------------------------------
# dist = sqrt( 3|Sen - Ave(Sen)| + 2|AUC - Ave(AUC)| + |F1 - Ave(F1)| )
x <- c('SVM: Linear, wgt: 50', 'SVM: Linear, wgt: 75', 'SVM: Linear, wgt: 100')
tmp.Reduced <- tmp %>%
  group_by(Method.L) %>%
  mutate(distance = sqrt(3*abs(Sensitivity - mean_Sen) + abs(Specificity - mean_Spe) + abs(Kappa - mean_Kap) + abs(F1 - mean_F1))) %>%
  arrange(distance) %>%
  filter(row_number() == 1) %>%
  mutate(Model = paste(Method, k)) %>%
  dplyr::select(Method, `Case weights`, Method.L, k) %>%
  filter(Method.L %in% x)


# Narrow down the top 3 methods
top3.metric <- merge(tmp.Reduced, modEval.Case.wgts.Analysis.NA.omit)
names(top3.metric)



p.1 <- top3.metric %>%
  dplyr::select(Method.L, Accuracy, Sensitivity, Specificity, F1, Kappa) %>%
  melt(id.vars='Method.L') %>%
  mutate(Method.Ord = factor(Method.L, levels = x)) %>%
  dplyr::select(Method.Ord, variable, value) %>%
  ggplot(aes(x = Method.Ord, y = value, color = variable, group = variable)) + 
  geom_point(size = 5) + geom_line(size = 1) +
  geom_text_repel(aes(label = round(value, 3)), vjust = -1, hjust = -.5, size = 3, 
                  color = 'black', fontface = 'bold') +
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank()) + 
  ylab('') + xlab('')

p.1





ncol(top3.metric)

top3.metric <- top3.metric %>%
  arrange(desc(`Case weights`)) %>%
  dplyr::select(Approach, Method, k, `Case weights`, Accuracy, Sensitivity, Specificity, Kappa, F1)

head(top3.metric)
print(xtable(top3.metric, 
             digits = c(rep(0,5), rep(3,5)), type = 'latex'), 
      file = "Cost-sensitive Training - Top 3 Methods.tex")

View(modEval.DownSampling.Analysis)
ncol(modEval.DownSampling.Analysis)
print(xtable(modEval.Case.wgts.Analysis, 
             digits = c(rep(0,7), rep(3,6)), type = 'latex'), 
      file = "[Stock] --- Cost-sensitive Training - All methods and models.tex")



write.csv(modEval.Case.wgts.Analysis, 
          file = '[Stock] --- Cost-sensitive Training - All Methods and Models Metrics.csv')


