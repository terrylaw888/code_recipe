library(data.table)
library(dplyr)
library(randomForest)
library(caret)
# library(xgboost)
# library(unbalanced)

claim_raw <- fread("claim.sample.csv", stringsAsFactors = FALSE)

#################### 1 : Data Cleaning ############################

# Create a field to indicate the target variable

claim_class <- mutate(claim_raw, target = Denial.Reason.Code %in% c("F13", "J8G", "JO5", "JB8", "JE1", "JC9", "JF1", "JF9", "JG1", "JPA", "JES"))
claim_class$target <- as.integer(claim_class$target)
claim_class$target <- as.factor(claim_class$target)

claim_class_clean <- claim_class

# Remove variables which are not appropiate for building a predictive model
# Assumption: The claim per line is independently determined for being denied or not.
# Assumption: No blacklist of customer ID is used for building the model.
claim_class_clean$Claim.Number <- NULL
claim_class_clean$Member.ID <- NULL
claim_class_clean$Claim.Line.Number <- NULL

# Re-code variables with too many levels
claim_class_clean <- mutate(claim_class_clean, Revenue.Code1 = substr(Revenue.Code, 1, 2))
claim_class_clean <- mutate(claim_class_clean, Revenue.Code2 = substr(Revenue.Code, 3, 3))
claim_class_clean <- mutate(claim_class_clean, Revenue.Code3 = substr(Revenue.Code, 4, 4))
claim_class_clean <- mutate(claim_class_clean, Service.Code1 = substr(Service.Code, 1, 1), Service.Code2 = substr(Service.Code, 2, 2), Service.Code3 = substr(Service.Code, 3, 4))
claim_class_clean <- mutate(claim_class_clean, Procedure.Code1 = substr(Procedure.Code, 1, 1), Procedure.Code2 = substr(Procedure.Code, 2, 2), Procedure.Code3 = substr(Procedure.Code, 3, 3), Procedure.Code4 = substr(Procedure.Code, 4, 4), Procedure.Code5 = substr(Procedure.Code, 5, 5))
claim_class_clean <- mutate(claim_class_clean, Diagnosis.Code1 = substr(Diagnosis.Code, 1, 1), Diagnosis.Code2 = substr(Diagnosis.Code, 2, 2), Diagnosis.Code3 = substr(Diagnosis.Code, 3, 3), Diagnosis.Code4 = substr(Diagnosis.Code, 4, 4), Diagnosis.Code5 = substr(Diagnosis.Code, 5, 5))
claim_class_clean$Revenue.Code <- NULL
claim_class_clean$Service.Code <- NULL
claim_class_clean$Procedure.Code <- NULL
claim_class_clean$Diagnosis.Code <- NULL

char <- sapply(claim_class_clean, is.character)
claim_class_clean[, colnames(claim_class_clean)[char] := lapply(claim_class_clean[,colnames(claim_class_clean)[char], with = FALSE], as.factor)]

str(claim_class_clean)

# 90:10 training testing split
set.seed(13579)
claim_training <- sample_frac(claim_class_clean, 0.9, replace = FALSE)
nrow(filter(claim_training, Denial.Reason.Code %in% c("F13", "J8G", "JO5", "JB8", "JE1", "JC9", "JF1", "JF9", "JG1", "JPA", "JES")))
nrow(filter(claim_raw, Denial.Reason.Code %in% c("F13", "J8G", "JO5", "JB8", "JE1", "JC9", "JF1", "JF9", "JG1", "JPA", "JES")))/nrow(claim_training)

claim_testing <- filter(claim_class_clean, !V1 %in% claim_training$V1)
nrow(filter(claim_testing, Denial.Reason.Code %in% c("F13", "J8G", "JO5", "JB8", "JE1", "JC9", "JF1", "JF9", "JG1", "JPA", "JES")))
nrow(filter(claim_testing, Denial.Reason.Code %in% c("F13", "J8G", "JO5", "JB8", "JE1", "JC9", "JF1", "JF9", "JG1", "JPA", "JES")))/nrow(claim_testing)

claim_training$V1 <- NULL
claim_training$Denial.Reason.Code <- NULL


#################### 2 : Model Building ############################

# Tune models with OOB errors
# grid_mtry <- seq(7,22,3)
# grid_size <- c(1,2,3,5,10,15)
# count <- 0
# for (i in 1:6) {
#   for (j in 1:6) {
#     count <- count + 1
#     assign(paste("fit_",count,sep = ""), randomForest(target~., data = claim_training, mtry = grid_mtry[i], sampsize=c(1700*grid_size[j], 1700), strata=claim_training$target, importance=TRUE, do.trace=TRUE))
#   }
# }

# Final model: Random Forest with undersampling (5:1), mtry = 7
# 5:1 ratio is picked to control the precision to be ~ 50%, while maintaining FN rate < 5%
set.seed(24680)
fit_rf_final <- randomForest(target~., data = claim_training, mtry = 7, sampsize=c(8500, 1700), strata=claim_training$target, importance=TRUE, do.trace=TRUE)
pred_final <- predict(fit_rf_final, newdata = claim_testing)
table(pred_final, claim_testing$target)
# Error rate
mean(pred_final!=claim_testing$target)
# Specificity
table(pred_final, claim_testing$target)[1]/(table(pred_final, claim_testing$target)[1]+table(pred_final, claim_testing$target)[2])
# Sensitivity
table(pred_final, claim_testing$target)[4]/(table(pred_final, claim_testing$target)[3]+table(pred_final, claim_testing$target)[4])


#################### 3 : Final Result ############################

confusionMatrix(pred_final, claim_testing$target, positive = "1")
varImpPlot(fit_rf_final)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 46877     8
# 1   187   184
# 
# Accuracy : 0.9959          
# 95% CI : (0.9953, 0.9964)
# No Information Rate : 0.9959          
# P-Value [Acc > NIR] : 0.6042          
# 
# Kappa : 0.6518          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.958333        
# Specificity : 0.996027        
# Pos Pred Value : 0.495957        
# Neg Pred Value : 0.999829        
# Prevalence : 0.004063        
# Detection Rate : 0.003894        
# Detection Prevalence : 0.007851        
# Balanced Accuracy : 0.977180        
# 
# 'Positive' Class : 1        






# Alternative models: xgboost(Tree) + up sampling
# set.seed(65535)
# fitControl <- trainControl(method = "cv", number = 5, repeat = 2, search = "random", verboseIter = TRUE, sampling = "up", allowParallel = TRUE)
# xgb1 <- caret::train(target~., data = claim_training, method = "xgbTree", trControl = fitControl, nthread =2)
# pred_xgb1 <- predict(xgb1, newdata = claim_testing)
# table(pred_xgb1, claim_testing$target)
# # Error rate
# mean(pred_xgb1!=claim_testing$target)
# # Specificity
# table(pred_xgb1, claim_testing$target)[1]/(table(pred_xgb1, claim_testing$target)[1]+table(pred_xgb1, claim_testing$target)[2])
# # Sensitivity
# table(pred_xgb1, claim_testing$target)[4]/(table(pred_xgb1, claim_testing$target)[3]+table(pred_xgb1, claim_testing$target)[4])
# confusionMatrix(pred_xgb1, claim_testing$target, positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 45871     5
# 1  1193   187


# # Alternative models: xgboost(Tree) + smote (It may take few hours to run..)
# set.seed(11911)
# fitControl2 <- trainControl(method = "cv", number = 5, repeats = 1, search = "random", verboseIter = TRUE, sampling = "smote", allowParallel = TRUE)
# xgb2 <- caret::train(target~., data = claim_training, method = "xgbTree", trControl = fitControl2, nthread =2)
# pred_xgb2 <- predict(xgb2, newdata = claim_testing)
# table(pred_xgb2, claim_testing$target)
# # Error rate
# mean(pred_xgb2!=claim_testing$target)
# # Specificity
# table(pred_xgb2, claim_testing$target)[1]/(table(pred_xgb1, claim_testing$target)[1]+table(pred_xgb1, claim_testing$target)[2])
# # Sensitivity
# table(pred_xgb2, claim_testing$target)[4]/(table(pred_xgb1, claim_testing$target)[3]+table(pred_xgb1, claim_testing$target)[4])
# confusionMatrix(pred_xgb2, claim_testing$target, positive = "1")
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 46515    10
# 1   549   182

