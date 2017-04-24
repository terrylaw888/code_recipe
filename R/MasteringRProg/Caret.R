install.packages("mlbench")
data(BreastCancer, package = "mlbench")
bc <- BreastCancer[complete.cases(BreastCancer),]
summary(bc)
str(bc)

bc <- bc[,-1]
for (i in 1:9) {
  bc[,i] <- as.numeric(as.character(bc[,i]))
}

str(bc)
bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
str(bc)
 
library(caret)

'%ni%' <- Negate('%in%')

trainDataIndex <- createDataPartition(bc$Class, p = 0.7, list = F)
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"], y = trainData$Class)
table(down_train$Class)

################################
### Try Caret

library(caret)
set.seed(100)
data(Vehicle, package = "mlbench")

train_rows <- createDataPartition(Vehicle$Class, p = 0.7, list = F)
trainData <- Vehicle[train_rows,]
testData <- Vehicle[-train_rows,]

install.packages("doMC")
library(doMC)
registerDoMC(cores = 2)
tc <- trainControl(method = "repeatedcv", 
                   number = 5, repeats = 3, 
                   search = "random", 
                   summaryFunction = multiClassSummary)
fit <- train(Class ~ ., data=trainData,
             method = "C5.0",
             trainControl = tc,
             metric = "Kappa",
             tuneLength = 5)
mod <- fit$finalModel
pred <- predict(mod, testData)

testResults <- predict(mod, testData, type = "prob")
testResults <- data.frame(testResults)
testResults$obs <- testData$Class
testResults$pred <- predict(fit, testData, type = "raw")
multiClassSummary(testResults, lev = levels(testData$Class))
