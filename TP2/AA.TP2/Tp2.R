install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))
# Load the caret package
library(caret)

# Import dataset
orange <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')

# Structure of the dataframe
str(orange)

# See top 6 rows and 10 columns
head(orange[, 1:10])


# Create the training and test datasets
set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(orange$Purchase, p=0.7, list=FALSE)

# Step 2: Create the training  dataset
trainData <- orange[trainRowNumbers,]

# Step 3: Get the remaining rows (going to be splitted in 2)
remainingData <- orange[-trainRowNumbers,]

# Step 4: Create the test and validation datasets
testRowNumbers <- createDataPartition(remainingData$Purchase, p=0.5, list=FALSE)

testData <- remainingData[testRowNumbers,]

validationData <- remainingData[-testRowNumbers,]

length(testData[testData$Purchase== "CH",]$Purchase) / length(testData$Purchase)
length(testData[testData$Purchase== "MM",]$Purchase) / length(testData$Purchase)

length(validationData[validationData$Purchase== "CH",]$Purchase) / length(validationData$Purchase)
length(validationData[validationData$Purchase== "MM",]$Purchase) / length(validationData$Purchase)


length(trainData[trainData$Purchase== "CH",]$Purchase) / length(trainData$Purchase)
length(trainData[trainData$Purchase== "MM",]$Purchase) / length(trainData$Purchase)

length(testData[testData$Purchase== "CH",]$Purchase)
length(testData[testData$Purchase== "MM",]$Purchase)

length(trainData$Purchase)
length(testData$Purchase)
length(validationData$Purchase)
# Store X and Y for later use.
x = trainData[, 2:18]
y = trainData$Purchase




library(skimr)
skimmed <- skim_to_wide(trainData)
skimmed[, c(1:5, 9:11, 13, 15:16)]

# Create the knn imputation model on the training data
preProcess_missingdata_model <- preProcess(trainData, method='knnImpute')
preProcess_missingdata_model

# Use the imputation model to predict the values of missing data points
library(RANN)  # required for knnInpute
trainData <- predict(preProcess_missingdata_model, newdata = trainData)
anyNA(trainData)
testData predict(preProcess_missingdata_model, newdata = trainData)
anyNA(testData)


modelLookup("J48")
modelLookup("knn")
modelLookup("naive_bayes")
modelLookup("mlp")
modelLookup("rf")


?caretList
library("caretEnsemble")
install.packages("MLmetrics")
?trainControl

fitControl <- trainControl(method = 'cv',                   # k-fold cross validation
                           number = 5,                      # number of folds
                           
                           savePredictions = 'final',       # saves predictions for optimal tuning parameter
                           classProbs = T,                  # should class probabilities be returned
                           #summaryFunction=twoClassSummary,  # results summary function
                           index = createFolds(trainData$Purchase, 5)
                           )
?trainControl
#models <- caretList(Purchase ~ ., data=trainData, trControl=trainControl, methodList=algorithmList) 
install.packages('naivebayes', dependencies=TRUE)

model_list_big <- caretList(
  Purchase~., data=trainData,
  #metric="ROC",
  trControl=fitControl,
  tuneList=list(
    j48=caretModelSpec(method="J48", tuneGrid=data.frame(expand.grid(C = c(0.1, 0.2, 0.3), M = c(2,3,10,20,30)))),
    kn=caretModelSpec(method="knn", tuneGrid=data.frame(k=c(2:20))),
    naive=caretModelSpec(method="naive_bayes")
    
  )
)

trellis.par.set(caretTheme())
ggplot(model_list_big$kn)
model_list_big$kn$resample
model_list_big

names(trainData[,2:18])
fitJ48 <- train(x = trainData[,2:18], y =trainData$Purchase, method = "J48", trControl= fitControl)

pred1 <- predict(fitJ48, newdata = testData)
confusionMatrix(pred1, testData$Purchase)
names(model_list_big)
summary(model_list_big$j48)
pred2 <- predict(model_list_big$j48, newdata = testData)
c <- confusionMatrix(pred2, testData$Purchase)

names(testData[,2:18])
names(trainData)


ggplot(model_list_big$j48)

xyplot(resamples(model_list_big))



?caretStack
greedy_ensemble <- caretEnsemble(
  model_list_big, 
  metric="ROC",
  trControl=trainControl(
    summaryFunction=twoClassSummary,
    classProbs=TRUE
  ))
greedy_ensemble$models
summary(greedy_ensemble)
