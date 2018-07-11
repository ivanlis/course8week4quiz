fun1 <- function()
{
    library(caret)
    set.seed(33833)
    
    library(ElemStatLearn)
    
    data(vowel.train)
    data(vowel.test)
    
    training <- vowel.train
    testing <- vowel.test
    
    training$y <- as.factor(training$y)
    testing$y <- as.factor(testing$y)
    
    
    fitRf <- train(y ~ ., data = training, method = "rf", trControl = trainControl(method = "cv"), number = 3)
    fitGbm <- train(y ~ ., data = training, method = "gbm", verbose = FALSE)
    
    predictRf <- predict(fitRf, testing)
    predictGbm <- predict(fitGbm, testing)
    
    accurRf <- sum(predictRf == testing$y) / nrow(testing)
    accurGbm <- sum(predictGbm == testing$y) / nrow(testing)
    accurAgree <- sum(predictRf == predictGbm & predictRf == testing$y) / sum(predictRf == predictGbm)
    
    c(accurRf, accurGbm, accurAgree)
}


fun2 <- function()
{
    library(caret)
    library(gbm)
    set.seed(3433)
    
    library(AppliedPredictiveModeling)
    
    data(AlzheimerDisease)
    
    adData <- data.frame(diagnosis, predictors)
    
    inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
    
    training <- adData[inTrain,]
    testing <- adData[-inTrain,]
    
    set.seed(62433)
    
    fitRf <- train(diagnosis ~ ., data = training, method = "rf")
    fitGbm <- train(diagnosis ~., data = training, method = "gbm", verbose = FALSE)
    fitLda <- train(diagnosis ~ ., data = training, method = "lda")
    
    predictRf <- predict(fitRf, testing)
    predictGbm <- predict(fitGbm, testing)
    predictLda <- predict(fitLda, testing)
    
    
    accurRf <- sum(predictRf == testing$diagnosis) / nrow(testing)
    accurGbm <- sum(predictGbm == testing$diagnosis) / nrow(testing)
    accurLda <- sum(predictLda == testing$diagnosis) / nrow(testing)
    
    print(c(accurRf, accurGbm, accurLda))
    
    
    secondaryDf <- data.frame(predictRf = predictRf, predictGbm = predictGbm, predictLda = predictLda,
                              diagnosis = testing$diagnosis)
    fitStacked <- train(diagnosis ~ ., data = secondaryDf, method = "rf")
    
    predictStackedOnTest <- predict(fitStacked, testing)
    
    accurStackedOnTest <- sum(predictStackedOnTest == secondaryDf$diagnosis) / nrow(secondaryDf)
    
    print(accurStackedOnTest)
}


fun3 <- function()
{
    set.seed(3523)
    
    library(AppliedPredictiveModeling)
    data(concrete)
    
    inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
    training = concrete[ inTrain,]
    testing = concrete[-inTrain,]
    
    set.seed(233)
    
    
    lassoFit <- train(CompressiveStrength ~ ., data = concrete, method = "lasso")
}