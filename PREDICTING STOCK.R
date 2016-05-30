stocks = read.csv("./data/StocksCluster.csv")
nrow(stocks)
sum(stocks$PositiveDec)/nrow(stocks)
variables = c("ReturnJan", "ReturnFeb", "ReturnMar", "ReturnApr", "ReturnMay", "ReturnJune", "ReturnJuly", "ReturnAug", "ReturnSep", "ReturnOct", "ReturnNov")
maxCor <- function(variable){
  max = 0;
  maxVar = c()
  for(i in 1:length(variable)){
    for(j in 1:length(variable)){
      if(cor(stocks[i],stocks[j]) > max && i !=  j){
        max = cor(stocks[i],stocks[j])
        maxVar = c(i,j)
      }
    }
  }
  max
}
summary(stocks)
set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~.,data=stocksTrain,family = binomial)
predictDec = predict(StocksModel, type= "response")
sum(diag(table(stocksTrain$PositiveDec,predictDec >0.5)))/nrow(stocksTrain)
predictDec2 = predict(StocksModel, type= "response",newdata = stocksTest)
sum(diag(table(stocksTest$PositiveDec,predictDec2 >0.5)))/nrow(stocksTest)
table(stocksTest$PositiveDec)
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)
set.seed(144)
km = kmeans(normTrain,centers = 3,iter.max = 1000)
table(km$cluster)
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)
stockTrain1 <- subset(stocksTrain,clusterTrain == 1)
stockTrain2 <- subset(stocksTrain,clusterTrain == 2)
stockTrain3 <- subset(stocksTrain,clusterTrain == 3)
stockTest1 <- subset(stocksTest,clusterTest == 1)
stockTest2 <- subset(stocksTest,clusterTest == 2)
stockTest3 <- subset(stocksTest,clusterTest == 3)
tapply(stocksTrain$PositiveDec, clusterTrain, mean)
stocksModel1 <- glm(PositiveDec ~., data = stockTrain1, family = binomial)
stocksModel2 <- glm(PositiveDec ~., data = stockTrain2, family = binomial)
stocksModel3 <- glm(PositiveDec ~., data = stockTrain3, family = binomial)
predictTest1 <- predict(stocksModel1, newdata = stockTest1, type = "response")
predictTest2 <- predict(stocksModel2, newdata = stockTest2, type = "response")
predictTest3 <- predict(stocksModel3, newdata = stockTest3, type = "response")
table(stockTest1$PositiveDec, predictTest1 >= 0.5)
table(stockTest2$PositiveDec, predictTest2 >= 0.5)
table(stockTest3$PositiveDec, predictTest3 >= 0.5)
summary(stocksModel1)
summary(stocksModel2)
summary(stocksModel2)
predictTest1 <- predict(stocksModel1, newdata = stockTest1, type = "response")
predictTest2 <- predict(stocksModel2, newdata = stockTest2, type = "response")
predictTest3 <- predict(stocksModel3, newdata = stockTest3, type = "response")
sum(diag(table(stockTest1$PositiveDec, predictTest1 >= 0.5)))/nrow(stockTest1)
sum(diag(table(stockTest2$PositiveDec, predictTest2 >= 0.5)))/nrow(stockTest2)
sum(diag(table(stockTest3$PositiveDec, predictTest3 >= 0.5)))/nrow(stockTest3)
AllPredictions = c(predictTest1, predictTest2, predictTest3)
AllOutcomes = c(stockTest1$PositiveDec, stockTest2$PositiveDec, stockTest3$PositiveDec)
