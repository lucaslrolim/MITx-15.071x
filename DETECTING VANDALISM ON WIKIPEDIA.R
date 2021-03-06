# DETECTING VANDALISM ON WIKIPEDIA
wiki = read.csv("./data/wiki.csv",stringsAsFactors = FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
sum(as.numeric(wiki$Vandal==1))
library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded,removeWords,stopwords("english"))
corpusAdded = tm_map(corpusAdded,stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
sparseAdded = removeSparseTerms(dtmAdded,0.997)
wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
corpusRemove = Corpus(VectorSource(wiki$Removed))
corpusRemove = tm_map(corpusRemove, removeWords, stopwords("english"))
corpusRemove = tm_map(corpusRemove, stemDocument)
dtmRemove = DocumentTermMatrix(corpusRemove)
sparseRemove = removeSparseTerms(dtmRemove,0.997)
wordsRemove= as.data.frame(as.matrix(sparseRemove))
colnames(wordsRemove) = paste("R",colnames(wordsRemove))
length(wordsRemove)
wikiWords = cbind(wordsAdded,wordsRemove)
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
spl = sample.split(wikiWords$Vandal,SplitRatio = 0.7)
train = subset(wikiWords,spl==TRUE)
test = subset(wikiWords,spl == FALSE)
table(test$Vandal)
library(rpart)
library(rpart.plot)
vandalCART = rpart(Vandal ~., data=train,method="class")
pred = predict(vandalCART,newdata=test)
pred.prob = pred[,2]
sum(diag(table(test$Vandal,pred.prob)))/nrow(test)
prp(vandalCART)
wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(as.numeric(wikiWords2$HTTP == 1))
wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)
httpCART = rpart(Vandal ~., data=wikiTrain2,method = "class")
httpPred = predict(httpCART,newdata = wikiTest2, type = "class")
sum(diag(table(wikiTest2$Vandal,httpPred)))/nrow(wikiTest2)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemove))
mean(wikiWords2$NumWordsAdded)
wikiTrain2 = subset(wikiWords2, spl==TRUE)
httpCART = rpart(Vandal ~., data=wikiTrain2,method = "class")
sum(diag(table(wikiTest2$Vandal,httpPred)))/nrow(wikiTest2)
wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, spl==TRUE)
wikiTest3 = subset(wikiWords3, spl==FALSE)
MLCART = rpart(Vandal ~., data=wikiTrain3,method = "class")
MLPred = predict(MLCART,newdata = wikiTest3, type = "class")
sum(diag(table(wikiTest3$Vandal,MLPred)))/nrow(wikiTest3)