#
gerber = read.csv("./data/gerber.csv")

# What proportion of people in this dataset voted in this election?
sum(gerber$voting)/nrow(gerber)
# Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
gerberVote = subset(gerber,gerber$voting == 1)
summary(gerber)
# Build a logistic regression model for voting using the four treatment group variables
# as the independent variables (civicduty, hawthorne, self, and neighbors).
logisticModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber,family= binomial)
summary(logisticModel)predict
# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predictTest = predict(logisticModel,type = "response")
table(gerber$voting,predictTest > 0.3)
# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
table(gerber$voting,predictTest > 0.5)
# Compare your previous two answers to the percentage of people who did not vote 
# (the baseline accuracy) and compute the AUC of the model. 
library(ROCR)
ROCRpred = prediction(predictTest, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
# Problem 2.1 - Trees
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
# Problem 2.2 - Trees
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
# Problem 2.4 - Trees
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
# Problem 3.1 - Interaction Terms
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel4,digits = 6)
abs(0.296638-0.34)
prp(CARTmodel5,digits = 6)
v1 = abs(0.290456-0.334176)
v2 = abs(0.302795-0.345818)
v1-v2
0.000697
# Problem 3.3 - Interaction Terms
logisticModel2 = glm(voting ~ sex + control , data = gerber,family= binomial)
summary(logisticModel2)
# Problem 3.4 - Interaction Terms
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logisticModel2, newdata=Possibilities, type="response")
# Problem 3.5 - Interaction Terms
LogisticModel3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
