parole = read.csv("./data/parole.csv")
# How many parolees are contained in the dataset?
nrow(parole)
# How many of the parolees in the dataset violated the terms of their parole?
sum(parole$violator)
# Using the as.factor() function, convert these variables to factors.su
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)

#
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

# Using glm (and remembering the parameter family="binomial"), train a logistic regression model on the 
# training set. Your dependent variable is "violator", and you should use all of the other variables as
# independent variables.

model = glm(violator ~ ., data = train, family = binomial)
summary(model)

# According to the model, what are the odds this individual is a violator?
Odds<-exp(-4.2411574+0.3869904+0.8867192+(-0.0001756*50)+ (-0.1238867*3)+(0.0802954*12)+ 0.6837143)
# What is the maximum predicted probability of a violation?
predictTest = predict(model,type = "response", newdata = test)
summary(predictTest)
# In the following questions, evaluate the model's predictions on the test set using a threshold of 0.5.
table(test$violator,predictTest > 0.5)
# What is the model's sensitivity?
12/23
# What is the model's specificity?
167/(167+11)
# What is the model's accuracy?
(167+12) / nrow(test)

# What is the accuracy of a simple model that predicts that every parolee is a non-violator?
(nrow(parole)- sum(parole$violator))/nrow(parole)

# Using the ROCR package, what is the AUC value for the model?
ROCRpred = prediction(predictTest,test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

