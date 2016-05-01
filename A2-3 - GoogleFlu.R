# Looking at the time period 2004-2011, which week corresponds to the highestpercentage 
# of ILI-related physician visits? Select the day of the month corresponding to the start of this week.

FluTrain = read.csv("./data/FluTrain.csv")
FluTrain$Week[which.max(FluTrain$ILI)]

# Which week corresponds to the highest percentage of ILI-related query fraction?
FluTrain$Week[which.max(FluTrain$Queries)]

# Let us now understand the data at an aggregate level. Plot the histogram of the dependent variable, ILI. What best describes the distribution of values of ILI?
hist(FluTrain$ILI)

# Plot the natural logarithm of ILI versus Queries. What does the plot suggest?.
plot(log(FluTrain$ILI),FluTrain$Queries)

# Based on the plot we just made, it seems that a linear regression model could be a good modeling choice. 
# Based on our understanding of the data from the previous subproblem, which model best describes our estimation problem?

#Answer: From the previous subproblem, we are predicting log(ILI) using the Queries variable.
#From the plot in the previous subproblem, we expect the coefficient on Queries to be positive.

# What is the training set R-squared value for FluTrend1 model (the "Multiple R-squared")?
FluTrend1 = lm(log(ILI) ~ Queries,data=FluTrain)
summary(FluTrend1)

# For a single variable linear regression model, there is a direct relationship between the R-squared and the correlation between
# the independent and the dependent variables. What is the relationship we infer from our problem?
correlation = cor(log(FluTrain$ILI),FluTrain$Queries)

# The csv file FluTest.csv provides the 2012 weekly data of the ILI-related search queries
# and the observed weekly percentage of ILI-related physician visits. 
# What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012?

FluTest <- read.csv("./data/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
predValue = PredTest1[which(FluTest$Week == "2012-03-11 - 2012-03-17")]

# What is the relative error betweeen the estimate (our prediction) and the observed value for the week of March 11, 2012? 

obsValue = FluTest$ILI[11]
(obsValue-predValue)/obsValue
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))

# How many values are missing in the new ILILag2 variable?

library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)

# Use the plot() function to plot the log of ILILag2 against the log of ILI. Which best describes the relationship between these two variables?
plot(log(FluTrain$ILI),log(ILILag2))

# Train a linear regression model on the FluTrain dataset to predict the log of the ILI variable using the Queries variable
# as well as the log of the ILILag2 variable. Call this model FluTrend2.
# Which coefficients are significant at the p=0.05 level in this regression model? (Select all that apply.)
FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2),data = FluTrain)
summary(FluTrend2)

# Modify the code from the previous subproblem to add an ILILag2 variable to the FluTest data frame.
# How many missing values are there in this new variable?
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)

# Fill in the missing values for ILILag2 in FluTest. In terms of syntax, you could set the value of ILILag2 in row "x" of the FluTest
# data frame to the value of ILI in row "y" of the FluTrain data frame with "FluTest$ILILag2[x] = FluTrain$ILI[y]".

FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[1] = FluTrain$ILI[nrow(FluTrain)-1]
     
# Obtain test set predictions of the ILI variable from the FluTrend2 model

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
