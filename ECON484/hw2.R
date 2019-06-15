# Hw2
# 2
# predict whether a given suburb has a crime rate above or below the median
#rm(list=ls())

library(MASS)
data("Boston")
head(Boston)
attach(Boston)  # from lecture
summary(Boston)

# actuals: if greater than median, T; otherwise F
binCrim <- rep(F, length(crim))
binCrim[crim > median(crim)] <- T
Boston <- cbind(Boston, binCrim)

# split data to training and test
bostonTrain <- Boston[1:(0.8*nrow(Boston)),]  # 80% for training
bostonTest <- Boston[(0.8*nrow(Boston)+1):nrow(Boston),]  # 20% for test
train <- rep(T, nrow(Boston))  # logical vector
train[(0.8*nrow(Boston)+1):nrow(Boston)+1] <- F  # apparently end case is exclusive?
binCrimTest <- binCrim[!train]  # save crim values for "actuals" (test)

# run LOGIT using training data
logit1 <- glm(binCrim ~ . - binCrim - crim, data = Boston, family = "binomial", subset = train)
summary(logit1)

# predict probability
logit1.prob <- predict(logit1, bostonTest, type = "response")

# predicting if crime rate below or above median
logit1.pred <- rep(F, nrow(bostonTest))
logit1.pred[logit1.prob > 0.5] <- T

# confusion matrix
table(logit1.pred, binCrimTest)
mean(logit1.pred==binCrimTest)  

# We can see that our predictions were around 89% correct.
# From the confusion matrix we can see that we correctly predicted all 86
# suburb cities that the crime rates were above the median, which is good.
# But, looking at the 15 cities had rates below the median, we incorrectly
# predicted 11 and only 4 were correct, in this sense our predictions are
# very bad.

# Same logit process but different ratios
bostonTrain2 <- Boston[1:(0.65*nrow(Boston)),]  # 65% for training
bostonTest2 <- Boston[(0.65*nrow(Boston)+1):nrow(Boston),]  # 35% for test
train2 <- rep(T, nrow(Boston))  # logical vector
train2[(0.65*nrow(Boston)+1):nrow(Boston)+1] <- F  # apparently end case is exclusive?
binCrimTest2 <- binCrim[!train2]  # save crim values for "actuals" (test)
logit2 <- glm(binCrim ~ . - binCrim - crim, data = Boston, family = "binomial", subset = train)
summary(logit2)
logit2.prob <- predict(logit2, bostonTest2, type = "response")
logit2.pred <- rep(F, nrow(bostonTest2))
logit2.pred[logit2.prob > 0.5] <- T
table(logit2.pred, binCrimTest2)
mean(logit2.pred==binCrimTest2) 

# This time, we correctly predicted about 88.7% of every prediction.
# From the confusion matrix, we can see that out of the 135 cities with
# crime rates above median, we correctly predicted 134, which is great.
# The number size itself grew because now the test data size is larger.
# Now, looking at the cities that have crime rates lower than the median,
# out of the 42, we incorrectly predicted 19 and 23 correctly. This does
# not mean that our prediction got better, it was simply the variation in
# our data that gave this output. The prediction for "safer" cities are
# still very bad.

# Now using KNN
library(class)
traink <- Boston[train,]
testk <- Boston[!train,]
binCrimTestk <- binCrim[train]
set.seed(999)
knn1 <- knn(traink, testk, binCrimTestk, k=1)
table(knn1, binCrimTest)
mean(knn1==binCrimTest)

# Using KNN, k=1, we correctly predicted about 92% of the times.
# Out of the 86 cities with crime rates above median, 83 predictions were
# correct. Out of the 15 with lower rates, 5 were incorrect. Ratio wise,
# the incorrectness for "safer" cities are better less using KNN.

# Same KNN but different K value
set.seed(999)
knn2 <- knn(traink, testk, binCrimTestk, k=1)
table(knn2, binCrimTest)
mean(knn2==binCrimTest)

# With k=10, we correctly predicted around 92% of the times.
# The correct and incorrect values for the both kind of cities are the same.
