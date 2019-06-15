# HW3
# 1
# a)
set.seed(1)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)
# n=100; p=2; model: y=x-2x^2+e (the error term)

# b)
plot(x,y)
# We see a negative parabolic curve. As we expect from
# normal distributions, the center is at 0 and there are
# more data points clustered around the center than the
# sparse few on each end of the tails.

# c)
library(boot)
mat1 <- matrix(data = NA, nrow = 4, ncol=2) # record errors
set.seed(1)
dat1 <- data.frame(x,y)
glm.1 <- glm(y~x)
cv.1 <- cv.glm(dat1, glm.1)
mat1[1,1] <- cv.1$delta[1]

glm.2 <- glm(y~poly(x,2))
cv.2 <- cv.glm(dat1, glm.2)
mat1[2,1] <- cv.2$delta[1]

glm.3 <- glm(y~poly(x,3))
cv.3 <- cv.glm(dat1, glm.3)
mat1[3,1] <- cv.3$delta[1]

glm.4 <- glm(y~poly(x,4))
cv.4 <- cv.glm(dat1, glm.4)
mat1[4,1] <- cv.4$delta[1]

# d)
set.seed(3000)
glm.1 <- glm(y~x)
cv.1 <- cv.glm(dat1, glm.1)
mat1[1,2] <- cv.1$delta[1]

glm.2 <- glm(y~poly(x,2))
cv.2 <- cv.glm(dat1, glm.2)
mat1[2,2] <- cv.2$delta[1]

glm.3 <- glm(y~poly(x,3))
cv.3 <- cv.glm(dat1, glm.3)
mat1[3,2] <- cv.3$delta[1]

glm.4 <- glm(y~poly(x,4))
cv.4 <- cv.glm(dat1, glm.4)
mat1[4,2] <- cv.4$delta[1]
mat1 # display errors
# The results are the same from each of the seeds. Results
# are identical because LOOCV uses the same MSE calculation
# process on all observations with a set n value.
# I.e. every single observation is evaluated n folds.

# e)
# The model that goes up to the 2nd power has the smallest
# LOOCV error. This can be what I expected because the
# original data had a clear quadratic shape. But I expected
# the 4th power model to do as well or better because, as 
# the direct square of a quadratic, although it may overfit,
# the errors could have been smaller.

# f)
summary(glm.1)
# The coefficients do not mean much when we are fitting
# a quadratic with just the intercept and linear slope.
# The 1st power is significant at the 0.01 level.
summary(glm.2)
# This shows that all coefficients up to the 2nd power are
# statistically significant.
summary(glm.3)
summary(glm.4)
# These two show that the coefficients up to the 2nd power
# are statistically significant. The 3rd and 4th power are
# insignificant, which agrees with our conclusions from the
# cross-validation results.

# 2
# a)
library(MASS)
attach(Boston)  # used in lecture; every name is like a vars
mu.hat <- mean(medv)
mu.hat

# b)
# standard error of the sample mean = 
#   sd(sample) / sqrt(observations count)
sd(medv)/sqrt(nrow(Boston))

# c)
# bootstrap for mu
# output should incluse SE of sample mean
fun <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
library(boot)
boot(medv, fun, R = 1000)
# SE = 0.4033299
# The bootstrap estimated standard error of the samle mean
# is very close to the calculated SE from the previous.

# d)
# approx 95% confidence interval using 
#   [mu.hat-2SE(mu.hat) , mu.hat+2SE(mu.hat)].
c(mu.hat-2*0.4033299 , mu.hat+2*0.4033299)
t.test(medv)$conf.int
# The 95% confidence intervals from bootstrapping and 
# the t.test() method are very close.

# e)
med.hat <- median(medv)
med.hat

# f) 
fun <- function(data, index) {
  med <- median(data[index])
  return (med)
}
boot(medv, fun, R = 1000)
# The estimated standard error of the median using bootstrap
# is reasonably small. The median is equal to the value we
# calculated previously.

# g)
quant.hat <- quantile(medv, 0.1)
quant.hat

# h)
fun <- function(data, index) {
  quant <- quantile(data[index], 0.1)
  return (quant)
}
boot(medv, fun, 1000)
# The estimated standard error of the 10th percentile using
# bootstrap is again reasonably small. The 10th percentile
# is equal to the value we calculated preciously.

# 3
# a)
library(ISLR)
data("College")
head(College)
attach(College)
# split data to training and testing
collegeTrain <- College[1:(0.8*nrow(College)),] # 80% for train
collegeTest <- College[(0.8*nrow(College)+1):nrow(College),] # 20% for test

# b)
lm.1 <- lm(Apps~., data = collegeTrain)
summary(lm.1)
mse.1 <- mean((predict(lm.1, collegeTest)-collegeTest$Apps)^2)
mse.1  # test error

# c)
# ridge regression with CV choosing lambda
x <- model.matrix(Apps~.,data=College)[,-1] # take out intercept
xtrain <- model.matrix(Apps~.,data=collegeTrain)[,-1]
xtest <- model.matrix(Apps~.,data=collegeTest)[,-1]
ytrain <- Apps[1:(0.8*nrow(College))]
# ytest <- Apps[(0.8*nrow(College)+1):nrow(College)]

library(glmnet)
#set.seed(1)
cv.ridge <- cv.glmnet(xtrain, ytrain, alpha = 0) # 0 for ridge
cv.lambda <- cv.ridge$lambda.min # get smallest lambda (tuning param)
# plot(cv.ridge)
ridge <- glmnet(xtrain, ytrain, alpha = 0, lambda = cv.lambda)
summary(ridge)
pred <- predict(ridge, s = cv.lambda, newx = xtest)
mse.2 <- mean((pred-ytest)^2)
mse.2  # test error

# d)
set.seed(1)
cv.lasso <- cv.glmnet(xtrain, ytrain, alpha = 1) # 1 for lasso
cv.lambda <- cv.lasso$lambda.min # get smallest lambda (tuning param)
# plot(cv.lasso)
lasso <- glmnet(xtrain, ytrain, alpha = 1, lambda = cv.lambda)
summary(lasso)
pred <- predict(lasso, s = cv.lambda, newx = xtest)
mse.3 <- mean((pred-ytest)^2)
mse.3  # test error

lassocoeffs <- predict(lasso, s = cv.lambda, type = "coefficients")
summary(lassocoeffs)
lassocoeffs[lassocoeffs!=0] # nonzero lasso coeffs

# g)
# In terms of test error there is not much of a huge 
# difference. Although we do see that the error from the 
# ridge regression is smaller than the least squares' and
# lasso's. That is, it is better at prediction that other
# models; however, we know that, like the LASSO, the
# coefficients shrink to zero due to regularization and 
# it is "impossible" to interpret our results. The problem
# with the lasso is that it works for low dimension models.
# The College data we dealt with will not be considered
# low dimensional data, but it is not high dimensional.

















