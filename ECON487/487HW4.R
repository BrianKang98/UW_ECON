rm(list = ls())
# 2a
library(glmnet)
library(reshape2)
#library(hdm)
setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)
df <- df[,-c(14:17)]
# From HW3. Create lagged variables.
dat1 <- df
dat1$week <- dat1$week+1
myvars <- c("price", "week", "brand","store")
dat1 <- dat1[myvars]
dat2 <- merge(df, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastWeek", names(dat2))
names(dat2) <- gsub(".y", ".thisWeek", names(dat2))
df <- dat2

# 1a
# randomize dataset
set.seed(487)
randdf <- df[sample(nrow(df)),]
# create 5 folds of equal size
fold <- cut(1:nrow(randdf), breaks = 5, labels = F)

# 1b
# get train and test sets
varnames <- paste(c(names(randdf)[-c(2,3,4)],  # exclude store, week, logmove
                    "brand*price.thisWeek","brand*price.lastWeek",
                    "EDUC*price.thisWeek","HHLARGE*price.thisWeek"), collapse = "+")  # exclude all logmove
formula <- paste(c("logmove", varnames), collapse = "~")
MSE.k <- c()
for (ii in 1:5) {
  index <- which(fold==ii, arr.ind = T)
  train <- randdf[-index,]  # 80% train
  test <- randdf[index,]  # 20% test
  # 1c
  # run OLS
  lm.k <- lm(formula, data=test)
  logmove_hat <- predict(lm.k, newdata = test)
  # 1ci
  # calculate k-fold'th MSE
  MSE.k <- c(MSE.k, mean((logmove_hat-test$logmove)^2))
}
# 1cii
# get cross validated MSE
cv.MSE.1 <- mean(MSE.k)
cv.MSE.1

# 2b
# take out intercept
formula <- paste(c(formula, "brand*feat"), collapse = "+")
xtrain <- model.matrix(as.formula(formula), data = train)[,-1]
xtest <- model.matrix(as.formula(formula), data = test)[,-1]
ytrain <- train$logmove
set.seed(487)
cv.lasso.1 <- cv.glmnet(xtrain, ytrain, alpha = 1)  # 1 for lasso
plot(cv.lasso.1)

# 2b
# lasso
cv.lambda <- cv.lasso.1$lambda.min  # get smallest tuning parameter
lasso.1 <- glmnet(xtrain, ytrain, alpha = 1, lambda = cv.lambda)
# 2c
# get coefficients
lasso.1$beta

# 2d
# out of sample mse from lasso
pred.lasso.1 <- predict(lasso.1, s = cv.lambda, newx = xtest)
MSE.2 <- mean((pred.lasso.1-test$logmove)^2)
MSE.2
# compare the two MSE values
MSE.2 < cv.MSE.1

# 3
newvars <- paste(rownames(lasso.1$beta)[which(lasso.1$beta != 0)], collapse = "+")
newformula <- paste(c("logmove", newvars), collapse = "~")
randdf$brandminute.maid <- randdf$brand == "minute.maid"
randdf$brandtropicana <- randdf$brand == "tropicana"
lm.2 <- lm(newformula, data = randdf)
summary(lm.2)

# 3a.iv
confint(lm.2, 'brandtropicanaTRUE', level=0.95)

# 4a
# self and cross price elasticity from hw3 sol.
#dcast is a function in the reshape2 library that turns "long data" into "wide data"
oj_prices <-df[,1:6]
oj_wide <- dcast(oj_prices, store + week ~ brand)
colnames(oj_wide)[3] <- "P_Dom"
colnames(oj_wide)[4] <- "P_MM"
colnames(oj_wide)[5] <- "P_Trop"
oj_cross <- merge(df, oj_wide, by=c("week","store"))

# tropicanan
trop_cross <- subset(oj_cross, brand=="tropicana")
lm.3 <- lm(logmove ~ log(P_Dom)+log(P_MM)+log(P_Trop)+
             AGE60+EDUC+ETHNIC+INCOME+HHLARGE+WORKWOM, data=trop_cross)
summary(lm.3)
# dominicks
trop_cross <- subset(oj_cross, brand=="dominicks")
lm.4 <- lm(logmove ~ log(P_Dom)+log(P_MM)+log(P_Trop)+
             AGE60+EDUC+ETHNIC+INCOME+HHLARGE+WORKWOM, data=trop_cross)
summary(lm.4)
# minute maid
trop_cross <- subset(oj_cross, brand=="minute.maid")
lm.5 <- lm(logmove ~ log(P_Dom)+log(P_MM)+log(P_Trop)+
             AGE60+EDUC+ETHNIC+INCOME+HHLARGE+WORKWOM, data=trop_cross)
summary(lm.5)



# 4b
# tropicanan
trop_cross <- subset(oj_cross, brand=="tropicana")
lm.3 <- lm(logmove ~ log(P_Dom)*feat+log(P_MM)*feat+log(P_Trop)*feat+
             AGE60+EDUC+ETHNIC+INCOME+HHLARGE+WORKWOM, data=trop_cross)
summary(lm.3)
# dominicks
trop_cross <- subset(oj_cross, brand=="dominicks")
lm.4 <- lm(logmove ~ log(P_Dom)*feat+log(P_MM)*feat+log(P_Trop)*feat+
             AGE60+EDUC+ETHNIC+INCOME+HHLARGE+WORKWOM, data=trop_cross)
summary(lm.4)
# minute maid
trop_cross <- subset(oj_cross, brand=="minute.maid")
lm.5 <- lm(logmove ~ log(P_Dom)*feat+log(P_MM)*feat+log(P_Trop)*feat+
             AGE60+EDUC+ETHNIC+INCOME+HHLARGE+WORKWOM, data=trop_cross)
summary(lm.5)






