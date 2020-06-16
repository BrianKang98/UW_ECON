rm(list = ls())
library(ggplot2)
library(dplyr)
library(stringr)

setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)
df <- df[,-c(14:17)]
set.seed(487)
train <- sample(1:nrow(df), nrow(df)*0.8) # split to train and test

# Split train & test because equation for R2_test != R2_train
# Final model from HW2 WITHOUT store demographics
lm.0 <- lm(logmove~log(price)*brand*feat, data = df[train,])
# Without demographics R2
summary(lm.0)$r.squared

# 1a: Add in the store demographics as linear features
demovars <- paste(c("log(price)*brand*feat",names(df)[-c(1:6)]), collapse = "+")
formula <- paste(c("logmove",demovars), collapse = "~")
lm.1 <- lm(formula, data = df[train,])
summary(lm.1)

# 1b: What demographics significantly (t-value>2) influence demand
names(which(abs(summary(lm.1)$coefficients[,3])>2))[-c(1:5,13:19)]

# 1c: Predict logmove_hat and get fair R2
# Use test data because equation for R2_test != R2_train
logmove_hat <- predict(lm.1, newdata = df[-train,])
cor(logmove_hat, df[-train,]$logmove)

# 1di: Create a new dataframe which is a random subset of 80% of the data 
df2 <- df[train,]

# 1dii: ii.	Estimate the model with and without demographic characteristics.
#           Construct MSE for the training and test set for the models
# Without demographics
#     Training MSE
mean((lm.0$fitted-df[train,]$logmove)^2)
#     Test MSE
nodemo_logmove_hat <- predict(lm.0, newdata = df[-train,])
nodemo_MSE <- mean((nodemo_logmove_hat-df[-train,]$logmove)^2)
nodemo_MSE

# With demographics
#     Training MSE
mean((lm.1$fitted-df[train,]$logmove)^2)
#     Test MSE
demo_MSE <- mean((logmove_hat-df[-train,]$logmove)^2)
demo_MSE

# 1diii: Compare the out of sample MSE for the models
nodemo_MSE >= demo_MSE # no demographics does better out of sample
nodemo_MSE <= demo_MSE


# 2a: means and percentiles of hhlarge and educ
summary(df$HHLARGE)
summary(df$EDUC)

# 2bi: If we move from the median value of HHLARGE to the 75th percentile (3rd quartile),
#      how much does log(quantity) change each week on average?
pp <- coef(lm.1)["HHLARGE"] *
  (summary(df$HHLARGE)["3rd Qu."] - summary(df$HHLARGE)["Median"])
exp(pp)

# 2bii: If we move from the median value of EDUC to the 75th percentile (3rd quartile),
#      how much does log(quantity) change each week on average?
pp2 <- coef(lm.1)["EDUC"] *
  (summary(df$EDUC)["3rd Qu."] - summary(df$EDUC)["Median"])
exp(pp2)
exp(pp) >= exp(pp2)

# 2ci: Add two interaction terms with log(price). What are the coefficients on the interaction terms?
# chose hhlarge and educ
intrdemovars <- paste(c("log(price)*brand*feat+HHLARGE*log(price)+EDUC*log(price)",names(df)[-c(1:6)]), collapse = "+")
formula2 <- paste(c("logmove",intrdemovars), collapse = "~")
lm.2 <- lm(formula2, data = df[train,])
summary(lm.2)$coefficients[13:21,1]

# 2ciii: What are the coefficient estimates on the constants EDUC and HHLARGE? 
#        How do they compare to your regression from 1b?
# New regression
coef(lm.2)["HHLARGE"]
coef(lm.2)["EDUC"]
# From 1b old regression
coef(lm.1)["HHLARGE"]
coef(lm.1)["EDUC"]

# 2civ: If we move from the median value of each variable to the 3rd quartile, how much does elasticity change? 
pp3 <- coef(lm.2)["HHLARGE"] *
  (summary(df$HHLARGE)["3rd Qu."] - summary(df$HHLARGE)["Median"])
exp(pp3)

pp4 <- coef(lm.2)["EDUC"] *
  (summary(df$EDUC)["3rd Qu."] - summary(df$EDUC)["Median"])
exp(pp4)
exp(pp3) >= exp(pp4)

# 3a: Investigate dat2 and rename the lagged store values needed for a lagged price within the same store
dat1 <- df
dat1$week <- dat1$week+1
dat2 <- merge(df, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastWeek", names(dat2))
names(dat2) <- gsub(".y", ".thisWeek", names(dat2))

# 3b: run a regression with this week's log(quantity) on current and last week's price
lm.3 <- lm(logmove.thisWeek ~ price.thisWeek + price.lastWeek, data=dat2)
summary(lm.3)

