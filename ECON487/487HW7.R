rm(list = ls())
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tree)
library(lfe)
library(rpart)
library(rattle)
library(caret)
library(maptree)
library(rpart.plot)
library(randomForest)
library(permute)
library(partykit)
setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)


df$price <- log(df$price) 
oj.rf <- randomForest(logmove ~ ., data = df, ntree = 	100, keep.forest = TRUE) 
df$pred <- oj.rf$pred


# red is actual
# black is predicted
ggplot(df, aes(x = price, y = logmove)) +
  geom_point(shape = 1,color = "red") +
  geom_point(aes(y = pred), shape = 1)  # Add the predicted values
