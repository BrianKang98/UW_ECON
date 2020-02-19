# Brian Kang
# UW Datathon

#rm(list = ls())
setwd("C:/Users/slexi/Downloads/fifa-20-complete-player-dataset")
library(hdm) #lasso, logit
library(glmnet) #cv
library(nnet) #rnn
library(pscl) #multinomial
library(caret) #confusion matrix
library(dplyr)
library(gbm) #boosting
library(randomForest) #random forest
# Run following code to install "reprtree" package"--------
#install.packages("devtools")
#library(devtools)
#devtools::install_github('skinner927/reprtree')
#----------------------------------------------------------
library(reprtree) #plot decision tree
library(car) #fit dists
library(fitdistrplus) #fit dists
library(tidyverse) #cleaning
library(lubridate)
library(reshape2)

temp15 <- read.csv("players_15.csv", header = T)
temp16 <- read.csv("players_16.csv", header = T)
temp17 <- read.csv("players_17.csv", header = T)
temp18 <- read.csv("players_18.csv", header = T)
temp19 <- read.csv("players_19.csv", header = T)
temp20 <- read.csv("players_20.csv", header = T)
templeague <- read.csv("teams_and_leagues.csv", header = T)
tempclub <- read.csv("club_id.csv", header = T)

names(tempclub) <- c("club", "url")

#sum(is.na(temp15))

# which(names(temp20)=="ls")
# merge league and club data into one dataset for each year
temp20 <- temp20[,-(79:104)]
temp20 <- temp20[,-(c(2,26,27,31))]
temp20 <- merge(x=temp20, y=tempclub, by="club",all=T)
temp20 <- merge(x=temp20, y=templeague, by="url",all=T)
temp20 <- temp20[,-1]

temp19 <- temp19[,-(79:104)]
temp19 <- temp19[,-(c(2,26,27,31))]
temp19 <- merge(x=temp19, y=tempclub, by="club",all=T)
temp19 <- merge(x=temp19, y=templeague, by="url",all=T)
temp19 <- temp19[,-1]

temp18 <- temp18[,-(79:104)]
temp18 <- temp18[,-(c(2,26,27,31))]
temp18 <- merge(x=temp18, y=tempclub, by="club",all=T)
temp18 <- merge(x=temp18, y=templeague, by="url",all=T)
temp18 <- temp18[,-1]

temp17 <- temp17[,-(79:104)]
temp17 <- temp17[,-(c(2,26,27,31))]
temp17 <- merge(x=temp17, y=tempclub, by="club",all=T)
temp17 <- merge(x=temp17, y=templeague, by="url",all=T)
temp17 <- temp17[,-1]

temp16 <- temp16[,-(79:104)]
temp16 <- temp16[,-(c(2,26,27,31))]
temp16 <- merge(x=temp16, y=tempclub, by="club",all=T)
temp16 <- merge(x=temp16, y=templeague, by="url",all=T)
temp16 <- temp16[,-1]

temp15 <- temp15[,-(79:104)]
temp15 <- temp15[,-(c(2,26,27,31))]
temp15 <- merge(x=temp15, y=tempclub, by="club",all=T)
temp15 <- merge(x=temp15, y=templeague, by="url",all=T)
temp15 <- temp15[,-1]

#write.csv(temp20, "players_20_1.csv")
#write.csv(temp19, "players_19_1.csv")
#write.csv(temp18, "players_18_1.csv")
#write.csv(temp17, "players_17_1.csv")
#write.csv(temp16, "players_16_1.csv")
#write.csv(temp15, "players_15_1.csv")

fifa20 <- temp20 #recover backup

# calculate average of Overall of each Club
club_avg <- aggregate(fifa20$overall, list(fifa20$club), mean)
names(club_avg) <- c("club", "avg_overall")
fifa20 <- merge(x=fifa20, y=club_avg, by="club",all=T)

# delete goalkeepers
fifa20 <- fifa20[!(grepl("GK",fifa20$team_position,fixed = T) | 
                     grepl("GK",fifa20$nation_position,fixed=T) |
                     grepl("GK",fifa20$player_positions,fixed=T)),]

# delete players >= 25 yrs old
fifa20 <- fifa20[!(fifa20$age >= 25),]

# split data into train & test
set.seed(987)
train <- sample(1:nrow(fifa20), nrow(fifa20)*0.8)  # 80% for training

# get which variables have <2 factor levels
get <- which(sapply(fifa20[train,], function(x) length(unique(x))<2))
# exclude encounter_id, patient_nbr, weight, payer_code, diag_1, daig_2,
# diag_3, readmitted, isReadmitted  
# Reason: unrelated to question OR too many factors
# also exclude acetohexamide, tolbutamide, troglitazone, 
# glimepiride.pioglitazone, metformin.rosiglitazone
# Reason: causes "<2 level" error from sampling
varnames <- paste(c(names(fifa20
                          [,-c(get,1,2,3,4,6,21,23,25,34:39,40,70:74,75,76 #must get rid of
                               ,9,10,14,24)])), collapse = "+") #alternatives to get rid of
formula <- paste(c("avg_overall",varnames), collapse = "~")


#---------------------------------------------------------------------
# Model 1: OLS
lm.1 <- lm(formula, data = fifa20[train,])
summary(lm.1)
# prediction on test data to predict patient readmission or not
prob.lm.1 <- predict(lm.1, newdata = fifa20[-train,]) # team position throws errors
summary(prob.lm.1)
length(na.omit(prob.lm.1)) # count remaining observations
# test error
mse.1 <- mean((prob.lm.1-fifa20[-train,]$avg_overall)^2, na.rm=T)
#cat("\nMSE\n")
mse.1

#--------------------------------------------------------------------
# Model 2: OLS with feature selected by group
lm.2 <- lm(avg_overall ~ potential + value_eur + wage_eur + contract_valid_until
           + skill_moves + movement_reactions + mentality_penalties, data = fifa20[train,])
summary(lm.2)
# prediction on test data to predict patient readmission or not
prob.lm.2 <- predict(lm.2, newdata = fifa20[-train,]) # team position throws errors
summary(prob.lm.2)
length(na.omit(prob.lm.2)) # count remaining observations
# test error
mse.2 <- mean((prob.lm.2-fifa20[-train,]$avg_overall)^2, na.rm=T)
#cat("\nMSE\n")
mse.2

#--------------------------------------------------------------------
# Model 3: LASSO then OLS
lasso.1 <- rlasso(formula , data = fifa20[train,], post = F)
#cat("Do LASSO on training set\n")
summary(lasso.1, all = F)

# get ceoffs that matter and make OLS formula
x <- which(coef(lasso.1)[-1]!=0)
#cat("\nCount and Kept Significant Variables by LASSO\nCount: ")
length(x)
#x
x <- paste(names(x), collapse = "+")
formula2 <- paste(c("avg_overall", x), collapse = " ~ ")

# name all extra variables created from doing OLS
fifa20$preferred_footRight <- fifa20$preferred_foot == "Right"
fifa20$nation_positionRB <- fifa20$nation_position == "RB"

# OLS regression on training set
olsLasso.1 <- lm(formula2, data = fifa20[train,])
summary(olsLasso.1)
#cat("\nDo OLS on training set using selected variables from LASSO\n")
summary(olsLasso.1)$coefficients[,1]
# prediction on test data to predict patient readmission or not
prob.lasso.1 <- predict(olsLasso.1, newdata = fifa20[-train,])
#cat("Predict on test set\n")
summary(prob.lasso.1)
#cat("\nCount remaining observations\n")
length(na.omit(prob.lasso.1)) # count remaining observations
# test error
mse.3 <- mean((prob.lasso.1-fifa20[-train,]$avg_overall)^2, na.rm=T)
#cat("\nMSE\n")
mse.3


