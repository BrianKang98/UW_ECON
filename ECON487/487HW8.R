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
df$price <- log(df$price) # log price
temp <- df

# From HW3. Create lagged variables.
dat1 <- df
dat1$week <- dat1$week+1
myvars <- c("price", "week", "brand","store")
dat1 <- dat1[myvars]
dat2 <- merge(df, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastweek", names(dat2))
names(dat2) <- gsub(".y", ".thisweek", names(dat2))
df <- dat2

# 3.a
dml1 <- function(dat) {
  # get price residual
  # exlude logmove, price.thisweek
  vars <- paste(c(names(dat)[-c(4,18)],"price.lastweek*EDUC","price.lastweek*INCOME","price.lastweek*HHLARGE"), collapse = "+")
  formula <- paste(c("price.thisweek",vars), collapse = "~")
  price.rf <- randomForest(as.formula(formula), data = dat, ntree = 	50, keep.forest = TRUE) 
  price.resid <- dat$price.thisweek - price.rf$pred
  
  # get logmove residual
  # exlude logmove
  vars <- paste(c(names(dat)[-c(4)],"price.lastweek*EDUC","price.lastweek*INCOME","price.lastweek*HHLARGE"), collapse = "+")
  formula <- paste(c("logmove",vars), collapse = "~")
  logmove.rf <- randomForest(as.formula(formula), data = dat, ntree = 	50, keep.forest = TRUE) 
  logmove.resid <- dat$logmove - logmove.rf$pred
  
  # lm of logmove tilde on price tilde
  lm <- lm(logmove.resid ~ price.resid, data = dat)
  (summary(lm))
}

# data by brand
df.trop <- subset(df, brand=="tropicana")
df.dom <- subset(df, brand=="dominicks")
df.mm <- subset(df, brand=="minute.maid")
dml1(df.trop)
dml1(df.dom)
dml1(df.mm)


# 3.b
# make cross price df
oj_prices <-temp[,1:6]
oj_wide <- dcast(oj_prices, store + week ~ brand)
colnames(oj_wide)[3] <- "price_D"
colnames(oj_wide)[4] <- "price_MM"
colnames(oj_wide)[5] <- "price_T"
oj_cross <- merge(temp, oj_wide, by=c("week","store"))

# Create lagged variables for each brand
dat1 <- oj_cross
dat1$week <- dat1$week+1
myvars <- c("price_T", "week", "brand","store")
dat1 <- dat1[myvars]
dat2 <- merge(oj_cross, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastweek", names(dat2))
names(dat2) <- gsub(".y", ".thisweek", names(dat2))
oj_cross <- dat2

dat1 <- oj_cross
dat1$week <- dat1$week+1
myvars <- c("price_D", "week", "brand","store")
dat1 <- dat1[myvars]
dat2 <- merge(oj_cross, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastweek", names(dat2))
names(dat2) <- gsub(".y", ".thisweek", names(dat2))
oj_cross <- dat2

dat1 <- oj_cross
dat1$week <- dat1$week+1
myvars <- c("price_MM", "week", "brand","store")
dat1 <- dat1[myvars]
dat2 <- merge(oj_cross, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastweek", names(dat2))
names(dat2) <- gsub(".y", ".thisweek", names(dat2))
oj_cross <- dat2
names(oj_cross)

df.trop <- subset(oj_cross, brand=="tropicana")
df.dom <- subset(oj_cross, brand=="dominicks")
df.mm <- subset(oj_cross, brand=="minute.maid")

# do double ml
# TROPICANA
# get price residual
# exlude logmove, price, price.thisweek of brands
vars <- paste(c(names(df.trop)[-c(4,6,21,22,23)],"price_T.lastweek*EDUC","price_T.lastweek*INCOME","price_T.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_T.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.trop, ntree = 	50, keep.forest = TRUE) 
price.resid_T <- df.trop$price_T.thisweek - price.rf$pred

vars <- paste(c(names(df.trop)[-c(4,6,21,22,23)],"price_D.lastweek*EDUC","price_D.lastweek*INCOME","price_D.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_D.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.trop, ntree = 	50, keep.forest = TRUE) 
price.resid_D <- df.trop$price_D.thisweek - price.rf$pred

vars <- paste(c(names(df.trop)[-c(4,6,21,22,23)],"price_MM.lastweek*EDUC","price_MM.lastweek*INCOME","price_MM.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_MM.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.trop, ntree = 	50, keep.forest = TRUE) 
price.resid_MM <- df.trop$price_MM.thisweek - price.rf$pred

# get logmove residual
# exlude logmove
vars <- paste(c(names(df.trop)[-c(4,6)],"price_T.lastweek*EDUC","price_T.lastweek*INCOME","price_T.lastweek*HHLARGE",
                "price_D.lastweek*EDUC","price_D.lastweek*INCOME","price_D.lastweek*HHLARGE",
                "price_MM.lastweek*EDUC","price_MM.lastweek*INCOME","price_MM.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("logmove",vars), collapse = "~")
logmove.rf <- randomForest(as.formula(formula), data = df.trop, ntree = 	50, keep.forest = TRUE) 
logmove.resid <- df.trop$logmove - logmove.rf$pred

# lm of logmove tilde on price tilde
lm1 <- lm(logmove.resid ~ price.resid_T, data = df.trop)
lm2 <- lm(logmove.resid ~ price.resid_D, data = df.trop)
lm3 <- lm(logmove.resid ~ price.resid_MM, data = df.trop)


rownames = c("Q Trop", "Q Dom", "Q MM")
colnames = c("P Trop", "P Dom", "P MM")
Elast_matrix <- matrix(,3,3, dimnames = list(rownames, colnames))

Elast_matrix[1,1] <- coef(lm1)[2]
Elast_matrix[1,2] <- coef(lm2)[2]
Elast_matrix[1,3] <- coef(lm3)[2]


# do double ml
# DOMINICKS
# get price residual
# exlude logmove, price, price.thisweek of brands
vars <- paste(c(names(df.dom)[-c(4,6,21,22,23)],"price_T.lastweek*EDUC","price_T.lastweek*INCOME","price_T.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_T.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.dom, ntree = 	50, keep.forest = TRUE) 
price.resid_T <- df.dom$price_T.thisweek - price.rf$pred

vars <- paste(c(names(df.dom)[-c(4,6,21,22,23)],"price_D.lastweek*EDUC","price_D.lastweek*INCOME","price_D.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_D.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.dom, ntree = 	50, keep.forest = TRUE) 
price.resid_D <- df.dom$price_D.thisweek - price.rf$pred

vars <- paste(c(names(df.dom)[-c(4,6,21,22,23)],"price_MM.lastweek*EDUC","price_MM.lastweek*INCOME","price_MM.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_MM.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.dom, ntree = 	50, keep.forest = TRUE) 
price.resid_MM <- df.dom$price_MM.thisweek - price.rf$pred

# get logmove residual
# exlude logmove
vars <- paste(c(names(df.dom)[-c(4,6)],"price_T.lastweek*EDUC","price_T.lastweek*INCOME","price_T.lastweek*HHLARGE",
                "price_D.lastweek*EDUC","price_D.lastweek*INCOME","price_D.lastweek*HHLARGE",
                "price_MM.lastweek*EDUC","price_MM.lastweek*INCOME","price_MM.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("logmove",vars), collapse = "~")
logmove.rf <- randomForest(as.formula(formula), data = df.dom, ntree = 	50, keep.forest = TRUE) 
logmove.resid <- df.dom$logmove - logmove.rf$pred

# lm of logmove tilde on price tilde
lm1 <- lm(logmove.resid ~ price.resid_T, data = df.dom)
lm2 <- lm(logmove.resid ~ price.resid_D, data = df.dom)
lm3 <- lm(logmove.resid ~ price.resid_MM, data = df.dom)

Elast_matrix[2,1] <- coef(lm1)[2]
Elast_matrix[2,2] <- coef(lm2)[2]
Elast_matrix[2,3] <- coef(lm3)[2]



# do double ml
# MINUTE MAID
# get price residual
# exlude logmove, price, price.thisweek of brands
vars <- paste(c(names(df.mm)[-c(4,6,21,22,23)],"price_T.lastweek*EDUC","price_T.lastweek*INCOME","price_T.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_T.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.mm, ntree = 	50, keep.forest = TRUE) 
price.resid_T <- df.mm$price_T.thisweek - price.rf$pred

vars <- paste(c(names(df.mm)[-c(4,6,21,22,23)],"price_D.lastweek*EDUC","price_D.lastweek*INCOME","price_D.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_D.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.mm, ntree = 	50, keep.forest = TRUE) 
price.resid_D <- df.mm$price_D.thisweek - price.rf$pred

vars <- paste(c(names(df.mm)[-c(4,6,21,22,23)],"price_MM.lastweek*EDUC","price_MM.lastweek*INCOME","price_MM.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("price_MM.thisweek",vars), collapse = "~")
price.rf <- randomForest(as.formula(formula), data = df.mm, ntree = 	50, keep.forest = TRUE) 
price.resid_MM <- df.mm$price_MM.thisweek - price.rf$pred

# get logmove residual
# exlude logmove
vars <- paste(c(names(df.mm)[-c(4,6)],"price_T.lastweek*EDUC","price_T.lastweek*INCOME","price_T.lastweek*HHLARGE",
                "price_D.lastweek*EDUC","price_D.lastweek*INCOME","price_D.lastweek*HHLARGE",
                "price_MM.lastweek*EDUC","price_MM.lastweek*INCOME","price_MM.lastweek*HHLARGE"), collapse = "+")
formula <- paste(c("logmove",vars), collapse = "~")
logmove.rf <- randomForest(as.formula(formula), data = df.mm, ntree = 	50, keep.forest = TRUE) 
logmove.resid <- df.mm$logmove - logmove.rf$pred

# lm of logmove tilde on price tilde
lm1 <- lm(logmove.resid ~ price.resid_T, data = df.mm)
lm2 <- lm(logmove.resid ~ price.resid_D, data = df.mm)
lm3 <- lm(logmove.resid ~ price.resid_MM, data = df.mm)

Elast_matrix[3,1] <- coef(lm1)[2]
Elast_matrix[3,2] <- coef(lm2)[2]
Elast_matrix[3,3] <- coef(lm3)[2]

Elast_matrix