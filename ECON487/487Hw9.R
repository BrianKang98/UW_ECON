rm(list = ls())
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(tree)
library(rpart)
library(rpart.plot)
library(rattle)
library(doBy)
library(caret)
library(tidyr)
library(stringr)
library(glmnet)
library(randomForest)

setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)
df$price <- log(df$price) # log price
temp <- df

# From HW8. Create lagged variables.
dat1 <- df
dat1$week <- dat1$week+1
myvars <- c("price", "week", "brand","store")
dat1 <- dat1[myvars]
dat2 <- merge(df, dat1, by = c("brand","store","week"))
names(dat2) <- gsub(".x", ".lastweek", names(dat2))
names(dat2) <- gsub(".y", ".thisweek", names(dat2))
df <- dat2

# 1b
# just interact all other focal variables
interact <- model.matrix( ~.^2, data=within(df, rm(price.thisweek, brand, logmove,store,week)))[,-1]
myvars <- c("store","week","brand","price.thisweek","logmove")
dat1 <- df[myvars]
df <- cbind(dat1, interact)
names(df) <- str_replace(names(df), ":", "_") # reformat interaction term names

# 1a. classify stores into three "leaves" based on price
fit <- rpart(price.thisweek~., data=within(df,rm(brand,logmove,store,week,price.lastweek)), method="anova",cp=0.07)
fancyRpartPlot(fit, main="price.thisweek",palettes=c("Greens","Reds"))

# assign stores into groups of leaves
df$leaf = fit$where 
leaves <- unique(df$leaf)
names(df)

# back to 1a, do 3*2 randomForests for each leafs (3)
dml <- function(dat, lf) {
  # get stores in leaf "lf"
  dat <- subset(dat, leaf==lf)
  
  # make cross price df
  dat.trop <- subset(dat, brand=="tropicana")
  dat.dom <- subset(dat, brand=="dominicks")
  dat.mm <- subset(dat, brand=="minute.maid")
  # 3,4 for store & week
  keep <- c(1,2,5,which(str_detect(names(dat.trop), "price")))
  temp.trop <- dat.trop[keep]
  for (ii in 3:length(names(temp.trop))) {
    names(temp.trop)[ii] <- paste(c(names(temp.trop)[ii],"T"), collapse = "_")
  }
  temp.dom <- dat.dom[keep]
  for (ii in 3:length(names(temp.dom))) {
    names(temp.dom)[ii] <- paste(c(names(temp.dom)[ii],"D"), collapse = "_")
  }
  temp.mm <- dat.mm[keep]
  for (ii in 3:length(names(temp.mm))) {
    names(temp.mm)[ii] <- paste(c(names(temp.mm)[ii],"MM"), collapse = "_")
  }
  # T and MM for D
  dat.dom <- merge(dat.dom, temp.trop, by=c("store","week"))
  dat.dom <- merge(dat.dom, temp.mm, by=c("store","week"))
  # T and D for MM
  dat.mm <- merge(dat.mm, temp.trop, by=c("store","week"))
  dat.mm <- merge(dat.mm, temp.dom, by=c("store","week"))
  # D and MM for T
  dat.trop <- merge(dat.trop, temp.dom, by=c("store","week"))
  dat.trop <- merge(dat.trop, temp.mm, by=c("store","week"))
  
  # delete store week, leaf, brand
  dat.trop <- dat.trop[,-which(names(dat.trop) %in% c("store","week","leaf","brand"))]
  dat.dom <- dat.dom[,-which(names(dat.dom) %in% c("store","week","leaf","brand"))]
  dat.mm <- dat.mm[,-which(names(dat.mm) %in% c("store","week","leaf","brand"))]
  
  # construct 3x3 elasticity matrix
  rownames = c("Q Trop", "Q Dom", "Q MM")
  colnames = c("P Trop", "P Dom", "P MM")
  Elast_matrix <- matrix(,3,3, dimnames = list(rownames, colnames))

  # get logmove residual
  # exlude logmove
  logmove.rf_T <- randomForest(logmove~., data = dat.trop, ntree = 	50, keep.forest = TRUE) 
  logmove.resid_T <- dat.trop$logmove - logmove.rf_T$pred
  logmove.rf_D <- randomForest(logmove~., data = dat.dom, ntree = 	50, keep.forest = TRUE) 
  logmove.resid_D <- dat.dom$logmove - logmove.rf_D$pred
  logmove.rf_MM <- randomForest(logmove~., data = dat.mm, ntree = 	50, keep.forest = TRUE) 
  logmove.resid_MM <- dat.mm$logmove - logmove.rf_MM$pred
  
  # 1c
  # ols v.s. random forest comparison plot
  makePlot <- function() {
    testlm <- lm(logmove~., data = dat.mm)
    testpred <- predict(testlm, newdata = dat.mm)
    plot(testlm$resid, col = "black", main = "Log Sales Residual of Minute Maid")
    points(logmove.resid_MM, col = "red",pch = 19,cex = 0.5)
  }
  makePlot()
  legend("topright", c("OLS", "Rand. Forest"), col=c("black","red"),pch=c(1,19),cex=.8)
  
  # get price residual
  # exlude logmove, logmove of brands, price, price.thisweek of brands
  price.rf_T <- randomForest(price.thisweek~., data = 
                               dat.trop[,-which(names(dat.trop) %in% c("logmove","logmove_D","logmove_MM",
                                                                       "price.thisweek_MM","price.thisweek_D"))]
                             , ntree = 50, keep.forest = TRUE) 
  price.resid_T <- dat.trop$price.thisweek - price.rf_T$pred
  price.rf_D <- randomForest(price.thisweek~., data = 
                               dat.dom[,-which(names(dat.dom) %in% c("logmove","logmove_T","logmove_MM",
                                                                       "price.thisweek_MM","price.thisweek_T"))]
                             , ntree = 50, keep.forest = TRUE) 
  price.resid_D <- dat.dom$price.thisweek - price.rf_D$pred
  price.rf_MM <- randomForest(price.thisweek~., data = 
                               dat.mm[,-which(names(dat.mm) %in% c("logmove","logmove_D","logmove_T",
                                                                     "price.thisweek_D","price.thisweek_T"))]
                             , ntree = 50, keep.forest = TRUE) 
  price.resid_MM <- dat.mm$price.thisweek - price.rf_MM$pred
  
  # lm of logmove tilde on price tilde
  lm1 <- lm(logmove.resid_T ~ price.resid_T+price.resid_D+price.resid_MM, data = dat.trop)
  lm2 <- lm(logmove.resid_D ~ price.resid_T+price.resid_D+price.resid_MM, data = dat.dom)
  lm3 <- lm(logmove.resid_MM ~ price.resid_T+price.resid_D+price.resid_MM, data = dat.mm)
  
  for (i in 1:3) {
    for (j in 1:3) {
      Elast_matrix[i,j] <- coef(get(paste(c("lm",i), collapse = "")))[j+1]
    }
  }
  
  # print 3x3 elasticity matrix
  print(Elast_matrix)
}

dml(df, leaves[1])
dml(df, leaves[2])

dml(df, leaves[3])

## --------------------------------------------------------------------
## IN THE THIRD LEAF DOMINICKS HAS TOO LITTLE DATA
## SO WHEN MERGED TO GET CROSS DATA, WE HAVE VERY LITTLE DATA LEFT
## Look below...
## --------------------------------------------------------------------

# get stores in leaf "lf"
dat <- subset(df, leaf==leaves[3])

# make cross price df
dat.trop <- subset(dat, brand=="tropicana")
dat.dom <- subset(dat, brand=="dominicks")
dat.mm <- subset(dat, brand=="minute.maid")

# note the number of data points
nrow(dat.trop)
nrow(dat.dom)
nrow(dat.mm)

# 3,4 for store & week
keep <- c(1,2,5,which(str_detect(names(dat.trop), "price")))
temp.trop <- dat.trop[keep]
for (ii in 3:length(names(temp.trop))) {
  names(temp.trop)[ii] <- paste(c(names(temp.trop)[ii],"T"), collapse = "_")
}
temp.dom <- dat.dom[keep]
for (ii in 3:length(names(temp.dom))) {
  names(temp.dom)[ii] <- paste(c(names(temp.dom)[ii],"D"), collapse = "_")
}
temp.mm <- dat.mm[keep]
for (ii in 3:length(names(temp.mm))) {
  names(temp.mm)[ii] <- paste(c(names(temp.mm)[ii],"MM"), collapse = "_")
}

# T and MM for D
dat.dom <- merge(dat.dom, temp.trop, by=c("store","week"))
dat.dom <- merge(dat.dom, temp.mm, by=c("store","week"))
# T and D for MM
dat.mm <- merge(dat.mm, temp.trop, by=c("store","week"))
dat.mm <- merge(dat.mm, temp.dom, by=c("store","week"))
# D and MM for T
dat.trop <- merge(dat.trop, temp.dom, by=c("store","week"))
dat.trop <- merge(dat.trop, temp.mm, by=c("store","week"))

# note the number of data points
# The issue is data points for dominicks being too scarce in this particular leaf
# With so little data, our random forest runs into issues.
nrow(dat.trop)
nrow(dat.dom)
nrow(dat.mm)

# delete store week, leaf, brand
dat.trop <- dat.trop[,-which(names(dat.trop) %in% c("store","week","leaf","brand"))]
dat.dom <- dat.dom[,-which(names(dat.dom) %in% c("store","week","leaf","brand"))]
dat.mm <- dat.mm[,-which(names(dat.mm) %in% c("store","week","leaf","brand"))]

# construct 3x3 elasticity matrix
rownames = c("Q Trop", "Q Dom", "Q MM")
colnames = c("P Trop", "P Dom", "P MM")
Elast_matrix <- matrix(,3,3, dimnames = list(rownames, colnames))

# get logmove residual
# exlude logmove
logmove.rf_T <- randomForest(logmove~., data = dat.trop, ntree = 	50, keep.forest = TRUE) 
logmove.resid_T <- dat.trop$logmove - logmove.rf_T$pred
logmove.rf_D <- randomForest(logmove~., data = dat.dom, ntree = 	50, keep.forest = TRUE) 
logmove.resid_D <- dat.dom$logmove - logmove.rf_D$pred
logmove.rf_MM <- randomForest(logmove~., data = dat.mm, ntree = 	50, keep.forest = TRUE) 
logmove.resid_MM <- dat.mm$logmove - logmove.rf_MM$pred

# get price residual
# exlude logmove, logmove of brands, price, price.thisweek of brands
price.rf_T <- randomForest(price.thisweek~., data = 
                             dat.trop[,-which(names(dat.trop) %in% c("logmove","logmove_D","logmove_MM",
                                                                     "price.thisweek_MM","price.thisweek_D"))]
                           , ntree = 50, keep.forest = TRUE) 
price.resid_T <- dat.trop$price.thisweek - price.rf_T$pred
price.rf_D <- randomForest(price.thisweek~., data = 
                             dat.dom[,-which(names(dat.dom) %in% c("logmove","logmove_T","logmove_MM",
                                                                   "price.thisweek_MM","price.thisweek_T"))]
                           , ntree = 50, keep.forest = TRUE) 
price.resid_D <- dat.dom$price.thisweek - price.rf_D$pred
price.rf_MM <- randomForest(price.thisweek~., data = 
                              dat.mm[,-which(names(dat.mm) %in% c("logmove","logmove_D","logmove_T",
                                                                  "price.thisweek_D","price.thisweek_T"))]
                            , ntree = 50, keep.forest = TRUE) 
price.resid_MM <- dat.mm$price.thisweek - price.rf_MM$pred

# lm of logmove tilde on price tilde
lm1 <- lm(logmove.resid_T ~ price.resid_T+price.resid_D+price.resid_MM, data = dat.trop)
lm2 <- lm(logmove.resid_D ~ price.resid_T+price.resid_D+price.resid_MM, data = dat.dom)
lm3 <- lm(logmove.resid_MM ~ price.resid_T+price.resid_D+price.resid_MM, data = dat.mm)

for (i in 1:3) {
  for (j in 1:3) {
    Elast_matrix[i,j] <- coef(get(paste(c("lm",i), collapse = "")))[j+1]
  }
}

# print 3x3 elasticity matrix
print(Elast_matrix)




















