# Brian Kang
# Must use 64-bit version of R
#knitr::opts_chunk$set(error=TRUE)
#rm(list = ls())

setwd("H:/Honors Research")
#chooseCRANmirror(graphics=FALSE, ind=1)
#install.packages(c("hdm","dplyr","stringr","glmnet","gdata","fastDummies"))
library(hdm)
library(dplyr)
library(stringr)
library(glmnet)
library(gdata)
library(fastDummies)
library(lmtest)
library(car)

# Import dataset ---------------------------------------------------------------------------------------------------
temp <- read.csv("psam_pusb.csv", header = T, nrows = 1)
# columns that don't import as factors using default settings
fnf <- c("DIVISION","PUMA","REGION","ST","ADJINC","CIT","COW","DDRS","DEAR",
         "DEYE","DOUT","DPHY","DRAT","DRATX","DREM","ENG","FER","GCL","GCM","GCR",
         "HINS1","HINS2","HINS3","HINS4","HINS5","HINS6","HINS7","JWTR","LANX",
         "MAR","MARHD","MARHM","MARHT","MARHW","MIG","MIL","MLPA","MLPB","MLPCD",
         "MLPE","MLPFG","MLPH","MLPI","MLPJ","MLPK","NWAB","NWAV","NWLA","NWLK",
         "NWRE","RELP","SCH","SCHG","SCHL","SEX","WKL","WKW","WRK","ANC","ANC1P",
         "ANC2P","DECADE","DIS","DRIVESP","ESP","ESR","FOD1P","FOD2P","HICOV",
         "HISP","INDP","JWAP","JWDP","LANP","MIGPUMA","MIGSP","MSP","NATIVITY",
         "NOP","OC","OCCP","PAOC","POBP","POWPUMA","POWSP","PRIVCOV","PUBCOV",
         "QTRBIR","RAC1P","RAC2P","RAC3P","RACAIAN","RACASN","RACBLK","RACNH",
         "RACNUM","RACPI","RACSOR","RACWHT","RC","SCIENGP","SCIENGRLP","SFN",
         "SFR","VPS","WAOB")
# columns that do import as factors using default setting
fif <- c("RT","SERIALNO","NAICSP","SOCP")
# all columns that are factors
fcf <- append(fnf,fif)
fcf <- append(fcf,names(temp[,c(131:286)]))
# vector of classes of data columns
colclass <- ifelse(colnames(temp) %in% fcf, 'factor', 'numeric')

temp1 <- read.csv("psam_pusa.csv", header = T, colClasses = colclass)    # U.S. PUMS data
temp2 <- read.csv("psam_pusb.csv", header = T, colClasses = colclass)
dat <- rbind(temp1,temp2)
dat <- dat[,-c(1,2)] # drop unecessary IDs
dat <- dat[,-c(129:284)] # drop unnecessary flag vars


# Data exploration ---------------------------------------------------------------------------------------------------
#which(colnames(dat)=="FAGEP")
#head(dat)
#summary(dat[,-c(131:286)])
#str(dat[,-c(131:286)])

#hist(dat$POVPIP)
#hist(dat$SPORDER)
#hist(dat$PERNP) # heavy right skew
#hist(dat$PINCP) # heavy right skew
#summary(dat$PINCP)
#summary(dat$PERNP)

#hist(dat$PERNP+dat$PINCP) # heavy right skew
#summary(dat$PERNP+dat$PINCP)

#plot(dat$SPORDER, dat$OC)
#summary(dat$OC)

# size of family unit, age, related children
# count under 18 yrs

# sporder: person number in family
# oc: own child (yes or no)
# paoc: gender and age of child
# rc: if this person is a related child or not

# agep: age of person
# SUM (intp, oip, pap, retp, semp, ssip, ssp,
# wagp) = 
# pernp, for total income


# US Census: "Income used to calculate poverty status includes PERNP (earnings) and PINCP (income)"



# Calculate Income-Poverty ratio ----------------------------------------------------------------------------------
# (POVPIP only shows NA or <0.5 or >=0.5 so calculate actual ratio)
PovertyThreshold <- rep(NA, nrow(dat))
getThreshold <- function(threshold) { # values from CPS 2018
  for (i in 1:nrow(dat)) {
    if (dat$AGEP[i] < 18) { # under 18 yrs
      threshold[i] <- NA
    } else if (dat$SPORDER[i]==1 & dat$AGEP[i] < 65) { # individual
      threshold[i] <- 13064
    } else if (dat$SPORDER[i]==1 & dat$AGEP[i] >= 65) {
      threshold[i] <- 12043
      # AGEP doesn't have NA values
    } else if (dat$SPORDER[i]==2 & dat$AGEP[i] < 65 & dat$OC[i]==0) { # two people
      threshold[i] <- 16815
    } else if (dat$SPORDER[i]==2 & dat$AGEP[i] < 65 & dat$OC[i]==1) {
      threshold[i] <- 17308
    } else if (dat$SPORDER[i]==2 & dat$AGEP[i] >= 65 & dat$OC[i]==0) {
      threshold[i] <- 15178
    } else if (dat$SPORDER[i]==2 & dat$AGEP[i] >= 65 & dat$OC[i]==1) {
      threshold[i] <- 17242
    } else if (dat$SPORDER[i]==2) { # OC is NA value
      threshold[i] <- 16247
    } else if (dat$SPORDER[i]==3 & dat$OC[i]==0) { # three people
      threshold[i] <- 19642
    } else if (dat$SPORDER[i]==3 & dat$OC[i]==1) {
      threshold[i] <- (20212+20231)/2
    } else if (dat$SPORDER[i]==3) { # OC is NA value
      threshold[i] <- 19985
    } else if (dat$SPORDER[i]==4 & dat$OC[i]==0) { # four people
      threshold[i] <- 25900
    } else if (dat$SPORDER[i]==4 & dat$OC[i]==1) {
      threshold[i] <- (26324+25465+25554)/3
    } else if (dat$SPORDER[i]==4) { # OC is NA value
      threshold[i] <- 25701
    } else if (dat$SPORDER[i]==5 & dat$OC[i]==0) { # five people
      threshold[i] <- 31234
    } else if (dat$SPORDER[i]==5 & dat$OC[i]==1) {
      threshold[i] <- (31689+30718+29967+29509)/4
    } else if (dat$SPORDER[i]==5) { # OC is NA value
      threshold[i] <- 30459
    } else if (dat$SPORDER[i]==6 & dat$OC[i]==0) { # six people
      threshold[i] <- 35925
    } else if (dat$SPORDER[i]==6 & dat$OC[i]==1) {
      threshold[i] <- (36068+35324+34612+33553+32925)/5
    } else if (dat$SPORDER[i]==6) { # OC is NA value
      threshold[i] <- 34533
    } else if (dat$SPORDER[i]==7 & dat$OC[i]==0) { # seven people
      threshold[i] <- 41336
    } else if (dat$SPORDER[i]==7 & dat$OC[i]==1) {
      threshold[i] <- (4159+40705+40085+38929+37581+36102)/6
    } else if (dat$SPORDER[i]==7) { # OC is NA value
      threshold[i] <- 39194
    } else if (dat$SPORDER[i]==8 & dat$OC[i]==0) { # eight people
      threshold[i] <- 46231
    } else if (dat$SPORDER[i]==8 & dat$OC[i]==1) {
      threshold[i] <- (46640+45800+45064+44021+42696+41317+40967)/7
    } else if (dat$SPORDER[i]==8) { # OC is NA value
      threshold[i] <- 43602
    } else if (dat$SPORDER[i]>=9 & dat$OC[i]==0) { # nine or more people
      threshold[i] <- 55613
    } else if (dat$SPORDER[i]>=9 & dat$OC[i]==1) {
      threshold[i] <- (55883+55140+54516+53491+52082+50807+50491+48546)/8
    } else if (dat$SPORDER[i]>=9) { # OC is NA value
      threshold[i] <- 51393
    } else {
      threshold[i] <- NA
    }
  } # individually assign poverty threshold
  return(threshold)
}
PovertyThreshold <- getThreshold(PovertyThreshold)
dat$IncomePovertyRatio <- (dat$PERNP + dat$PINCP)/PovertyThreshold

#summary(dat$IncomePovertyRatio) # there are negative values, will cause problems for log()
dat$IncomePovertyRatio <- dat$IncomePovertyRatio + 1 + abs(min(dat$IncomePovertyRatio, na.rm=na.omit)) # ensure all values are positive


# Data Cleaning (for initial LASSO attempt) -------------------------------------------------------------------------------------------------------------
#temp <- dat
#dat <- temp # recover original dataset

#library(dplyr)
#nums <- unlist(lapply(dat, is.factor))
#dattemp <- dat[,nums] # all the factors of dataset
# delete variables have >51 factor levels
#get1 <- names(which(sapply(dattemp, function(x) length(unique(x))>51)))
#delete <- which(names(dat) %in% get1)
#dat <- dat[,-c(delete)]
# delete variables deemed unrelated or unnecessary
#dat <- dat[,-c(9,11:19,35,43:52,82,90,108,109,95:102)] # delete CITWP,DDRS~ENG,LANX,MIL~MLPK,HISP,POVPIP,VPS,WAOB,RACAIAN~RACWHT
# delete NA rows for Income-Poverty ratio
#dat <- dat[!is.na(dat$IncomePovertyRatio),]
#which(colnames(dat)=="JWRIP") # likely unrelated to overall model, redundant
#which(colnames(dat)=="YOEP") # too many NAs
#dat <- dat[,-c(23,53)]

# delete NA rows for JWMNP (travel time to work)
#dat <- dat[!is.na(dat$JWMNP),]
# delete NA rows for MARHYP: for factor <2 error
#dat <- dat[!is.na(dat$MARHYP),]

# get which variables have <2 factor levels
#get2 <- which(sapply(dat, function(x) length(unique(x))<2))
#varnames <- paste(c(names(dat[,-c(get2,ncol(dat))])), collapse = "+")

# throw in everything and see what happens with LASSO
#formula <- paste(c("IncomePovertyRatio",varnames), collapse = "~")

# Initial Model 1 (### AN ATTEMPT ###): LASSO OLS ---------------------------------------------------------------------------------------------------------
# throw in everything and see what happens with LASSO
#sink("lasso_output.txt")  # start outputing to text file
#library(hdm)
#lasso.1 <- rlasso(formula, data = dat, post = F)
#cat("\nSummary of LASSO: \n")
#summary(lasso.1, all = F)

# get ceoffs that matter and make OLS formula
#x <- which(coef(lasso.1)[-1]!=0)
#cat("\nHow many variables to keep: \n")
#length(x)
#x <- paste(names(x), collapse = "+")
#formula <- paste(c("IncomePovertyRatio", x), collapse = " ~ ")

# function for catching error
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(error=err)
}

#library(stringr)
# name all extra variables created from doing OLS
#dattemp <- dat
#for (i in 1:90) { # look at formula and count how many new vars need to be made
#  error <- myTryCatch(olsLasso.1<- lm(formula, data = dattemp))
#  newvars <- substr(error[[1]], 45, str_length(error[[1]])-12)
#  existingvars <- names(dattemp)[which(str_detect(newvars, names(dattemp)))]
#  existingchars <- sub(existingvars, "", newvars)
#  dattemp[,newvars] <- dattemp[,which(names(dattemp)==existingvars)]==existingchars
#}

#delete <- which(colSums(is.na(dattemp))==nrow(dattemp))
#dattemp$MARHT2 <- dattemp$MARHT == "2"
#dattemp$SCHG14 <- dattemp$SCHG == "14"
#dattemp$SCHG15 <- dattemp$SCHG == "15"
#dattemp$SCHG16 <- dattemp$SCHG == "16"
#dattemp$PAOC2 <- dattemp$PAOC == "2"
#dattemp$PAOC4 <- dattemp$PAOC == "4"


# OLS regression post LASSO
#olsLasso.1 <- lm(formula, data = dattemp)
#result <- summary(olsLasso.1)
#cat("\nPost-LASSO OLS Regression output: \n")
#result
#sink()  # stop writing to text file



## Initial Model 2: Double ML (Double LASSO) (with endogeneity) ----------------------------------------------------------------
# Based on MODEL 3 from "main.R" (econ483 code)
temp <- dat
dat <- temp # recover original dataset

# DATA CLEANING FOR DOUBLE LASSO
nums <- unlist(lapply(dat, is.factor))
dattemp <- dat[,nums] # all the factors of dataset
# delete variables have >51 factor levels
get1 <- names(which(sapply(dattemp, function(x) length(unique(x))>51)))
delete <- which(names(dat) %in% get1[-16])
dat <- dat[,-c(delete)]
dat <- dat[,-c(9,43:52,82,90,108,95,100)] # delete CITWP,MIL~MLPK,HISP,POVPIP,SFR,RAC1P,RACNUM
dat <- dat[,-c(1,3,76,77)] # delete DIVISION,REGION,PERNP,PINCP
# delete NA rows for Income-Poverty ratio
dat <- dat[!is.na(dat$IncomePovertyRatio),]
#which(colnames(dat)=="JWRIP") # likely unrelated to overall model, redundant
#which(colnames(dat)=="YOEP") # too many NAs
dat <- dat[,-c(which(colnames(dat)=="JWRIP"),which(colnames(dat)=="YOEP"))]
# delete NA rows for JWMNP (travel time to work)
dat <- dat[!is.na(dat$JWMNP),]
# delete NA rows for MARHYP: for factor <2 error
dat <- dat[!is.na(dat$MARHYP),]
# delete NA rows for WKHP (usual hours worked per week)
dat <- dat[!is.na(dat$WKHP),]
#colSums(is.na(dat))


# delete/add vars that should be deleted/added (from correlation/causal inference)
dat <- dat[, -which(names(dat) %in% c("INTP","OIP","PAP","RETP","SEMP","SSIP","SSP","WAGP"))]   # drop values related to Income
dat <- dat[, -which(names(dat) %in% c("SCH","SCHG"))]   # drop weird educ vars
dat <- dat[, -which(names(dat) %in% c("ANC"))]  # drop ancestry
dat <- droplevels(dat)
#str(dat)
dat <- dat[, -which(names(dat) %in% c("ESR","ESP"))]  # drop meaningless labor vars
dat <- dat[, -which(names(dat) %in% c("HICOV","PRIVCOV","PUBCOV"))]  # drop redundant insurance vars
dat <- dat[, -which(names(dat) %in% c("OC","RC"))]  # drop child vars. Lack data
dat <- dat[, -which(names(dat) %in% c("SFN"))]  # drop "subfamily number"
dat <- dat[, -which(names(dat) %in% c("WRK"))]  # drop "worked last week" (we don't know when is "last week")

dat <- cbind(dat, model.matrix(~(AGEP:HINS3), dat)[,-1])  # age*medicare
dat <- cbind(dat, model.matrix(~(SCIENGP:SCHL), dat)[,-1])  # stem degree*attained degree
dat <- cbind(dat, model.matrix(~(SCIENGRLP:SCHL), dat)[,-1])  # stem related degree*attained degree
dat$VETERAN <- ifelse((dat$DRATX %in% c("1","2")), 1, 0)  # veteran or not
dat <- dat[, -which(names(dat) %in% c("DRATX","VPS","DRAT"))]  # drop veteran related vars
dat <- cbind(dat, model.matrix(~(AGEP:VETERAN), dat)[,2])  # age*veteran or not
names(dat)[ncol(dat)] <- "AGEP_VETERAN"
dat <- cbind(dat, model.matrix(~(AGEP:GCL), dat)[,-1])  # age*grandparent living with grandchild
dat <- cbind(dat, model.matrix(~(AGEP:GCR), dat)[,-1])  # age*grandparent responsible grandchild


# MUST INCLUDE THIS LINE OF CODE MUST INCLUDE THIS LINE OF CODE MUST INCLUDE THIS LINE OF CODE
# logical dummy for ST==POWSP: most are FALSE
dat$SameResidenceWorkplace <- (as.numeric(dat$ST)==as.numeric(dat$POWSP))
dat <- dat[,-which(names(dat) %in% c("ST","POWSP"))]  # delete ST,POWSP
#colSums(is.na(dat))
# get which variables have <2 factor levels
get2 <- which(sapply(dat, function(x) length(unique(x)))<2)
dat <- dat[,-get2]
names(dat) <- str_replace(names(dat), ":", "_") # reformat interaction term names

# temporary interaction term
#getInteraction <- interaction(dat$SameResidenceWorkplace, dat$JWMNP, drop=T)
# delete interaction terms with 0 or 1 observations
# because some interactions have too few obsersavtions, with k-fold cv.glmnet, some only have 1 observations
#interactionDelete <- 
#  names(which(tapply(getInteraction,getInteraction,length) < 15))
#get3 <- which(getInteraction %in% interactionDelete)
#dat <- dat[-c(get3),]



# STEP 1: FIRST LASSO: LOG(IncPovRatio) on ALL POTENTIAL VARIATES (i.e. y on focal)
varnames <- paste(c(names(dat[,-c(which(names(dat) %in% 
                                          c("IncomePovertyRatio","SameResidenceWorkplace",
                                            "JWMNP","JWTR")))])), collapse = "+")
# throw in everything and see what happens with this LASSO
formula <- paste(c("log(IncomePovertyRatio)",varnames), collapse = "~")
#cat("LASSO Formula for y ~ focal variables\n")
#formula

# Split data into train and test for K-fold CV
set.seed(497)
train <- sample(1:nrow(dat), nrow(dat)*0.8)  # 80% for training

# get which variables have <2 factor levels AFTER SUBSETTING
get <- which(sapply(dat[train,], function(x) length(unique(x))<2))

# get train and test datasets
# takeout intercept
xtrain <- model.matrix(as.formula(formula), data = dat[train,])[,-1]
#xtest <- model.matrix(as.formula(formula), data = dat[-train,])[,-1]
ytrain <- log(dat[train,]$IncomePovertyRatio)

# cross validation then fit LASSO
cv.lasso.1 <- cv.glmnet(xtrain, ytrain, alpha = 1)  # 1 for lasso
#cat("CV on LASSO and get min tuning parameter\n")
#cv.lasso.1[c(8,9)]
cv.lambda.1 <- cv.lasso.1$lambda.min  # get smallest tuning parameter
#cat("\nLambda chosen from K-fold CV\n")
cv.lambda.1
#png(filename="lassoCV.png")  # save plot
plot(cv.lasso.1)

# run lasso and get the necessary focal variables
dlasso.1 <- rlasso(formula, data = dat[train,], 
                   lambda.start = cv.lambda.1, post = F)
#summary(lasso.2, all = F)
control <- which(coef(dlasso.1)[-1]!=0)
#cat("\nHow many variables to keep\n")
length(control)


# STEP 2: SECOND LASSO: Core vars on ALL POTENTIAL VARIATES (i.e. controls on focal)
formula2.1 <- paste(c("SameResidenceWorkplace",varnames), collapse = "~")
# k-fold cv
xtrain <- model.matrix(as.formula(formula2.1), data = dat[train,])[,-1]
ytrain <- dat[train,]$SameResidenceWorkplace
cv.lasso.2.1 <- cv.glmnet(xtrain, ytrain, alpha = 1)  # 1 for lasso
cv.lambda.2.1 <- cv.lasso.2.1$lambda.min  # get smallest tuning parameter
cv.lambda.2.1
plot(cv.lasso.2.1)
# lasso
dlasso.2.1 <- rlasso(formula2.1, data = dat[train,], 
                     lambda.start = cv.lambda.2.1, post = F)
#summary(dlasso.2.1, all = F)
focal1 <- which(coef(dlasso.2.1)[-1]!=0)
length(focal1)


# travel time
formula2.2 <- paste(c("JWMNP",varnames), collapse = "~")
# k-fold cv
xtrain <- model.matrix(as.formula(formula2.2), data = dat[train,])[,-1]
ytrain <- dat[train,]$JWMNP
cv.lasso.2.2 <- cv.glmnet(xtrain, ytrain, alpha = 1)  # 1 for lasso
cv.lambda.2.2 <- cv.lasso.2.2$lambda.min  # get smallest tuning parameter
cv.lambda.2.2
plot(cv.lasso.2.2)
# lasso
dlasso.2.2 <- rlasso(formula2.2, data = dat[train,], 
                     lambda.start = cv.lambda.2.2, post = F)
#summary(dlasso.2.2, all = F)
focal2 <- which(coef(dlasso.2.2)[-1]!=0)
length(focal2)


#set.seed(497)
#train <- sample(1:nrow(dat), nrow(dat)*0.8)  # 80% for training
# interaction term SameResidenceWorkplace * travel time
#formula2.3 <- paste(c("SameResidenceWorkplace_JWMNP",varnames), collapse = "~")
# k-fold cv
#tempdat <- dat

#model.matrix(~SameResidenceWorkplace:JWMNP,dat)[,c(2,3)]

##tempdat$SameResidenceWorkplace_JWMNP <- interaction(tempdat$SameResidenceWorkplace, tempdat$JWMNP, drop=T)
#xtrain <- model.matrix(as.formula(formula2.3), data = tempdat[train,])[,-1]
#ytrain <- tempdat[train,]$SameResidenceWorkplace_JWMNP
#tempdat %>% summarise_all(n_distinct)
#any(tapply(ytrain, ytrain, length)<9)
##### THIS LINE THROWS ERROR ###########
#cv.lasso.2.3 <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "multinomial", nfolds = 3)  # 1 for lasso
#Error in lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs,  : 
#one multinomial or binomial class has 1 or 0 observations; not allowed
#In addition: Warning messages:
#  1: In lognet(x, is.sparse, ix, jx, y, weights, offset, alpha, nobs,  :
#                 one multinomial or binomial class has fewer than 8  observations; dangerous ground
#               2: from glmnet Fortran code (error code -47); Convergence for 47th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned 
#               Error in save(list = names(.GlobalEnv), file = outfile, version = version,  : 
#                               error writing to connection
#                             Error saving session (search_path): R code execution error
#                             Error in save(list = names(.GlobalEnv), file = outfile, version = version,  : 
#                                             error writing to connection
#                                           Error saving session (search_path): R code execution error
#                                           Error in save(list = names(.GlobalEnv), file = outfile, version = version,  : 
#                                                           error writing to connection
#                                                         Error saving session (search_path): R code execution error
########################################
#cv.lambda.2.3 <- cv.lasso.2.3$lambda.min  # get smallest tuning parameter
#cv.lambda.2.3
#plot(cv.lasso.2.3)
# lasso
#dlasso.2.3 <- rlasso(formula2.3, data = tempdat[train,], 
#                     lambda.start = cv.lambda.2.3, post = F)
#summary(dlasso.2.3, all = F)
#focal3 <- which(coef(dlasso.2.3)[-1]!=0)
#length(focal3)


formula2.4 <- paste(c("JWTR",varnames), collapse = "~")
# k-fold cv
xtrain <- model.matrix(as.formula(formula2.4), data = dat[train,])[,-1]
ytrain <- drop.levels(dat[train,]$JWTR) # factor level "11" has 0 observations, use drop.levels()
cv.lasso.2.4 <- cv.glmnet(xtrain, ytrain, alpha = 1, family = "multinomial", nfolds = 3) # 1 for lasso
cv.lambda.2.4 <- cv.lasso.2.4$lambda.min  # get smallest tuning parameter
cv.lambda.2.4
plot(cv.lasso.2.4)
# lasso
tempdat2 <- fastDummies::dummy_cols(dat)
focal4 <- c()
for (ii in 1:11) {
  if (ii<10) {
    formula2.4.n <- paste(c(paste("JWTR_0",ii,sep=""),varnames), collapse = "~")
    dlasso.2.4.n <- rlasso(formula2.4.n, data = tempdat2[train,], 
                           lambda.start = cv.lambda.2.4, post = F)
    focal4.n <- which(coef(dlasso.2.4.n)[-1]!=0)
    focal4 <- unique(c(focal4, names(focal4.n)))
  } else if (ii==10) {
    formula2.4.n <- paste(c("JWTR_10",varnames), collapse = "~")
    dlasso.2.4.n <- rlasso(formula2.4.n, data = tempdat2[train,], 
                           lambda.start = cv.lambda.2.4, post = F)
    focal4.n <- which(coef(dlasso.2.4.n)[-1]!=0)
    focal4 <- unique(c(focal4, names(focal4.n)))
  } else if (ii==11) {
    formula2.4.n <- paste(c("JWTR_12",varnames), collapse = "~")
    dlasso.2.4.n <- rlasso(formula2.4.n, data = tempdat2[train,], 
                           lambda.start = cv.lambda.2.4, post = F)
    focal4.n <- which(coef(dlasso.2.4.n)[-1]!=0)
    focal4 <- unique(c(focal4, names(focal4.n)))
  }
}
length(focal4)


# STEP 3: Take union of all remainder potential variates
union <- c(names(control), names(focal1), names(focal2), focal4)
if (any(duplicated(union))==T) {
  union <- unique(union)
}

#cat("\nTotal number of feature variables kept from Double Lasso\n")
length(union)


# STEP 4: do OLS of y on focals and kept potential variates
unionf <- paste(c("SameResidenceWorkplace*JWMNP+JWTR*JWMNP",union), collapse = "+")
formula <- paste(c("log(IncomePovertyRatio)", unionf), collapse = "~")

# name all extra variables created from doing LASSO
dattemp <- dat
#olsDLasso1<- lm(formula, data = dattemp)
# run this code if rlasso created extra dummy vars
for (i in 1:500) { # look at formula and count how many new vars need to be made
  error <- myTryCatch(olsDLasso1<- lm(formula, data = dattemp)) # CAUTION
  newvars <- substr(error[[1]], 45, str_length(error[[1]])-12)
  existingvars <- names(dattemp)[which(str_detect(newvars, names(dattemp)))]
  existingchars <- sub(existingvars, "", newvars)
  dattemp[,newvars] <- dattemp[,which(names(dattemp)==existingvars)]==existingchars
}


# start with declairing the new vars
which(colSums(is.na(dattemp))==nrow(dattemp))
dattemp$SCIENGRLP1 <- dattemp$SCIENGRLP == "1"
dattemp$SCIENGRLP2 <- dattemp$SCIENGRLP == "2"

which(lapply(dattemp, class)=="matrix")
dattemp$MARHT3 <- dattemp$MARHT == "3"
dattemp$MARHD2 <- dattemp$MARHD == "2"
dattemp$MARHD8 <- dattemp$MARHD == "8"
dattemp$MARHT2 <- dattemp$MARHT == "2"

# multicolinearity: get which variables have <2 unique values
multicol <- names(which(sapply(dattemp[train,], function(x) length(unique(x))<2)))
# manually delete some of the rest (NA values in summary of lm, multicollinearity)
multicol <- c(multicol,
              "MSP3","MSP4","MSP5","ENG1","SCHL21","DRIVESP6",
              "NATIVITY2","SCHL18","DECADE6","WAOB4")
union <- union[-which(union %in% multicol)] # delete them from formula
aliased <- which(summary(lm(formula, data = dattemp[train,]))$aliased)
union <- union[-which(union %in% names(aliased))]

# rewrite formula for OLS
unionf <- paste(c("SameResidenceWorkplace*JWMNP+JWTR*JWMNP",union), collapse = "+")
formula <- paste(c("log(IncomePovertyRatio)", unionf), collapse = "~")

# Training OLS regression post LASSO
olsDLasso1 <- lm(formula, data = dattemp[train,])
DMLresult <- summary(olsDLasso1)
#cat("\nPost-Double LASSO OLS Result\n")
DMLresult


# Test Prediction
pred.olsDLasso.1 <- predict(olsDLasso1, newdata = dattemp[-train,])
summary(pred.olsDLasso.1)

length(na.omit(pred.olsDLasso.1)) # count remaining observations
# test error
mse.1 <- mean((pred.olsDLasso.1-log(dattemp[-train,]$IncomePovertyRatio))^2, na.rm=T)
#cat("\nMSE\n")
mse.1


# 3 Ways of getting Test R2
y <- log(dattemp[-train,]$IncomePovertyRatio)-mean(log(dattemp[-train,]$IncomePovertyRatio))
yhat <- pred.olsDLasso.1-mean(pred.olsDLasso.1)
u <- y - yhat
# 1:
# R2 = yhat*yhat/yTy
r2_1 <- (yhat %*% yhat)/(y %*% y)
r2_1

# 2:
# R2 = 1- SSR/SST = 1- uTu/yTy
r2_2 <- 1 - (u %*% u)/(y %*% y)
r2_2

# 3:
# R2 = corr(y, yhat)^2, "fair r-squared"
r2_3 <- cor.test(y, yhat, use = "complete.obs")
# now, square the correlation coefficient
r2_3
r2_3$estimate^2
# or r2_3 = 
require(miscTools)
rSquared(log(dattemp[-train,]$IncomePovertyRatio), resid = (log(dattemp[-train,]$IncomePovertyRatio)-pred.olsDLasso.1))


# False Discovery Rate control
p <- as.data.frame(DMLresult$coefficients[,4])
sigcode <- cut(p[,1], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, 1), 
               labels = c("***", "**", "*", ".", " "))
p$"" <- sigcode

# sort by increasing p-value
p <- p[order(p$`DMLresult$coefficients[, 4]`),]
p$BY <- 0
m <- nrow(p)
Q = 0.10  # 10%
cm=0
for (ii in 1:m) {
  cm = cm + 1/ii
  p[ii,3] <- ii/m/cm*Q
}
noreject <- (!(p[,1] < p[,3]))
plot(p$`DMLresult$coefficients[, 4]`,ylab="P-Value", col = ifelse(noreject,'red','black'))
noreject <- which(noreject)
p <- p[noreject,]   # these one's we cannot reject the null
names(p) <- c("p-value","Sig. Level","BY Stat")
p

# get BY-adjusted p-values
pBY <- as.data.frame(p.adjust(p[,1], method = "BY"))   #Benjamini-Yekutieli
rownames(pBY) <- rownames(p)
adjsigcode <- cut(pBY[,1], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, 1), 
               labels = c("***", "**", "*", ".", " "))
pBY$"" <- adjsigcode

# compare p-values for non-rejected
fdr <- cbind.data.frame(p[,c(1,2)], pBY)
#rownames(fdr) <- getName
colnames(fdr) <- c("Original","Sig. Level", "FDR Adj.","Sig. Level")
fdr

# BP test for heteroskedasticity
bpres1 <- bptest(olsDLasso1, data = dattemp[-train,]) #reject homoskedasticity if p-value is small
bpres1


# F-test
null = c("SameResidenceWorkplaceTRUE","JWMNP",
         "JWTR02","JWTR03","JWTR04","JWTR05","JWTR06","JWTR07","JWTR08",
         "JWTR09","JWTR10","JWTR12")
if (bpres1$p.value >= 0.001) {   # homoskedastic
  linearHypothesis(olsDLasso1, null, vcov = hccm(olsDLasso1, type = "hc0")) # classical White VCOV
} else {
  linearHypothesis(olsDLasso1, null) # default homoskedastic error
}

null = c("JWMNP",
         "JWTR02","JWTR03","JWTR04","JWTR05","JWTR06","JWTR07","JWTR08",
         "JWTR09","JWTR10","JWTR12",
         "JWMNP:JWTR02","JWMNP:JWTR03","JWMNP:JWTR04","JWMNP:JWTR05","JWMNP:JWTR06",
         "JWMNP:JWTR07","JWMNP:JWTR08","JWMNP:JWTR09","JWMNP:JWTR10","JWMNP:JWTR12")
if (bpres1$p.value >= 0.001) {   # homoskedastic
  linearHypothesis(olsDLasso1, null, vcov = hccm(olsDLasso1, type = "hc0")) # classical White VCOV
} else {
  linearHypothesis(olsDLasso1, null)
}

null = c("SameResidenceWorkplaceTRUE","JWMNP",
         "JWTR02","JWTR03","JWTR04","JWTR05","JWTR06","JWTR07","JWTR08",
         "JWTR09","JWTR10","JWTR12",
         "SameResidenceWorkplaceTRUE:JWMNP",
         "JWMNP:JWTR02","JWMNP:JWTR03","JWMNP:JWTR04","JWMNP:JWTR05","JWMNP:JWTR06",
         "JWMNP:JWTR07","JWMNP:JWTR08","JWMNP:JWTR09","JWMNP:JWTR10","JWMNP:JWTR12")
if (bpres1$p.value >= 0.001) {   # homoskedastic
  linearHypothesis(olsDLasso1, null, vcov = hccm(olsDLasso1, type = "hc0")) # classical White VCOV
} else {
  linearHypothesis(olsDLasso1, null)
}


## Model 2 Post-Double ML OLs with exogenous variable selection ----------------------------------------------------------------
# manually delete potentially endogenous variables
# Can X affect or cause Income or Poverty of Both?
endog <- c("SPORDER",    # household size
           "CIT3","CIT4","CIT5",     # citizenship status
           "COW2","COW3","COW4","COW5","COW6","COW7","COW8",   # class of worker
           "DDRS2","DEYE2","DPHY2","DREM2",     # disability
           "ENG2","ENG3","ENG4","FER1","FER2",     # level of english and child birth
           "GCL2","GCR2",     # grandparents with grandchildren
           "HINS12","HINS22","HINS42","HINS52","HINS62","HINS72",    # insurance
           "MAR2","MAR3","MAR4","MARHD2","MARHT2","MARHT3",   # marriage
           "MIG2","MIG3",    # migration
           "NWAB2","NWAV5","NWLA3","NWLK3","NWRE2",   # current work status
           "RELP01","RELP02","RELP03","RELP04","RELP05","RELP06","RELP07",    # relationship in household
           "RELP08","RELP09","RELP10","RELP11","RELP12","RELP13","RELP15","RELP17"
           )
endog2 <- c(61:79,80,81:86,    # degree, sex, work
            91,92:96,97,    # disability, num cars per ppl, marriage status, 
            102:106,107:108,    # race, stem degree
            116:122,123:124,125,    # stem*degree, age*stuff, disability
            126:134,    # marriage, work, race, age*stuff, citizenship, disability, work
            136:140,    # school, veteran, grandparents with grandchild
            142:144,    # insurance, work, school
            148:152    # age*stuff, grandparents with grandchild, work
            )

union <- union[-endog2]
union <- union[-which(union %in% endog)] # delete them from formula

# rewrite formula for OLS
exogunionf <- paste(union, collapse = "+")
exogformula <- paste(c("log(IncomePovertyRatio)", exogunionf), collapse = "~")

# Training OLS regression post LASSO
olsDLasso2 <- lm(exogformula, data = dattemp[train,])
DMLresult2 <- summary(olsDLasso2)
# Post-Double LASSO OLS only on Exogeneous vars Result
DMLresult2


# Test Prediction
pred.olsDLasso.2 <- predict(olsDLasso2, newdata = dattemp[-train,])
summary(pred.olsDLasso.2)

length(na.omit(pred.olsDLasso.2)) # count remaining observations
# test error
mse.2 <- mean((pred.olsDLasso.2-log(dattemp[-train,]$IncomePovertyRatio))^2, na.rm=T)
# MSE
mse.2


# 3 Ways of getting Test R2
y2 <- log(dattemp[-train,]$IncomePovertyRatio)-mean(log(dattemp[-train,]$IncomePovertyRatio))
yhat2 <- pred.olsDLasso.2-mean(pred.olsDLasso.2)
u2 <- y2 - yhat2
# 1:
# R2 = yhat*y/yTy
r2_1_2 <- (yhat2 %*% yhat2)/(y2 %*% y2)
r2_1_2

# 2:
# R2 = 1- SSR/SST = 1- uTu/yTy
r2_2_2 <- 1 - (u2 %*% u2)/(y2 %*% y2)
r2_2_2

# 3:
# R2 = corr(y, yhat)^2, "fair r-squared"
r2_3_2 <- cor.test(y2, yhat2, use = "complete.obs")
# now, square the correlation coefficient
r2_3_2
r2_3_2$estimate^2

# BP test for heteroskedasticity
bpres2 <- bptest(olsDLasso2, data = dattemp[-train,]) #reject homoskedasticity if p-value is small
bpres2

# False Discovery Rate control
p2 <- as.data.frame(DMLresult2$coefficients[,4])
sigcode2 <- cut(p2[,1], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, 1), 
               labels = c("***", "**", "*", ".", " "))
p2$"" <- sigcode2

# sort by increasing p-value
p2 <- p2[order(p2$`DMLresult2$coefficients[, 4]`),]
p2$BY <- 0
m2 <- nrow(p2)
Q = 0.10  # 10%
cm=0
for (ii in 1:m2) {
  cm = cm + 1/ii
  p2[ii,3] <- ii/m2/cm*Q
}
noreject2 <- (!(p2[,1] < p2[,3]))
plot(p2$`DMLresult2$coefficients[, 4]`,ylab="P-Value", col = ifelse(noreject2,'red','black'))
noreject2 <- which(noreject2)
p2 <- p2[noreject2,]   # these one's we cannot reject the null
names(p2) <- c("p-value","Sig. Level","BY Stat")
p2

# get BY-adjusted p-values
pBY2 <- as.data.frame(p.adjust(p2[,1], method = "BY"))   #Benjamini-Yekutieli
rownames(pBY2) <- rownames(p2)
adjsigcode <- cut(pBY2[,1], breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, 1), 
                  labels = c("***", "**", "*", ".", " "))
pBY2$"" <- adjsigcode

# compare p-values for non-rejected
fdr2 <- cbind.data.frame(p2[,c(1,2)], pBY2)
colnames(fdr2) <- c("Original","Sig. Level", "FDR Adj.","Sig. Level")
fdr2









