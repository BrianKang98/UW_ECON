# Brian Kang
# Must use 64-bit version of R
#rm(list = ls())
setwd("H:/Honors Research")
#dat <- read.table("e20185wa0002000.txt", sep = ",")    # 5-year ACS survey Washington data

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


# Data Cleaning -------------------------------------------------------------------------------------------------------------
temp <- dat
dat <- temp # recover original dataset

library(dplyr)
nums <- unlist(lapply(dat, is.factor))
dattemp <- dat[,nums] # all the factors of dataset
# delete variables have >51 factor levels
get1 <- names(which(sapply(dattemp, function(x) length(unique(x))>51)))
delete <- which(names(dat) %in% get1)
dat <- dat[,-c(delete)]
# delete variables deemed unrelated or unnecessary
dat <- dat[,-c(9,11:19,35,43:52,82,90,108,109,95:102)] # delete CITWP,DDRS~ENG,LANX,MIL~MLPK,HISP,POVPIP,VPS,WAOB,RACAIAN~RACWHT
# delete NA rows for Income-Poverty ratio
dat <- dat[!is.na(dat$IncomePovertyRatio),]
#which(colnames(dat)=="JWRIP") # likely unrelated to overall model, redundant
#which(colnames(dat)=="YOEP") # too many NAs
dat <- dat[,-c(23,53)]

# delete NA rows for JWMNP (travel time to work)
dat <- dat[!is.na(dat$JWMNP),]
# delete NA rows for MARHYP: for factor <2 error
dat <- dat[!is.na(dat$MARHYP),]

# get which variables have <2 factor levels
get2 <- which(sapply(dat, function(x) length(unique(x))<2))
varnames <- paste(c(names(dat[,-c(get2,ncol(dat))])), collapse = "+")

# throw in everything and see what happens with LASSO
formula <- paste(c("IncomePovertyRatio",varnames), collapse = "~")

# Model 1: LASSO OLS ---------------------------------------------------------------------------------------------------------
# throw in everything and see what happens with LASSO
#sink("lasso_output.txt")  # start outputing to text file
library(hdm)
lasso.1 <- rlasso(formula, data = dat, post = F)
#cat("\nSummary of LASSO: \n")
summary(lasso.1, all = F)

# get ceoffs that matter and make OLS formula
x <- which(coef(lasso.1)[-1]!=0)
#cat("\nHow many variables to keep: \n")
length(x)
x <- paste(names(x), collapse = "+")
formula <- paste(c("IncomePovertyRatio", x), collapse = " ~ ")

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

library(stringr)
# name all extra variables created from doing OLS
dattemp <- dat
for (i in 1:90) { # look at formula and count how many new vars need to be made
  error <- myTryCatch(olsLasso.1<- lm(formula, data = dattemp))
  newvars <- substr(error[[1]], 45, str_length(error[[1]])-12)
  existingvars <- names(dattemp)[which(str_detect(newvars, names(dattemp)))]
  existingchars <- sub(existingvars, "", newvars)
  dattemp[,newvars] <- dattemp[,which(names(dattemp)==existingvars)]==existingchars
}

delete <- which(colSums(is.na(dattemp))==nrow(dattemp))
dattemp$MARHT2 <- dattemp$MARHT == "2"
dattemp$SCHG14 <- dattemp$SCHG == "14"
dattemp$SCHG15 <- dattemp$SCHG == "15"
dattemp$SCHG16 <- dattemp$SCHG == "16"
dattemp$PAOC2 <- dattemp$PAOC == "2"
dattemp$PAOC4 <- dattemp$PAOC == "4"


# OLS regression post LASSO
olsLasso.1 <- lm(formula, data = dattemp)
result <- summary(olsLasso.1)
#cat("\nPost-LASSO OLS Regression output: \n")
#result
#sink()  # stop writing to text file

#head(sort(result$coeff[which(result$coeff[,4] < 0.0001),1], decreasing = T)) # high coeff to low
#head(sort(result$coeff[which(result$coeff[,4] < 0.005),1], decreasing = T)) # high coeff to low
#head(sort(result$coeff[which(result$coeff[,4] < 0.0001),1], decreasing = F)) # low coeff to high
#head(sort(result$coeff[which(result$coeff[,4] < 0.005),1], decreasing = F)) # low coeff to high


# Model 2: OLS -----------------------------------------------------------------------------------------------------------
dat <- temp # recover original dataset

# first consider only the factors that could alter decision
# making for labor, residency, and migration, income
# i.e. exclude all "society"  factors except directly related to family
# y ~ ST, AGEP, SPORDER, COW, OC, (PAOC), MAR,(MSP) JWMNP,JWTR, WKPH,WKW,
# MIG,(MIGSP),POWSP,
# HINS1:6,(HICOV  has health cov, ys or no), (PRIVCOV  private cover,PUBCOV   public cover, ys or no)


# logical dummy for ST==POWSP: most are FALSE
dat$SameResidenceWorkplace <- (as.numeric(dat$ST)==as.numeric(dat$POWSP))
x <- c("SameResidenceWorkplace","ST","AGEP","SPORDER","COW","OC","MAR","JWMNP","JWTR","WKHP","WKW","MIG","POWSP"
       ,"HINS1","HINS2","HINS3","HINS4","HINS5","HINS6")
#keep <- which(names(dat) %in% x)
#summary(dat[,keep])
#colSums(is.na(dat[,keep]))
dat <- dat[!is.na(dat$JWMNP),]


# Some Plots --------------------------------------------------------------------------------------------
#par(mfrow = c(1,2))
#hist(dat$IncomePovertyRatio,
#     main = "Income Poverty Ratio") # extreme right skew
#hist(log(dat$IncomePovertyRatio),
#     main = "log(Income Poverty Ratio)") # much better
#par(mfrow = c(1,1))

#plot(dat$AGEP, log(dat$IncomePovertyRatio),
#     main = "Age v. log(Income/Poverty)",
#     xlab = "Age",
#     ylab = "log Ratio")
#abline(lm(log(IncomePovertyRatio) ~ AGEP, data = dat), col = "red")


# intersting result for this one
#plot(dat$SPORDER, log(dat$IncomePovertyRatio),
#     main = "Household Size v. log(Income/Poverty)",
#     xlab = "Number of People",
#     ylab = "log Ratio")
#abline(lm(log(IncomePovertyRatio) ~ SPORDER, data = dat), col = "red")


#plot(dat$JWMNP, log(dat$IncomePovertyRatio),
#     main = "Travel Time to Work v. log(Income/Poverty)",
#     xlab = "Time (minutes)",
#     ylab = "log Ratio")
#abline(lm(log(IncomePovertyRatio) ~ JWMNP, data = dat), col = "red")


#plot(dat$JWTR, log(dat$IncomePovertyRatio),
#     main = "Means of Transit to Work v. log(Income/Poverty)",
#     xlab = "Transportation",
#     ylab = "log Ratio")
#abline(h=mean(log(dat$IncomePovertyRatio), na.rm = T),
#       col="red", lwd=1, lty="solid")


#plot(dat$POWSP, log(dat$IncomePovertyRatio),
#     main = "Place of Work v. log(Income/Poverty)",
#     xlab = "Place of Work",
#     sub = "US: 1-56; Other: 72-555",
#     ylab = "log Ratio")
#abline(h=mean(log(dat$IncomePovertyRatio), na.rm = T),
#       col="red", lwd=2, lty="solid")



# Regressions (### Caution: Out of order ###) -----------------------------------------------------------------------------------
# FAILED ATTEMPT
#sink("OLS_output_leftover.txt")
x <- paste(x, collapse = "+")
formula2 <- paste(c("log(IncomePovertyRatio)", x), collapse = " ~ ")
lm.attempted <- lm(formula2, data=dat)
result.attempted <- summary(lm.attempted)
#cat("\nOLS without sociodemographic variables: \n")
#result.attempted
#sink()

# 5th Model
# What if we take out location variables?
#sink("OLS_output4.txt")
x <- c("SameResidenceWorkplace*JWMNP","JWMNP*JWTR","MIG","AGEP","SPORDER","COW","OC","MAR","WKHP","WKW"
       ,"HINS1","HINS2","HINS3","HINS4","HINS5","HINS6")
dat <- dat[!is.na(dat$JWMNP),]
x <- paste(x, collapse = "+")
formula3 <- paste(c("log(IncomePovertyRatio)", x), collapse = " ~ ")
lm.5 <- lm(formula3, data=dat)
result3 <- summary(lm.5)
#cat("\nOLS without sociodemographic variables and (excluding location variables): \n")
#result3
#sink()

# Model 1
# What if regression was ONLY done on the core interest vars -----------------------------------------------------
# : "JWMNP" & logical dummy for ST==POWSP
#sink("OLS_output1.txt")
dat <- dat[!is.na(dat$JWMNP),]
lm.1 <- lm(log(IncomePovertyRatio) ~ SameResidenceWorkplace+JWMNP, data=dat)
result1 <- summary(lm.1)
#cat("\nFirst OLS regression output ONLY on the core interest vars: \n")
#result1
#sink()

# Model 2
# Let's examine the plots of the first regression ----------------------------------------------------------------
#par(mfrow = c(2,2))
#plot(lm.1, which=c(1,2,4,5))

# display the fitted lines: just with intercept of dummy
#par(mfrow = c(1,1))
#plot(log(dat$IncomePovertyRatio) ~ dat$JWMNP,
#     main = "Different Intercept Only (Without Interaction)")
#legend("bottomright", legend=c("F","T"), lty=c(1,1), lwd=2, cex=1, col=c("red","blue"))
#abline(coef = c(lm.1$coefficients[1],lm.1$coefficients[3]),
#       col = "red", lwd = 2)
#abline(coef = c(lm.1$coefficients[1]+lm.1$coefficients[2],lm.1$coefficients[3]),
#       col = "blue", lwd = 2)


# now with interaction term: for the slope of dummy --------------------------------------------
#sink("OLS_output1_2.txt")
lm.2 <- lm(log(IncomePovertyRatio) ~ SameResidenceWorkplace*JWMNP, data=dat)
result2 <- summary(lm.2)
#cat("\nFirst OLS regression output BUT with interaction term: \n")
#result2
#sink()

# Let's examine the plots of the second regression ----------------------------------------------------------------------
#par(mfrow = c(2,2))
#plot(lm.1.2, which=c(1,2,4,5))

# display the fitted lines: with interaction term
#par(mfrow = c(1,1))
#plot(log(dat$IncomePovertyRatio) ~ dat$JWMNP,
#     main = "Different Intercept and Slope (With Interaction)")
#legend("bottomright", legend=c("F","T"), lty=c(1,1), lwd=2, cex=1, col=c("red","blue"))
#abline(coef = c(lm.1.2$coefficients[1],lm.1.2$coefficients[3]),
#       col = "red", lwd = 2)
#abline(coef = c(lm.1.2$coefficients[1]+lm.1.2$coefficients[2],lm.1.2$coefficients[3]+lm.1.2$coefficients[4]),
#       col = "blue", lwd = 2)

# Build upon these models ------------------------------------------------------------------
# Regard the OLS with ST and POWSP as a "failed attempt"
# 3rd Model: include secondary core vars "JWTR" and interaction
#            (time to commute and transportation mean will be related)
#sink("OLS_output2.txt")
dat <- dat[!is.na(dat$JWMNP),]
lm.3 <- lm(log(IncomePovertyRatio) ~ SameResidenceWorkplace*JWMNP+JWTR*JWMNP, data=dat)
result3 <- summary(lm.3)
#cat("\nOLS regression output with another interaction term: \n")
#result3
#sink()

# 4th Model: include secondary core vars "MIG"
#sink("OLS_output3.txt")
dat <- dat[!is.na(dat$JWMNP),]
lm.4 <- lm(log(IncomePovertyRatio) ~ SameResidenceWorkplace*JWMNP+JWTR*JWMNP+MIG, data=dat)
result4 <- summary(lm.4)
#cat("\nOLS regression output on core and secondary interest vars: \n")
#result4
#sink()

# 5th Model is above

# 6th Model: Now include all "society"  factors not included in 5th model
#sink("OLS_output5.txt")
dat <- dat[!is.na(dat$JWMNP),]
formula4 <- paste(c(formula3,"MIL","SCH","SEX","NATIVITY","RAC1P","SCIENGRLP"), collapse = "+")
lm.6 <- lm(formula4, data=dat)
result6 <- summary(lm.6)
#cat("\nOLS with core, secondary, sociodemographic variables (excluding location variables) \n")
#result6
#sink()

# Correlation matrix for multicollinearity check --------------------------------------------
x <- unlist(strsplit(formula4, c(" ~ ")))
x <- unlist(strsplit(x[2], c("\\+")))
x <- unlist(strsplit(x, c("\\*")))
x <- x[-2]
keep <- which(names(dat) %in% x)

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

DF <- na.omit(dat[,keep])
DF[] <- lapply(DF,as.integer)

library(Hmisc)
corrmat <- rcorr(as.matrix(DF))
#write.csv(flattenCorrMatrix(corrmat$r, corrmat$P),"finalCorrMatrix.csv")









