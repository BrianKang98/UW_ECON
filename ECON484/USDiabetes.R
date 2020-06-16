## Brian Kang
## 05/17/2019
## ECON484: Advanced ML and Data Science for Econometricians
## Final Project

# import data----------------------------------------------------
rm(list = ls())  # reset working vars
setwd("C:/Users/slexi/Documents/UW_ECON/ECON484")  # set working directory
temp <- read.csv("diabetic_data.csv", na.strings = "?")  # save temp data
temp2 <- temp  # backup
#temp <- temp2  # recover backup

#names(temp)
#head(temp)
#str(temp)
#sapply(temp, class)
#summary(temp)

# install packages when needed
#install.packages("naniar")
#install.packages("car")
#install.packages("fitdistrplus")
#install.packages("hdm")
#install.packages("stringr")
#install.packages("caret")
#install.packages("kableExtra")
#webshot::install_phantomjs()
#install.packages("magick")

# clean data------------------------------------------------------
# unique type int identifiers should be type factors
temp$encounter_id <- as.factor(temp$encounter_id)
temp$patient_nbr <- as.factor(temp$patient_nbr)
temp$admission_type_id <- as.factor(temp$admission_type_id)
temp$discharge_disposition_id <- as.factor(temp$discharge_disposition_id)
temp$admission_source_id <- as.factor(temp$admission_source_id)

# replace meaningless identifiers to NA
#library(naniar)
#replace_with_na(temp, replace = list(admission_type_id=c(5,6,8)))

# c(5,6,8) replaces only some to NA
temp$admission_type_id[temp$admission_type_id==5] <- NA
temp$admission_type_id[temp$admission_type_id==6] <- NA
temp$admission_type_id[temp$admission_type_id==8] <- NA

# c(18,25,26) replaces only some to NA
temp$discharge_disposition_id[temp$discharge_disposition_id==18] <- NA
temp$discharge_disposition_id[temp$discharge_disposition_id==25] <- NA
temp$discharge_disposition_id[temp$discharge_disposition_id==26] <- NA

# c(9,15,17,20,21) replaces only some to NA
temp$admission_source_id[temp$admission_source_id==9] <- NA 
temp$admission_source_id[temp$admission_source_id==15] <- NA
temp$admission_source_id[temp$admission_source_id==17] <- NA
temp$admission_source_id[temp$admission_source_id==20] <- NA
temp$admission_source_id[temp$admission_source_id==21] <- NA

# rename levels of vars "age"
for (ii in 0:(length(levels(temp$age))-1)) {
  nm <- paste("[",ii*10,"-",(ii+1)*10,")", sep = "")
  chng <- paste(ii*10,"_",(ii+1)*10, sep = "")
  levels(temp$age)[levels(temp$age)==nm] <- chng
}
# rename levels of vars "weight"
for (ii in 0:(length(levels(temp$weight))-2)) {
  nm <- paste("[",ii*25,"-",(ii+1)*25,")", sep = "")
  chng <- paste(ii*25,"_",(ii+1)*25, sep = "")
  levels(temp$weight)[levels(temp$weight)==nm] <- chng
}
# rename levels of vars "medical_specialty"
library(stringr)
levels(temp$medical_specialty) <- str_replace_all(
  levels(temp$medical_specialty), "[&/\\-]", "_")

# delete rows with three unknown genders
delete <- which(temp$gender=="Unknown/Invalid")
temp <- temp[-delete,]
# delete rows with num_lab_procedures >97
outlier <- boxplot(temp$num_lab_procedures, plot = F)$out
delete <- outlier[outlier>97]
temp <- temp[-which(temp$num_lab_procedures %in% delete),]
# delete rows with num_medication >45
outlier <- boxplot(temp$num_medications, plot = F)$out
delete <- outlier[outlier>45]
temp <- temp[-which(temp$num_medications %in% delete),]
# delete rows with number_outpatient >2
outlier <- boxplot(temp$number_outpatient, plot = F)$out
delete <- outlier[outlier>2]
temp <- temp[-which(temp$number_outpatient %in% delete),]
# delete rows with number_emergency >3
outlier <- boxplot(temp$number_emergency, plot = F)$out
delete <- outlier[outlier>3]
temp <- temp[-which(temp$number_emergency %in% delete),]
# delete rows with number_inpatient >=5
outlier <- boxplot(temp$number_inpatient, plot = F)$out
delete <- outlier[outlier>=5]
temp <- temp[-which(temp$number_inpatient %in% delete),]

# modeling--------------------------------------------------------
diabetic <- temp  # rename data after cleaning
temp3 <- diabetic  # backup

# Question:
# Did the treatment and medication actually work?
# Predict readmitted or not, using variables related to treatment
# and/or examination in hospital and variables related to medication

# Function to reset dataset for each model
reset <- function() {
  diabetic <- temp3  # recover backup
  isReadmitted <- ifelse(diabetic$readmitted %in% c("<30",">30"),F,T)
  diabetic <- cbind(diabetic,isReadmitted)
  # split data into train & test
  set.seed(987)
  train <- sample(1:nrow(diabetic), nrow(diabetic)*0.8)  # 80% for training
  
  # get which variables have <2 factor levels
  get <- which(sapply(diabetic[train,], function(x) length(unique(x))<2))
  # exclude encounter_id, patient_nbr, weight, payer_code, diag_1, daig_2,
  # diag_3, readmitted, isReadmitted  
  # Reason: unrelated to question OR too many factors
  # also exclude acetohexamide, tolbutamide, troglitazone, 
  # glimepiride.pioglitazone, metformin.rosiglitazone
  # Reason: causes "<2 level" error from sampling
  varnames <- paste(c(names(diabetic
    [,-c(get,1,2,6,11,19,20,21,30,33,38,45,46,50,51)])), collapse = "+")
  formula <- paste(c("isReadmitted",varnames), collapse = "~")
  return(list(diabetic, train, get, varnames, formula))
}
resetData <- reset()
diabetic <- resetData[[1]]
train <- resetData[[2]]
get <- resetData[[3]]
varnames <- resetData[[4]]
formula <- resetData[[5]]

# Model 1: LASSO
#sink("lasso_output.txt")  # start outputing to text file
library(hdm)
lasso.1 <- rlasso(formula , data = diabetic[train,], post = F)
#cat("Do LASSO on training set\n")
summary(lasso.1, all = F)

# get ceoffs that matter and make OLS formula
x <- which(coef(lasso.1)[-1]!=0)
#cat("\nCount and Kept Significant Variables by LASSO\nCount: ")
length(x)
#x
x <- paste(names(x), collapse = "+")
formula <- paste(c("isReadmitted", x), collapse = " ~ ")

# name all extra variables created from doing OLS
diabetic$raceAsian <- diabetic$race == "Asian"
diabetic$raceHispanic <- diabetic$race == "Hispanic"
diabetic$raceOther <- diabetic$race == "Other"
diabetic$genderMale <- diabetic$gender == "Male"
diabetic$age30_40 <- diabetic$age == "30_40"
diabetic$age50_60 <- diabetic$age == "50_60"
diabetic$age70_80 <- diabetic$age == "70_80"
diabetic$age80_90 <- diabetic$age == "80_90"
diabetic$age90_100 <- diabetic$age == "90_100"
diabetic$admission_type_id2 <- diabetic$admission_type_id == "2"
diabetic$discharge_disposition_id5 <- 
  diabetic$discharge_disposition_id == "5"
diabetic$discharge_disposition_id6 <- 
  diabetic$discharge_disposition_id == "6"
diabetic$discharge_disposition_id11 <- 
  diabetic$discharge_disposition_id == "11"
diabetic$discharge_disposition_id13 <- 
  diabetic$discharge_disposition_id == "13"
diabetic$discharge_disposition_id14 <- 
  diabetic$discharge_disposition_id == "14"
diabetic$discharge_disposition_id19 <- 
  diabetic$discharge_disposition_id == "19"
diabetic$discharge_disposition_id22 <- 
  diabetic$discharge_disposition_id == "22"
diabetic$discharge_disposition_id23 <- 
  diabetic$discharge_disposition_id == "23"
diabetic$admission_source_id4 <- diabetic$admission_source_id == "4"
diabetic$admission_source_id5 <- diabetic$admission_source_id == "5"
diabetic$admission_source_id6 <- diabetic$admission_source_id == "6"
diabetic$admission_source_id7 <- diabetic$admission_source_id == "7"
diabetic$medical_specialtyEmergency_Trauma <- 
  diabetic$medical_specialty == "Emergency_Trauma"
diabetic$medical_specialtyFamily_GeneralPractice <- 
  diabetic$medical_specialty == "Family_GeneralPractice"
diabetic$medical_specialtyGastroenterology <- 
  diabetic$medical_specialty == "Gastroenterology"
diabetic$medical_specialtyGynecology <- 
  diabetic$medical_specialty == "Gynecology"
diabetic$medical_specialtyHematology <- 
  diabetic$medical_specialty == "Hematology"
diabetic$medical_specialtyInternalMedicine <- 
  diabetic$medical_specialty == "InternalMedicine"
diabetic$medical_specialtyNephrology <- 
  diabetic$medical_specialty == "Nephrology"
diabetic$medical_specialtyNeurology <- 
  diabetic$medical_specialty == "Neurology"
diabetic$medical_specialtyObstetricsandGynecology <- 
  diabetic$medical_specialty == "ObstetricsandGynecology"
diabetic$medical_specialtyObstetrics <- 
  diabetic$medical_specialty == "Obstetrics"
diabetic$medical_specialtyOncology <- 
  diabetic$medical_specialty == "Oncology"
diabetic$medical_specialtyOrthopedics <- 
  diabetic$medical_specialty == "Orthopedics"
diabetic$medical_specialtyOrthopedics_Reconstructive <- 
  diabetic$medical_specialty == "Orthopedics_Reconstructive"
diabetic$medical_specialtyOtolaryngology <- 
  diabetic$medical_specialty == "Otolaryngology"
diabetic$medical_specialtyPediatrics_Endocrinology <- 
  diabetic$medical_specialty == "Pediatrics_Endocrinology"
diabetic$medical_specialtyPediatrics_Pulmonology <- 
  diabetic$medical_specialty == "Pediatrics_Pulmonology"
diabetic$medical_specialtyPulmonology <- 
  diabetic$medical_specialty == "Pulmonology"
diabetic$medical_specialtySurgeon <- 
  diabetic$medical_specialty == "Surgeon"
diabetic$medical_specialtySurgery_Cardiovascular <- 
  diabetic$medical_specialty == "Surgery_Cardiovascular"
diabetic$medical_specialtySurgery_Cardiovascular_Thoracic <- 
  diabetic$medical_specialty == "Surgery_Cardiovascular_Thoracic"
diabetic$medical_specialtySurgery_Neuro <- 
  diabetic$medical_specialty == "Surgery_Neuro"
diabetic$medical_specialtySurgery_Vascular <- 
  diabetic$medical_specialty == "Surgery_Vascular"
diabetic$A1CresultNone <- diabetic$A1Cresult == "None"
diabetic$A1CresultNorm <- diabetic$A1Cresult == "Norm"
diabetic$metforminNo <- diabetic$metformin == "No"
diabetic$repaglinideNo <- diabetic$repaglinide == "No"
diabetic$glipizideNo <- diabetic$glipizide == "No"
diabetic$pioglitazoneUp <- diabetic$pioglitazone == "Up"
diabetic$acarboseNo <- diabetic$acarbose == "No"
diabetic$tolazamideSteady <- diabetic$tolazamide == "Steady"
diabetic$insulinSteady <- diabetic$insulin == "Steady"
diabetic$metforminSteady <- diabetic$metformin == "Steady"
diabetic$changeNo <- diabetic$change == "No"
diabetic$diabetesMedYes <- diabetic$diabetesMed == "Yes"

# OLS regression on training set
olsLasso.1 <- lm(formula, data = diabetic[train,])
#cat("\nDo OLS on training set using selected variables from LASSO\n")
summary(olsLasso.1)$coefficients[,1]
# prediction on test data to predict patient readmission or not
prob.lasso.1 <- predict(olsLasso.1, newdata = diabetic[-train,])
#cat("Predict on test set\n")
summary(prob.lasso.1)
#cat("\nCount remaining observations\n")
length(na.omit(prob.lasso.1)) # count remaining observations
# test error
mse.1 <- mean((prob.lasso.1-diabetic[-train,]$isReadmitted)^2, na.rm=T)
#cat("\nMSE\n")
mse.1
#sink()  # stop writing to text file

# --------------------------------------------------------
# Model 2: LASSO with CV choosing Tuning Parameter
#sink("lassoCV_output.txt")  # start outputing to text file
resetData <- reset()  # reset data
diabetic <- resetData[[1]]
train <- resetData[[2]]
get <- resetData[[3]]
varnames <- resetData[[4]]
formula <- resetData[[5]]

# split into train & test
# takeout intercept
xtrain <- model.matrix(as.formula(formula), data = diabetic[train,])[,-1]
xtest <- model.matrix(as.formula(formula), data = diabetic[-train,])[,-1]
set.seed(987)
# nrow unequal so adjust
ytrain <- diabetic[sample(train, nrow(xtrain)),]$isReadmitted

# cross validation then fit LASSO
library(glmnet)
set.seed(987)
cv.lasso.1 <- cv.glmnet(xtrain, ytrain, alpha = 1)  # 1 for lasso
#cat("CV on LASSO and get min tuning parameter\n")
cv.lasso.1[c(8,9)]
cv.lambda <- cv.lasso.1$lambda.min  # get smallest tuning parameter
#png(filename="lassoCV.png")  # save plot
plot(cv.lasso.1)
#dev.off()
lasso.2 <- glmnet(xtrain, ytrain, alpha = 1, lambda = cv.lambda)
#cat("\nDo LASSO using min tuning parameter then predict\n")
summary(lasso.2)
lasso.2
# prediction on test data to predict patient readmission or not
pred.lasso.2 <- predict(lasso.2, s = cv.lambda, newx = xtest)
summary(pred.lasso.2)
test <- (1:nrow(diabetic))[-train]  # test data
# test error
mse.2 <- mean((pred.lasso.2-diabetic
           [sample(test,length(pred.lasso.2)),]$isReadmitted)^2, na.rm=T)
#cat("\nMSE\n")
mse.2
#sink()  # stop writing to text file

# --------------------------------------------------------
# Model 3: Logistic Regression
#sink("logit_output.txt")  # start outputing to text file
resetData <- reset()  # reset data
diabetic <- resetData[[1]]
train <- resetData[[2]]
get <- resetData[[3]]
# update formula
# also exclude admission_type_id, discharge_disposition_id, admission_source_id
# and medical_specialty  
# Reason: error in dataset jams logit
varnames <- paste(c(names(diabetic
  [,-c(get,1,2,6,7,8,9,11,12,19,20,21,33,38,45,46,50,51)])), collapse = "+")
formula <- paste(c("isReadmitted",varnames), collapse = "~")

# fit logit using training data 
logit.1 <- glm(formula, data = diabetic, family = "binomial", subset=train)
#cat("Do logit on training set\n")
summary(logit.1)  # very sparse results, many individually insignificant
#plot(logit.1)

# predict probability of readmission using test data
logit.prob.1 <- predict(logit.1, newdata = diabetic[-train,], type = "response")
#cat("\nPredict using test set\n")
summary(logit.prob.1)
hist(logit.prob.1)

# predicting whether patient will be readmitted or not
# if prob > 1/2 then patient will not readmit
logit.pred.1 <- rep(F, nrow(diabetic[-train,]))
logit.pred.1[logit.prob.1 > 0.5] <- T

# test error
mse.3 <- mean((logit.pred.1-diabetic$isReadmitted[-train])^2)
#cat("\nMSE\n")
mse.3

#cat("\nConfusion matrix\n")
# confusion matrix
table(logit.pred.1, diabetic$isReadmitted[-train])
#cat("\nAccuracy\n")
mean(logit.pred.1 == diabetic$isReadmitted[-train])
#cat("\nNOTICE, sum of MSE and accuracy = \n")
mse.3 + mean(logit.pred.1 == diabetic$isReadmitted[-train]) # =1!
#sink()  # stop writing to text file

# --------------------------------------------------------
# Model 3.5: Logit with K-fold CV
## Author: Matt Kelly
## Date: 06/05/2019
set.seed(987)
k = 10
folds = sample(1:k, nrow(diabetic), replace = TRUE)
cv.error.10 = matrix(NA, nrow = k, ncol = 19)
diab.pred.function = function(object, newdata, id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object,id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}
## end ---------------------------------------------------


# --------------------------------------------------------
# Instead of predicting whether patient was readmitted or not,
# predict the range of days between previous and next readmission
# factors: None, <30 days, >30 days
# Model 4: Multinomial Logistic using Neural Network
#sink("nn_output.txt")  # start outputing to text file
library(nnet)
library(pscl)
resetData <- reset()  # reset data
diabetic <- resetData[[1]]
train <- resetData[[2]]
get <- resetData[[3]]
# use formula from above logit but for readmitted, not isReadmitted
formula <- paste(c("readmitted",varnames), collapse = "~")

# fit multinomial
nn.1 <- multinom(formula, data = diabetic[train,])
#cat("Fit multinomial logistic neural net and get number of features
#    in model, probabilities, and effective DF\n")
length(nn.1$coefnames)  # number of features
summary(nn.1$fitted.values)
nn.1$edf  # effective DF exhausted up by model
#nn.1$deviance  # residual deviance, minus twice log-likelihook
#nn.1$AIC  # AIC for fit

# predict probability of days between readmission using test data
nn.pred.1 <- predict(nn.1, newdata = diabetic[-train,], type = "probs")
# predict intervals between readmission using test data
nn.class.1 <- predict(nn.1, newdata = diabetic[-train,])
#cat("\nConfusion matrix\n")
# confusion matrix
library(caret)
caret::confusionMatrix(as.factor(nn.class.1), 
                       as.factor(diabetic[-train,]$readmitted))
# test error
mse.4 <- mean(as.character(nn.class.1)!=
                as.character(diabetic[-train,]$readmitted), na.rm = T)
#cat("\nMSE\n")
mse.4
#sink()  # stop writing to text file

# calculate z score and p values
c <- summary(nn.1)$coefficients
se <- summary(nn.1)$standard.errors
z <- c/se
p <- (1-pnorm(abs(z),0,1))*2  # I am using two-tailed z test
z
p
summ <- as.data.frame(rbind(c[2,],se[2,],z[2,],p[2,]))
rownames(summ) <- c("Coefficient","Std. Errors","Z stat","P-value")
summ <- t(summ)

# make neat table
library(knitr)
library(kableExtra)
library(dplyr)
library(magick)
#summ %>%
#  mutate_if(is.numeric, function(x) {
#    cell_spec(x, bold = T, 
#              color = spec_color(x, end=0),
#              font_size = spec_font_size(x, end = 12))
#  }) %>%

#save_kable(
  kable(summ, escape = F) %>% 
  kable_styling(fixed_thead = T, bootstrap_options = 
                  c("striped", "condensed", "responsive"), 
                full_width = F, font_size = 12)
#  , "nn1.png")

# --------------------------------------------------------
# Model 4.5: Same model but with variable selection
#sink("nn_output2.txt")  # start outputing to text file
# calculate important variables
impvars <- varImp(nn.1)
impvars$Variables <- row.names(impvars)
impvars <- impvars[order(-impvars$Overall),]
#cat("Look at first few most important variables\n")
head(impvars)

# choose variables that matter
imp1 <- names(summ)[which(p[2,-1]<0.001)]  # individual significance
imp2 <- impvars$Variables[which(impvars$Overall>1)]  # overall importance
critvars <- union(imp1, imp2)

# make formula
varnames <- paste(critvars, collapse = "+")
formula <- paste(c("readmitted",varnames), collapse = "~")

# name all extra variables created
diabetic$age10_20 <- diabetic$age == "10_20"
diabetic$age20_30 <- diabetic$age == "20_30"
diabetic$age30_40 <- diabetic$age == "30_40"
diabetic$age40_50 <- diabetic$age == "40_50"
diabetic$age50_60 <- diabetic$age == "50_60"
diabetic$age60_70 <- diabetic$age == "60_70"
diabetic$age70_80 <- diabetic$age == "70_80"
diabetic$age80_90 <- diabetic$age == "80_90"
diabetic$age90_100 <- diabetic$age == "90_100"
diabetic$metforminUp <- diabetic$metformin == "Up"
diabetic$repaglinideNo <- diabetic$repaglinide == "No"
diabetic$repaglinideSteady <- diabetic$repaglinide == "Steady"
diabetic$repaglinideUp <- diabetic$repaglinide == "Up"
diabetic$nateglinideNo <- diabetic$nateglinide == "No"
diabetic$chlorpropamideSteady <- diabetic$chlorpropamide == "Steady"
diabetic$pioglitazoneUp <- diabetic$pioglitazone == "Up"
diabetic$rosiglitazoneNo <- diabetic$rosiglitazone == "No"
diabetic$rosiglitazoneSteady <- diabetic$rosiglitazone == "Steady"
diabetic$rosiglitazoneUp <- diabetic$rosiglitazone == "Up"
diabetic$acarboseNo <- diabetic$acarbose == "No"
diabetic$acarboseSteady <- diabetic$acarbose == "Steady"
diabetic$acarboseUp <- diabetic$acarbose == "Up"
diabetic$miglitolSteady <- diabetic$miglitol == "Steady"
diabetic$insulinUp <- diabetic$insulin == "Up"
diabetic$glyburide.metforminNo <- diabetic$glyburide.metformin == "No"
diabetic$glyburide.metforminSteady <- 
  diabetic$glyburide.metformin == "Steady"
diabetic$changeNo <- diabetic$change == "No"
diabetic$chlorpropamideUp <- diabetic$chlorpropamide == "Up"
diabetic$miglitolUp <- diabetic$miglitol == "Up"
diabetic$nateglinideUp <- diabetic$nateglinide == "Up"
diabetic$glipizide.metforminSteady <- 
  diabetic$glipizide.metformin == "Steady"
diabetic$tolazamideUp <- diabetic$tolazamide == "Up"
diabetic$miglitolNo <- diabetic$miglitol == "No"
diabetic$metformin.pioglitazoneSteady <- 
  diabetic$metformin.pioglitazone == "Steady"
diabetic$glyburide.metforminUp <- diabetic$glyburide.metformin == "Up"

#cat("\nFit multinomial logistic neural net and get number of features
#    in model, probabilities, and effective DF\n")
# do multinomial logistic neural nets
nn.2 <- multinom(formula, data = diabetic[train,])
length(nn.2$coefnames)  # number of features
summary(nn.2$fitted.values)
nn.2$edf  # effective DF exhausted up by model
#nn.2$deviance  # residual deviance, minus twice log-likelihook
#nn.2$AIC  # AIC for fit

# predict probability of days between readmission using test data
nn.pred.2 <- predict(nn.2, newdata = diabetic[-train,], type = "probs")
# predict intervals between readmission using test data
nn.class.2 <- predict(nn.2, newdata = diabetic[-train,])
# confusion matrix
caret::confusionMatrix(as.factor(nn.class.2), 
                       as.factor(diabetic[-train,]$readmitted))
# test error
mse.4.5 <- mean(na.omit(as.character(nn.class.2) != 
                          as.character(diabetic[-train,]$readmitted)))
#cat("\nMSE\n")
mse.4.5
#sink()  # stop writing to text file

# calculate z score and p values
c2 <- summary(nn.2)$coefficients
se2 <- summary(nn.2)$standard.errors
z2 <- c2/se2
p2 <- (1-pnorm(abs(z2),0,1))*2  # I am using two-tailed z test
z2
p2
summ2 <- as.data.frame(rbind(c2[2,],se2[2,],z2[2,],p2[2,]))
rownames(summ2) <- c("Coefficient","Std. Errors","Z stat","P-value")
summ2 <- t(summ2)

# make neat table
#save_kable(
  kable(summ2, escape = F) %>% 
  kable_styling(fixed_thead = T, bootstrap_options = 
                  c("striped", "condensed", "responsive"), 
                full_width = F, font_size = 12)
#  , "nn2.png")

# --------------------------------------------------------
# Model 5: Boosting
#sink("boosting.txt")  # start outputing to text file
resetData <- reset()  # reset data
diabetic <- resetData[[1]]
train <- resetData[[2]]
get <- resetData[[3]]
varnames <- resetData[[4]]
formula <- resetData[[5]]

## Author: Tatsuya Okuda
## Date: 06/08/2019 --------------------------------------
set.seed(987)
diabetic.sub = diabetic[,-c(get,1,2,6,7,8,9,11,12,19,20,21,33,38,45,46,50)]
varnames <- paste(c(names(diabetic
  [,-c(get,1,2,6,7,8,9,11,12,19,20,21,33,38,45,46,50,51)])), collapse = "+")
formula <- paste(c("isReadmitted",varnames), collapse = "~")
formula = as.formula(formula)

diabetic = na.omit(diabetic.sub) #omit NA
train = sample(1:nrow(diabetic.sub), floor(nrow(diabetic.sub)*0.7))
test = setdiff(1:nrow(diabetic.sub), train)

library(gbm)

boosting = gbm(formula,data=diabetic.sub[train,], distribution = "bernoulli", 
               n.trees = 1000, interaction.depth = 4)
#cat("Do boosting\n")
summary(boosting)

boosting.pred = predict.gbm(boosting, newdata = diabetic.sub[test,],
                            n.trees = 1000, type = "response")
prediction = rep(0,length(test))
prediction[boosting.pred>0.5] = "TRUE"
prediction[boosting.pred<=0.5] = "FALSE"

diabetic.sub$isReadmitted[diabetic.sub$isReadmitted==0] = "FALSE"
diabetic.sub$isReadmitted[diabetic.sub$isReadmitted==1] = "TRUE"

correct = sum(prediction == diabetic.sub$isReadmitted[test])
total = length(test)
accuracy = correct/total
#cat("\nAccuracy\n")
accuracy
## end ---------------------------------------------------
#sink()  # stop writing to text file

# --------------------------------------------------------
# Model 6: Random Forest
#sink("randomforest.txt")  # start outputing to text file
resetData <- reset()  # reset data
diabetic <- resetData[[1]]
train <- resetData[[2]]
get <- resetData[[3]]
varnames <- resetData[[4]]
formula <- resetData[[5]]

## Author: Tatsuya Okuda
## Date: 06/08/2019 --------------------------------------
diabetic$isReadmitted = as.factor(diabetic$isReadmitted) #factor 

#remove medical specialty because RF does not work for too many levels
set.seed(987)
diabetic.sub = diabetic[,-c(get,1,2,6,7,8,9,11,12,19,20,21,33,38,45,46,50)]
diabetic.sub = na.omit(diabetic.sub) #omit NA
train = sample(1:nrow(diabetic.sub), floor(nrow(diabetic.sub)*0.7))
test = setdiff(1:nrow(diabetic.sub), train)

varnames <- paste(c(names(diabetic
  [,-c(get,1,2,6,7,8,9,11,12,19,20,21,33,38,45,46,50,51)])), collapse = "+")
formula <- paste(c("isReadmitted",varnames), collapse = "~")
formula = as.formula(formula)

library(randomForest)
#cat("Do random forest on training set\n")
rf = randomForest(formula, data = diabetic.sub, subset = train, 
                  mtry = 6, importance = TRUE)
rf

rf.predict = predict(rf, newdata = diabetic.sub[test,])
#cat("\nConfusion matrix using the test set\n")
table(rf.predict, diabetic.sub$isReadmitted[test])

num.correct = sum(rf.predict==diabetic.sub$isReadmitted[test])
num.total = length(test)
accuracy = num.correct/num.total
#cat("\nAccuracy\n")
accuracy  

#cat("\nCalculate importance of the variables\n")
importance(rf)
#sink()  # stop writing to text file
#png(filename="rfImportance.png")  # save plot
varImpPlot(rf)
#dev.off()
## end ----------------------------------------------------
#png(filename="randomforest.png")  # save plot
# plot the random forest
plot(rf.predict, diabetic.sub$isReadmitted[test])
# abline(0,1)  # not used because we are predicting binary response
#dev.off()

####################DON'T RUN#######################
# make all variables into factors
#factorVars <- numeric(ncol(trainedDiabetic))
#for (ii in 1:ncol(trainedDiabetic)) {
#  if (is.factor(trainedDiabetic[,ii])) {
#    factorVars[ii] <- ii
#  }
#}
#for (ii in 1:length(factorVars)) {
#  if (factorVars[ii]!=0) {
#    trainedDiabetic[,ii] <- as.factor(trainedDiabetic[,ii])
#  }
#}
####################################################

# observing data----------------------------------------------------
# races with weight data not available
summary(temp$race[which(is.na(temp$weight))])
# races with weight data not available
summary(temp$race[which(!is.na(temp$weight))])

# highest number of lab procedures in one encounter
temp[temp$num_lab_procedures==132,]
sort(temp$num_lab_procedures,decreasing = T)
#png(filename="cleandat1.png")  # save plot
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
min(boxplot(temp$num_lab_procedures,horizontal = T, 
            main = "Boxplot of Number of Lab Procedures")$out)
hist(temp$num_lab_procedures, main = "Histogram of Number of Lab Procedures", 
     xlab = "num_lab_procedures")
hist(temp$num_lab_procedures, xlim = c(60,140), main = "Enlarged Histogram", 
     xlab = "num_lab_procedures")  # DELETE >97
#dev.off()

# highest number of medications in one encounter
temp[temp$num_medications==81,]
sort(temp$num_medications,decreasing = T)
#png(filename="cleandat2.png")  # save plot
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
min(boxplot(temp$num_medications,horizontal = T, 
            main = "Boxplot of Number of Medication")$out)
hist(temp$num_medications, main = "Histogram of Number of Medication", 
     xlab = "num_medication")
hist(temp$num_medications, xlim = c(30,60), main = "Enlarged Histogram", 
     xlab = "num_medication")  # DELETE >45
#dev.off()

# highest number of outpatient visits
temp[temp$number_outpatient==42,]
sort(temp$number_outpatient,decreasing = T)
#png(filename="cleandat3.png")  # save plot
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
boxplot(temp$number_outpatient, horizontal = T, 
        main = "Boxplot of Number of Outpatients")
hist(temp$number_outpatient, main = "Histogram of Number of Outpatients", 
     xlab = "number_outpatient")
hist(temp$number_outpatient, xlim = c(1,4), main = "Enlarged Histogram", 
     xlab = "number_outpatient")  # DELETE >2
#dev.off()

# highest number of emergency visits in one year
temp[temp$number_emergency==76,]
sort(temp$number_emergency,decreasing = T)
#png(filename="cleandat4.png")  # save plot
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
min(boxplot(temp$number_emergency, horizontal = T, 
            main = "Boxplot of Number of Emergency Visits")$out)
hist(temp$number_emergency, main = "Histogram of Number of Emergency Visits",
     xlab = "number_emergency")
hist(temp$number_emergency, xlim = c(2,8), main = "Enlarged Histogram", 
     xlab = "number_emergency")  # DELETE >3
#dev.off()

# highest number of inpatient visits
temp[temp$number_inpatient==21,]
sort(temp$number_inpatient,decreasing = T)
#png(filename="cleandat5.png")  # save plot
#layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
min(boxplot(temp$number_inpatient, horizontal = T, 
            main = "Boxplot of Number of Inpatients")$out)
hist(temp$number_inpatient, main = "Histogram of Number of Inpatients", 
     xlab = "number_inpatient")
hist(temp$number_inpatient, xlim = c(1,5), main = "Enlarged Histogram", 
     xlab = "number_inpatient")  # DELETE >=5
#dev.off()

# try fit different distributions
library(car)
#png(filename="fitdat1.png")  # save plot
par(mfrow=c(1,2))
qqPlot(temp$number_inpatient, dist = "lnorm", main = "Lognormal Fit QQPlot", 
       ylab = "number_inpatient")  # log is not normally distributed
qqPlot(temp$number_inpatient, dist = "exp", main = "Exponential Fit QQPlot", 
       ylab = "number_inpatient")  # lognormal still looks like better fit
#dev.off()

# fit exponential distribution
library(fitdistrplus)
#png(filename="fitdat2.png")  # save plot
par(mfrow=c(1,1))
plot(fitdist(temp$number_inpatient, "exp"))
#dev.off()

# explore
hist(temp$number_diagnoses)
sort(temp$number_diagnoses,decreasing = T)
plot(temp$age)
plot(temp$race)
plot(temp$race:temp$age, xlab = "Grouping of Race w.r.t. Age")



