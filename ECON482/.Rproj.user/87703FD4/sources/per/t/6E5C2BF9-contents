#rm(list = ls())

# C1)
# i)
# The sign for Beta2 is most likely positive. It is conceived
# that infants born in wealthier families will be well fed,
# therefore, the birth weight will likely increase as income
# increases.

# ii)
# I think cigs and faminc are likely to be inversely correlated.
# The wealthier families might be more concerned with personal
# development and health and may be more informed of the harms
# of smoking, so they might smoke less. On the other hand, those
# with lower income, people may be less educated and are more
# prone to smoke as an habit. 
# To simplify, I think cigarettes are an inferior good, so the
# correlation between cigs and faminc might be negative.

# iii)
library(wooldridge)
data(bwght)
lm.1 <- lm(bwght ~ cigs, data = bwght)
# sample size
length(bwght$cigs)  # or 
# R-squared value
summary(lm.1)$r.squared
# equation form
# bwght = 119.77190 - 0.51377*cigs
lm.2 <- lm(bwght ~ cigs+faminc, data = bwght)  # overfit?
# sample size
length(lm.2$fitted)
# R-squared value
summary(lm.2)$r.squared
# equation form
# bwght = 116.97413 - 0.46341*cigs + 0.09276*faminc
summary(lm.1)
summary(lm.2)
summary(lm(cigs~faminc, data = bwght))
# From the summary of the results of the linear model, we can
# conclude that adding faminc does not substantially change the
# estimated effect of cigs on bwght. The R-squared values both
# with and without faminc are low, close to 0.02, and the std.
# error values don't change significantly. The std. error for
# the intercept estimate even increases from 0.57 to 1.04. When
# there is barely any benefit from adding another predictor,
# might as well not add the term. But also, we don't want to fit
# too many terms everytime just because more variation can be
# explained because overfitting can be misleading.
# From summary() you can see the Beta coefficients. They do not
# change much from estimaiting with faminc. From the lm()
# between cigs and faminc we see that they do not strongly
# correlate either.

# C2)
# i)
data("hprice1")
lm.3 <- lm(price ~ sqrft+bdrms, data = hprice1)
# equation form
# price = -19.31500 + 0.12844*sqrft + 15.19819*bdrms

# ii)
# estimated increase in price with 1 more bdrm, holding sqrft
summary(lm.3)$coefficient[3,1]

# iii)
# estimated increase in price with 1 bdrm ie 140 sqrft
summary(lm.3)$coefficient[3,1]+140*summary(lm.3)$coefficient[2,1]
# (ii) shows the increase in price when we get 1 more bdrm with
# 0 sqrt, which does not make sense. (iii) shows the more
# realistic idea of how much price increases with an extra bdrm
# with a physical area of 140 sqrft.

# iv)
summary(lm.3)$r.squared
# About 63.2% of the variation in price is explained by the 
# square footage and the number of bedrooms.

# v)
# predicted selling price of the first house
lm.3$fitted[1]

# vi)
# residual for the first house
lm.3$residuals[1]
# This suggests that the buyer underpaid for this house by $54k

# C8)
# i)
data("discrim")
# to check if AT LEAST 1 NA, [anyNA()] or [sum(is.na(data))>0]
# deal with NA values
mean(discrim$prpblck, na.rm = T)
sd(discrim$prpblck, na.rm = T)
# unit of measurement is proporiton of blacks within a zipcode
mean(discrim$income, na.rm = T)
sd(discrim$income, na.rm = T)
# unit of measurement is median income ($s) within a zipcode

# ii)
lm.4 <- lm(psoda ~ prpblck+income, data = discrim, 
           na.action = na.omit)
# sample size
length(lm.4$fitted)
# R-squared value
summary(lm.4)$r.squared
# equation form
# psoda = 9.563e-01 + 1.150e-01*prpblck + 1.603e-06*income
# Interpretation of 1.150e-01:
# We estimate that with each increase of prpblck by 1, holding
# all other variables constant, psoda will increase by 1.150e-01.
# I think this is a insignificant value. It is saying that with
# each 100 percentage point increase in the proportion of blacks,
# The price of medium soda increases by 10 cents.

# iii)
lm.5 <- lm(psoda ~ prpblck, data = discrim, 
           na.action = na.omit)
summary(lm.5)
# equation form
# psoda = 1.03740 + 0.06493*prpblck
# The discrimination effect smaller without the income.
# But the estimated intercept has increased.

# iv)
lm.6 <- lm(log(psoda) ~ prpblck+log(income), data = discrim, 
           na.action = na.omit)
summary(lm.6)
# equation form
# log(psoda) = -0.79377 + 0.12158*prpblck + 0.07651*log(income)
# According to Appendix A, in a semi-elastic relationship,
# % change in psoda = 100*slope*change in prpblack. Therefore,
# % change in psoda = 100*0.12158*0.2 = 2.4316

# v)
lm.7 <- lm(log(psoda) ~ prpblck+log(income)+prppov, data = discrim, 
           na.action = na.omit)
lm.7$coefficients["prpblck"]
# The slope decreased by about 0.05 after addint the new term.

# vi)
# solve correlation coeffecicient AND deal with NA values
cor(log(discrim$income),discrim$prppov, use = "complete.obs")
# No, the sign is roughly what I expected, but I expected a
# much smaller value. I was expecting a value of 0.2 maximum.

# vii)
summary(lm(log(psoda) ~ prpblck+prppov, data = discrim, 
           na.action = na.omit))
# equation form:
# log(psoda) = 0.039803 + 0.101558*prpblck - 0.155739*prppov
# The statement is not true at all. High correlation does not mean
# they will produce equal results. Creating a lm with prppov in
# place for log(income), we can see that the signs of our estimates
# changed. R^2 decreased from 0.0821 to 0.02. Also, our t values
# and their Pr(>|t|) value also drastically changed. P-value
# increased from 8.039e-07 to 0.004581 which is not good. Therefore,
# we can conclude that the statement is not true.
