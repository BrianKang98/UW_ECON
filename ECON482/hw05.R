library("wooldridge")
data("lawsch85")

# C2)
# i)
# Null Hypotheses H0: Beta5=0
# Alt. Hypotheses H1: Beta5=/=0
lm.1 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, 
           data = lawsch85, na.action = na.omit)
summary(lm.1)
# sample size
n <- length(lm.1$fitted)
# R squared
r2 <- summary(lm.1)$r.squared
# t- stat of rank
summary(lm.1)$coefficients[5+1,3]
# critical value
qt(1-0.05/2, n-1-1)
# The at the 5% confidence level, the abs value of the t-stat 
# is greater than the critical value; therefore, we reject our
# null hypothesis and conclude that the rank of law schools has
# a ceteris paribus effect on median starting salary.

# ii)
# t-stat of LAST
summary(lm.1)$coefficients[1+1,3]
# |1.171045| < 1.977826
# Therefore we cannot reject the null hypothesis and LSAT is
# individually insignificant.

# t-stat of GPA
summary(lm.1)$coefficients[2+1,3]
# |2.749133| > 1.977826
# Therefore we reject the null hypothesis in favor of the alt.
# GPA is individually significant for explaining salary.

# F-stat
# testing LSAT and GPA jointly
# H0: Beta1=Beta2=0
# k=5; q=2;
reg.r <- lm(log(salary)~log(libvol)+log(cost)+rank, 
           data = lawsch85, na.action = na.omit)
rsquared.ur <- summary(lm.1)$r.squared
rsquared.r <- summary(reg.r)$r.squared
# calculating the F-stat
((rsquared.ur - rsquared.r)/(1-rsquared.ur))*(n-5-1)/2
# critical value
qf(1-0.05,2,n-5-1)
# 8.038666 > 3.065839; therefore, LSAT and GPA jointly are
# significant.

# iii)
# This time, H0: Beta6,7=0
# k=7; q=2;
lm.2 <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank
           +clsize+faculty, 
           data = lawsch85, na.action = na.omit)
summary(lm.2)
# sample size
n2 <- length(lm.2$fitted)
reg.r <- lm(log(salary)~LSAT+GPA+log(libvol)+log(cost)+rank, 
            data = lawsch85, na.action = na.omit)
rsquared.ur <- summary(lm.2)$r.squared
rsquared.r <- summary(reg.r)$r.squared
# calculating the F-stat
((rsquared.ur - rsquared.r)/(1-rsquared.ur))*(n-7-1)/2
# critical value
qf(1-0.05,2,n-7-1)
# 0.9431595 < 3.066952, so clsize and faculty are not jointly
# significant at the 5% confidence level. Therefore, they do 
# not need to be added to this equation

# iv)
cor(lawsch85$rank, lawsch85$lcost, use = "complete.obs")
cor(lawsch85$rank, lawsch85$age, use = "complete.obs")
cor(lawsch85$rank, lawsch85$llibvol, use = "complete.obs")
# Looking at the correlation coefficients of the rank and other
# variables within the data frame, we can see that some outside
# of our regression equation may influence the rank of the law
# school. The three I found include lcost,age,and llibevol.