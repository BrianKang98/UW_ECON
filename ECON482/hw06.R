# hw6
# ch.10
# C1
library("wooldridge")
data(intdef)
dat1 <- intdef
y79 <- ifelse(dat1$year>1979,1,0) # if true then 1, if false then 0
cbind(dat1, y79) # finished making and adding dummy variable
lm.1 <- lm(i3 ~ inf+def+y79, data = dat1)
summary(lm.1)
# I say that there is a shift in the interest rate equation
# after 1979. We see a decrease in intercept from 1.73 to 1.2962,
# decrease in coefficient of def from 0.513 to 0.3626. Also,
# interpreting the coefficient of our dummy, we expect that the interest
# rate i3 to increase by 1.5587 per increase in years after 1979 on avg.,
# holding all else constant.

# C7
# i)
data(consump)
dat2 <- consump
lm.2 <- lm(gc ~ gy, data = dat2)
summary(lm.2)
# Form: gc = 0.008079 + 0.570781*gy
# Interpretation:
# I: We estimate that when gy=0, gc will equal 0.008079, all else equal.
# Slope: On average, we estimate that gc will increase by 0.570781 per increase
# in gy, hold all else equal.
# Both intercept and slope are statistically significant even at the 0.001 level

# ii)
tsdat1 <- ts(dat2)  # create time series data
# linear regression of gc on lag of gy
#install.packages("dynlm")
library(dynlm)
# gc ~ gy + L(gy)
res1 <- dynlm(gc ~ gy + L(gy), data = tsdat1)
summary(res1)
# We see that the t-stat for the lag is 1.394, insignificant even at the 10%
# confidence level. Also, the lag's slope is not large and the slopes of other
# variables have not altered much too. We cannot say that adjustment lags
# are really affecting consumptions.

# iii)
lm.3 <- lm(gc ~ gy+r3, data = dat2)
summary(lm.3)
# The t-stat for the coefficient of r3 is -0.343 and the p-value is
# 0.733901. It is insignificant at the 5% confidence level.
# Also, the slope itself is very small -0.0002148. Therefore, we cannot
# say that real interest rate affects consumption growth.

# C11
data(traffic2)
dat3 <- traffic2
# i)
temp <- (subset(dat3, dat3[,"spdlaw"]==1)$t)[1] # the time trend
dat3[dat3$t==temp,]
# 1987, May the Speed Law was passed
temp <- (subset(dat3, dat3[,"beltlaw"]==1)$t)[1]
dat3[dat3$t==temp,]
# 1986, January the Seatbelt Law was passed

# ii)
tsdat2 <- ts(dat3, start = 1981, frequency = 12)
#months <- paste(names(dat3)[c(23:33)],sep = "",collapse = "+")
res2 <- dynlm(log(totacc) ~ t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,data = tsdat2)
summary(res2)
# F-test
library(car)
null = c("feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
linearHypothesis(res2,null)
# We can see that, basing on Jan, only Feb has a lower totacc (only it has neg. coeff).
# Also, Dec is the peak, with 9.61571% more totacc than Jan.
# From the F-test we get a F-stat of 5.1501 with DF 11,95. So, the monthly dummies are
# jointly statistically significant even at a 0.1% confidence level.
# Individually, feb,mar,aug - dev were statistically significant at the 5% level.
# Also, the coefficient of trend variable t shows that, ignoring seasonality,
# totacc increases by 0.27471% per month over the time series we have.
# Therefore, we conclude that there is evidence of seasonality.

# iii)
res3 <- dynlm(log(totacc) ~ wkends+unem+spdlaw+beltlaw+t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,data = tsdat2)
summary(res3)
# Slope of unem: -0.0212173
# Yes this makes sense. Higher unemployment means a weaker and more unstable economy,
# so it is likly that less cars will be on the street and so less car accidents will
# occure. Also, 1% increase in unem causing a decrease in car accidents by 2.12173%
# could seem plausible due to the decrease in economic activity.

# iv)
# spdlaw: We estimate that, after the spdlaw passes, log(totacc) will
# decrease by 5.37593% on average, holding all else constant.
# beltlaw: We estimate that, after the beltlaw passes, log(totacc) will
# increase by 9.54526% on average, holding all else constant.
# The effects of spdlaw make sense. With the spdlaw, cars will likely slow down,
# and the liklihood that car accidents happening will decrease.
# The effect of beltlaw could make sense, but not likely. With the belt law
# requirement, more people put on seatbelts and this could cause a false sense of
# security, driving people to drive more dangerously and cause accidents.
# This would be contrary to the intention: mandating seatbels for people's safety.

# v)
mean(dat3$prcfat)
# Avg. percentage of fatal accidents is 0.8856363. Less that 1% of accidents
# turns out to be fatal apparently. Highest value is 1.217. I think this makes sense
# because fatal would be defined "life threatening or impaling".
# Although many accidents are very dangerous, I think the small pertencage of
# "fatal" accidents occuring makes logical sense.

# vi)
res4 <- dynlm(prcfat ~ wkends+unem+spdlaw+beltlaw+t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,data = tsdat2)
summary(res4)
# F-test
library(car)
null = c("spdlaw","beltlaw")
linearHypothesis(res4,null)
null = c("spdlaw=1","beltlaw=1")
linearHypothesis(res4,null)
# spdlaw: 6.709e-02; beltlaw: -2.951e-02
# With the speed law, interestingly, the percentage of fatal accidents
# increase, and with the belt law it decreases, as it should.
# The spdlaw is individually significant at the 1% level, but
# beltlaw is insignificant.
# Jointly they are significant.


# ch.11
# C9
# i)
tsdat3 <- ts(dat3)
acf(tsdat3[,"prcfat"],plot = F,lag.max = 1) # 1st Order autocorr coeff
# The 1st order autocorr coeff is high but it should not be a huge
# concern because it is does not contain a unit root.
acf(tsdat3[,"unem"],plot = F,lag.max = 1)
# But for unem, the 1st order autocorr coeff is very high (close to 1)
# so we should be concerned about it possibily containing a unit root.

# ii)
res5 <- dynlm(d(prcfat) ~ wkends+d(unem)+spdlaw+beltlaw+t+feb+mar+apr+may+jun+jul+aug+sep+oct+nov+dec,data = tsdat3)
summary(res5)
summary(res5)$r.squared
# We see some seasonality with only Oct and Nov being lower than
# Jan. The slope of d(unem) is small and individually insignificant
# so we cannot say that the difference in prcfat can be explained
# by the difference in the difference of unemployment. Also,
# most of the variables are considered insignificant. Even the trend
# is showing insignificance and the slope is small. Note that
# the high R2 value is affected by seasonality.

# iii)
# The reasoning isn't entirely correct. It is not necessarily the safest 
# strategy and we end up not getting similar results using the levels, 
# we mostly lose the interesting interpretations in our model. However,
# this shouldn't be the reason for not using the different levels in
# our models. It is difficult to say when to take the 1st order
# differences.

# C13
# i)
##############################################
#CHAPTER 11 HOMEWORK PROBLEMS NOT DUE ANYMORE#
##############################################

