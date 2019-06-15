# ch.5
# c1
library(wooldridge)
data("wage1")
linm <- lm(wage~educ+exper+tenure, data=wage1)
hist(linm$residuals)
linm <- lm(log(wage)~educ+exper+tenure, data=wage1)
hist(linm$residuals)
# the log-level model is closer to satisfying MLR6

# c2
data(gpa2)
linm <- lm(colgpa~hsperc+sat, data=gpa2)
summary(linm)
# colgpa=1.392e+00 - 1.352e-02*hsperc + 1.476e-03*sat
linm2 <- lm(colgpa~hsperc+sat, data=gpa2[1:2070,])
summary(linm2)
# colgpa=1.436e+00 - 1.275e-02*hsperc + 1.468e-03*sat
summary(linm)$coefficients[,2]/summary(linm2)$coefficients[,2]

# ch.6
# c1
data("kielmc")
linm <- lm(log(price)~log(dist), data=kielmc)
summary(linm)
# I assume that the sign is positive because as the distance from the incinerator increases, the price of the house will most likely increase.
# And the results say that "we predict that as the log(dist) increases by one percentage point, on average, 0.31722 percentage point of log(price) will increase, holding all else constant."
# Intercept says that "we predict that on average, if log(dist) equals zero, log(price) equals 8.25750, holding all else constant."
linm <- lm(log(price)~log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age, data=kielmc)
summary(linm)
# With a whole new bunch of explanatory added to the regression I predict that the slope will still be positive, but the value will be very small
# Contrary to my prediction, the slope is now negative. Adding a whole bunch of new variables defintiely contributed to a change of the value of the slope. Could be overfitting, but it could be due some collinearity effect caused by one of more of the explanatory variables that were added.
linm <- lm(log(price)~log(dist)+log(intst)+I(log(intst)^2)+log(area)+log(land)+rooms+baths+age, data=kielmc)
summary(linm)
# The log(intst) came back to positive. The squared value's slope is negative though.
# Changing the functional form can drastically changes the signs of some of the slopes of the variables that werw used in the regression.
# The squared variable also turned out to individually statistically significant.

# c4
data(gpa2)
linm <- lm(sat~hsize+I(hsize^2), data = gpa2)
summary(linm)
# sat = 997.981 + 19.814*hsize -2.131*hsize^2
# The quadratic term is statisistically significant.
# Optimal hsize will be whatever value that makes sat=2400
# No, hsize is not enough to represent the academic performance of all high school seniors. Many other variables come to play when people attempt to quantify ability of student's academic performance
linm <- lm(log(sat)~hsize+I(hsize^2), data = gpa2)
summary(linm)
# Answer shouldn't be that different from the previous. Just find the hsise that makes log(sat)=log(2400)

# c5
data("hprice1")
linm <- lm(log(price)~log(lotsize)+log(sqrft)+bdrms,data=hprice1)
summary(linm)
# log(price)=-1.29704+0.16797*log(lotsize)+0.70023*log(sqrft)+0.03696*bdrms
-1.29704+0.16797*log(20000)+0.70023*log(2500)+0.03696*4
linm <- lm((price)~(lotsize)+(sqrft)+bdrms,data=hprice1)
summary(linm)
# I would prefer the log function over the level function

# c7,9,11,13
# just know how to do F-tests and calculate t & F stats













