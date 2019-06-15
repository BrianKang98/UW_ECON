# hw3
# 1)
n <- 500
# i)
set.seed(123456)
xi <- runif(n,0,10)
mean(xi)
sd(xi)

# ii)
set.seed(123456)
ui <- rnorm(n,0,sqrt(36))
mean(ui)
# The sample average of ui will not be zero unless all
# 500 generated error numbers equals to zero OR the
# samples are exactly symmetric with zero being the center.
# Since we generated with mu=zero with sd=6, it is extremely 
# unlikely that we will get a sample mean exactly equal to 0.
sd(ui)

# iii)
yi <- 1+2*xi+ui
lm.0 <- lm(yi~xi)
summary(lm.0)
# Intercept: 0.99934
# Slope: 2.01509
# Both estimated intercept and slope are pretty close to the
# population intercept and slope. They are not equal due to
# the random generation of numbers and addition of random
# error from a normal distribution to a term from a uniform 
# distribution.

# iv)
resids <- lm.0$residuals
sum(resids)
sum(xi*resids)
# Both sums are extremely close to zero.
# Thus, verifying two of our assumptions.

# v)
sum(ui)
sum(xi*ui)
# These sums are nowhere near zero. Clarifying that there is
# a difference between errors and residuals.

# vi)
# vi) i)
set.seed(567890)
xi2 <- runif(n,0,10)
mean(xi2)
sd(xi2)

# vi) ii)
set.seed(567890)
ui2 <- rnorm(n,0,sqrt(36))
mean(ui2)
# Again, the sample average of ui2 will not be zero unless all
# 500 generated error numbers equals to zero OR the
# samples are exactly symmetric with zero being the center.
# Since we generated with mu=zero with sd=6, it is extremely 
# unlikely that we will get a sample mean exactly equal to 0.
sd(ui2)

# vi) iii)
yi2 <- 1+2*xi2+ui2
lm.02 <- lm(yi2~xi2)
summary(lm.02)
# Intercept: 0.15412 (Beta0_hat)
# Slope: 2.11078 (Beta1_hat)
# This time, our intercept estimate is quite off and our slope
# estimate is a bit off than before from the population values.
# Our estimators' values are different from before's because
# we are using different samples. And with the variations
# caused by the random samples, our Beta0_hat and Beta1_hat
# values will vary almost everytime.

# 2)
n <- 1000
set.seed(123456)
x <- runif(n,0,1) # x val
e <- rnorm(n,0,1) # error
v <- rnorm(n,1,1)
z <- v*x # iv val
y <- 4+0.3*x+e # true val
# a)
lm.1 <- lm(y~x)
# b)
# install.packages("AER")
library(AER)
lm.2 <- ivreg(y~x|z)
# plots to compare
par(mfrow = c(3,2))
plot(x,y,main = "True Value with Regression")
abline(lm.1, col = 'red')
abline(lm.2, col = 'blue')
plot(lm.1$fitted,lm.2$fitted, main = "OLS vs IV", 
     xlab = "OLS", ylab = "IV")
# residual plots
plot(x,lm.1$resid, main = "OLS Resid")
plot(x,lm.2$resid, main = "IV Resid")
# qqplots
qqplot(y,lm.1$fitted, ylim = c(3.95,4.35), main = "OLS qqPlot")
qqplot(y,lm.2$fitted, ylim = c(3.95,4.35), main = "IV qqPlot")
par(mfrow = c(1,1))
plot(y)
points(lm.2$fitted, col = "blue")
points(lm.1$fitted, col = "red")
# summaries
summary(lm.1)
summary(lm.2)
# summary(lm.1)$sigma
# summary(lm.2)$sigma
# summary(lm.1)$r.squared
# summary(lm.2)$r.squared

# i)
# Comparing the OLS estimates and the true values, we can see
# that the redisuals are quite large with a R-squared value of
# 0.002613. The residual SE is 1.001. The linear model's
# intercept is 4.06372 with a slope 0.17777. Looking at the
# qqplot we can clearly tell that the generated data comes from
# a normal distribution. Overall, we can say that the OLS
# regression captures the general trend of the true values.

# ii)
# Comparing the IV estimates and the true values, we can see
# that the residuals are again quite large with a R-squared
# value of 0.001122. The residual SE is 1.002. The model's
# intercept is 3.9971 with a slope 0.3121. From the qqplot we
# can again see that the data comes from a normal distribution.
# Here as well, we can say that the IV regression captures the
# general trend of the true values.

# iii)
# The interesting facts come from comparing OLS and IV. Even 
# though they give similar estimates, they are different. The
# IV's R-squared value is smaller. Its residual SE is larger.
# Its intercept is close enough but has a steeper slope. The
# min and max values of the residuals are a slightly more
# extreme than the OLS's. I would say that these two estimates
# give mostly similar results from our simple generated true
# data, but the IV estimate is just slightly more "spread out."