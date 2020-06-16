rm(list = ls())
library(reshape2)
setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)
#df <- df[,-c(14:17)]
oj_prices <-df[,1:6]
oj_wide <- dcast(oj_prices, store + week ~ brand)
colnames(oj_wide)[3] <- "P_Dom"
colnames(oj_wide)[4] <- "P_MM"
colnames(oj_wide)[5] <- "P_Trop"
oj_cross <- merge(df, oj_wide, by=c("week","store"))
dom_cross <- subset(oj_cross, brand=="dominicks")

# 2a
summary(dom_cross$INCOME)

# 2b
# below 1st qtr
qtr1 <- which(dom_cross$INCOME <= summary(dom_cross$INCOME)["1st Qu."])
mean(dom_cross$logmove[qtr1])
# between 1st qtr and median
qtr2 <- which((dom_cross$INCOME <= summary(dom_cross$INCOME)["Median"]) & 
    (dom_cross$INCOME > summary(dom_cross$INCOME)["1st Qu."]))
mean(dom_cross$logmove[qtr2])
# between median and 3rd qtr
qtr3 <- which((dom_cross$INCOME <= summary(dom_cross$INCOME)["3rd Qu."]) & 
    (dom_cross$INCOME > summary(dom_cross$INCOME)["Median"]) )
mean(dom_cross$logmove[qtr3])
# above 3rd qtr
qtr4 <- which(dom_cross$INCOME > summary(dom_cross$INCOME)["3rd Qu."])
mean(dom_cross$logmove[qtr4])

# 2c
lm.1 <- lm(formula = logmove ~ log(P_Trop) + log(P_MM) + log(P_Dom) * 
             feat + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM + 
             HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5, data = dom_cross)
# mse below 1st qtr
mean((lm.1$fitted[qtr1]-dom_cross$logmove[qtr1])^2)
# mse between 1st qtr and median
mean((lm.1$fitted[qtr2]-dom_cross$logmove[qtr2])^2)
# mse between median and 3rd qtr
mean((lm.1$fitted[qtr3]-dom_cross$logmove[qtr3])^2)
# mse above 3rd qtr
mean((lm.1$fitted[qtr4]-dom_cross$logmove[qtr4])^2)

# 2d
# The MSE between the median and 3rd quartile was the lowest.
# This means that the observations within this range is closer to a linear fit, since the fitted values were created using a linear function.
# This allows the MSE to be smaller.
# The obervations of logplot of those who are in this range is plotted below.
# Although not obvious, after analysis these observations had smaller standard deviation in fact.
# It can also be seen in the residual plot below after additional analysis.

# 2e
# The MSE between the 1st quartile and the median was the largest.
# This means that these observations have more curvature in this range.
# Fitting a higher complexity curve will decrease the MSE for this range.

par(mfrow = c(1,1))
plot(dom_cross$logmove[qtr3], col="red", cex = 0.7, ylab = "logmove", main = "Blue line is the mean of logmove of those within 3rd Qu.")
points(dom_cross$logmove[-qtr3], cex = 0.7)
abline(a=9.056749, b=0, col="blue", lwd = 2)
legend("bottomleft",legend=c("Within 3rd Qu.", "Outside 3rd Qu."),
       col=c("red", "black"), pch = 1, cex=0.75, inset=.02, pt.cex = 0.85)

par(mfrow = c(2,2))
plot(lm.1)

