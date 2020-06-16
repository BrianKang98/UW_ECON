library(ggplot2)
library(dplyr)

setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)

require(gridExtra)
grid.arrange(
# 4ai
ggplot(df, aes("", price)) + geom_boxplot(),
# 4b
ggplot(df, aes("", log(price))) + geom_boxplot(), ncol=2)

grid.arrange(
# 4c
ggplot(df, aes(factor(brand), price)) + geom_boxplot(aes(fill = factor(brand))),
# 4d
ggplot(df, aes(factor(brand), log(price))) + geom_boxplot(aes(fill = factor(brand))), ncol=2)

# 5a
ggplot(df, aes(logmove, log(price))) + geom_point(aes(color = factor(brand)))

# 6a
lm.1 <- lm(logmove ~ log(price), data = df)
summary(lm.1)
#6b
lm.2 <- lm(logmove ~ log(price)+brand, data = df)
summary(lm.2)
#6c
lm.3 <- lm(logmove ~ log(price)*brand, data = df)
summary(lm.3)

# 7a
ggplot(df, aes("", feat)) + geom_point(aes(color = factor(brand)), position = "jitter")
# result is not entirely clear so make table
table(df$brand, df$feat)

# 7ai
aggregate(df[, c(which(names(df)=="price"),which(names(df)=="feat"))]
          , list(df$brand), mean) 
# same answer
df %>%
  group_by(brand) %>%
  summarize(avg_price = mean(price, na.rm = T),
            feat_rate = mean(feat, na.rm = T))

# 7b
lm.4 <- lm(logmove ~ price+factor(feat), data = df)
summary(lm.4)
# 7c
lm.5 <- lm(logmove ~ price*factor(feat), data = df)
summary(lm.5)
# 7d
lm.6 <- lm(logmove ~ price*factor(feat)+price*brand+factor(feat)*brand, data = df)
summary(lm.6)
# 7e
lm.7 <- lm(logmove ~ price*factor(feat)+price*brand+factor(feat)*brand+INCOME+EDUC, data = df)
summary(lm.7)

# 8c
avgprice <- aggregate(df[, c(which(names(df)=="price"),which(names(df)=="feat"))]
                                  , list(df$brand), mean)[,2]
elast <- c(-1.52839, -1.52839+0.61736, -1.52839+0.78958)
unitcosts <- -1/elast*avgprice
cbind(c("Dominicks", "Minute Maid","Tropicana"),unitcosts)
