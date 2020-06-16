rm(list = ls())
library(reshape2)
library(plyr)
library(dplyr)
library(tree)
library(lfe)
library(rpart)
library(rattle)
library(caret)
library(maptree)
library(rpart.plot)
library(permute)
library(partykit)
setwd("C:/Users/slexi/Documents/UW_ECON/ECON487")
df <- read.csv("oj.csv", header = T)

# 1a
df$Q <- exp(df$logmove)

# 1b
dfQ <- ddply(df, c("store","week"), function(x) c(weighted_price = weighted.mean(x$price,x$Q)))
df <- merge(df, dfQ, by = c("store", "week"))

# 2a: Done
# 2b
dataToPass<-df[,c("weighted_price","AGE60","EDUC","ETHNIC","INCOME","HHLARGE","WORKWOM","HVAL150","SSTRDIST","SSTRVOL","CPDIST5","CPWVOL5")]
# One way of making tree
out_tree <- tree(weighted_price~., data = dataToPass)
# two ways of plotting a tree()
plot(out_tree)
text(out_tree,pretty=TRUE)
draw.tree(out_tree,cex=.8) 

# 2c
# Second way of making tree
fit<-rpart(weighted_price~., data=dataToPass,method="anova",cp=0.007)
# two ways of potting a rpart()
draw.tree(fit)
fancyRpartPlot(fit, main="weighted_price",palettes=c("Greens","Reds"))

# 2d
# assign stores into groups of leaves
df$leaf = fit$where 
leaves <- unique(df$leaf)

# 3
# own price elasticities depending on leaf
reg_int1 <- glm(logmove~log(price)*brand*feat, data=subset(df, leaf==leaves[1]))
summary(reg_int1)
reg_int2 <- glm(logmove~log(price)*brand*feat, data=subset(df, leaf==leaves[2]))
summary(reg_int2)
reg_int3 <- glm(logmove~log(price)*brand*feat, data=subset(df, leaf==leaves[3]))
summary(reg_int3)


# 3a
# make cross price df
oj_prices <-df[,1:6]
oj_wide <- dcast(oj_prices, store + week ~ brand)
colnames(oj_wide)[3] <- "price_D"
colnames(oj_wide)[4] <- "price_MM"
colnames(oj_wide)[5] <- "price_T"
oj_cross <- merge(df, oj_wide, by=c("week","store"))

# tropicanan
trop_cross <- subset(oj_cross, brand=="tropicana")
reg_int <- glm(logmove~log(price_D)*feat + log(price_T)*feat + log(price_MM)*feat, data=trop_cross)
summary(reg_int)
# dominicks
dom_cross <- subset(oj_cross, brand=="dominicks")
reg_int <- glm(logmove~log(price_D)*feat + log(price_T)*feat + log(price_MM)*feat, data=dom_cross)
summary(reg_int)
# minute maid
mm_cross <- subset(oj_cross, brand=="minute.maid")
reg_int <- glm(logmove~log(price_D)*feat + log(price_T)*feat + log(price_MM)*feat, data=mm_cross)
summary(reg_int)


# 3b x3 more regressions for each leaf
oj_prices <-oj_cross[,1:6]
oj_wide <- dcast(oj_prices, store + week ~ brand, value.var = "logmove")
colnames(oj_wide)[3] <- "logmove_D"
colnames(oj_wide)[4] <- "logmove_MM"
colnames(oj_wide)[5] <- "logmove_T"
oj_cross <- merge(oj_cross, oj_wide, by=c("week","store"))

oj_cross_l1 <- subset(oj_cross, leaf == leaves[1])
oj_cross_l2 <- subset(oj_cross, leaf == leaves[2])
oj_cross_l3 <- subset(oj_cross, leaf == leaves[3])

# leaf 1
reg_int1 <- glm(logmove_T~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l1)
reg_int2 <- glm(logmove_MM~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l1)
reg_int3 <- glm(logmove_D~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l1)

# 3bi: leaf 1
rownames = c("Q Trop", "Q MM", "Q Dom")
colnames = c("P Trop", "P MM", "P Dom")
Elast_matrix <- matrix(,3,3, dimnames = list(rownames, colnames))

Elast_matrix[1,1] <- coef(reg_int1)["log(price_T)"] 
Elast_matrix[1,2] <- coef(reg_int1)["log(price_MM)"] 
Elast_matrix[1,3] <- coef(reg_int1)["log(price_D)"] 
Elast_matrix[2,1] <- coef(reg_int2)["log(price_T)"] 
Elast_matrix[2,2] <- coef(reg_int2)["log(price_MM)"] 
Elast_matrix[2,3] <- coef(reg_int2)["log(price_D)"] 
Elast_matrix[3,1] <- coef(reg_int3)["log(price_T)"] 
Elast_matrix[3,2] <- coef(reg_int3)["log(price_MM)"] 
Elast_matrix[3,3] <- coef(reg_int3)["log(price_D)"] 
Elast_matrix1 <- Elast_matrix

# 3b: leaf 2
reg_int4 <- glm(logmove_T~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l2)
reg_int5 <- glm(logmove_MM~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l2)
reg_int6 <- glm(logmove_D~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l2)

# 3bi: leaf 2
rownames = c("Q Trop", "Q MM", "Q Dom")
colnames = c("P Trop", "P MM", "P Dom")
Elast_matrix <- matrix(,3,3, dimnames = list(rownames, colnames))

Elast_matrix[1,1] <- coef(reg_int4)["log(price_T)"] 
Elast_matrix[1,2] <- coef(reg_int4)["log(price_MM)"] 
Elast_matrix[1,3] <- coef(reg_int4)["log(price_D)"] 
Elast_matrix[2,1] <- coef(reg_int5)["log(price_T)"] 
Elast_matrix[2,2] <- coef(reg_int5)["log(price_MM)"] 
Elast_matrix[2,3] <- coef(reg_int5)["log(price_D)"] 
Elast_matrix[3,1] <- coef(reg_int6)["log(price_T)"] 
Elast_matrix[3,2] <- coef(reg_int6)["log(price_MM)"] 
Elast_matrix[3,3] <- coef(reg_int6)["log(price_D)"] 
Elast_matrix2 <- Elast_matrix

# 3b: leaf 3
reg_int7 <- glm(logmove_T~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l3)
reg_int8 <- glm(logmove_MM~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l3)
reg_int9 <- glm(logmove_D~log(price_D)*feat*brand + log(price_T)*feat*brand + log(price_MM)*feat*brand, data=oj_cross_l3)

# 3bi: leaf 3
rownames = c("Q Trop", "Q MM", "Q Dom")
colnames = c("P Trop", "P MM", "P Dom")
Elast_matrix <- matrix(,3,3, dimnames = list(rownames, colnames))

Elast_matrix[1,1] <- coef(reg_int7)["log(price_T)"] 
Elast_matrix[1,2] <- coef(reg_int7)["log(price_MM)"] 
Elast_matrix[1,3] <- coef(reg_int7)["log(price_D)"] 
Elast_matrix[2,1] <- coef(reg_int8)["log(price_T)"] 
Elast_matrix[2,2] <- coef(reg_int8)["log(price_MM)"] 
Elast_matrix[2,3] <- coef(reg_int8)["log(price_D)"] 
Elast_matrix[3,1] <- coef(reg_int9)["log(price_T)"] 
Elast_matrix[3,2] <- coef(reg_int9)["log(price_MM)"] 
Elast_matrix[3,3] <- coef(reg_int9)["log(price_D)"] 
Elast_matrix3 <- Elast_matrix

# 3bii, 3biii
Elast_matrix1
Elast_matrix2
Elast_matrix3

# 3 unique 3x3 matrices are made above










