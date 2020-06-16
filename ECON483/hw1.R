# Brian Kang
# Econ 487: HW1

#setwd("C:/Users/slexi/Documents/UW_ECON/ECON483")

x <- c(1,3,2,5)
x # print x
y = c(1,4,3)
y # print y
length(x) # 4
length(y) # 3
x+y # invalid
x <- c(1,6,2)
x+y # (2 10 5)
ls()
rm(x,y) # remove x,y
ls() # empty
# rm(list = ls())

x <- matrix(data = c(1,2,3,4), nrow = 2, ncol=2)
x
matrix(data = c(1,2,3,4), nrow = 2, ncol=2, byrow = T)
sqrt(x) # element wise sqrt
x^2 # element wise squaring

x <- rnorm (50) # std norm, n=50
y <- x + rnorm (50, mean=50, sd=0.1) # make new vars~normal dist.
cor(x,y) # correlation

set.seed(123)
rnorm(40)
set.seed(890) # different!
rnorm(40)

set.seed(123)
y <- rnorm(10)
mean(y)
v <- var(y) # variance
sqrt(v) # std dev
sd(y) # std dev

x <- rnorm(100)
y <- rnorm(100)
#plot(x,y, main = " x vs y", xlab = "x axis", ylab = "y axis")

#jpeg("hw1.jpg") # start jpg output plot
#plot(x,y, main = " x vs y", 
#     xlab = "x axis", ylab = "y axis",
#     col = "red")
#dev.off() # stop plotting

x <- seq(1,10)
y <- 1:10
x==y # true 10 times
seq(0,1,length = 10) # 0, 0.111, 0.222, ... 0.8889, 1.

x <- seq(-pi ,pi ,length = 50)
y <- x
f <- outer(x,y,function (x,y)cos(y)/(1+x^2)) # cross product
#contour(x,y,f) # contour plot
#contour(x,y,f,nlevels = 45, add=T) # more levels
fa <- (f-t(f))/2 # t() is transpose
#contour(x,y,fa,nlevels = 15)

#image(x,y,fa) # heatmap
#persp(x,y,fa) # 3d
#persp(x,y,fa ,theta =30, phi =20) # 3d different angles

A <- matrix(1:16, 4,4)
A
A[2,3] # 10
A[c(1,3), c(2,4)] # row 1,3 and column 2,4
A[1:3, 2:4] # row 1 to 3, column 2 to 4
A[1:2,] # everything in row 1,2
A[-(1:2) ,] # everything BUT row 1,2
dim(A) # 4x4 matrix

#install.packages("ISLR")
library(ISLR) # Auto dataset readily available
#fix(Auto) # view data
dim(Auto)
Auto <- na.omit(Auto)
dim(Auto)
names(Auto) # variable names

#plot(Auto$cylinders, Auto$mpg) # scatterplot
str(Auto)
#plot(as.factor(Auto$cylinders), Auto$mpg) # gives boxplot
attach(Auto) # make variable names as referable vectors
cylinders <- as.factor(cylinders)
#plot(cylinders , mpg)
#plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders",
#     ylab ="MPG")
#hist(mpg, col=2, breaks=15) # 2 is red

#pairs(Auto) # scatterplot of all vars against all vars
#jpeg(hw1.jpg)
pairs(~ mpg + displacement + horsepower + weight +
         acceleration, Auto) # scatterplot matrix of some pairs
#dev.off()

#plot(horsepower, mpg)
#identify (horsepower ,mpg ,name) # click specific points to identify

summary(Auto)
summary(mpg)

#savehistory() # save commands from this session
#loadhistory() # load commands from most recent session
#q() # quit