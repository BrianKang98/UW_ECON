# setting working directory
setwd()
x <- 5
y <- 2

# deleting values
rm(x) # deleting just "x"
ls() # shows just a list
rm(list = ls()) # deleting everything

a <- c(7,2,6,9,4,1,3)
sort(a)
length(a)
max(a)
min(a)
30:40

cities <- c("Seatte", "Bellevue","Redmond")

# labeling
k <- c(1,2,3,2,1)
kf <- factor(k,labels = c("NA","A","B"))

# assigning names
age <- c(18,21,26,24,20)
students <- c("Amy","Bob","Chris","David","Emily")
names(age) <- students
age[2]
age["Bob"]
age[age>20]

# matrices
v <- c(2,-4,-1,5,7,0)
A <- matrix(v, nrow=2)
colnames(A) <- c("one","tow","three")
rownames(A) <- c("uno","dos")

# subset of matrix elements
A[2,3]
A[,2]
A[,c(1,3)]

# matrix alg.
B <- matrix(c(2,1,0,3,-1,5), nrow = 2)
A*B # element mult.
C <- t(B)
D <- A%*%C # matrix mult.
solve(D) # the inverse

# data frames
year <- c(2008:2013)
product1 <- c(0,3,6,9,7,8)
product2 <- c(1,2,3,5,9,6)
product3 <- c(2,4,4,2,3,2)

# making matrix
sales_matrix <- cbind(product1,product2, product3)
# making data frame
sales = as.data.frame(sales_matrix)

# subset of data frame
ab = sales$product2

# basic plots of q and p
p = c(1,2,5,6,8)
q = c(0,3,4,6,9)
plot(p,q)
plot(p,q, type = "l")

## Section 1.5.1 Discrete Distributions ----------------------------------
# loading foreign package to deal with stata files
library(foreign)

# downloading the data
affairs = read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/affairs.dta") #we have data set called affairs stored in our environment

head(affairs)

# generate factors to attach labels
haskids <- factor(affairs$kids, labels = c("no","yes"))
marriage <- factor(affairs$ratemarr, labels = c("vunhappy","unhappy","aver","hap","vhap"))

# frequencies: counts and shares
table(haskids)
table(marriage)
prop.table(table(haskids)) # shows the proportions
prop.table(table(marriage))

# contingencies
countstab <- table(marriage, haskids)
prop.table(countstab, margin = 1) #look at x values
prop.table(countstab, margin = 2) # look at y values (sum of columns = 1)


# pie plots


# bar plots
barplot(table(marriage))
barplot(table(haskids,marriage))

## Section 1.5.4 Fundamental Statistics ----------------------------------
# loading the data
library(foreign)
ceosal1 = read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/ceosal1.dta")

# basic summary statistics
mean()
sd()
median()














