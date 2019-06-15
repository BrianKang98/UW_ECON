# lab 1, the basic intro to R
b <- runif(5)
b
log(b)
# comment following class

1:5/(1:5)

matrix(runif(42),10,15)
matrix(1:5,5,5)
mm <- t(matrix(1:5,5,5))

# me just screwing around trying to remember stuff from math390
# testm <- matrix(rnorm(250, mean = 5, sd = 2),125,2)
plot(testm[,1], testm[,2], main = 'this is a random plot')
lines(testm,type = 'l', col = 'red')

b/log(b)
# install.packages(c('excel.link','survival'))

mmm <- matrix(runif(16),4,4)
solve(mmm)
solve(mmm) %*% mmm  # essencially the identity matrix








