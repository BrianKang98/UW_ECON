# Hw1
# 4
# a)
setwd(dir = "C:/Users/slexi/Documents/ECON484")
#install.packages("ISLR", lib = "C:/Users/slexi/Documents/ECON484")
library(ISLR)
data("College")
college <- read.csv("College.csv")

# b)
# The names of colleges are considered as a data still,
# so make a new "private" column with the names of the 
# colleges.
rownames(college) = college[,1]
fix(college)

# Now get rid of the old "public" column with the names
# that were still considered data
college = college[,-1]
fix(college)

# c)
# i)
summary(college)
# ii)
pairs(college[,1:10])
# iii)
plot(college$Private, college$Outstate, 
     xlab = "Private", 
     ylab ="Out of State tuition (USD)", 
     main = "Outstate Tuition")
# iv)
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(Elite)

plot(college$Elite, college$Outstate, 
     xlab = "Elite", 
     ylab ="Out of State tuition (USD)", 
     main = "Outstate Tuition")

# v)
par(mfrow=c(2,2))
hist(college$Grad.Rate)
hist(college$Grad.Rate, breaks = 20)
summary(college$Grad.Rate)  # There is a college with a 118% graduation rate...
row.names(college[nrow(college[college$Grad.Rate==118,]),])
hist(college$PhD)
hist(college$PhD, breaks = 30)
summary(college$PhD)  # There is a college with a 103% of workers with a PhD...
row.names(college[nrow(college[college$PhD==103,]),])
# We can see that same data for this college is incorrect.

# vi)
par(mfrow=c(4,5))
name <- colnames(college)
#for (i in c(1:ncol(college))) {
#  hist(as.numeric(college[,i]), xlab = name[i], main = NULL)
#}

# found online, heatmap of overall variables
#install.packages("pheatmap")
library(pheatmap)
pheatmap(t(as.matrix(scale(College[, 2:18]))),
         annotation=College[1],
         show_colnames=FALSE)

# As epected from the histograms we can see that the application counts,
# acceptances, enrollments all show exponential trends. We can see that
# the top 25% absorbs the top 10% trend. Out of state tuition, room board,
# student-faculty ratio, graduation rate seem normal. Books, personal,
# perc. alumni, expend are skewed right. PhD and terminal are skewed left.
# F&P undergrad's heavy skew is expected.
# The heatmap is standardized and transposed for convenience, meaning
# the vertical bars correspond to every one college and the horizontals are
# the 19 variables each college information is stored. The general behavior
# I see is the general spread out of numbers (the blue color) but there
# seems to be a couple exceptions. There is one college with a lot of books;
# one with particularly high expenditure; one with a lot of part time
# undergrads and applications and acceptances.