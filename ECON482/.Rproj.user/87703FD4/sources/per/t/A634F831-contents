# hw1
# a)
setwd(dir = "C:/Users/slexi/Downloads")
affairs <- read.table("affairs.txt", header = F)
# b)
colnames(affairs) <- c("id","male","age","yrsmarr","kids","relig","educ","occup","ratemarr","naffairs","affair","vryhap","hapavg","avgmarr","unhap","vryrel","smerel","slghtrel","notrel")
#head(affairs)

# c)
# i)
sum(affairs$male)
# we have 286 men in our dataset
# ii)
mean(affairs$educ)
# the average year of schooling is 16.16639
# iii)
sum((affairs$male)==0 & (affairs$affair==1))
# 72 women had affairs within the last year

# d)
count <- table(affairs$kids,affairs$relig)
rownames(count) <- c("without Kids","with Kids")
barplot(count,main = "Religion by Kids",
        xlab = "Number People...", names.arg = c("Anti","Not at all","Slightly","Somewhat","Very"),
        horiz = T,legend = rownames(count))

