setwd(dir = "C:/Users/slexi/Documents/ECON484")
#install.packages("RODBC")
#install.packages("ImportExport")

#library(ImportExport)
#access_import("SINAC_CIERRE_2017_SDP",c("NACIMIENTO"))



library(RODBC)
path <- file.path("SINAC_CIERRE_2017_SDP")
connect <- odbcConnect(path)
birthData <- sqlFetch(channel, "NACIMIENTO")


#install.packages("xlsx")
#library(xlsx)
#read.xlsx(file.choose(),1)