rm(list=ls(all=TRUE))

if (!require(Rlab)) install.packages('Rlab')
library(Rlab)

#Set winrate####
startdate <- "2015-01-01"
enddate <- "2015-12-31"
setwinrate <- 0.5

#Read the data####
index <- read.csv("/Users/ystar/Desktop/backup/index.csv")
index[,"date"] <- as.Date(index[,"date"])
index <- subset(index, index$indexreturn != Inf & index$date>=as.Date(startdate) & index$date<=as.Date(enddate))
index <- index[,c("date","indexreturn")]

allstocks <- read.csv("/Users/ystar/Desktop/backup/allstocks.csv")
allstocks[,"date"] <- as.Date(allstocks[,"date"])
allstocks <- subset(allstocks, allstocks$return != Inf & allstocks$date>=as.Date(startdate) & allstocks$date<=as.Date(enddate))
allstocks <- allstocks[,c("date","id","return")]

stockno <- read.csv("/Users/ystar/Desktop/backup/stockno.csv")[,1]

#Merge the daily return of allstocks and index####
data <- merge(allstocks, index, by= "date")

#Good stock TURE better than the index, vice versa####
data$mark <- as.character(data$return > data$indexreturn)

#Show the result####
showdata <- data[order(data$id,data$date),]

#Caculate the trade day####
trade <- nrow(index)

#Caculate the winrate of each stock####
winno <- numeric()
for (i in 1:length(stockno))
{
  winno[i] <- count(data$mark == "TRUE" & data$id == stockno[i])
}
winrate <- winno/trade

#Seperate the good from the bad by setwinrate####
mark <- (winrate >= setwinrate)

#Finish the preperation for data mining####
final <- as.data.frame(cbind(stockno, winno, winrate, mark))
colnames(final)[1] = "id"
final[which(mark == 1),"mark"] <- "Good"
final[which(mark == 0),"mark"] <- "Bad"
write.csv(final,"/Users/ystar/Desktop/backup/markdata.csv")
