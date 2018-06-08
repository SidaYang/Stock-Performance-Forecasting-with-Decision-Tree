rm(list=ls(all=TRUE))

if (!require(quantmod)) install.packages('quantmod')
library(quantmod)

#Give all parameters####
#startdate <- "1980-01-01"
#enddate <- "2016-06-07"
update <- "2016-06-08"
startid <- 1101
endid <- 9999

#Incremental update of index####
index <- read.csv("/Users/ystar/Desktop/backup/index.csv")
index[,"date"] <- as.Date(index[,"date"])
addindex <- getSymbols("^TWII", from=update ,to=update, auto.assign=FALSE)
colnames(addindex) <- c("open","high","low","close","volume","adjusted")
date <- as.Date(index(addindex))
addindex <- as.data.frame(addindex)
addindex$date <- date
addindex$indexreturn <- log(addindex[1,"close"])-log(index[nrow(index),"close"])
index <- rbind(index, addindex)
write.csv(index,"/Users/ystar/Desktop/backup/index.csv",row.names=F)

#Incremental update of allstocks####
#Convert data.frame to list#
stockslist <- list()
stockno <- read.csv("/Users/ystar/Desktop/backup/stockno.csv")[,1]
allstocks <- read.csv("/Users/ystar/Desktop/backup/allstocks.csv")
allstocks[,"date"] <- as.Date(allstocks[,"date"])
for (a in stockno)
{
  b <- paste0(a,".TW")
  stockslist[[b]] <- allstocks[which(allstocks$id==a),]
}

#Update#
allstocks <- data.frame()
for (a in startid:endid)
{
  b <- paste0(a,".TW")
  tryit <- try(getSymbols(b, from=startdate, to=enddate, auto.assign=FALSE))
  if (inherits(tryit, "try-error"))
  {
    print(paste("Missed", a))
    next
  }
  else 
  {
    addeachstock <- getSymbols(b, from=update, to=update, auto.assign=FALSE)
    colnames(addeachstock) <- c("open","high","low","close","volume","adjusted")
    addeachstock$id <- a
    date <- as.Date(index(addeachstock))
    addeachstock <- as.data.frame(addeachstock)
    addeachstock$date <- date
    addeachstock$return <- log(addeachstock[1,"close"])-log(stockslist[[b]][nrow(stockslist[[b]]),"close"])
    stockslist[[b]] <- rbind(stockslist[[b]], addeachstock)
    allstocks <- rbind(allstocks, stockslist[[b]])
    print(paste("Updated", a))
    stockno <- append(stockno, a)
  }
}
write.csv(stockno, "/Users/ystar/Desktop/backup/stockno.csv", row.names = F)
write.csv(allstocks,"/Users/ystar/Desktop/backup/allstocks.csv", row.names = F)
