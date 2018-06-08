rm(list=ls(all=TRUE))

if (!require(quantmod)) install.packages('quantmod')
library(quantmod)

#Give all parameters####
startdate <- "1980-01-01"
enddate <- "2016-06-13"
startid <- 1101
endid <- 1200

#Get the index price info####
index <- getSymbols("^TWII", from=startdate, to=enddate, auto.assign=FALSE)
colnames(index) <- c("open","high","low","close","volume","adjusted")
closelag <- as.numeric(lag(index$close,1))
closelag[is.na(closelag)] <- 0
close <- as.numeric(index$close)
return <- log(close)-log(closelag)
date <- as.Date(index(index))
index <- as.data.frame(index)
index$date <- date
index$indexreturn <- return
write.csv(index,"/Users/ystar/Desktop/backup/index.csv",row.names=F)

#Get all stocks price info####
stockno <- vector()
stockslist <- list()
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
    eachstock <- getSymbols(b, from=startdate, to=enddate, auto.assign=FALSE)
    colnames(eachstock) <- c("open","high","low","close","volume","adjusted")
    eachstock$id <- a
    closelag <- as.numeric(lag(eachstock$close,1))
    closelag[is.na(closelag)] <- 0
    close <- as.numeric(eachstock$close)
    return <- log(close)-log(closelag)
    date <- as.Date(index(eachstock))
    eachstock <- as.data.frame(eachstock)
    eachstock$date <- date
    eachstock$return <- return
    stockslist[[b]] <- eachstock
    allstocks <- rbind(allstocks,stockslist[[b]])
    print(paste("Got", a))
    stockno <- append(stockno, a)
  }
}

write.csv(stockno, "/Users/ystar/Desktop/backup/stockno.csv", row.names = F)
write.csv(allstocks, "/Users/ystar/Desktop/backup/allstocks.csv", row.names = F)

