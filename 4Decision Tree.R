rm(list=ls(all=TRUE))

if (!require(rpart)) install.packages('rpart')
library(rpart)

times <- 10
maxdepth <- 10

#load stockmining data
data1 <- read.csv("/Users/ystar/Desktop/0617/financialdata.csv")
data2 <- read.csv("/Users/ystar/Desktop/backup/markdata.csv")
stockmining <- merge(data1, data2, by="id")

opar<- par()
correct <- vector()
result <- list()
eachresult <- data.frame()
for (k in 3:maxdepth)
{
  for (i in 1:times)
  {
    n <- paste0("depth:", k)
    eachresult[i, "times"] <- i
    np = ceiling(0.1*nrow(stockmining))
    np
    
    test.index = sample(1:nrow(stockmining),np)
    
    stockmining.testdata  = stockmining[test.index,]
    stockmining.traindata = stockmining[-test.index,]
    
    stockmining.tree = rpart(mark ~  A1 + A2 + A3 + A4 + B1 + B2 + B3 + C1 + C2 + D1 + D2 + E1 + E2 + F1 + G1 + G2 + G3 + G4, method="class",  data=stockmining.traindata
                      ,control=rpart.control(minsplit=1, cp=0.0001, 
                          maxdepth=k) )
    
    #stockmining.tree
    #summary(stockmining.tree)

    par(mar=rep(0.1,4))
    jpgname <- paste0("depth", k, "repeat", i,".jpg") 
    jpeg(file=jpgname)
    plot(stockmining.tree, main=paste("test", i)) ; text(stockmining.tree)
    dev.off()
    
    #show out-sample correct rate####
    mark.traindata = stockmining$mark[-test.index]
    train.predict=factor(predict(stockmining.tree, stockmining.traindata,
                                 type='class'), levels=levels(mark.traindata))
    
    table.traindata =table(mark.traindata,train.predict)
    table.traindata
    correct.traindata=sum(diag(table.traindata))/sum(table.traindata)*100
    eachresult[i,"out-sample"] <- as.numeric(correct.traindata)
    
    #show in-sample correct rate####
    mark.testdata = stockmining$mark[test.index]
    test.predict=factor(predict(stockmining.tree, stockmining.testdata,
                                type='class'), levels=levels(mark.testdata)) 
    table.testdata  =table(mark.testdata,test.predict)
    table.testdata
    correct.testdata=sum(diag(table.testdata))/sum(table.testdata)*100
    eachresult[i,"in-sample"] <- as.numeric(correct.testdata)
  }
  eachresult[i+1,] <- c("average", mean(as.numeric(eachresult[1:i,2])),mean(as.numeric(eachresult[1:i,3])))
  result[[n]] <- eachresult
  correct <- append(correct, mean(as.numeric(eachresult[1:i,2])))
  
}
#write.csv(result,"/Users/ystar/Desktop/0617/result0.csv",row.names=F)
par(opar)
klist <- c(3:maxdepth)
jpeg(file="depth and correct rate")
plot(klist,correct,main = "Depth and Correct Rate", 
     xlab = "depth", ylab = "correct rate", 
     xlim = c(3,maxdepth), ylim=c(75,100))
dev.off()
final <- rbind (klist, correct)
write.csv(final,"/Users/ystar/Desktop/0617/miningfinal.csv",col.names=F)
write.csv()
getwd()
