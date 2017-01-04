#R Library Packages
library(Amelia)
library(forecast)
library(ggplot2)
 #install.packages('devtools')
 #devtools::install_github("ellisp/forecastxgb-r-package/pkg")
 #install.packages('xgboost')
 #install.packages('forecastxgb')
library(forecastxgb)
library(hydroGOF)

#Implort DataSet
dataSetGoal1 <- read.csv("F:/R/BigD/8.16.csv",header=TRUE,sep=",",na.strings=c(""),quote = "")
sapply(dataSetGoal1,function(x) sum(is.na(x)))

#Plot Missing Values to Analyse data stability
missmap(dataSetGoal1, main = "Missing values vs observed")
#c(1,31,32,33,34,35,36,37,38))# select columns based on missing values
#c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38))# select columns based on missing values
dataTrain <- subset(dataSetGoal1,select=c(1,26,27,28,29,30,31,32,33,34,35,36,37,38))# select columns based on missing values
names(dataTrain) <- substring(names(dataTrain), 2,5) #renaming names of columns

#Clean-up Script
#Use alternatingly with "Complete-Case Script"
################################################
#Replace NA's with 0
dataTrain[is.na(dataTrain)] <- 0
dataTrain$total <- with(dataTrain, rowSums(dataTrain[-1]))
#Remove 0 sum rows
dataTrainNoNulls<-dataTrain[!(dataTrain$total==0),]
dataTrainNoNullsMeans <- dataTrainNoNulls[-c(11)]
dataTrainNoNullsMeans[dataTrainNoNullsMeans == 0.0000000] <- NA
dataTrainNoNullsMeansMat <- as.matrix(dataTrainNoNullsMeans)
#Fill with RowMeans
ind <- which(is.na(dataTrainNoNullsMeansMat), arr.ind=TRUE)
dataTrainNoNullsMeansMat[ind] <- rowMeans(dataTrainNoNullsMeansMat[,-1],  na.rm = TRUE)[ind[,1]]
all(apply(dataTrainNoNullsMeansMat, 2, class) == "numeric") 
#Cleaned Data
dataTrain <- as.data.frame(dataTrainNoNullsMeansMat)
################################################

#Complete Case Script
dataTrain<- dataTrain[complete.cases(dataTrain), ]
dd <-   dataTrain[,c(-1,-10)]

#Generate Models
dFrame <- dataTrain$D
resultArima <- data.frame(Date=as.Date(character()),
                          File=character(), 
                          User=character(), 
                          stringsAsFactors=FALSE)
resultXg <- data.frame(Date=as.Date(character()),
                       File=character(), 
                       User=character(), 
                       stringsAsFactors=FALSE)
resultEts <- data.frame(Date=as.Date(character()),
                        File=character(), 
                        User=character(), 
                        stringsAsFactors=FALSE)
resultNnet <- data.frame(Date=as.Date(character()),
                         File=character(), 
                         User=character(), 
                         stringsAsFactors=FALSE)
#Run Models
for(i in 1:nrow(dd)){
  
  newData <- t(dataTrain[i,-1])
  
  mm1 <- as.matrix(dd[i,])
  mm2 <- matrix(mm1, ncol = ncol(newData), dimnames = NULL)
  #Change Start
  myts <- ts(mm2, start=c(1995), end=c(2006), frequency=1)
  #print(myts)
  fitArima <- auto.arima(myts)
  fcArima<-forecast(fitArima, 6)
  dfArima<-as.data.frame(t(fcArima$mean))
  resultArima<-rbind(resultArima,dfArima)
  
  fitXg <- xgbar(myts,nfold= 10)
  fcXg <- forecast(fitXg, h = 6)
  dfXg<-as.data.frame(t(fcXg$mean))
  resultXg<-rbind(resultXg,dfXg)
  
  
  fitEts <- ets(myts)
  fcEts<-forecast(fitEts,h=6)
  dfEts<-as.data.frame(t(fcEts$mean))
  resultEts<-rbind(resultEts,dfEts)
  
  fitNnet <- nnetar(myts)
  fcNnet<-forecast(fitNnet,h=6)
  dfNnet<-as.data.frame(t(fcNnet$mean))
  resultNnet<-rbind(resultNnet,dfNnet)
}

#plot(fc)
#Accumulate the Result
#Calulate the Model RMSE
#1.ARIMA
ddArima<-cbind(dFrame,resultArima)
colnames(ddArima)[1:7] <- c("ID","2007","2008","2009","2010","2011","2012")
#write.csv(ddtemp,file="/Users/Downloads/shobhikapanda/Gaal1.csv",append=T,row.names = F)
write.csv(ddArima, file = "F:/R/BigD/Goal1Model1.csv")
# require(hydroGOF)
rmse(dataTrain["2007"], ddArima["2007"], na.rm=TRUE)
write("RMSE ARIMA Model:",file="F:/R/BigD/Goal1Model1.csv",append=TRUE)
write(rmse(dataTrain["2007"], ddArima["2007"], na.rm=TRUE),file="F:/R/BigD/Goal1Model1.csv",append=TRUE)

#2. XGBoost
ddXg<-cbind(dFrame,resultXg)
colnames(ddXg)[1:7] <- c("ID","2007","2008","2009","2010","2011","2012")
#write.csv(ddtemp,file="/Users/Downloads/shobhikapanda/Gaal1.csv",append=T,row.names = F)
write.csv(ddXg, file = "F:/R/BigD/Goal1Model2.csv")
rmse(dataTrain["2007"], ddXg["2007"], na.rm=TRUE)
write("RMSE DDXG Model:",file="F:/R/BigD/Goal1Model2.csv",append=TRUE)
write(rmse(dataTrain["2007"], ddXg["2007"], na.rm=TRUE),file="F:/R/BigD/Goal1Model2.csv",append=TRUE)

#3. ETS:Smoothing
ddEts<-cbind(dFrame,resultEts)
colnames(ddEts)[1:7] <- c("ID","2007","2008","2009","2010","2011","2012")
#write.csv(ddtemp,file="/Users/Downloads/shobhikapanda/Gaal1.csv",append=T,row.names = F)
write.csv(ddEts, file = "F:/R/BigD/Goal1Model3.csv")
rmse(dataTrain["2007"], ddEts["2007"], na.rm=TRUE)
write("RMSE ddEts Model:",file="F:/R/BigD/Goal1Model3.csv",append=TRUE)
write(rmse(dataTrain["2007"], ddEts["2007"], na.rm=TRUE),file="F:/R/BigD/Goal1Model3.csv",append=TRUE)

#4. Neural Net for TS Object
ddNnet<-cbind(dFrame,resultNnet)
colnames(ddNnet)[1:7] <- c("ID","2007","2008","2009","2010","2011","2012")
#write.csv(ddtemp,file="/Users/Downloads/shobhikapanda/Gaal1.csv",append=T,row.names = F)
write.csv(ddNnet, file = "F:/R/BigD/Goal1Model4.csv")
rmse(dataTrain["2007"], ddNnet["2007"], na.rm=TRUE)
write("RMSE ddNnet Model:",file="F:/R/BigD/Goal1Model4.csv",append=TRUE)
write(rmse(dataTrain["2007"], ddNnet["2007"], na.rm=TRUE),file="F:/R/BigD/Goal1Model4.csv",append=TRUE)

#RMSE Plots Script
rmseDat <- c(0.04438501, 
          0.02529083,
          0.04097916,
          0.08437936)
plot(rmseDat, type="o", col="blue",axes=FALSE, ann=FALSE)
# Make x axis using labels
axis(1, at=1:4, lab=c("ARIMA","XGPLOT","ETS","NNET"))

# Make y axis with horizontal labels that display ticks at every mark
axis(2, las=1, axes=TRUE)

# Label the x and y axes with dark green text
title(xlab="Learning Models", col.lab=rgb(0,0.5,0))
title(ylab="RMSE", col.lab=rgb(0,0.5,0))
