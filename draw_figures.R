##################################################
#
# This file provides how to draw figures 
#
##################################################


#################################################################################
###
### Figure1:  Market excess return forecasts based on long-leg anomaly returns
###
#################################################################################

rm(list=ls())

Forecast_long <- read.csv("./Forecast/forecastLongHorizon01All.csv")

Forecast_benchmark <- read.csv("./Forecast/forecastPm.csv")

long <- as.matrix(cbind(Forecast_long, Forecast_benchmark[,2]))
colnames(long)[8] <- "Benchmark"

graphics.off()
library("ggplot2")
par(mfrow=c(3,2))

long1<- cbind(long[,2],long[,8])

long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return")
title('OLS')
legend("topright",c("OLS","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 


long1<- cbind(long[,3],long[,8])

long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-12,12))
title('Enet')
legend("topright",c("Enet","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 


long1<- cbind(long[,4],long[,8])

long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('Combine')
legend("topright",c("Combine","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 


long1<- cbind(long[,5],long[,8])

long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('Avg')
legend("topright",c("Avg","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 

long1<- cbind(long[,6],long[,8])

long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('PC')
legend("topright",c("PC","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 


long1<- cbind(long[,7],long[,8])

long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('PLS')
legend("topright",c("PLS","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 




#############################################################################################
###
### Figure2:  Market excess return forecasts based on short-leg anomaly returns
###
#############################################################################################
rm(list=ls())
Forecast_short <- read.csv("./Forecast/forecastShortHorizon01All.csv")

Forecast_benchmark <- read.csv("./Forecast/forecastPm.csv")

short <- as.matrix(cbind(Forecast_short, Forecast_benchmark[,2]))
colnames(short)[8] <- "Benchmark"


graphics.off()
library("ggplot2")
par(mfrow=c(3,2))

short1<- cbind(short[,2],short[,8])

short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return")
title('OLS')
legend("topright",c("OLS","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 



short1<- cbind(short[,3],short[,8])

short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-14,14))
title('Enet')
legend("topright",c("Enet","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 



short1<- cbind(short[,4],short[,8])

short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('Combine')
legend("topright",c("Combine","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 


short1<- cbind(short[,5],short[,8])

short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('Avg')
legend("topright",c("Avg","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 


short1<- cbind(short[,6],short[,8])

short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('PC')
legend("topright",c("PC","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 



short1<- cbind(short[,7],short[,8])

short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2),lwd=3,col=c('royalblue4','gold2'),ylab = "Market excess return",ylim=c(-6,6))
title('PLS')
legend("topright",c("PLS","Benchmark"),lty =c(1,2),lwd=2,col=c('royalblue4','gold2'),cex = 0.7) 




#############################################################################################
###
###  Figure3: Log cumulative excess returns for portfolios constructed using long-leg anomaly returns
###
#############################################################################################
rm(list=ls())
long <- read.csv("./Table/tablePortfolioRxLongAll.csv")

numeric_columns <- sapply(long, is.numeric)
long_cumsum <- apply(long[, numeric_columns], 2, cumsum)
long_cumsum <- cbind(long[ , !numeric_columns], long_cumsum)


graphics.off()
library("ggplot2")
par(mfrow=c(3,2))

long1<- cbind(long_cumsum[,2],long_cumsum[,3],long_cumsum[,4])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('OLS')
legend("topleft",c("Mkt","Benchmark","OLS"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


long1<- cbind(long_cumsum[,2],long_cumsum[,3],long_cumsum[,5])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Enet')
legend("topleft",c("Mkt","Benchmark","Enet"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


long1<- cbind(long_cumsum[,2],long_cumsum[,3],long_cumsum[,6])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Combine')
legend("topleft",c("Mkt","Benchmark","Combine"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


long1<- cbind(long_cumsum[,2],long_cumsum[,3],long_cumsum[,7])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Avg')
legend("topleft",c("Mkt","Benchmark","Avg"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


long1<- cbind(long_cumsum[,2],long_cumsum[,3],long_cumsum[,8])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('PC')
legend("topleft",c("Mkt","Benchmark","PC"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 



long1<- cbind(long_cumsum[,2],long_cumsum[,3],long_cumsum[,9])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('PLS')
legend("topleft",c("Mkt","Benchmark","PLS"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 



#############################################################################################
###
###  Figure4: Log cumulative excess returns for portfolios constructed using short-leg anomaly returns
###
#############################################################################################
rm(list=ls())
short <- read.csv("./Table/tablePortfolioRxShortAll.csv")

numeric_columns <- sapply(short, is.numeric)
short_cumsum <- apply(short[, numeric_columns], 2, cumsum)
short_cumsum <- cbind(short[ , !numeric_columns], short_cumsum)


graphics.off()
library("ggplot2")
par(mfrow=c(3,2))

short1<- cbind(short_cumsum[,2],short_cumsum[,3],short_cumsum[,4])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('OLS')
legend("topleft",c("Mkt","Benchmark","OLS"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


short1<- cbind(short_cumsum[,2],short_cumsum[,3],short_cumsum[,5])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Enet')
legend("topleft",c("Mkt","Benchmark","Enet"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


short1<- cbind(short_cumsum[,2],short_cumsum[,3],short_cumsum[,6])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Combine')
legend("topleft",c("Mkt","Benchmark","Combine"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


short1<- cbind(short_cumsum[,2],short_cumsum[,3],short_cumsum[,7])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Avg')
legend("topleft",c("Mkt","Benchmark","Avg"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


short1<- cbind(short_cumsum[,2],short_cumsum[,3],short_cumsum[,8])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('PC')
legend("topleft",c("Mkt","Benchmark","PC"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


short1<- cbind(short_cumsum[,2],short_cumsum[,3],short_cumsum[,9])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('PLS')
legend("topleft",c("Mkt","Benchmark","PLS"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 



#############################################################################################
###
###  Figure5: Log cumulative excess returns for portfolios constructed using long-short anomaly returns
###
#############################################################################################
rm(list=ls())
longshort <- read.csv("./Table/tablePortfolioRxLongShortAll.csv")

numeric_columns <- sapply(longshort, is.numeric)
longshort_cumsum <- apply(longshort[, numeric_columns], 2, cumsum)
longshort_cumsum <- cbind(longshort[ , !numeric_columns], longshort_cumsum)


graphics.off()
library("ggplot2")
par(mfrow=c(3,2))

longshort1<- cbind(longshort_cumsum[,2],longshort_cumsum[,3],longshort_cumsum[,4])
longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('OLS')
legend("topleft",c("Mkt","Benchmark","OLS"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


longshort1<- cbind(longshort_cumsum[,2],longshort_cumsum[,3],longshort_cumsum[,5])
longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Enet')
legend("topleft",c("Mkt","Benchmark","Enet"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


longshort1<- cbind(longshort_cumsum[,2],longshort_cumsum[,3],longshort_cumsum[,6])
longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Combine')
legend("topleft",c("Mkt","Benchmark","Combine"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


longshort1<- cbind(longshort_cumsum[,2],longshort_cumsum[,3],longshort_cumsum[,7])
longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('Avg')
legend("topleft",c("Mkt","Benchmark","Avg"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


longshort1<- cbind(longshort_cumsum[,2],longshort_cumsum[,3],longshort_cumsum[,8])
longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('PC')
legend("topleft",c("Mkt","Benchmark","PC"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 


longshort1<- cbind(longshort_cumsum[,2],longshort_cumsum[,3],longshort_cumsum[,9])
longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =1,lwd=3,col=c('gray50','skyblue3','royalblue4'),ylab = "Cumulative excess return")
title('PLS')
legend("topleft",c("Mkt","Benchmark","PLS"),lty =1,lwd=2,col=c('gray50','skyblue3','royalblue4'),cex = 0.7) 



#############################################################################################
###
###  Figure6: Slope Coefficients
###
#############################################################################################
rm(list=ls())
graphics.off()
library("ggplot2")
par(mfrow=c(3,2))

long_slope <- read.csv("./Table/tableLongRecursiveSlope.csv")
long_se <- read.csv("./Table/tableLongRecursivese.csv")

long <- cbind(long_slope,long_slope[,2]-1.65*long_se[,2],long_slope[,3]-1.65*long_se[,3],long_slope[,4]-1.65*long_se[,4],long_slope[,2]+1.65*long_se[,2],long_slope[,3]+1.65*long_se[,3],long_slope[,4]+1.65*long_se[,4])

long1<- cbind(long[,2],long[,5],long[,8])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2,2),lwd=2.7,col=c('royalblue4','skyblue3','skyblue3'),ylab = "Slope coefficient")
abline(h=0)
title('Avg')

long1<- cbind(long[,3],long[,6],long[,9])
long1<-ts(long1,start=2014,freq=12)
plot(long1,plot.type="single",lty =c(1,2,2),lwd=2.7,col=c('royalblue4','skyblue3','skyblue3'),ylab = "Slope coefficient")
abline(h=0)
title('PC')


short_slope <- read.csv("./Table/tableShortRecursiveSlope.csv")
short_se <- read.csv("./Table/tableShortRecursivese.csv")

short <- cbind(short_slope,short_slope[,2]-1.65*short_se[,2],short_slope[,3]-1.65*short_se[,3],short_slope[,4]-1.65*short_se[,4],short_slope[,2]+1.65*short_se[,2],short_slope[,3]+1.65*short_se[,3],short_slope[,4]+1.65*short_se[,4])

short1<- cbind(short[,2],short[,5],short[,8])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2,2),lwd=2.7,col=c('royalblue4','skyblue3','skyblue3'),ylab = "Slope coefficient")
abline(h=0)
title('Avg')

short1<- cbind(short[,3],short[,6],short[,9])
short1<-ts(short1,start=2014,freq=12)
plot(short1,plot.type="single",lty =c(1,2,2),lwd=2.7,col=c('royalblue4','skyblue3','skyblue3'),ylab = "Slope coefficient")
abline(h=0)
title('PC')


longshort_slope <- read.csv("./Table/tableLongShortRecursiveSlope.csv")
longshort_se <- read.csv("./Table/tableLongShortRecursivese.csv")

longshort <- cbind(longshort_slope,longshort_slope[,2]-1.65*longshort_se[,2],longshort_slope[,3]-1.65*longshort_se[,3],longshort_slope[,4]-1.65*longshort_se[,4],longshort_slope[,2]+1.65*longshort_se[,2],longshort_slope[,3]+1.65*longshort_se[,3],longshort_slope[,4]+1.65*longshort_se[,4])


longshort1<- cbind(longshort[,2],longshort[,5],longshort[,8])

longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =c(1,2,2),lwd=2.7,col=c('royalblue4','skyblue3','skyblue3'),ylab = "Slope coefficient")
abline(h=0)
title('Avg')

longshort1<- cbind(longshort[,3],longshort[,6],longshort[,9])

longshort1<-ts(longshort1,start=2014,freq=12)
plot(longshort1,plot.type="single",lty =c(1,2,2),lwd=2.7,col=c('royalblue4','skyblue3','skyblue3'),ylab = "Slope coefficient")
abline(h=0)
title('PC')









