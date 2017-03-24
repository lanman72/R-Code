#Load Function Script
source("c:/proc/r/Trading/TradeFuncNew.r")
require(quantmod)
require(PerformanceAnalytics)

#Get Stock Data
flush.console()



#Get Data
datSP500 <- getsdat('^GSPC', 1000) 
datSPY <- getsdat('SPY', 1000)
datGLD <- getsdat('GLD', 1000)
datTLT <- getsdat('TLT', 1000)
datUUP <- getsdat('UUP', 1000)
datFAS <- getsdat('FAS', 1000)
datVIX <- getsdat('^VIX', 1000)

datGLD["2017-03/", c('Open', 'High', 'Low', 'Close')]
data.frame(rollapply(datGLD$High,20,FUN = "max") - rollapply(datGLD$High,14,FUN = "max"), 
rollapply(datGLD$Low,20,FUN = "min") - rollapply(datGLD$Low,14,FUN = "min"),
(datGLD$High+datGLD$Low)/2,
rollapply(datGLD$Close, 2, FUN = "mean"),
((datGLD$High+datGLD$Low)/2) / rollapply(datGLD$Open+datGLD$Close, 2, FUN = "mean"),
datGLD$High, datGLD$Low)

rollapply(100 * (datGLD$Close - rollapply(datGLD$Low,14,FUN = "min")) / (rollapply(datGLD$High,14,FUN = "max") - rollapply(datGLD$Low,14,FUN = "min") ), 3,"mean") -
rollapply(rollapply(100 * (datGLD$Close - rollapply(datGLD$Low,14,FUN = "min")) / (rollapply(datGLD$High,14,FUN = "max") - rollapply(datGLD$Low,14,FUN = "min") ), 3,"mean"), 3, "mean")

n = 14*3*5

lan <- data.frame(n,
rollapply(datGLD$Low,n,FUN = "min") + ((rollapply(datGLD$High,n,FUN = "max") - rollapply(datGLD$Low,n,FUN = "min")) * .75), 
rollapply(datGLD$Low,n,FUN = "min") + ((rollapply(datGLD$High,n,FUN = "max") - rollapply(datGLD$Low,n,FUN = "min")) * .25), 
datGLD$Low, datGLD$Close, datGLD$Close - Lag(datGLD$Close))


rollapply(datGLD$Low,n,FUN = "min") + ((rollapply(datGLD$High,n,FUN = "max") - rollapply(datGLD$Low,n,FUN = "min")) * .80)

rollmean(datGLD$High, 5) - rollmean(datGLD$High, 14)   
rollmean(datGLD$Low, 5) - rollmean(datGLD$Low, 14)   
rollSFM(datSP500$Close, datTLT$Close, 14)



datSP500[datVIX$Longsig != 0, c("Open", "Close") ]



datSP500[datSP500$Longsig != 0, c("Open", "Close") ]
datSP500[datSP500$Longsig != 0 | datSP500$Shortsig != 0, c("Open", "Close", "Longsig", "Shortsig") ]
datSP500[(datSP500$Longsig != 0 & datGLD$Shortsig !=0) | (datSP500$Shortsig != 0 & datGLD$Longsig !=0), c("Open", "Close", "Longsig", "Shortsig") ]
merge(datSP500[(datSP500$Longsig != 0 & datTLT$Shortsig !=0) | (datSP500$Shortsig != 0 & datTLT$Longsig !=0), c("Open", "Close", "Longsig", "Shortsig") ], 
      datSPY[datSPY$Longsig != 0 | datSPY$Shortsig != 0, c("Open", "Close", "Longsig", "Shortsig") ])

merge(datTLT[(datTLT$Longsig != 0 & datGLD$Longsig !=0) | (datTLT$Shortsig != 0 & datGLD$Shortsig !=0), c("Open", "Close", "Longsig", "Shortsig") ], 
      datGLD[datGLD$Longsig != 0 | datGLD$Shortsig != 0, c("Open", "Close", "Longsig", "Shortsig") ])

qdat <- getQuote("^VIX", mType="Quote", verbose = TRUE)

  
ticker <- "VIX"

ticname <- "VIX Index"

chartHdr <- paste("VIX"," - ", "VIX INDEX", sep = "")
buildCharts(datVIX, paste(qdat$Date, qdat$Time), chartHdr, n = 500)

rm(outdata)

for (ticker in row.names(tdata) ) {
  ticname <- tdata[ticker,"TICKERNAME"]
  qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)
  ifelse(exists("outdata"), outdata <- rbind(outdata, qdat), outdata <- qdat)
}


mtext(chartHdr, line=2, adj=.5, cex=.5, col="black")
sdat <- datFAS



