
#Load Function Script
source("c:/proc/r/Trading/histFunc.r")
require(quantmod)
require(PerformanceAnalytics)

#Get Stock Data
flush.console()

fileloc <- "c:/proc/prod/data/"
fileExt <- ".csv"
pdf(file="c:/proc/prod/Trading.pdf", 7, 10, onefile=TRUE, paper="letter")
#Read in Ticker Symbol List
tdata <- read.table("c:/proc/prod/SymbolList.dat", sep = ",")

# Process each Symbol in list
for (ticker in tdata[,1]) {
  
  sdat <- getsdat(ticker, 200)
  cdat <- sdat[,c("Open","High", "Low", "Close","Volume")]
#sdata <- getSymbols(ticker)
  

chartSeries(cdat, type="line")
dropTA('Vo')
zoomChart("last 300 days")
addEMA(20, col="red")
addEMA(50, col="blue")
addEMA(100, col="orange")
addSAR(col="red")
addCMF()
addADX()
addRSI()
addWPR()
addZLEMA(50, col="yellow")

}

dev.flush()
