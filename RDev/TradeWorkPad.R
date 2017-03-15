#Load Function Script
source("c:/proc/r/Trading/histFunc.r")
require(stringi)
require(quantmod)
require(PerformanceAnalytics)

#Get Stock Data
flush.console()
tdata <- read.table("c:/proc/prod/SymbolList.dat", sep = ",")
index(tdata)
tdata[index(tdata),1]
# Process each Symbol in list
for (tindx in index(tdata) ) {
  ticker <- tdata[tindx,1]  
  sdat <- getsdat(ticker, 500)
  qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)
}

ticker = "^DJI"
ticker = "DUST"


qurl <- paste("http://download.finance.yahoo.com/d/quotes.csv?s=", ticker, ",&f=sl1d1t1c1ohgvba&e=.csv", sep = "")
qdat <- read.table(qurl,header = FALSE, sep = ",")
colnames(qdat) <- c("Symbol", "Close", "Date","Time","Change","Open","High","Low","Volume","Bid","Ask")
# Clean up Quote data when not data retrieve (only N/A returned)
if (qdat$Date == "N/A") {
      qdat$Date = Sys.Date()
      qdat$Time = format(Sys.time(), "%r")
  }
if (is.numeric(qdat$Change) & is.numeric(qdat$Close)) {
  qdat$PctChg <- round(qdat[,"Change"]/(qdat[,"Close"]-qdat[,"Change"])*100 , 3) 
} else {
    qdat[,c("Close","Change","Bid","Ask","PctChg")] <- 0
  }

  
print(qdat[,c("Symbol", "Date", "Time", "Close", "Change","Bid","Ask","PctChg")])
qcdat <- qdat[,c("Date","Open", "High", "Low", "Close", "Volume")]
qcdat[,"Date"] <- as.character(as.Date(qcdat[,"Date"], "%m/%d/%Y"))






ticker = "DUST"

sdat <- getsdat(ticker, 500)
qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)
qdat
sdat[nrow(sdat)-2,1]
cdat <- as.table(sdat[nrow(sdat),])
cdate = paste("as of:", qdat$Date, qdat$Time)
cout = paste("Last", as.character(cdat$Close), 
             "  Chg:", as.character(cdat$Chg),
             "  %Chg:", as.character(cdat$PctChg),
             "  RSI:", as.character(cdat$RSI14),
             "  SAR%Close:", as.character(100-cdat$SARClPct),
             "  CMF:", as.character(cdat$CMF),
             "  HMA Stgth:", as.character(round(100-(cdat$Close/cdat$HMA*100), 2)))

print (qdat, sep="")

DaystoChart <- 200
startdt <- paste(as.character(Sys.Date() - DaystoChart),"/", sep="")
cd <- (sdat[startdt,])
par(mfrow=c(3,1))
par(mar=c(0,4,3,1))
#PRICE Price Stregth Chart
yrange<-range(c(cd$Close, cd$EMA20, cd$EMA50, cd$SAR ))
plot(cd$Close, main = ticker, type = "l", pch = NULL, ylim = yrange)
abline(v= as.numeric(strptime('2015-10-11', format = "%Y-%m-%d")), lwd = 1, col = 'red')
lines(cd$EMA20, col="Red" )
lines(cd$EMA50, col="green")
lines(cd$EMA100, col="purple" )
lines(cd$HMA, col="blue" )
points(cd$SAR, col="Red", pch=20)
mtext("EMA20", line=0, adj=.40, cex=.5, col="Red")
mtext("EMA50", line=0, adj=.46, cex=.5, col="green")
mtext("EMA100", line=0, adj=.52, cex=.5, col="purple")
mtext("HMA", line=0, adj=.60, cex=.5, col="blue")
mtext(cdate, line=3, adj=0, cex=.5, col="black")
cout = paste("Last", as.character(cdat$Close), 
             "  Chg:", as.character(cdat$Chg),
             "  %Chg:", as.character(cdat$PctChg))
mtext(cout, line=2, adj=0, cex=.5, col="black")
cout = paste("RSI:", as.character(cdat$RSI14), 
             "  SAR%Close:", as.character(round(100-cdat$SARClPct, 2)),
             "  CMF:", as.character(cdat$CMF),
             "  HMA Stgth:", as.character(round((cdat$Close/cdat$HMA*100)-100, 2)))
mtext(cout, line=1, adj=0, cex=.5, col="black")



par(mar=c(2,4,0,1))
plot(cd$MFI, main="", ylim=yrange)

par(mfg=c(3,1))



##### Test Position Checking #########
pdat <- read.table("c:/proc/prod/positions.csv", sep = ",", header = TRUE)
for (pindx in index(pdat) ) {
  ticker <- pdat[pindx,"Symbol"]  
  posType <- pdat[pindx,"Type"]  
  expiredt <- as.Date(pdat[pindx,"Expiry"], "%m/%d/%Y")  
  strikePrice <- pdat[pindx,"Strike"]
  
#  Optdat <- getOptionChain(ticker, expiredt)  
#ticker <- "PCLN"
#posType <- "call"
#expiredt <- as.Date("11/06/2015", "%m/%d/%Y")
#strikePrice <- 1465
#optID <- paste(ticker, format(expiredt,"%y%m%d"), ifelse(casefold(posType) == "call", "C", "P"), stri_pad_left(strikePrice*1000, width = 8, pad="0"), sep="") 
#Optdat$calls[optID,]
qdat <- getQuote(ticker, mType = "Quote", verbose = FALSE)  
qdat <- cbind(qdat, pdat[pindx,c("Type", "Expiry", "Strike", "OptionCost", "numOpts", "TotBasis", "PriceAtPurch")])
print(qdat)

}


pdat[pindx,"Symbol"]
qdat$Change*.50*pdat[pindx,"numOpts"]*100
qdat
strikePrice-pdat[pindx,"OptionCost"]




par(mfrow=c(3,2))
par(mfg=c(2,2))
plot(c(1,3), c(1,11), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")


rect(0,10.5,5.5,12, col = "red")
text (1.5,11, "BEAR TREND (PUT or SHORT)", cex = 2)
for (r in 1:10) {
  for (c in 1:2) {
    text (c, r, paste("x=",c," y=", r, sep=""), col = "black")
  }}
