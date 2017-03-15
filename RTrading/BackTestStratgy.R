#Load Function Script
#require(sqldf)
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)

#Get Stock Data
flush.console()

fileloc <- "c:/proc/prod/data/"
fileExt <- ".csv"

#pdf(file="c:/proc/prod/Trading.pdf", 7, 10, onefile=TRUE, paper="letter")

#Read in Ticker Symbol List
tdata <- read.table("c:/proc/prod/FullList.csv", sep = ",", header = TRUE, row.names = 1)

tdata[ticker,"TICKERNAME"]
# Process each Symbol in list
for (ticker in row.names(tdata) ) {
  ticname <- tdata[ticker,"TICKERNAME"]
  
ticker <- "^RUT"
ticname  <- "Russell 2000"

ticker <- "B"
ticname  <- "Barnes"

ticker <- "^VIX"
ticname  <- "VIX Volitility"


ticker <- "^GSPC"
ticname  <- "S&P 500"

ticker <- "IWM"


ticker <- "GLD"
ticname  <- "Gold EFT"


sdat <- getsdat(ticker, 1500)
sdat <- getsdat(ticker, 500)
qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)

vixdat <- getsdat('^VIX', 1500)

cor(sdat$Close, vixdat$close)
cor(sdat$Close, sdat$Open)

runMax
runMax(sdat$High, 7) - runMin(sdat$Low, 7)

sdat$High14  <- round(runMax(sdat[,"High"],14), 3)
sdat$Low14  <- round(runMin(sdat[,"Low"],14), 3)

sdat$StochHigh14  <- round(runMax(sdat[,"stochFastD14"],14), 3)
sdat$StochLow14  <- round(runMin(sdat[,"stochFastD14"],14), 3)

#Bearish Divergence (Price Higher/high, Stoch Lower/high)
MYDAT <- sdat[sdat$High14 > lag(sdat$High14,1) & sdat$StochHigh14 < lag(sdat$StochHigh14,1), c('Close', 'High14', 'Low14', 'StochHigh14', 'StochLow14')]
sdat[sdat$High14 > lag(sdat$High14,1) & sdat$StochHigh14 < lag(sdat$StochHigh14,1), c('Close', 'High14', 'Low14', 'StochHigh14', 'StochLow14')]
MYDAT <- sdat[sdat$Low14 < lag(sdat$Low14,1) & sdat$StochLow14 > lag(sdat$StochLow14,1), c('Close', 'High', 'High14', 'Low14', 'StochHigh14', 'StochLow14')]
sdat[sdat$Low14 < lag(sdat$Low14,1) & sdat$StochLow14 > lag(sdat$StochLow14,1), c('Close', 'High', 'High14', 'Low14', 'StochHigh14', 'StochLow14')]

#Bullish Stoch Crossover
bulltest <- sdat[sdat$stochFastD14 < 80
  & sdat$stochFastD14 >= sdat$stochSlowD14
   & lag(sdat$stochFastD14,1) < lag(sdat$stochSlowD14, 1)
#  & SMA(sdat$stochSlowD14, 2) <= sdat$stochSlowD14
  & sdat$Open < lag(sdat$Stch14.80,1)
#   & lag(sdat$stochSlowD14,1) < sdat$stochSlowD14
          , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]

#Bearish Stoch Crossover
Beartest <- sdat[sdat$stochFastD14 > 35
     & sdat$stochFastD14 <= sdat$stochSlowD14
     & lag(sdat$stochFastD14,1) > lag(sdat$stochSlowD14, 1)
     #  & SMA(sdat$stochSlowD14, 2) <= sdat$stochSlowD14
     & sdat$Open > lag(sdat$Stch14.20,1)
     #   & lag(sdat$stochSlowD14,1) < sdat$stochSlowD14
     , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]


sdat[lag(sdat$stochFastD14,1) <= lag(sdat$stochSlowD14, 1)  , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'StochHigh14', 'StochLow14')]
sdat[sdat$Open < lag(sdat$Stch14.80,1) , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]


ldat <- merge(bulltest, Beartest, join='inner')
ldat <- merge(bulltest, Beartest)
ldat

if(sdat[nrow(sdat)-10, "Longsig"] > 0) {
  mtext(TrdInd <- "Buy/Call/Bullish", line=0, adj=.75, cex=.75, col="darkgreen")
  mtext(format(sdat[nrow(sdat), "dcH" ], 2), line=-1, adj=.75, cex=.75, col="darkgreen")
  mtext(format(sdat[nrow(sdat), "dcM" ], 2), line=-1, adj=.85, cex=.75, col="darkgreen")
  mtext(format(sdat[nrow(sdat), "dcL" ], 2), line=-1, adj=.95, cex=.75, col="darkgreen")
}  
format(sdat[nrow(sdat), c("dcH", "dcM", "dcL") ], 2)

sdat[nrow(sdat), c("dcH", "dcM", "dcL") ]
ifelse(sdat[nrow(sdat), "Shortsig"] > 0 , "Bear/Short", "Wait")

sdat$dcH[nrow]

buildCharts(n=250)

.index(cd["2015-09-10"])

#Stocholic Fast/Slow Chart
par(mar=c(.5,4,.5,1))
yrange<-range(c(0, 1))
xrange <- range(.index(cd))
plot(cd$stochFastK , main="", ylim=yrange,  ylab = "Stoch Fast/Slow", xaxt="n")
lines(cd$stochFastD, col="blue" )
lines(cd$stochSlowD, col="red")
lines(cd$stochFastK, col="black" )

abline(h= .8, col="red")
abline(h= .2, col="green")
rect(xleft = xrange[1] , ybottom =.80, xright = xrange[2],   ytop = 1, density = 20, border = "transparent",col = "lightpink")
rect(xleft = xrange[1] , ybottom = 0, xright =xrange[2],   ytop = .2, density = 20,  border = "transparent", col = "olivedrab1")

#abline(v=.index(sdat), col="red")

#abline(v=.index(sdat["2015-11-10"]), col="red")
#abline(v= cd$Longsig, col = "green")
mtext("K%", line=0, adj=.40, cex=.5, col="black")
mtext("Fast", line=0, adj=.45, cex=.5, col="green")
mtext("Slow", line=0, adj=.50, cex=.5, col="red")

nrow(cd)
for (rec in 0:nrow(cd)) {
  l <- cd[rec]
}


#Test Text Box  
#  par(mfrow=c(3,2))
par(mar=c(.5,4,0,1))
par(mfg=c(4,1))
plot(c(1,3), c(1,11), xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")


rect(0,10.5,5.5,12, col = "red")
text (1.5,11, "BEAR TREND (PUT or SHORT)", cex = 1.5)
if(cd[nrow(cd),"HMA"] <  cd[nrow(cd),"Close"]) {
  text (1, 10, paste("HMA below Cur Price", sep=""), pos = 4, col = "black")
}


## Add BUY/SELL Indicator Lines
abline(v=.index(sdat["2015-11-13"]), col="red")
par(mfg=c(2,1))
abline(v=.index(sdat["2015-09-10"]), col="red")

abline(v = .inde, col = 'blue', lty = 3, lwd = 2)



cd.$Bull <- buysell(cd$Bull)

round((lag(sdat$MFIHMAMomt)-lag(sdat$MFIHMAMomt,3)) - (sdat$MFIHMAMomt-lag(sdat$MFIHMAMomt)), 3)
round((lag(sdat$CMFHMAMomt)-lag(sdat$CMFHMAMomt,3)) - (sdat$MFIHMAMomt-lag(sdat$CMFHMAMomt)), 3)
sdat$MFIHMAMomt-lag(sdat$MFIHMAMomt)
sdat$CMFHMAMomt-lag(sdat$CMFHMAMomt)
round((sdat$HMA -lag(sdat$HMA, 2))/lag(sdat$HMA, 2)*100, 3)
sdat$MFIHMAMomt
sdat$CMFHMAMomt
#sig <- Lag(ifelse((sdat$DIp >= lag(sdat$DIp,1) & sdat$DIp > sdat$DIn & 
#                    sdat$SAR < sdat$Close ), 1, 0))
#sdat$CMF >= (lag(sdat$CMF,1)+lag(sdat$CMF,2))/2 & sdat$CMF > .10
#	B  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) > 0 & sdat$SARClPct < 100)
#	sdat[B,"Long"] <- sdat[B,"Close"]
#	S  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) < 0 & sdat$SARClPct > 100)
#	sdat[S,"Short"] <- sdat[S,"Close"]


cd$Longsig <- Lag(ifelse((sdat$HMA <= sdat$Close & sdat$SAR < sdat$Close), .index(sdat), 0))
cd$Shortsig <- Lag(ifelse((sdat$HMA <= sdat$Close), .index(sdat),0))


ret <- ROC(sdat[,"Close"])*sig
ret <- ret[startdt]
eq <- exp(cumsum(ret))
plot(eq, main=paste("Strategy Return for:", ticker), xaxt="n" )
mtext(Return.annualized(ret), line=3, adj=.25, cex=.5, col="black")

# Step 5: Evaluate strategy performance

#table.Drawdowns(ret, top=10)

table.DownsideRisk(ret)
UpsidePotentialRatio(ret, MAR=0)
Return.annualized(ret)
charts.PerformanceSummary(ret, main=ticker)

sdat$Long  <- 0
sdat$Short <- 0

#Write to data file
filepre <- paste(fileloc, gsub("\\^","",ticker), sep="")

write.table(as.matrix(sdat)[NROW(sdat):1,,drop=FALSE], paste(filepre,fileExt, sep=""), 
            quote = FALSE, row.names = TRUE, sep = ",")
}
print("Process Complete")

