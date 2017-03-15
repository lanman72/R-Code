#Load Function Script
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)

ticker <- "^VIX"
ticname <- "SP500"
sdat <- getsdat(ticker, 1500)
qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)

ticker <- "^GSPC"
ticname <- "SP500"
sdat <- getsdat(ticker, 1500)
qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)







testdata <- sdat[,c("Open", "High", "Low", "Close")]
stochtmp <- stoch(testdata[,c("High","Low","Close")], nFastK = 14, nFastD = 3, nSlowD = 3)
stochtmpH <- stoch(testdata[,c("High", "Low", "High")], nFastK = 14, nFastD = 3, nSlowD = 3)
stochtmpL <- stoch(testdata[,c("High", "Low", "Low")], nFastK = 14, nFastD = 3, nSlowD = 3)



testdata$fastK <- stochtmp$fastK
testdata$fastD <- stochtmp$fastD
testdata$slowD <- stochtmp$slowD

testdata$HfastK <- stochtmpH$fastK
testdata$HfastD <- stochtmpH$fastD
testdata$HslowD <- stochtmpH$slowD

testdata$LfastK <- stochtmpL$fastK
testdata$LfastD <- stochtmpL$fastD
testdata$LslowD <- stochtmpL$slowD

testdata$Stch71.80 <- round(runMin(testdata[,"Low"],13), 3) + ( (round(runMax(testdata[,"High"],13), 3) - round(runMin(testdata[,"Low"],13), 3)) * lag(testdata$HfastD,1) )
testdata$Stch71.20 <- round(runMin(testdata[,"Low"],13), 3) + ( (round(runMax(testdata[,"High"],13), 3) - round(runMin(testdata[,"Low"],13), 3)) * lag(testdata$LfastD,1) )




write.table(as.matrix(testdata)[NROW(testdata):1,,drop=FALSE], "/proc/prod/data/stochtest.csv", 
            quote = FALSE, row.names = TRUE, sep = ",")

rptpds <- 0
startrow <- nrow(testdata)-100 - rptpds
endrow <- nrow(testdata)-rptpds
testdata <- testdata[startrow:endrow, ]

testdata$Stch71.20
buildCharts(n = 1000)



testdata <- sdat[startdt,]


par(mfrow=c(3,1))
yrange<-range(c(0, 1))
plot(testdata$fastD,  ylim=yrange)
lines(testdata$slowD, col="blue")
lines(lag(testdata$HfastD,1), col = "red")
lines(lag(testdata$LfastD,1), col = "green")

#abline(h=testdata[nrow(testdata)-5, "HfastD"])
#abline(h=testdata[nrow(testdata)-5, "LfastD"])

yrange<-range(c(testdata$Close, testdata$Stch71.80, testdata$Stch71.20 ))
plot(testdata$Close,  ylim=yrange)
lines(lag(testdata$Stch71.80,1), col="red")
lines(lag(testdata$Stch71.20,1), col="green")




#Monthly (14, 3,3) Stoch Oscilator
stochOSC <- stoch(sdat[,c("High","Low","Close")], nFastK = 14, nFastD = 3, nSlowD = 3)
sdat$stochFastK280 <- round(stochOSC$fastK, 6)*100
sdat$stochFastD280 <- round(stochOSC$fastD, 6)*100
sdat$stochSlowD280 <- round(stochOSC$slowD, 6)*100



#sdat$High14  <- round(runMax(sdat[,"High"],14), 3)
#sdat$Low14  <- round(runMin(sdat[,"Low"],14), 3)

merge(ATR(sdat[, c('High', 'Low', 'Close')], n = 10)$atr, 
      ATR(sdat[, c('High', 'Low', 'Close')], n = 14)$atr, 
      ATR(sdat[, c('High', 'Low', 'Close')], n = 71)$atr,
      runSD(ATR(sdat[, c('High', 'Low', 'Close')], n = 10)$tr, n = 10) )

sdat$StochHigh14  <- round(runMax(sdat[,"stochFastD14"],14), 3)
sdat$StochLow14  <- round(runMin(sdat[,"stochFastD14"],14), 3)
sdat$TrdRng.14D <- sdat$High14 - sdat$Low14

#Bullish Stoch Crossover
Bulltest1 <- sdat[lag(sdat$stochFastD14, 0) < 80
                 & lag(sdat$stochFastD14, 0) >= lag(sdat$stochSlowD14, 0)
                 & lag(sdat$stochFastD14, 1) < Lag(sdat$stochSlowD14, 1)
                 # & SMA(sdat$stochSlowD14, 2) <= sdat$stochSlowD14
                 #& sdat$Open < Lag(sdat$Stch14.80, 1)
                 #& sdat$Open >= Lag(sdat$Stch14.20, 1)
                 #  & lag(sdat$stochSlowD14,1) < sdat$stochSlowD14
                 , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'TrdRng.14D', 'stochFastD14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]

#Bearish Stoch Crossover
Beartest1 <- sdat[lag(sdat$stochFastD14, 0) > 20
                 & lag(sdat$stochFastD14, 0) <= lag(sdat$stochSlowD14, 0)
                 & lag(sdat$stochFastD14, 1) > Lag(sdat$stochSlowD14, 1)
                 #  & SMA(sdat$stochSlowD14, 2) <= sdat$stochSlowD14
                 #& sdat$Open >= Lag(sdat$Stch14.20, 1)
                 & sdat$Open <= Lag(sdat$Stch14.80, 1)
                 # & lag(sdat$stochSlowD14,1) < sdat$stochSlowD14
                 , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'TrdRng.14D', 'stochFastD14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]


Bulltest <- sdat[sdat$Low14 < Lag(sdat$Low14, 1) & sdat$StochLow14 >= Lag(sdat$StochLow14, 1)
      , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'TrdRng.14D', 'stochFastD14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]
Beartest <- sdat[sdat$High14 < Lag(sdat$High14, 1) & sdat$StochHigh14 <= Lag(sdat$StochHigh14,1)
      , c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'TrdRng.14D', 'stochFastD14', 'StochHigh14', 'StochLow14', 'Stch14.80', 'Stch14.20')]

#plot(cd$stochFastD14)
#lines(cd$stochFastD71)
#lines(EMA(cd$stochFastD71, n = 7), col = 'blue')
#lines(EMA(cd$stochSlowD71, n = 7), col = 'blue')

buildCharts()

rm(lr)
lr <- sdat["2016",]

lr$CloseStoch <- (lr$Close-lr$Low)/(lr$High-lr$Low)
lr$OpenStoch <- (lr$Open-lr$Low)/(lr$High-lr$Low)
lr$OpenN <- lag(lr$Open,-1)
lr$CloseN <- lag(lr$Close,-1)
lr$LowN <- lag(lr$Low,-1)
lr$HighN <- lag(lr$High, -1)

Bulltest1[, c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'TrdRng.14D')]
Beartest1[, c('Open', 'High','Low', 'Close', 'High14', 'Low14', 'TrdRng.14D')]
stoch(lr[,c("High","Low","Close")], nFastK = 2, nFastD = 3, nSlowD = 3)$fastD
stoch(lr[,c("High","Low","Open")], nFastK = 2, nFastD = 3, nSlowD = 3)$fastK

#Closed Lower, Closed Closer to Low
wd <- lr[lr$Open > lr$Close & (lr$High - lr$Close) > (lr$Close - lr$Low)  , c('Open', 'Close', 'High', 'Low', 'CloseStoch', 'OpenStoch', 'OpenN', 'LowN', 'HighN')]
wd$ChangN <- round((((wd$HighN - wd$LowN) *.8) + wd$LowN - wd$Close)/wd$Close*100, 3)
wd
wd$High
print (wd$HighN - wd$LowN) + wd$LowN
wd[wd$OpenStoch > .90, ]

#Closed Lower, Close closer to High
wd <- lr[lr$Open > lr$Close & (lr$High - lr$Close) < (lr$Close - lr$Low)  , c('Open', 'Close', 'High', 'Low', 'CloseStoch', 'OpenStoch', 'OpenN', 'LowN', 'HighN')]
wd$ChangN <- round((wd$LowN - wd$Close)/wd$Close*100, 3)
wd
wd[wd$OpenStoch > .90, ]

#Closed Higher, Closed Closer to Low
wd <- lr[lr$Open < lr$Close & (lr$High - lr$Close) > (lr$Close - lr$Low)  ,  c('Open', 'Close', 'High', 'Low', 'CloseStoch', 'OpenStoch', 'OpenN', 'LowN', 'HighN')]
wd$ChangN <- round((wd$HighN - wd$Close)/wd$Close*100, 3)
wd
wd[wd$OpenStoch < .20, ]

#Closed Higher, Close closer to High
wd <- lr[lr$Open < lr$Close & (lr$High - lr$Close) < (lr$Close - lr$Low)  , c('Open', 'Close', 'High', 'Low', 'CloseStoch', 'OpenStoch', 'OpenN', 'LowN', 'HighN')]
wd$ChangN <- round((wd$HighN - wd$Close)/wd$Close*100, 3)
wd
wd[wd$OpenStoch < .20, ]



lr$Open/lr$Close

wd$CloseStoch <- (wd$Close-wd$Low)/(wd$High-wd$Low)
wd$OpenStoch <- (wd$Open-wd$Low)/(wd$High-wd$Low)
wd$OpenN <- lag(wd$Open,-1)
wd$LowN <- lag(wd$Low,-1)
wd$ChangN <- round((wd$LowN - wd$Close)/wd$Close*100, 3)
wd
wd[wd$OpenStoch > .90, ]
c( (lr$Close-lr$Low)/(lr$High-lr$Low), (lr$Open-lr$Low)/(lr$High-lr$Low) )

c((lr$High - lr$Open)/ lr$Open, (lr$Open - lr$Low)/ lr$Open ) 

sdat[sdat$Open >= lag(sdat$Close, 1) & lag(sdat$Open,1) > lag(sdat$Close, 1), c('Open', 'Close', 'High', 'Low')]

ATR(sdat[, c('High', 'Low', 'Close')], n = 7)

merge(Bulltest1, Beartest1, join='inner')

ldat <- merge(Bulltest1, Beartest1, suffixes = c("LG", "SH"))
print(ldat[, c('Open', 'Close', 'Open.1', 'Close.1')])
cd <- sdat[paste(index(ldat[nrow(ldat)-15, ])-2,"/" ), ]


par(mfrow=c(2,1))
par(mar=c(0,4,3,1))
ticname <- "VIX Volatility"
xrange <- range(.index(cd))
#PRICE Price Stregth Chart
yrange<-range(c(cd$Close, cd$EMA20, cd$EMA50, cd$SAR, cd$Stch14.80, cd$Stch14.20, cd$High, cd$Low ))
plot(cd$Close, main=paste(ticker," - ", ticname, sep = ""), ylim=yrange, ylab = "Price", xaxt="s")

abline(v= .index(ldat), col = ifelse(is.na(ldat$Close), "Red", "Green"))

lines(lag(cd$Stch14.80, 0), col="darkred", lwd = 1.5)
lines(cd$Stch14.20, col="darkgreen", lwd = 1.5)
rect(.index(cd)-1500, cd$High, .index(cd)+1500, cd$Low, border = NA, col = "black")
rect(.index(cd)-15000, cd$Open, .index(cd)+15000, cd$Close, border = NA, col = ifelse(cd$Open <= cd$Close, 'green', "red"))

#abline(v= .index(Bulltest1), col = 'blue', lwd = .75)
#abline(v= .index(Beartest1), col = 'orange', lwd = .75)


yrange<-range(c(0, 100))

plot(cd$ADX, main=NULL, ylim=yrange, ylab = "ADX", xaxt="s")

abline(v= .index(ldat), col = ifelse(is.na(ldat$Close), "Red", "Green"))



lines(cd$DIp, col="darkgreen", lwd=2)
lines(cd$DIn, col="darkred", lwd=2)
lines(cd$DX, col="black", lwd=2)

#lines(cd$MFI.HMA, col="purple", lwd=1)
#axis(4, at=c(0, 25,37.5, 50, 62.5, 75, 100), 
#     labels=c(-1.00,-.50,-.25, 0,.25, .50, 1.00), col = "royalblue")

abline(h=c(50))
abline(h=c(25,75), col="blue")

mtext("RSI14", line=-2, adj=.40, cex=.5, col="black")
mtext("CMF", line=-2,  adj=.50, cex=.5, col="royalblue")
mtext("MFI", line=-2,  adj=.60, cex=.5, col="orange")

ldat[, c('Open', 'High','Low', 'Close', 'TrdRng.14D', 'Stch14.80', 'Stch14.20', 'Open.1', 'High.1','Low.1', 'Close.1', 'TrdRng.14D.1', 'Stch14.80.1', 'Stch14.20.1')]

ldat[nrow(ldat), c("Close", "Close.1")]

#n <- min(.index(cd))

#t <- (max(.index(cd)-min(.index(cd)))) / 86400
#for(i in 1:t ) {
#    n <- n + 86400
#    abline(v = n, lty = "dotted")
#}
#abline(v= (min(.index(cd)):max(.index(cd))))

