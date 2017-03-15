library(TTR)
library(xts)
#library(sendmailR)
#library(mailR)


#emailme <- function(subtxt, btxt, attch = NULL) {
#		sender <- "@gmail.com"  # Replace with a valid address
#		recipients <- c("<@gmail.com>")  # Replace with one or more valid addresses
#		email <- send.mail(from = sender,
#           		       to = recipients,
#		                   subject=subtxt,
#           		       body = btxt,
#		                   smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "l@gmail.com", passwd = "", ssl = TRUE),
#           		       authenticate = TRUE,
#		                   attach.files = attch,
#		                   send = TRUE,
#           		       debug = FALSE)
#}
#
#textme <- function(subtxt, btxt, attch = NULL) {
#		sender <- "@gmail.com"  # Replace with a valid address
#		recipients <- c("<9991231234@vtext.com>")  # Replace with one or more valid addresses
#		email <- send.mail(from = sender,
#           		       to = recipients,
#		                   subject=subtxt,
#           		       body = btxt,
#		                   smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "l@gmail.com", passwd = "", ssl = TRUE),
#           		       authenticate = TRUE,
#		                   attach.files = attch,
#		                   send = TRUE,
#           		       debug = FALSE)
#}



# Get Quote data
getQuote <- function(symbol, mType="Hist", verbose = FALSE) {
  #qurl <- paste("http://real-chart.finance.yahoo.com/table.csv?s=", symbol, "&d=2&e=8&f=2016&g=d&a=1&b=5&c=1971&ignore=.csv", sep = "")
  qurl <- paste("http://download.finance.yahoo.com/d/quotes.csv?s=", symbol, ",&f=sl1d1t1c1ohgvba&e=.csv", sep = "")
  qdat <- read.table(qurl,header = FALSE, sep = ",")
  colnames(qdat) <- c("Symbol", "Close", "Date","Time","Change","Open","High","Low","Volume","Bid","Ask")
  # Clean up Quote data when not data retrieve (only N/A returned)
  if (qdat$Date == "N/A") {
    qdat$Date = Sys.Date()
    qdat$Time = format(Sys.time(), "%r")
  }
  if (qdat$Open == "N/A") {
    qdat$Open = qdat$Close + qdat$Change
  }      
  if (is.numeric(qdat$Change) & is.numeric(qdat$Close)) {
    qdat$PctChg <- round(qdat[,"Change"]/(qdat[,"Close"]-qdat[,"Change"])*100 , 3) 
  } else {
    qdat[,c("Close","Change","Open","High","Low","Volume", "Bid","Ask","PctChg")] <- 0
  }
  qdat
  if(verbose) print(qdat[,c("Symbol", "Date", "Time", "Close", "Change","Bid","Ask","PctChg")])
  qcdat <- qdat[,c("Date","Open", "High", "Low", "Close", "Volume")]
  qcdat[,"Date"] <- as.character(as.Date(qcdat[,"Date"], "%m/%d/%Y"))
  if(mType =="Quote") { return(qdat)} else {return(qcdat) }
}


getHistDat <- function(symbol, n) {
  hurl <- paste("http://ichart.finance.yahoo.com/table.csv?s=", symbol, "&ignore=.csv")
  hdat <- read.table(hurl, header = TRUE, sep = ",")
  adjFactor <- hdat[,"Adj.Close"] / hdat[,"Close"]
  hdat[, "Open"] <- hdat[, "Open"] * adjFactor
  hdat[, "High"] <- hdat[, "High"] * adjFactor
  hdat[, "Low"] <- hdat[, "Low"] * adjFactor
  hdat[, "Close"] <- hdat[, "Close"] * adjFactor
  hdat <- hdat[, c("Date", "Open", "High", "Low", "Close", "Volume")]
  return(hdat)
  
}

getQH <- function(symbol,n) {
  Histdat = getHistDat(symbol, 10)
  Qdat = getQuote(symbol,10)
  # Check for num of rows to return is not greater than data available
  if (nrow(Histdat) < n) {n <- nrow(Histdat)-1 }
  if (n==0) {n <- nrow(Histdat)-1 }
  
  Histdat = Histdat[1:n,]
  if (as.character(as.Date(Histdat[1,"Date"])) != Qdat[1,"Date"]) {
    Histdat <- rbind(Histdat,Qdat)
  }
  Histsdat <- Histdat[order(Histdat[,"Date"]),]
  Histdat <- xts(Histdat[, -1], as.Date(as.character(Histdat[,"Date"])))
}

MACDCalc <- function(x, short = 12, long = 26, sgnl = 9) {
  
  short.ema <- EMA(x[,1],n=short)
  long.ema <- EMA(x[,1],n=long)
  
  calc.macd <- short.ema[,"EMA"] - long.ema[,"EMA"]
  colnames(calc.macd)[1] = "MACD"
  
  calc.signal <- EMA(calc.macd[,"MACD"],n=sgnl)
  colnames(calc.signal)[1] = "SIGNAL"
  
  calc.diverg <- calc.macd[,"MACD"] - calc.signal[,"SIGNAL"]
  colnames(calc.diverg)[1] = "DIVERG"
  colnames(short.ema)[1] = paste("EMA", as.character(short))
  colnames(long.ema)[1] = paste("EMA", as.character(long))
  macdout <- merge(short.ema, long.ema, calc.macd,calc.signal, calc.diverg)
  
  return(macdout)
}

getsdat <- function(symbol, n=0) {
  print(paste("Getting History for: ", symbol))
  #Get History w/ Quote
  sdat <- getQH(symbol,n)
  sdat <- sdat[,1:5]
  #Format Open, High, Low, Close 
  sdat$Open <- round(sdat$Open, 3)
  sdat$High <- round(sdat$High, 3)   
  sdat$Low <- round(sdat$Low, 3)
  sdat$Close <- round(sdat$Close, 3)
  #daily Percent change
  sdat$Chg <- round(momentum(sdat[,"Close"], 1), 2)
  sdat$PctChg <- round(sdat$Chg/(sdat$Close-sdat$Chg)*100, 3)
  sdat$SD10Chg  <- round(runSD(sdat[,"Chg"],10), 3)
  sdat$MedPrice <- round((sdat$High + sdat$Low)/2, 3)
  sdat$OpenChg <- round(sdat$Open -sdat$Close + sdat$Chg, 3)
  
  #High/Low (5 and 10)
  sdat$High14  <- round(runMax(sdat[,"Close"],14), 3)
  sdat$High71  <- round(runMax(sdat[,"Close"],71), 3)
  sdat$Chg14DH  <- round(sdat$Close - sdat$High14, 3)
  sdat$Low14  <- round(runMin(sdat[,"Close"],14), 3)
  sdat$Low71  <- round(runMin(sdat[,"Close"],71), 3)
  sdat$Chg14DL  <- round(sdat$Close - sdat$Low14, 3)
  
  #StdDev (10,20,50,100)
  sdat$SD10  <- round(runSD(sdat[,"Close"],10), 3)
  sdat$SD20  <- round(runSD(sdat[,"Close"],20), 3)
  sdat$SD50  <- round(runSD(sdat[,"Close"],50), 3)
  sdat$SD100  <- round(runSD(sdat[,"Close"],100), 3)
  
  #EMAs (10,20,50,100)
  sdat$EMA10 <- round(EMA(sdat[,"Close"], 10), 3)
  sdat$EMA20 <- round(EMA(sdat[,"Close"], 20), 3)
  sdat$EMA50 <- round(EMA(sdat[,"Close"], 50), 3)
  sdat$EMA100 <- round(EMA(sdat[,"Close"], 100), 3)
  sdat$EMA100 <- round(EMA(sdat[,"Close"], 200), 3)
  
  #RSI14
  sdat$RSI14 <- round(RSI(sdat[,"Close"], n=14, maType="EMA"), 3)
  #Parabolic Stop-and-Reverse SAR
  sdat$SAR <- round(SAR(sdat[,c("High","Low")]), 2)
  sdat$SARClPct <- round(sdat$SAR / sdat$Close * 100, 3)
  sdat$SARChg <- round(momentum(sdat[,"SAR"], 1), 3)
  sdat$SARChg <- round((sdat$SARChg/(sdat$SAR - sdat$SARChg))*100, 3)
  #Calc MACD(12,26,9)
  macd  <- MACDCalc(sdat[,"Close"], 12, 26, 9)
  sdat$EMA.12 <- round(macd$EMA.12, 3) 
  sdat$EMA.26 <- round(macd$EMA.26, 3) 
  sdat$MACD <- round(macd$MACD, 3) 
  sdat$SIGNAL <- round(macd$SIGNAL, 3)
  sdat$DIVERG <- round(macd$DIVERG, 3)
  #Direct Movmt Ind
  dmi.adx <- ADX(sdat[,c("High","Low","Close")])
  sdat$DIp <- round(dmi.adx$DIp, 3)
  sdat$DIn <- round(dmi.adx$DIn, 3)
  sdat$DX <- round(dmi.adx$DX, 3)
  sdat$ADX <- round(dmi.adx$ADX, 3)
  sdat$DIp_Chg <- round(ROC(dmi.adx[,"DIp"]*100, 10), 3)
  #chaikinVolatility
  sdat$CVol <- round(chaikinVolatility(sdat[,c("High","Low")]), 3)
  # KST Known Sure Thing
  kst <- KST(sdat[,"Close"])
  sdat$KST <- round(kst$kst, 2)
  sdat$KSTSignal <- round(kst$signal, 2)
  sdat$HMA <- round(HMA(sdat$Close), 2)
  #Stoch
  #Daily (7,3,3) Stoch Oscilator
  stochOSC <- stoch(sdat[,c("High","Low","Close")], nFastK = 7, nFastD = 3, nSlowD = 3)
  sdat$stochFastK7 <- round(stochOSC$fastK, 6)*100
  sdat$stochFastD7 <- round(stochOSC$fastD, 6)*100
  sdat$stochSlowD7 <- round(stochOSC$slowD, 6)*100
  #Stoch Oscilator 80 - 20 range	
  sdat$Stch7.80 <- round(runMin(sdat[,"Low"],6), 3) + ( (round(runMax(sdat[,"High"],6), 3) - round(runMin(sdat[,"Low"],6), 3)) *.80)
  sdat$Stch7.20 <- round(runMin(sdat[,"Low"],6), 3) + ( (round(runMax(sdat[,"High"],6), 3) - round(runMin(sdat[,"Low"],6), 3)) *.20)
  #Daily (14,3,3) Stoch Oscilator	    	
  stochOSC <- stoch(sdat[,c("High","Low","Close")], nFastK = 14, nFastD = 3, nSlowD = 3)
  sdat$stochFastK14 <- round(stochOSC$fastK, 6)*100
  sdat$stochFastD14 <- round(stochOSC$fastD, 6)*100
  sdat$stochSlowD14 <- round(stochOSC$slowD, 6)*100
  #Stoch Oscilator 80 - 20 range	
  sdat$Stch14.80 <- round(runMin(sdat[,"Low"],13), 3) + ( (round(runMax(sdat[,"High"],13), 3) - round(runMin(sdat[,"Low"],13), 3)) *.80)
  sdat$Stch14.20 <- round(runMin(sdat[,"Low"],13), 3) + ( (round(runMax(sdat[,"High"],13), 3) - round(runMin(sdat[,"Low"],13), 3)) *.20)
  #Weekly (71, 3,3) Stoch Oscilator
  stochOSC <- stoch(sdat[,c("High","Low","Close")], nFastK = 71, nFastD = 3, nSlowD = 3)
  sdat$stochFastK71 <- round(stochOSC$fastK, 6)*100
  sdat$stochFastD71 <- round(stochOSC$fastD, 6)*100
  sdat$stochSlowD71 <- round(stochOSC$slowD, 6)*100
  #Stoch Oscilator 80 - 20 range	
  sdat$Stch71.80 <- round(runMin(sdat[,"Low"],70), 3) + ( (round(runMax(sdat[,"High"],70), 3) - round(runMin(sdat[,"Low"],70), 3)) *.80)
  sdat$Stch71.20 <- round(runMin(sdat[,"Low"],70), 3) + ( (round(runMax(sdat[,"High"],70), 3) - round(runMin(sdat[,"Low"],70), 3)) *.20)
  #Weekly (140, 3,3) Stoch Oscilator
  stochOSC <- stoch(sdat[,c("High","Low","Close")], nFastK = 140, nFastD = 3, nSlowD = 3)
  sdat$stochFastK140 <- round(stochOSC$fastK, 6)*100
  sdat$stochFastD140 <- round(stochOSC$fastD, 6)*100
  sdat$stochSlowD140 <- round(stochOSC$slowD, 6)*100
  #Stoch Oscilator 80 - 20 range	
  sdat$Stch140.80 <- round(runMin(sdat[,"Low"],139), 3) + ( (round(runMax(sdat[,"High"],139), 3) - round(runMin(sdat[,"Low"],139), 3)) *.80)
  sdat$Stch140.20 <- round(runMin(sdat[,"Low"],139), 3) + ( (round(runMax(sdat[,"High"],139), 3) - round(runMin(sdat[,"Low"],139), 3)) *.20)
  #Monthly (280, 3,3) Stoch Oscilator
  stochOSC <- stoch(sdat[,c("High","Low","Close")], nFastK = 280, nFastD = 3, nSlowD = 3)
  sdat$stochFastK280 <- round(stochOSC$fastK, 6)*100
  sdat$stochFastD280 <- round(stochOSC$fastD, 6)*100
  sdat$stochSlowD280 <- round(stochOSC$slowD, 6)*100
  #Stoch Oscilator 80 - 20 range	
  sdat$Stch280.80 <- round(runMin(sdat[,"Low"],279), 3) + ( (round(runMax(sdat[,"High"],279), 3) - round(runMin(sdat[,"Low"],279), 3)) *.80)
  sdat$Stch280.20 <- round(runMin(sdat[,"Low"],279), 3) + ( (round(runMax(sdat[,"High"],279), 3) - round(runMin(sdat[,"Low"],279), 3)) *.20)
  #Change in day Stoch High/Low
  sdat$StochHigh14  <- round(runMax(sdat[,"stochFastD14"],14), 3)
  sdat$StochLow14  <- round(runMin(sdat[,"stochFastD14"],14), 3)
  sdat$TrdRng.14D <- sdat$High14 - sdat$Low14
  
  #Daily Stoch Moment Indicator	
  SMI <- SMI(sdat[,c("High","Low","Close")])
  sdat$SMI <- round(SMI$SMI, 3)
  sdat$SMISig <- round(SMI$signal, 3)
  
  #William R
  sdat$WillR <- round(WPR(sdat[,c("High","Low","Close")]), 3)
  #Donchian Channels
  dc <- DonchianChannel( sdat[,c("High","Low")], n=10, include.lag = FALSE)
  sdat$dcH <- round(dc$high, 3)
  sdat$dcM <- round(dc$mid, 3)
  sdat$dcL <- round(dc$low, 3)
  sdat$dcHchg <- sdat$dcH - lag(sdat$dcH)
  sdat$dcMchg <- sdat$dcM - lag(sdat$dcM)
  sdat$dcLchg <- lag(sdat$dcL) - sdat$dcL  
  #Aroon Indicator    	
  aroondat <- aroon(sdat[, c("High", "Low")], n = 20)
  sdat$aroonUp = aroondat$aroonUp
  sdat$aroonDn = aroondat$aroonDn
  sdat$aroonOsc = aroondat$oscillator
  #Chaikin Money Flow
  sdat$CMF <- round(CMF(sdat[,c("High","Low","Close")], sdat[,"Volume"]), 3)
  sdat$CMF <- ifelse(is.na(sdat$CMF), 0, sdat$CMF)
  sdat$CMF.HMA <- round(HMA(sdat$CMF), 3)
  sdat$CMFHMAMomt <- (round((sdat$CMF.HMA - lag(sdat$CMF.HMA,2))/lag(sdat$CMF.HMA,2)*100, 2))
  sdat$CLV <- round(CLV(sdat[,c("High","Low","Close")]), 3)
  sdat$MFI <- MFI(sdat[,c("High","Low","Close")], sdat[,"Volume"])
  sdat$MFI <- round(ifelse(is.na(sdat$MFI), 0, sdat$MFI), 3)
  sdat$MFI.HMA <- round(HMA(sdat$MFI), 3)
  sdat$MFIHMAMomt <- (round((sdat$MFI.HMA - lag(sdat$MFI.HMA,2))/lag(sdat$MFI.HMA,2)*100, 2))
  #ATR
  atrdat <- ATR(sdat[, c('High', 'Low', 'Close')])
  sdat$tr <- atrdat$tr
  sdat$atr <- atrdat$atr
  sdat$trueHigh <- atrdat$trueHigh
  sdat$trueLow <- atrdat$trueLow
  
  #sdat$Longsig <- Lag(ifelse(lag(sdat$stochFastK14,1) < 20 & sdat$stochFastK14 > 20, .index(sdat), 0),0)
  #sdat$Shortsig <- Lag(ifelse(lag(sdat$stochFastK14,1) > 82 & sdat$stochFastK14 < 82, .index(sdat), 0),0)
  
  #Stoch Crossover Buy/Sell
  sdat$Longsig <- Lag(ifelse(lag(sdat$stochFastD14, 0) < 80 & lag(sdat$stochFastD14, 0) >= lag(sdat$stochSlowD14, 0) & lag(sdat$stochFastD14, 1) < Lag(sdat$stochSlowD14, 1), .index(sdat), 0),0)
  sdat$Shortsig <- Lag(ifelse(lag(sdat$stochFastD14, 0) > 20 & lag(sdat$stochFastD14, 0) <= lag(sdat$stochSlowD14, 0)	& lag(sdat$stochFastD14, 1) > Lag(sdat$stochSlowD14, 1), .index(sdat), 0),0)
  
  
  
  sdat <- sdat
  
}

buildCharts <- function(n = 180) {
  #Assign date and Data from repor
  cdate <- paste("as of:", qdat$Date, qdat$Time)
  cdat <- as.table(sdat[nrow(sdat),])
  
  
  # Create reporting data set based on num of days passed in variable n
  DaystoChart <- n
  startdt <- paste(as.character(Sys.Date() - DaystoChart),"/", sep="")
  cd <- sdat[startdt,]
  
  
  par(mfrow=c(4,1))
  par(mar=c(0,4,3,1))
  
  xrange <- range(.index(cd))
  #PRICE Price Stregth Chart
  yrange<-range(c(cd$Close, cd$EMA20, cd$EMA200, cd$SAR ))
  par(lwd = .75)
  plot(cd$Close, main=paste(ticker," - ", ticname, sep = ""), ylim=yrange, ylab = "Price", xaxt="n")
  #Add Buy Sell heading with colors
  par(lwd = 1)
  if(cd[nrow(cd), "Longsig"] > 0) {
    mtext(TrdInd <- "Bullish/Buy/Call", line=0, adj=.75, cex=.75, col="darkgreen")
    mtext(format(cd[nrow(cd), "dcH" ], 2), line=1, adj=.75, cex=.75, col="darkgreen")
    mtext(format(cd[nrow(cd), "dcM" ], 2), line=1, adj=.85, cex=.75, col="darkgreen")
    mtext(format(cd[nrow(cd), "dcL" ], 2), line=1, adj=.95, cex=.75, col="darkgreen")
  }
  if(cd[nrow(cd), "Shortsig"] > 0) {
    mtext(TrdInd <- "Bearish/Short/Put", line=0, adj=.75, cex=.75, col="red")
    mtext(format(cd[nrow(cd), "dcH" ], 2), line=1, adj=.75, cex=.75, col="red")
    mtext(format(cd[nrow(cd), "dcM" ], 2), line=1, adj=.85, cex=.75, col="red")
    mtext(format(cd[nrow(cd), "dcL" ], 2), line=1, adj=.95, cex=.75, col="red")
  }
  #Add Buy sell Signales
  abline(v= cd$Longsig, col = "green", lwd = .5)
  abline(v= cd$Shortsig, col = "red", lwd = .5)  
  
  #Add EMA Lines and SAR Points
  lines(cd$EMA50, col="red" )
  lines(cd$EMA100, col="orange")
  lines(cd$EMA200, col="purple" )
  #lines(cd$HMA, col="blue", lwd=2)
  points(cd$SAR, col="Red", pch=20)
  
  #Add Stoch 80/20 Price lines
  lines(lag(cd$Stch14.80, 0), col="darkred", lwd = 1.5)
  lines(cd$Stch14.20, col="darkgreen", lwd = 1.5)
  #Draw candles on Chart
  rect(.index(cd)-500, cd$High, .index(cd)+500, cd$Low, lwd = 0.40, col = "black")
  rect(.index(cd)-25000, cd$Open, .index(cd)+25000, cd$Close, border = NA, col = ifelse(cd$Open <= cd$Close, 'darkgreen', "orange"))
  
  chanHcol <- "black"
  chanLcol <- "black"
  if(cd[nrow(cd),"dcHchg"] > 0) {chanHcol <- "green"} 
  if(cd[nrow(cd),"dcLchg"] > 0) {chanLcol <- "Red"}
  
  abline(h=cd[nrow(cd),"dcH"], col=chanHcol, lwd=.5 )
  abline(h=cd[nrow(cd),"dcL"], col=chanLcol, lwd=.5 )
  
  mtext("EMA50", line=0, adj=.40, cex=.5, col="Red")
  mtext("EMA100", line=0, adj=.46, cex=.5, col="orange")
  mtext("EMA200", line=0, adj=.52, cex=.5, col="purple")
  mtext("HMA", line=0, adj=.60, cex=.5, col="blue")
  mtext(cdate, line=3, adj=0, cex=.5, col="black")
  cout = paste("Last", as.character(cdat$Close), 
               "  Chg:", as.character(cdat$Chg),
               "  %Chg:", as.character(cdat$PctChg))
  mtext(cout, line=2, adj=0, cex=.5, col="black")
  cout = paste("RSI:", as.character(cdat$RSI14), 
               "  SAR%Close:", as.character(round(100-cdat$SARClPct, 2)),
               "  CMF:", as.character(cdat$CMF),
               "  HMA Stgth:", as.character(round((cdat$Close/cdat$HMA*100)-100, 2)) )
  
  mtext(cout, line=1, adj=0, cex=.5, col="black")
  
  
  #RSI / CMF / MFI
  par(mar=c(4,4,.5,1))
  
  yrange<-range(c(0, 100))
  #  plot(cd$RSI14, main=NULL, ylim=yrange, ylab = "RSI", xaxt="s")
  #  abline(v= cd$Longsig, col = "green")
  #  abline(v= cd$Shortsig, col = "red")
  #  
  #    lines((cd$CMF*100/2)+50, col="royalblue", lwd=2)
  #  #lines((cd$CMF.HMA*100/2)+50, col="red", lwd=1)
  #  lines(cd$MFI, col="orange", lwd=1)
  #  #lines(cd$MFI.HMA, col="purple", lwd=1)
  #  axis(4, at=c(0, 25,37.5, 50, 62.5, 75, 100), 
  #       labels=c(-1.00,-.50,-.25, 0,.25, .50, 1.00), col = "royalblue")
  #  abline(h=c(50))
  #  abline(h=c(20,80), col="blue")
  #  abline(h=c(37.5,62.5), lty=3)
  #  mtext("RSI14", line=-2, adj=.40, cex=.5, col="black")
  #  mtext("CMF", line=-2,  adj=.50, cex=.5, col="royalblue")
  #  mtext("HMA", line=-2.6,  adj=.50, cex=.5, col="red")
  #  mtext("MFI", line=-2,  adj=.60, cex=.5, col="orange")
  #  mtext("HMA", line=-2.6,  adj=.60, cex=.5, col="purple")
  
  
  #Stocholic Fast/Slow Charts
  #  par(mar=c(.5,4,.5,1))
  #  yrange<-range(c(0, 100))
  plot(cd$stochFastK14 , main="", ylim=yrange,  ylab = "Stoch Fast/Slow (14,3,3)", xaxt="s")
  abline(v= cd$Longsig, col = "green", lwd = .5)
  abline(v= cd$Shortsig, col = "red", lwd = .5)  
  abline(h= 80, col="red")
  abline(h= 20, col="green")
  rect(xleft = xrange[1] , ybottom =80, xright = xrange[2],   ytop = 100, density = 20, border = "transparent",col = "lightpink")
  rect(xleft = xrange[1] , ybottom = 0, xright =xrange[2],   ytop = 20, density = 20,  border = "transparent", col = "olivedrab1")
  lines(cd$stochFastK14, col="black", lwd=1)
  lines(cd$stochFastD14, col="red", lwd=1)
  lines(cd$stochSlowD14, col="royalblue", lwd=1)
  abline(h=c(50))
  mtext("K%", line=0, adj=.40, cex=.5, col="black")
  mtext("Fast", line=0, adj=.45, cex=.5, col="red")
  mtext("Slow", line=0, adj=.50, cex=.5, col="royalblue")
  
  #Plot Stoch Fast/Slow(71,3,3)
  par(mar=c(.5,4,.5,1))
  yrange<-range(c(0, 100))
  plot(cd$stochFastK71 , main="", ylim=yrange,  ylab = "Stoch Fast/Slow (71,3,3)", xaxt="n")
  abline(v= cd$Longsig, col = "green", lwd = .5)
  abline(v= cd$Shortsig, col = "red", lwd = .5)  
  abline(h= 80, col="red")
  abline(h= 20, col="green")
  rect(xleft = xrange[1] , ybottom =80, xright = xrange[2],   ytop = 100, density = 20, border = "transparent",col = "lightpink")
  rect(xleft = xrange[1] , ybottom = 0, xright =xrange[2],   ytop = 20, density = 20,  border = "transparent", col = "olivedrab1")
  lines(cd$stochFastK71, col="black", lwd=1)
  lines(cd$stochFastD71, col="red", lwd=1)
  lines(cd$stochSlowD71, col="royalblue", lwd=1)
  abline(h=c(50))
  mtext("K%(wk)", line=0, adj=.40, cex=.5, col="black")
  mtext("Fast(Wk)", line=0, adj=.45, cex=.5, col="red")
  mtext("Slow(wk)", line=0, adj=.50, cex=.5, col="royalblue")
  
  plot(cd$stochFastK280 , main="", ylim=yrange,  ylab = "Stoch Fast/Slow (280,3,3)", xaxt="n")
  abline(v= cd$Longsig, col = "green", lwd = .5)
  abline(v= cd$Shortsig, col = "red", lwd = .5)  
  abline(h= 80, col="red")
  abline(h= 20, col="green")
  rect(xleft = xrange[1] , ybottom =80, xright = xrange[2],   ytop = 100, density = 20, border = "transparent",col = "lightpink")
  rect(xleft = xrange[1] , ybottom = 0, xright =xrange[2],   ytop = 20, density = 20,  border = "transparent", col = "olivedrab1")
  lines(cd$stochFastK280, col="black", lwd=1)
  lines(cd$stochFastD280, col="red", lwd=1)
  lines(cd$stochSlowD280, col="royalblue", lwd=1)
  abline(h=c(50))
  mtext("K%(wk)", line=0, adj=.40, cex=.5, col="black")
  mtext("Fast(Wk)", line=0, adj=.45, cex=.5, col="red")
  mtext("Slow(wk)", line=0, adj=.50, cex=.5, col="royalblue")
  
  #  lines(cd$stochFastK280, col="black", lwd=1)
  #  lines(cd$stochFastD280, col="orange", lwd=1)
  #  lines(cd$stochSlowD280, col="green", lwd=1)
  #  mtext("K%(mth)", line=-.75, adj=.40, cex=.5, col="black")
  #  mtext("Fast(mth)", line=-.75, adj=.45, cex=.5, col="orange")
  #  mtext("Slow(mth)", line=-.75, adj=.50, cex=.5, col="green")
  
  #Directional INDEX Chart
  #  par(mar=c(.5,4,.5,1))
  #  yrange<-range(c(0, 100))
  #  plot(cd$DX, main="", ylim=yrange,  ylab = "Directional Indx", xaxt="n")
  #  lines(cd$DIp, col="green" )
  #  lines(cd$DIn, col="red")
  #  lines(cd$ADX, col="royalblue", lwd=2)
  #  mtext("DX", line=0, adj=.40, cex=.5, col="black")
  #  mtext("DPp", line=0, adj=.45, cex=.5, col="green")
  #  mtext("DPn", line=0, adj=.50, cex=.5, col="red")
  #  mtext("ADX", line=0, adj=.55, cex=.5, col="royalblue")
  
}
