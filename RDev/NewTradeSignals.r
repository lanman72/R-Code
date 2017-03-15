ticker <- "FAS"
tickname <- "Fincnaicl 3X"
tdat1 <- datFAS
tdat2 <- datVIX
chead <-qdat

n = 500

  #Assign date and Data from repor
  cdate <- paste("as of:", chead$Date, chead$Time)
  cdat1 <- as.table(tdat1[nrow(tdat1),])
  cdat2 <- as.table(tdat2[nrow(tdat2),])
  
  
  # Create reporting data set based on num of days passed in variable n
  DaystoChart <- n
  startdt <- paste(as.character(Sys.Date() - DaystoChart),"/", sep="")
  cd <- cdat1[startdt,]
  
  
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
  
