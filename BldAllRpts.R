#Load Function Script
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)

#Get Stock Data
flush.console()

#Initialize using Command Arguments
cmdargs <- commandArgs(trailingOnly = TRUE)
tickerlist <- "IndexList.csv"
procType <- "DISPLAY"
argx <- 1
while (argx < 5) {
  argtst <- casefold(cmdargs[argx], upper = TRUE)  
  if (is.na(argtst)) break;
  if(argtst == "FULL") tickerlist <- "FullList.csv";
  if(argtst == "INDX") tickerlist <- "IndexList.csv";
  if(argtst == "EQTY") tickerlist <- "EquityList.csv";
  if(argtst == "TRADE") tickerlist <- "TradeList.csv";
  if(argtst == "TEST") tickerlist <- "TestList.csv";
  if(argtst == "ALLSTKS") tickerlist <- "MyList.csv";
  if(argtst == "PDFSEP" | argtst == "PDF" | argtst == "DATAONLY" | argtst == "WINDOW") procType <- argtst;
  argx <- argx+1
}

listfileloc <- "/proc/prod/"
datafileloc <- "/proc/prod/data/"
datafileExt <- ".csv"
pdffileloc <- "/proc/prod/pdf/"

fileid <- paste(format(Sys.time(), "%y%m%d%H%M" ), sep = "")
if(procType == "PDF") {
    pdfFileName = paste(pdffileloc, "Trading.pdf", sep = "")
    pdf(file=pdfFileName, 8.5, 11, onefile=TRUE, paper="letter")
  }

#Read in Ticker Symbol List
tdata <- read.table(paste (listfileloc, tickerlist, sep=""), sep = ",", header = TRUE, row.names = 1)
TradeSum <- tdata
TradeSum$Action <- ""

# Process each Symbol in list
for (ticker in row.names(tdata) ) {
  ticname <- tdata[ticker,"TICKERNAME"]
  
  if(procType == "PDFSEP") {
      pdfFileName = paste("c:/proc/prod/pdf/", ticker, ".pdf", sep = "")
      pdf(file=pdfFileName, 8.5, 11, onefile=TRUE, paper="letter")
  }
  
sdat <- getsdat(ticker, 500)
qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)


vBullDt <- structure(max(sdat$Longsig, na.rm = TRUE), class=c('POSIXt', 'POSIXct') )
vBearDt <- structure(max(sdat$Shortsig, na.rm = TRUE), class=c('POSIXt', 'POSIXct') )

# Set HTML Tag to set Row Color (red, orange, yellow, lightgreen, green)
TradeSum[ticker,"htmlrowcol"] <- '<tr>'
if(vBullDt > vBearDt || abs(as.Date(Sys.Date()) - as.Date(vBullDt)) <= 3) TradeSum[ticker,"htmlrowcol"] <-'<tr bgcolor="lightgreen">'
if(vBearDt > vBullDt || abs(as.Date(Sys.Date()) - as.Date(vBearDt)) <= 3) TradeSum[ticker,"htmlrowcol"] <-'<tr bgcolor="red">'

TradeSum[ticker, "Close"] <- sdat[nrow(sdat), c("Close")]

if (sdat[nrow(sdat),"Longsig"] > 0) TradeSum[ticker,"Action"] <- "Bullish/Buy/Call"
if (sdat[nrow(sdat),"Shortsig"] > 0) TradeSum[ticker,"Action"] <- "Bearish/Short/Put" 

if (sdat[vBullDt,"Longsig"] > 0) {  TradeSum[ticker,"LastBuySig"] <- as.character.Date(vBullDt)
                                    TradeSum[ticker,"LBClose"] <- sdat[vBullDt, c("Close")]
                                    TradeSum[ticker,"LBATR"] <- sdat[vBullDt, c("atr")]
                                    TradeSum[ticker,"LastSellSig"] <- as.character.Date(vBearDt)
                                    TradeSum[ticker,"LSClose"] <- sdat[vBearDt, c("Close")]
                                    TradeSum[ticker,"LSATR"] <- sdat[vBearDt, c("atr")]
                                    TradeSum[ticker,"ATR"] <- sdat[nrow(sdat)-1, c("atr")]
                                    TradeSum[ticker,"TrailSL"] <- TradeSum[ticker,"ATR"]*2.17/2
                                    TradeSum[ticker,"STProfExit"] <- sdat[nrow(sdat)-1, c("Close")] + TradeSum[ticker,"ATR"]*.46
                                    TradeSum[ticker,"LTProfExit"] <- sdat[nrow(sdat)-1, c("Close")] + TradeSum[ticker,"ATR"]*2.17
                                    TradeSum[ticker,"LTOptSL30D"] <- round(TradeSum[ticker,"ATR"]*2.17*0.3 , 2)
                                    TradeSum[ticker,"LTOptSL40D"] <- round( TradeSum[ticker,"ATR"]*2.17*0.4 , 2)
                                    TradeSum[ticker,"LTOptSL45D"] <- round( TradeSum[ticker,"ATR"]*2.17*0.45 , 2)
                                    TradeSum[ticker,"STOptSL30D"] <- round( TradeSum[ticker,"ATR"]*0.3 , 2)
                                    TradeSum[ticker,"STOptSL40D"] <- round( TradeSum[ticker,"ATR"]*0.4 , 2)
                                    TradeSum[ticker,"STOptSL45D"] <- round( TradeSum[ticker,"ATR"]*0.45 , 2)
}



buildCharts(n = 300)

    if(procType == "PDFSEP") dev.off();

sdat$Long  <- 0
sdat$Short <- 0

#Write to data file
datafilepre <- paste(datafileloc, gsub("\\^","",ticker), sep="")
datafilename <- paste(datafilepre,datafileExt, sep="")
#write.table(as.matrix(sdat)[NROW(sdat):1,,drop=FALSE], paste(datafilepre,datafileExt, sep=""), 
#            quote = FALSE, row.names = TRUE, sep = ",")
write.zoo(sdat, file = paste(datafilepre,datafileExt, sep=""), index.name = "Date", sep = ",")
}
print("Process Complete")
if(procType != "PDFSEP") dev.off()

#****************************************************************************************
#Create Trading Summary Report
print("Creating Trading Summary Report")


########################################HTML OUTPUT SUMMARY
# Begin writing output to file
sink(paste (listfileloc, "TradeSummaryReport.htm", sep=""))                     
### HTML Build Table
cat('<table style="width:100%">')
#Write column Headers
cat(paste('<tr>', '<th>Ticker</th>
          <th align="right">', 'Name', '</th>
          <th align="right">', 'Last Buy Signal', '</th>
          <th align="right">', 'LBS Close', '</th>
          <th align="right">', 'LBsATR', '</th>
          <th align="right">', 'Last Sell Signal', '</th>
          <th align="right">', 'LSS Close', '</th>
          <th align="right">', 'LSSATR', '</th>
          <th align="right">', 'Trail<BR>Stop Loss', '</th>
          <th align="right">', 'Prof Exit<BR> (Short Term)', '</th>
          <th align="right">', 'LTProfExit', '</th>
          <th align="right">', 'LTOptSL30D', '</th>
          <th align="right">', 'LTOptSL40D', '</th>
          <th align="right">', 'LTOptSL45D', '</th>
          <th align="right">', 'STOptSL30D', '</th>
          <th align="right">', 'STOptSL40D', '</th>
          <th align="right">', 'STOptSL45D', '</th>
          <th align="right">', 'ATR', '</th>
          </tr>', sep="  " ) )

### write each record to a sperate table row (th, with TH Column)
cat(paste(TradeSum$htmlrowcol, '<th>',row.names(TradeSum),'</th>
          <th align="right">', TradeSum$TICKERNAME, '</th>
          <th align="right">', TradeSum$LastBuySig, '</th>
          <th align="right">', round(TradeSum$LBClose, 2), '</th>
          <th align="right">', round(TradeSum$LBATR, 2), '</th>
          <th align="right">', TradeSum$LastSellSig, '</th>
          <th align="right">', round(TradeSum$LSClose, 2), '</th>
          <th align="right">', round(TradeSum$LSATR, 2), '</th>
          <th align="right">', round(TradeSum$TrailSL, 2), '</th>
          <th align="right">', round(TradeSum$STProfExit, 2), '</th>
          <th align="right">', round(TradeSum$LTProfExit, 2), '</th>
          <th align="right">', round(TradeSum$LTOptSL30D, 2), '</th>
          <th align="right">', round(TradeSum$LTOptSL40D, 2), '</th>
          <th align="right">', round(TradeSum$LTOptSL45D, 2), '</th>
          <th align="right">', round(TradeSum$STOptSL30D, 2), '</th>
          <th align="right">', round(TradeSum$STOptSL40D, 2), '</th>
          <th align="right">', round(TradeSum$STOptSL45D, 2), '</th>
          <th align="right">', round(TradeSum$ATR, 2), '</th>
          </tr>', sep="  " ) )
cat("</table>")
sink()   
######################################################HTML OUTPUT SUMMARY END ##############

  #Create PDF FIle
pdf(file="/proc/prod/pdf/tradingsummay.PDF", 8.5, 11, onefile=TRUE, paper="letter")

  #Define Plot
yrange<-range(c(0, 50))
xrange<-range(c(1, 20))
par(adj = 0)
plot(0, 0, main = "Trading Summary", xlim = xrange, ylim = yrange, xlab = "", ylab = "", xaxt="n", yaxt="n")
mtext(format(Sys.time(), "%a %b %d %Y %I:%M:%S %P" ), cex = .5)

  #Output each line into plot
for (lnnum in index(TradeSum)) {
  tcol <- "black"
  if (TradeSum[lnnum,"Action"] == "Bullish/Buy/Call") { tcol <- "green" }
  if (TradeSum[lnnum,"Action"] == "Bearish/Short/Put") { tcol <- "red" }
  
  text(1, 50-(lnnum*1),row.names(TradeSum[lnnum,]), cex = .5, col = tcol)
  text(4, 50-(lnnum*1),TradeSum[lnnum,"TICKERNAME"], cex = .5, col = tcol)
  text(8, 50-(lnnum*1),TradeSum[lnnum,"Action"], cex = .5, col = tcol)
}

dev.off()  #Close PDF Output file




