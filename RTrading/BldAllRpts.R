#Load Function Script
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)

#Get Stock Data
flush.console()

#Initialize using Command Arguments
cmdargs <- commandArgs(trailingOnly = TRUE)
tickerlist <- "EquityList.csv"
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

if (sdat[nrow(sdat),"Longsig"] > 0) TradeSum[ticker,"Action"] <- "Bullish/Buy/Call"
if (sdat[nrow(sdat),"Shortsig"] > 0) TradeSum[ticker,"Action"] <- "Bearish/Short/Put" 


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




