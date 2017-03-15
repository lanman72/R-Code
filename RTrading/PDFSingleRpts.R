#Load Function Script
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)


#Get Stock Data
flush.console()

fileloc <- "c:/proc/prod/data/"
fileExt <- ".csv"

fileid <- paste(format(Sys.time(), "%y%m%d%H%M" ), sep = "")

#Read in Ticker Symbol List
tdata <- read.table("c:/proc/prod/SymbolList.dat", sep = ",")

# Process each Symbol in list
for (tindx in index(tdata) ) {
  ticker <- tdata[tindx,1]

   pdfFileName = paste("c:/proc/prod/pdf/", ticker, ".pdf", sep = "")
   pdf(file=pdfFileName, 8.5, 11, onefile=TRUE, paper="letter")
  

sdat <- getsdat(ticker, 500)
qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)

buildCharts(n = 180)

dev.off()

sdat$Long  <- 0
sdat$Short <- 0

#Write to data file
filepre <- paste(fileloc, gsub("\\^","",ticker), sep="")

write.table(as.matrix(sdat)[NROW(sdat):1,,drop=FALSE], paste(filepre,fileExt, sep=""), 
            quote = FALSE, row.names = TRUE, sep = ",")
}
print("Process Complete")



