#Load Function Script
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)
require(RCurl)
require(jsonlite)
require(httr)


listfileloc <- "/proc/prod/"
datafileloc <- "/proc/prod/data/"
datafileExt <- ".csv"
pdffileloc <- "/proc/prod/pdf/"



tryCatch

("http://download.finance.yahoo.com/d/quotes.csv?s=ABDC&f=sl1d1t1c1ohgvba")

mySym <- stockSymbols(exchange = c("AMEX", "NASDAQ", "NYSE"), sort.by = c("Exchange", "Symbol"), quiet = FALSE)
mySym <- stockSymbols(exchange = c("NASDAQ"), sort.by = c("Exchange", "Symbol"), quiet = FALSE)

#mySym[mySym$Symbol == "BOKF", ]
#mycount <- 0
for (ticker in mySym$Symbol ) {
  ticname <- mySym[ticker,"Name"]

  sdat <- getsdat(ticker, 500)
  qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)

  
  
  #Write to data file
  datafilepre <- paste(datafileloc, gsub("\\^","",ticker), sep="")
  datafilename <- paste(datafilepre,datafileExt, sep="")
  #write.table(as.matrix(sdat)[NROW(sdat):1,,drop=FALSE], paste(datafilepre,datafileExt, sep=""), 
  #            quote = FALSE, row.names = TRUE, sep = ",")
  write.zoo(sdat, file = paste(datafilepre,datafileExt, sep=""), index.name = "Date", sep = ",")
}
  

getSymbols("APHB")
rm(YHOO)

