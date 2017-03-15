devdata <- rptpds <- 0
startrow <- nrow(testdata)-100 - rptpds
endrow <- nrow(testdata)-rptpds
devdata <- testdata[startrow:endrow, ]
devdata

testfun <- function(hlc) {
  hlc
  
}
  

testfun(devdata[,c("High", "Low", "Close")])

for (xdat in devdata ) {
  h1 <- xdat$High
}




c(sdat$stochFastD14 - Lag(sdat$stochFastD14, 1), sdat$stochFastD71 - Lag(sdat$stochFastD71, 1) )

rm(pcratio.vix)
rm(pcratio.indx)
rm(pcratio.eqty)
rm(pcratio.compare)

pcratio.indx <- read.table("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/indexpc.csv",header = TRUE, skip = 2, sep = ",")
pcratio.eqty <- read.table("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/equitypc.csv",header = TRUE, skip = 2, sep = ",")
pcratio.vix <- read.table("http://www.cboe.com/publish/scheduledtask/mktdata/datahouse/vixpc.csv",header = TRUE, skip = 1, sep = ",")

#pcratio.indx$DATE[1]
#as.Date(pcratio.indx$DATE, format = "%m/%d/%Y")
pcratio.indx$DATE <- as.Date(pcratio.indx$DATE, format = "%m/%d/%Y")
pcratio.eqty$DATE <- as.Date(pcratio.eqty$DATE, format = "%m/%d/%Y")
pcratio.vix$Date <- as.Date(pcratio.vix$Date, format = "%m/%d/%Y")

pcratio.vix$runSum(pcratio.vix$VIX.Put.Call.Ratio)


pcratio.vix <- as.xts(pcratio.vix, pcratio.vix$Date, )
pcratio.indx <- as.xts(pcratio.indx, pcratio.indx$DATE)
pcratio.eqty <- as.xts(pcratio.eqty, pcratio.eqty$DATE)
type
typeof(pcratio.vix$VIX.Put.Call.Ratio)
pcratio.compare <-  merge(pcratio.vix,pcratio.indx, by.x = "Date", by.y = "DATE", sort = TRUE)

pcratio.compare <-  merge(pcratio.compare,pcratio.eqty, by.x = "Date", by.y = "DATE", sort = TRUE)

pcratio.compare <- as.xts(pcratio.compare, pcratio.compare$Date)

colnames(pcratio.compare) <-c("Date","vix.ratio", "vix.put", "vix.call", "vix.total", 
                                   "indx.call", "indx.put", "indx.total", "indx.ratio", 
                                   "eqty.call", "eqty.put", "eqty.total", "eqty.ratio")



pcratio.compare$indx.ratio.EMA10 <- runMean(pcratio.compare$indx.ratio, n = 10)
pcratio.compare$indx.ratio.SD10 <-  runSD(pcratio.compare$indx.ratio, n = 10)

typeof(pcratio.compare$indx.ratio)
runMean(pcratio.vix$VIX.Put.Call.Ratio)
pcratio.compare <- merge(pcratio.compare,sdat)

write.zoo(pcratio.compare, file = "c:/proc/prod/data/pcratio.csv", index.name = "DATE", sep = ",")

  pcratio.compare['2016/', c("vix.ratio", "indx.ratio", "eqty.ratio") ]


rm(pcratio.indx)
pcratio.vix$Date
pcratio.eqty$DATE
pcratio.indx$DATE

getFin('AAPL')
viewFin(AAPL.f, "IS", "Q")
AAPL.f["IS", "Revenue"]
