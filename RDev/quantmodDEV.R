require(quantmod)
require(PerformanceAnalytics)
require(ggplot2)

rm(stockData)
rm(armcsrIntersection)
rm(armSubset)
rm(csrSubset)
rm(stkSym)
rm(indicatorValuesBBands)

Sym[Sym$Symbol == "OXY",  ]
Sym[Sym$Symbol == "OXY", c("Sector") ]
Sym[Sym$Sector == "Energy" & ! is.na(Sym$Sector), c("Symbol", "Name", "MarketCap" ) ]

stockData <- new.env() #Create new enviroment for quantmod to work in

startDate = as.Date("2014-01-02") #Specify period of time we are interested in
endDate = as.Date("2016-05-27")

tickers <- c("GSPC","VIX") #Define the tickers we are interested in

#Download the stock history (for all tickers)
getSymbols(tickers, env = stockData, src = "yahoo", from = startDate, to = endDate)

Sym <- stockSymbols()
Sym[Symbol == "DIS"]
head(stkSym)
#Use head to show first six rows of matrix
head(stockData$GSPC)

#Lets look at the just the closing prices
Cl(stockData$GSPC)

#Lets plot the data
chartSeries(stockData$GSPC, type = "bars", theme=chartTheme('white'), TA = 'addSAR(col="red");addEMA(n=50, col = "blue");addEMA(n=100, col = "darkred");addEMA(n=200, col = "darkgreen")')
addSMI(n=14, slow =3, fast = 14, signal = 3)
addSMI(n=71, slow =71, fast = 3, signal = 3)

findPeaks(stochData$GSPC)

#Lets add some bollinger bands to the plot (with period 50 & width 2 standard deviations)
?addBBands #Make R display the help documentation so we know what variables to pass to the function
addBBands(n=50, sd=2)

#Lets get the technical indicator values saved into a variable
#Note must give it a single time series (I gave it the close price in this example)
indicatorValuesBBands <- BBands(Cl(stockData$ARM),n=50, sd=2)

#Lets examine only a 1 month period of data
armSubset<-  window(stockData$GSPC, start = as.Date("2010-02-15"), end = as.Date("2010-03-15"))
armSubset #Lets see the data

#Lets extract a 1 month period of data for CSR but starting midway through the arm data
csrSubset<-  window(stockData$VIX, start = as.Date("2010-02-25"), end = as.Date("2010-03-25"))
csrSubset #Lets see the data

#Now we want to get the intersection of the two subsets of data
#this will gives us all the sets of data where the dates match
#Its important to match the date series to stop spurious analysis of non-synchronised data
#All=FALSE specifies the intersection as in don't include all dates in the merge
armcsrIntersection <- merge(armSubset, csrSubset, all = FALSE)
subset(armcsrIntersection,select = c("ARM.Open","CSR.Open")) #Select the open columns and display
