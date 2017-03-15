#Load Function Script
source("c:/proc/r/Trading/TradeFunc.r")
require(quantmod)
require(PerformanceAnalytics)

TradeIndex <- c('^GSPC', '^NDX', 'GLD')



  
  
  

TradeIndex <- c('^GSPC', '^VIX', 'GLD')

tbars <- c(1, 2, 3, 5, 7, 14, 20, 50, 71, 140, 280)
for (tindx in TradeIndex) {
    print(tindx)
    testdata <- getsdat(tindx, 2500)
    for (xbars in tbars) {
      stoch.xbars <- stoch(testdata[,c('High', 'Low', 'Close')], nFastK = xbars)
      print(xbars)
      print(stoch.xbars[nrow(stoch.xbars),])
      test <- stoch.xbars$fastD
      for (rvsLook in 10:1) {
        test <- (stoch.xbars$fastD - Lag(stoch.xbars$fastD, rvsLook))
        print(paste (rvsLook," -->", test[nrow(test)]) )  #sum(stoch.xbars$fastD, Lag(stoch.xbars$fastD, rvsLook), stoch.xbars$fastD - Lag(stoch.xbars$fastD, rvsLook)))
      
      }
      #Reverse test for xbars
      
    }
}

