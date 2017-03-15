print(Sys.time())


buyfactor = 1.75
if (!is.na(sdat$Chg10DH + (sdat$SD10Chg * buyfactor) > 0) & !is.na(sdat$SARClPct < 100)) {
		sdat$buy <- sdat$Close
	} else {
		sdat$buy <- 0
	}

sdat$buy <- (sdat[,"Chg10DH"] + (sdat[,"SD10Chg"] * buyfactor) > 0 & sdat[,"SARClPct"] < 100) 
?filter
sdat$Chg - lag(sdat$Chg,1)

sdat$Close - lag(sdat$Close,1)


sdat$Chg10DH + (sdat$SD10Chg * buyfactor) > 0 & sdat$SARClPct < 100

  #Add Sell Logic (1 = True  0=False)
	sdat$Sell <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) < 0 
			& sdat$SARClPct > 100 )| sdat$DIVERG < 0
  #Add Buy Logic (1 = True  0=False)
	sdat$Buy  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) > 0 
			& sdat$SARClPct < 100) & sdat$Sell != 1)

sdat$Sell <- 0
sdat$Buy  <- 0

B  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) > 0 & sdat$SARClPct < 100)
sdat[B,"Buy"] <- sdat[B,"Close"]
S  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) < 0 & sdat$SARClPct > 100)
sdat[S,"Sell"] <- sdat[S,"Close"]



sdat['2015-07-13/',c("Open", "Close", "Buy", "Sell", "CMF", "CLV", "RSI14","DX", "ADX", "DIVERG")]
colnames(sdat)

?date
sdat$High/sdat$Open
sdat$Low/sdat$Open

print(paste("Symbol:",ticker, "Avg:", sdat$Index, (sdat$Low+sdat$Open)/2, "Close:", sdat$Close))


sdat[,"Low"]/sdat[,"Open"]
(sdat[,"High"]+sdat[,"Low"])/2


#Buy: Chg10DH + (SD10Chg + [factor]) < 0
     and SARClPct < 100
#Sell: Not [Buy]
#Short: Chg10DH + (SD10Chg + [factor]) > 0
     and SARClPct < 100
     and not Position

