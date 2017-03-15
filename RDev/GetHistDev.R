#Load Function Script
source("c:/proc/r/histFunc.r")

#Get Stock Data
flush.console()

fileloc <- "c:/proc/prod/data/alldates/"
fileExt <- "full.csv"

#Read in Ticker Symbol List
tdata <- read.table("c:/proc/prod/SymbolList.dat", sep = ",")

# Process each Symbol in list
for (ticker in tdata[,1]) {
	sdat <- getsdat(ticker, 0)

   #Calculate Buy/Sell Logic
	buyfactor = 1.75
	sdat$Buy  <- 0
	sdat$Sell <- 0

	B  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) > 0 & sdat$SARClPct < 100)
	sdat[B,"Buy"] <- sdat[B,"Close"]
	S  <- (sdat$Chg10DH + (sdat$SD10Chg * buyfactor) < 0 & sdat$SARClPct > 100)
	sdat[S,"Sell"] <- sdat[S,"Close"]

#Write to data file
filepre <- paste(fileloc, gsub("\\^","",ticker), sep="")

write.table(as.matrix(sdat)[NROW(sdat):1,,drop=FALSE], paste(filepre,fileExt, sep=""), 
            quote = FALSE, row.names = TRUE, sep = ",")
}
print("Process Complete")