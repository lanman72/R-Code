as.data.frame(sdat, rowname=index(sdat))

sdat$CMF <- CMF(sdat[,c("High","Low","Close")], sdat[,"Volume"])
sdat$CLV <- CLV(sdat[,c("High","Low","Close")])
cmf <- merge(sdat, cmf, clv)
cmf
runSD(sdat[,"Close"],10)
sdat$SMA10 <- runMean(sdat[,"Close"],10)
sdat$SD10  <- runSD(sdat[,"Close"],10)
sdat$SD20  <- runSD(sdat[,"Close"],20)
sdat$SD100  <- runSD(sdat[,"Close"],100)
sdat$High5  <- runMax(sdat[,"Close"],5)
sdat$High10  <- runMax(sdat[,"Close"],10)
sdat$Low5  <- runMin(sdat[,"Close"],5)
sdat$Low10  <- runMin(sdat[,"Close"],10)



sdat$TDI <- TDI(sdat[,"Close"],10)
sdat['2015-07/',c("Close", "TDI", "PctChg")]

	sdat$Chg10DH  <- round(sdat[,"Close"]-sdat[,"High5"], 3)
      sdat$MAD10    <- runMAD(sdat[,"Close"], 10)

sdat['2015-05-22/2015-05-28',]



sdat[,"SD100"]/lag(sdat[,"SD100"],100)
sdat['2015-10',"SD100"]/lag(sdat[,"SD100"],100)

t = as.matrix(alldat)[NROW(alldat):1,,drop=FALSE]
t
dim(alldat)
dim(t)
class(t)
colnames(t)[]
colnames(alldat)
alldat['2015-10',]
SMA(sdat[,"Close"],10)


nr = 500
	Histdat = getHistDat("KO", 10)

if (nrow(Histdat) < nr) {nr <- nrow(Histdat) }
nr

