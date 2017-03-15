source("c:/proc/r/histFunc.r")

#Get Positions data file
pos <- read.table("c:/proc/prod/positions.csv", header = TRUE, sep = ",")

for (pInd in index(pos)) {
      pSymbol <- pos[pInd,"Symbol"]
      posType <- pos[pInd,"Type"]
	sdat <- getsdat(pSymbol, 500)
      if (posType == "LC") {
		print(paste("Long Call", pSymbol))
	} else if (posType == "STK"){
		print(paste("Long Stock", pSymbol))
	} else {
		print("No Posistion Type Defined")
      }
}

emailme("Stock Data", "Check you email for Details")