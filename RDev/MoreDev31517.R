maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[len-N+1]
}

maxN(, N=2)


listfileloc <- "/proc/prod/"
datafileloc <- "/proc/prod/data/"
datafileExt <- ".csv"
pdffileloc <- "/proc/prod/pdf/"

tickerlist <- "IndexList.csv"
# Get ticker list

paste (listfileloc, tickerlist, sep="")
tdata <- read.table(paste (listfileloc, tickerlist, sep=""), sep = ",", header = TRUE, row.names = 1)
rm(outdata)


for (ticker in row.names(tdata) ) {
  ticname <- tdata[ticker,"TICKERNAME"]
  qdat <- getQuote(ticker, mType="Quote", verbose = TRUE)
  ifelse(exists("outdata"), outdata <- rbind(outdata, qdat), outdata <- qdat)
}

lm(qdat[1:50,"Close"] ~ qdat[1:50,"Open"] )

fileConn<-file("output.htm")
writeLines('<table style="width:100%">', fileConn)
writeLines(paste("<R>",outdata$Symbol,"</b>", format(round(outdata$Close, 2),, "<BR>", sep="  "), fileConn)
close(fileConn)

sink("output.htm")                     # Begin writing output to file
cat('<table style="width:50%">')
cat("<tr>")
cat(paste("<th>",outdata$Symbol,'</th><th align="right">', round(outdata$Close, 2), "</th></tr>", sep="  "))
cat("</table>")
sink()   


paste(outdata$Symbol, round(outdata$Close, 2), "<BR>", sep = "  ")
outdata[,c('Symbol', 'Open', 'High', 'Low', 'Close', 'Change', 'PctChg')]

qdat[1:50,"Close"] ~ qdat[1:50,"Open"]

outdata <- qdat
outdata <- rbind(outdata, qdat)
summary(outdata)
index.outdata()
outdata$new1 <- as.character("x")

outdata$Symbol[1, ] 
outdata$Symbol=="^VIX"
outdata[1, ] <- "TIX"

d <- data.frame(x=c(1,4,2,5,2,3,NA), y = c(3,2,5,3,8,1,1), z = c(NA,NA,4,9,7,8,3))
d[x==2, y <- 10]
outdata[outdata$Symbol=="^VIX",]
outdata

  
myroll <- function(x, n=5) {
  x[NROW(x)-n:NROW(x),]
  myroll(x)
 }
myroll(datFAS$Open)
runSum
n = 5
runSum(datFAS[1:20,"Close"], 3)
runSum
x<- 1:10
embed(datVIX[1:20,"Close"],10)
mydat <- embed(datVIX[1:50,"Close"],10)

embed(1:171,10)
index(datVIX)
vdat <- as.matrix(index(datVIX[1:50,]))
embed(vdat,7)
mydat
mydat[nrow(mydat)-10:nrow(mydat),]
max.col(mydat)
mydat[2]
mydat[2,]
