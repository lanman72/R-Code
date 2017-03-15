Sys.Date()
plot(x <- sdat[paste(Sys.Date()-252,'/'),"Close"], type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")

?index
t <- line(sdat[,"EMA10"])
t
residuals(t)
fitted(t)
plot(residuals(t) ~ fitted(t), main = deparse(t$call))

?table