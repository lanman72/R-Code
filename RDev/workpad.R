 require(datasets)

 require(grDevices); require(graphics)

 ## --- "math annotation" in plots :
 
pdf(file="c:/proc/prod/test.pdf", 7, 10, onefile=TRUE, paper="letter")
win.graph(10, 10, 10)
 ######
 # create tables of mathematical annotation functionality
 ######
 make.table <- function(nr, nc) {
     savepar <- par(mai=rep(0, 4), pty="m")
     plot(c(0, nc*2 + 1), c(0, -(nr + 1)),
          type="n", xlab="", ylab="", axes=FALSE)
     savepar
 }

 get.r <- function(i, nr) {
     i %% nr + 1
 }

 get.c <- function(i, nr) {
     i %/% nr + 1
 }

 draw.title.cell <- function(title, i, nr) {
     r <- get.r(i, nr)
     c <- get.c(i, nr)
     text(2*c - .5, -r, title)
     rect((2*(c - 1) + .5), -(r - .5), (2*c + .5), -(r + .5))
 }

 draw.plotmath.cell <- function(expr, i, nr, string = NULL) {
     r <- get.r(i, nr)
     c <- get.c(i, nr)
     if (is.null(string)) {
         string <- deparse(expr)
         string <- substr(string, 12, nchar(string) - 1)
     }
     text((2*(c - 1) + 1), -r, string, col="Grey")
     text((2*c), -r, expr, adj=c(.5,.5), col="Red")
     rect((2*(c - 1) + .5), -(r - .5), (2*c + .5), -(r + .5), border="grey")
 }

nr <- 20

nc <- 1

oldpar <- make.table(nr, nc)

i <- 0

draw.title.cell("Arithmetic Operators", i, nr); i <- i + 1

draw.plotmath.cell(expression(x + y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x - y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x * y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x / y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x %+-% y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x %/% y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x %*% y), i, nr); i <- i + 1

draw.plotmath.cell(expression(x %.% y), i, nr); i <- i + 1

draw.plotmath.cell(expression(-x), i, nr); i <- i + 1

draw.plotmath.cell(expression(+x), i, nr); i <- i + 1

draw.title.cell("Sub/Superscripts", i, nr); i <- i + 1

draw.plotmath.cell(expression(x[i]), i, nr); i <- i + 1

draw.plotmath.cell(expression(x^2), i, nr); i <- i + 1

draw.title.cell("Juxtaposition", i, nr); i <- i + 1

draw.plotmath.cell(expression(x * y), i, nr); i <- i + 1

draw.plotmath.cell(expression(paste(x, y, z)), i, nr); i <- i + 1

draw.title.cell("Radicals", i, nr); i <- i + 1

draw.plotmath.cell(expression(sqrt(x)), i, nr); i <- i + 1
t <- expression(sqrt(x))

### Big Operators

 nr <- 10

 nc <- 1

 make.table(nr, nc)

 i <- 0

 draw.title.cell("Big Operators", i, nr); i <- i + 1

 draw.plotmath.cell(expression(sum(x[i], i=1, n)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(prod(plain(P)(X == x), x)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(integral(f(x) * dx, a, b)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(union(A[i], i==1, n)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(intersect(A[i], i==1, n)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(lim(f(x), x %->% 0)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(min(g(x), x >= 0)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(inf(S)), i, nr); i <- i + 1

 draw.plotmath.cell(expression(sup(S)), i, nr); i <- i + 1

dev.off()


## Build Output
 nr <- 10

 nc <- 1

oldpar <- make.table(nr, nc)
 i <- 0
?rect
rect(1, 1.5, 2, 2.5)

sdat$Close
text(sdat$Close)
text(sdat[nrow(sdat),c("Symbol", "Date", "Time", "Close", "Change","Bid","Ask","PctChg")], i, nr) ; i <- i+1
