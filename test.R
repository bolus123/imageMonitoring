download.file("https://www.ua.edu/assets/img/ua-square-logo-100x100.png",
  destfile = "UA.png")
  
  
install.packages('png')
install.packages('colorspace')
library("png")
library("colorspace")


x <- readPNG('UA.png')
dim(x)

y <- rgb(x[,,1], x[,,2], x[,,3], alpha = x[,,4])
yg <- desaturate(y)
yn <- col2rgb(yg)[1, ]/255
dim(y) <- dim(yg) <- dim(yn) <- dim(x)[1:2


pixmatplot <- function (x, ...) {
  d <- dim(x)
  xcoord <- t(expand.grid(1:d[1], 1:d[2]))
  xcoord <- t(xcoord/d)
  par(mar = rep(1, 4))
  plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, 
    xlim = c(0, 1), ylim = c(0, 1), ...)
  rect(xcoord[, 2L] - 1/d[2L], 1 - (xcoord[, 1L] - 1/d[1L]), 
    xcoord[, 2L], 1 - xcoord[, 1L], col = x, border = "transparent")
}


pixmatplot(y)
pixmatplot(yg)
