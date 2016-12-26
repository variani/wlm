### inc
library(Matrix)

library(gridExtra)

### par
N <- 12
rho <- 0.5

### matrices
V1 <- Matrix(diag(N))

val <- rep(c(1, 2, 3), each = N/3)
V2 <- Matrix(diag(val))

V3 <- Matrix(sapply(val, function(x) val * as.numeric(x == val)))

### plot
png("varcovar-matrices.png", width = 3*480, height = 480)

args <- list(sub = NULL, xlab = NULL, ylab = NULL)
grid.arrange(
  do.call(image, c(list(V1, main = "Ordinary Least Squares (OLS)\nR package stats"), args)),
  do.call(image, c(list(V2, main = "Weighted Least Squares (WLS)\nR package stats"), args)),
  do.call(image, c(list(V3, main = "Generalized Least Squares (GLS)\nR package wls"), args)),
  nrow = 1)

dev.off()
