### inc
library(Matrix)

library(gridExtra)

### par
N <- 12
rho <- 0.5

### matrices
V1 <- Matrix(diag(N))

V2 <- Matrix(diag(rep(c(0.1, 1, 10), each = N/3)))

V3 <- diag(N)
V3 <- rho^abs(row(V3) - col(V3))
V3 <- Matrix(V3)

### plot
png("varcovar-matrices.png", width = 3*480, height = 480)

args <- list(sub = NULL, xlab = NULL, ylab = NULL)
grid.arrange(
  do.call(image, c(list(V1, main = "OLS"), args)),
  do.call(image, c(list(V2, main = "WLS"), args)),
  do.call(image, c(list(V3, main = "GLS"), args)),
  nrow = 1)

dev.off()
