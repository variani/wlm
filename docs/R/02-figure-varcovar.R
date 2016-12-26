### inc
library(reshape)

library(ggplot2)

### par
N <- 12
rho <- 0.5

### matrices
V1 <- diag(N)

val <- rep(c(1, 2, 3), each = N/3)
V2 <- diag(val)

V3 <- sapply(val, function(x) val * as.numeric(x == val))

### plot
plot_mat <- function(mat, title = "") 
{
  pf <- melt(mat)

  p <- ggplot(pf, aes(X1, X2)) + geom_tile(aes(fill = value), color = "white") + 
    scale_fill_gradient(low = "white", high = "purple")
  p <- p + scale_y_reverse()
  p <- p + guides(fill = "none") + theme_void() + theme(plot.title = element_text(hjust = 0.5))
  
  p <- p + ggtitle(title)
  
  return(p)
}

png("varcovar-matrices-ggplot2.png", width = 3*480, height = 480)

grid.arrange(
  plot_mat(V1, title = "\nOrdinary Least Squares (OLS)\nR package stats"),
  plot_mat(V2, title = "\nWeighted Least Squares (WLS)\nR package stats"),
  plot_mat(V3, title = "\nGeneralized Least Squares (GLS)\nR package wls"),    
  nrow = 1)

dev.off()
