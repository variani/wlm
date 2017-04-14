library(lmmlite)

library(tidyverse)

### par
use_cpp <- FALSE
REML <- FALSE

### data
data(recla)

### (1) compute using lmmlite
R <- recla$kinship
y <- recla$pheno[, 1]
X <- recla$covar

e <- eigen_rotation(R, y, X, use_cpp = use_cpp)
mod1 <- fitLMM(e$Kva, e$y, e$X, use_cpp = use_cpp, reml = REML)

### (2) compute using wls
dat <- data.frame(y = recla$pheno[, 1], sex = recla$covar[, 2])
mod2 <- lmm1(y ~ sex, dat, rvarcov = R, REML = REML)

### debugging
ll1 <- as.numeric(calcLL(0.5, e$Kva, e$y, e$X, use_cpp = use_cpp, reml = REML))

decompose <- decompose_varcov(R, output = "all")
  
### rotated y, X
X_rotated <- crossprod(decompose$vectors, X)
y_rotated <- as.numeric(crossprod(decompose$vectors, y))
  
ll2 <- lmm1_compute_wls_ll(0.5, y = y_rotated, X = X_rotated, evalues = decompose$values, REML = REML)

tab <- data_frame(r2 = seq(0, 1, by = 0.1)) %>%
  mutate(ll = sapply(r2, function(x) lmm1_compute_wls_ll(x, y = y_rotated, X = X_rotated, 
    evalues = decompose$values, REML = REML)))

p <- ggplot(tab, aes(r2, ll)) + geom_point() + geom_line()
