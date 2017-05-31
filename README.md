# R package wlm

[![travis-ci build status](https://travis-ci.org/variani/wlm.svg?branch=master)](https://travis-ci.org/variani/wlm)

![](docs/figures/varcovar-matrices-ggplot2.png)


The `wlm` R package supports linear models, which are more complex than OLS, 
but the output is still that as returned by `lm`.


See [Fitting linear mixed models for QTL mapping](https://kbroman.wordpress.com/2015/11/24/fitting-linear-mixed-models-for-qtl-mapping/) blog post by Karl Broman 
for examplanation of the eigen-decomposition trick possible for LMM1 model (1 random genetic + 1 random noise effects).

# Installation

```
library(devtools)
install_github("variani/wlm")
```

# Example code

```
library(wlm)

# data
data(mtcars)

mtcars <- within(mtcars, {
  cyl <- factor(cyl)
  weight_cyl = 1/sqrt(as.numeric(cyl))
})
  
# OLS
m1 <- lm(mpg ~ disp + cyl, mtcars)

# WLS
m2 <- lm(mpg ~ disp, mtcars, weights = weight_cyl)

# GLS
varcov_cyl <- with(mtcars, sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))

m3 <- wlm(mpg ~ disp, mtcars, varcov = varcov_cyl)

# LMM1
varcov_cyl <- with(mtcars, sapply(cyl, function(x) as.numeric(x == cyl)))

m4 <- lmm1(mpg ~ disp, mtcars, varcov = varcov_cyl)

#> m4$lmm$r2
#[1] 0.1049433
# ~10% of variance explained by the random effect with `varcov = varcov_cyl`

# variance-covariance matrix of the outcome 
var_mpg <- m4$lmm$r2 * varcov_cyl + (1 - m4$lmm$r2) * diag(nrow(varcov_cyl))

# `m4` is equivalent to the following GLS
m4_wlm <- wlm(mpg ~ disp, mtcars, varcov = varcov_mpg)

# LMM1 + precomputed eigendecomposition 
# - useful when you need to test many predictors such as in GWAS
evd_varcov_cyl <- eigen(varcov_cyl)

m5 <- lmm1(mpg ~ disp, mtcars, varcov = evd_varcov_cyl)

# Computation time of LMMs
system.time(lmm1(mpg ~ disp, mtcars, varcov = varcov_cyl))
#   user  system elapsed
#  0.005   0.000   0.005

library(lme4qtl)
system.time(lmer(mpg ~ disp + (1|cyl), mtcars))
#   user  system elapsed
#  0.014   0.000   0.014
```
