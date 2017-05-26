# R package wls

[![travis-ci build status](https://travis-ci.org/variani/wlm.svg?branch=master)](https://travis-ci.org/variani/wlm)

![](docs/figures/varcovar-matrices-ggplot2.png)

```
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

library(wls)
m3 <- wlm(mpg ~ disp, mtcars, varcov = varcov_cyl)
```
