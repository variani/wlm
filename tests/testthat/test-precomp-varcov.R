context("precomputed varcov")

test_that("complete cases", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)
  data <- within(mtcars, {
    cyl <- factor(cyl)
  })
  
  # var-covar matrix
  cyl <- data$cyl
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  # decomposition
  decomp <- decompose_varcov_evd(V, output = "all")
  
  # model
  mod1 <- wlm(mpg ~ disp, data, varcov = V)
  
  mod2 <- wlm(mpg ~ disp, data, varcov = decomp, dmethod = "evd")
  
  # check
  expect_true(crossprod(coef(mod1) - coef(mod2)) < 1e-10)
})

test_that("evd for low-rank varcov (expected error)", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)
  data <- within(mtcars, {
    cyl <- factor(cyl)
  })
  
  # var-covar matrix
  cyl <- data$cyl
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  # decomposition
  decomp <- eigen(V, symmetric = TRUE)
  
  # check
#> decompose_varcov(decomp) %>% str
# num [1:32, 1:32] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
  expect_error(suppressWarnings(mod <- wlm(mpg ~ disp, data, varcov = decomp)))
})


test_that("uncomplete cases (exptected error)", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)
  data <- within(mtcars, {
    mpg[1:3] <- NA
    cyl <- factor(cyl)
  })
  
  # var-covar matrix
  cyl <- data$cyl
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  # decomposition
  decomp <- decompose_varcov_evd(V, output = "all")
  
  # check
  expect_error(mod <- wlm(mpg ~ disp, data, varcov = decomp))
})



