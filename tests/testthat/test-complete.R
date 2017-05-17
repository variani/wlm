context("complete")

test_that("wlm: varcov", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)

  cyl <- with(mtcars, factor(cyl))
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  data <- mtcars
  data$mpg[1] <- NA
  data$disp[2] <- NA
  
  mod <- wlm(mpg ~ disp, data, varcov = V)
  
  expect_true(nobs(mod) == nrow(data) - 2)
})

test_that("lmm1: varcov", {
  # inc
  stopifnot(require(Matrix))

  # data
  data(mtcars)

  cyl <- with(mtcars, factor(cyl))
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  data <- mtcars
  data$mpg[1] <- NA
  data$disp[2] <- NA

  # lmm1
  mod <- lmm1(mpg ~ disp, data, varcov = V)

  expect_true(nobs(mod) == nrow(data) - 2)  
})


