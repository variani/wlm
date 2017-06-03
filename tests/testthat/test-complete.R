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

test_that("wlm: transform", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)

  cyl <- with(mtcars, factor(cyl))
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  rownames(V) <- rownames(mtcars)
  V <- V[-c(1:2), -c(1:2)]
  
  data <- mtcars
  data$mpg[1] <- NA
  data$disp[2] <- NA
  
  decomp <- decompose_varcov(V, method = "evd", output = "all")
  transform <- decomp$transform

  mod1 <- wlm(mpg ~ disp, data, transform = transform)
  mod2 <- wlm(mpg ~ disp, data, varcov = V)
  
  expect_true(nobs(mod1) == nrow(data) - 2)
  expect_equal(as.numeric(coef(mod1)), as.numeric(coef(mod2)), tolerance = 1e-10)    
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


