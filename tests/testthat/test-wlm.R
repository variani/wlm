context("wlm")

test_that("mtcars example", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)
  data <- within(mtcars, {
    cyl <- factor(cyl)
  })
  
  cyl <- data$cyl
  V <- Matrix(diag(cyl))
  
  mod <- wlm(mpg ~ disp*cyl, data, varcov = V)
})

test_that("longley example", {
  # inc
  stopifnot(require(nlme))
  
  # data
  data(longley)
  nobs <- nrow(longley)

  # GLS model from `nlme`
  mod0 <- nlme::gls(Employed ~ GNP + Population, correlation = corAR1(form = ~Year), data = longley)
  
  rho <- 0.64417 # mod0$modelStruct$corStruct
  V <- diag(nobs)
  V <- rho^abs(row(V) - col(V))
  
  # WLS model
  mod <- wlm(Employed ~ GNP + Population, data = longley, varcov = V)
  
  expect_equal(as.numeric(coef(mod0)), as.numeric(coef(mod)), tolerance = 1e-3)
})
