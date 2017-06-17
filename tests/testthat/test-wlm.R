context("wlm")

test_that("mtcars example (wls)", {
  # data
  data(mtcars)
  mtcars <- within(mtcars, {
    cyl <- factor(cyl)
    weight_cyl = 1/sqrt(as.numeric(cyl))
  })
  
  varcov <- diag(1/mtcars$weight_cyl)
  
  m1 <- lm(mpg ~ disp, mtcars, weights = weight_cyl)  
  m2 <- wlm(mpg ~ disp, mtcars, varcov = varcov)  

  expect_equal(unname(coef(m1)), unname(coef(m2)), tolerance = 1e-10)
})


test_that("mtcars example (gls)", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)
  data <- within(mtcars, {
    cyl <- factor(cyl)
  })
  
  ord <- with(data, order(cyl))
  data <- data[ord, ]
  
  cyl <- data$cyl
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  #mod <- wlm(mpg ~ disp*cyl, data, varcov = V)
  mod <- wlm(mpg ~ disp, data, varcov = V)
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
