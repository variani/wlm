context("precomputed transform")

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
  transform <- decomp$transform
  
  # model
  mod1 <- wlm(mpg ~ disp, data, varcov = V)
  
  mod2 <- wlm(mpg ~ disp, data, transform = transform)
  
  expect_equal(as.numeric(coef(mod1)), as.numeric(coef(mod2)), tolerance = 1e-10)  
})



