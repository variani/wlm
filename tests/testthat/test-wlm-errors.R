context("wlm errors")

test_that("varcov dimension", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)

  cyl <- with(mtcars, factor(cyl))
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  V <- V[-1, -1] 
  expect_error(wlm(mpg ~ disp, mtcars, varcov = V), "varcov dimension")
})

test_that("transform dimension", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  data(mtcars)

  cyl <- with(mtcars, factor(cyl))
  V <- Matrix(sapply(cyl, function(x) as.numeric(x) * as.numeric(x == cyl)))
  
  decompose <- decompose_varcov(V, output = "all")
  transform <- decompose$transform

  transform <- transform[-1, -1] 
  expect_error(wlm(mpg ~ disp, mtcars, transform = transform), "transform dimension")
})

