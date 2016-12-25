context("formula interface")

test_that("model.frame", {
  # data
  data(mtcars)
  
  formula <- mpg ~ disp*cyl
  data <- within(mtcars, {
    cyl <- factor(cyl)
    mpg[1] <- NA
    disp[2] <- NA
    cyl[3] <- NA
  })
  
  x <- model.matrix(formula, data)
  y <- model.extract(model.frame(formula, data), "response")
  
  # processing of NAs is ok
  expect_true(nrow(x) != nrow(data))
  expect_equal(nrow(x), length(y))  
  # expanding interaction terms is ok
  expect_equal(ncol(x), 1 + 1 + (nlevels(data$cyl) - 1)*2) # Intercept + Cont. + (#Levels - 1) * (Marginal + Interaction)
})

