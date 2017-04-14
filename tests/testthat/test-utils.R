context("utils")

test_that("varcov_transform", {
  stopifnot(require(lme4))
  
  # data
  data(mtcars)
  data <- within(mtcars, {
    cyl <- factor(cyl)
  })
  
  # mixed-effects model
  mod1 <- lmer(mpg ~ disp + (1|cyl), data)
  
  transform <- mod_varcov_transform(mod1)
  
  mod2 <- wlm(mpg ~ disp, data, transform = transform)
  
  expect_equal(as.numeric(fixef(mod1)), as.numeric(coef(mod2)), tolerance = 1e-10)  
})

