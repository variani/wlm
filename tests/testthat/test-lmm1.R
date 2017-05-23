context("lmm1")

test_that("mtcars example: variance proportion", {
  # inc
  stopifnot(require(Matrix))
  stopifnot(require(lme4))

  # data
  data(mtcars)
    
  # lme4
  mod1 <- lmer(mpg ~ disp + (1|cyl), mtcars)

  p1 <- mod1 %>% VarCorr %>% as.data.frame %>% 
    with(vcov[grp == "cyl"] / sum(vcov))  

  # lmm1
  cyl <- with(mtcars, factor(cyl))
  R <- Matrix(sapply(cyl, function(x) as.numeric(x == cyl)))
  mod2 <- lmm1(mpg ~ disp, mtcars, varcov = R)
    
  p2 <- mod2$lmm$r2

  expect_equal(p1, p2, tolerance = 1e-4) 
})

test_that("mtcars example: fixed effect", {
  # inc
  stopifnot(require(Matrix))
  stopifnot(require(lme4))

  # data
  data(mtcars)
    
  # lme4
  mod1 <- lmer(mpg ~ disp + (1|cyl), mtcars)

  b1 <- fixef(mod1)[["disp"]]
  bvar1 <- vcov(mod1)["disp", "disp"]
  
  # lmm1
  cyl <- with(mtcars, factor(cyl))
  R <- Matrix(sapply(cyl, function(x) as.numeric(x == cyl)))
  mod2 <- lmm1(mpg ~ disp, mtcars, varcov = R)
    
  b2 <- coef(mod2)[["disp"]]
  bvar2 <- vcov(mod2)["disp", "disp"]
    
  expect_equal(b1, b2, tolerance = 1e-5) 
  expect_equal(bvar1, bvar2, tolerance = 1e-6)   
})
