context("lmm1lr")

test_that("mtcars example: lmm1lr vs. lmm1", {
  # inc
  stopifnot(require(lme4))

  # data
  data(mtcars)
  mtcars <- within(mtcars, {
    cyl <- factor(cyl)
  })

  Z <- model.matrix(~ cyl - 1, mtcars)
  G <- tcrossprod(Z)
    
  # lme4
  m1 <- lmer(mpg ~ disp + (1|cyl), mtcars)

  p1 <- m1 %>% VarCorr %>% as.data.frame %>% 
    with(vcov[grp == "cyl"] / sum(vcov))  

  # lmm1
  m2 <- lmm1(mpg ~ disp, mtcars, varcov = G)
    
  p2 <- m2$lmm$r2

  # lmm1lr
  m3 <- lmm1lr(mpg ~ disp, mtcars, zmat = Z)
    
  p3 <- m3$lmm$r2
  
  expect_equal(p1, p2, tolerance = 1e-4) 
  expect_equal(p2, p3, tolerance = 1e-4) 
})

test_that("mtcars example: estimates by lmm1lr", {
  # inc
  stopifnot(require(lme4))

  # data
  data(mtcars)
  mtcars <- within(mtcars, {
    cyl <- factor(cyl)
  })

  Z <- model.matrix(~ cyl - 1, mtcars)
  #G <- tcrossprod(Z)
    
  # lme4
  m1 <- lmer(mpg ~ disp + (1|cyl), mtcars)
  coef1 <- summary(m1) %$% coefficients %>% as.data.frame
  
  # lmm1
  #m2 <- lmm1(mpg ~ disp, mtcars, varcov = G)
    
  # lmm1lr
  m3 <- lmm1lr(mpg ~ disp, mtcars, zmat = Z)
  coef3 <- m3$coef
  
  expect_true(all.equal(coef1$Estimate, coef3$estimate, tol = 1e-4)) 
  expect_true(all.equal(coef1[["Std. Error"]], coef3$se, tol = 1e-4)) 
})

test_that("lmm1lr: starting values", {
  # data
  data(mtcars)
  mtcars <- within(mtcars, {
    cyl <- factor(cyl)
  })
  Z <- model.matrix(~ cyl - 1, mtcars)

  # models
  m1 <- lmm1lr(mpg ~ disp, mtcars, zmat = Z)
  r2_est <- m1$lmm$r2
  m2 <- lmm1lr(mpg ~ disp, mtcars, zmat = Z, start = r2_est)
})
