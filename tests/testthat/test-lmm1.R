context("lmm1")

test_that("mtcars example", {
  # inc
  stopifnot(require(Matrix))
  stopifnot(require(lme4))
  stopifnot(require(lme4qtl))    

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

  # lme4qtl
  ids <- as.character(1:nrow(mtcars))
  mtcars$id <- ids
    
  rownames(R) <- ids
  colnames(R) <- ids
    
  mod3 <- relmatLmer(mpg ~ disp + (1|id), mtcars, relmat = list(id = R))

  p3 <- mod3 %>% VarCorr %>% as.data.frame %>% 
    with(vcov[grp == "id"] / sum(vcov))  
 
  expect_equal(p1, p2, p3, tolerance = 1e-4) 
})
