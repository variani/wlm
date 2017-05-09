context("lmm1")

test_that("mtcars example: variance proportion", {
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

test_that("mtcars example: fixed effect", {
  # inc
  stopifnot(require(Matrix))
  stopifnot(require(lme4))
  stopifnot(require(lme4qtl))    

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
  
  # lme4qtl
  ids <- as.character(1:nrow(mtcars))
  mtcars$id <- ids
    
  rownames(R) <- ids
  colnames(R) <- ids
    
  mod3 <- relmatLmer(mpg ~ disp + (1|id), mtcars, relmat = list(id = R))

  b3 <- fixef(mod3)[["disp"]]
  bvar3 <- vcov(mod3)["disp", "disp"]
    
  expect_equal(b1, b2, b3, tolerance = 1e-5) 
  expect_equal(bvar1, bvar2, bvar3, tolerance = 1e-6)   
})
