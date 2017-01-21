context("interaction")

test_that("synthetic example: marginal", {
  ### inc 
  stopifnot(require(lme4))
  stopifnot(require(Matrix))
  
  ### data simulation
  n <- 100
  size_fam <- 5
  num_fam <- n / size_fam
  
  p <- 0.3
  q <- 1 - p
  f <- 0.4
  
  beta <- 0.1
  
  set.seed(5) 
  g <- sample(c(0, 1, 2), n, prob = c(q^2, 2*p*q, p^2), replace = TRUE)
  e <- rbinom(n, 1, f)
  
  fam <- rep(1:num_fam, each = size_fam)
  rand_fam <- rep(rnorm(num_fam), each = size_fam)
  
  y <- beta * g * e +  rand_fam + rnorm(n)
  
  dat <- data.frame(y = y, g = g, e = e, fam = fam)
  
  ### marginal models
  mod1 <- lmer(y ~ g + e + (1|fam), dat)
  
  ZZt <- crossprod(getME(mod1, "Zt"))
  LLt <- crossprod(getME(mod1, "Lambdat"))
  s2 <- sigma(mod1)^2
  V <-  s2 * (tcrossprod(crossprod(getME(mod1, "Zt"), getME(mod1, "Lambda"))) + Diagonal(n))
  # beta = (XT R−1 X)−1 XT R−1 y
  
  Vinv <- chol2inv(chol(V))
  X <- getME(mod1, "X")
  XVX <- crossprod(X, Vinv) %*% X
  XVXinv <- solve(XVX)
  
  y <- getME(mod1, "y")
  beta <- as.numeric(tcrossprod(XVXinv, X) %*% Vinv %*% y)
  
  evd <- eigen(V)
  ind <- which(evd$values > 1e-10)
  
  out <- decompose_varcov(as.matrix(V), output = "all")
  Vinv <- out$vectors %*% diag(out$values) %*% t(out$vectors)

  mod2 <- wlm(y ~ g + e, dat, varcov = V)  
  
  expect_true(all((fixef(mod1) - beta) < 1e-10))
  expect_true(all((coef(mod2) - beta) < 1e-10))  
})

test_that("synthetic example: interaction", {
  ### inc 
  stopifnot(require(lme4))
  stopifnot(require(Matrix))
  
  ### data simulation
  n <- 100
  size_fam <- 5
  num_fam <- n / size_fam
  
  p <- 0.3
  q <- 1 - p
  f <- 0.4
  
  beta <- 0.1
  
  set.seed(5) 
  g <- sample(c(0, 1, 2), n, prob = c(q^2, 2*p*q, p^2), replace = TRUE)
  e <- rbinom(n, 1, f)
  
  fam <- rep(1:num_fam, each = size_fam)
  rand_fam <- rep(rnorm(num_fam), each = size_fam)
  
  y <- beta * g * e +  rand_fam + rnorm(n)
  
  dat <- data.frame(y = y, g = g, e = e, fam = fam)
  
  ### marginal models
  mod1 <- lmer(y ~ g + e + g:e + (1|fam), dat)
  
  ZZt <- crossprod(getME(mod1, "Zt"))
  LLt <- crossprod(getME(mod1, "Lambdat"))
  s2 <- sigma(mod1)^2
  V <-  s2 * (tcrossprod(crossprod(getME(mod1, "Zt"), getME(mod1, "Lambda"))) + Diagonal(n))
  # beta = (XT R−1 X)−1 XT R−1 y
  
  Vinv <- chol2inv(chol(V))
  X <- getME(mod1, "X")
  XVX <- crossprod(X, Vinv) %*% X
  XVXinv <- solve(XVX)
  
  y <- getME(mod1, "y")
  beta <- as.numeric(tcrossprod(XVXinv, X) %*% Vinv %*% y)
  
  evd <- eigen(V)
  ind <- which(evd$values > 1e-10)
  
  out <- decompose_varcov(as.matrix(V), output = "all")
  Vinv <- out$vectors %*% diag(out$values) %*% t(out$vectors)

  mod2 <- wlm(y ~ g + e + g:e, dat, varcov = V)  
  
  expect_true(all((fixef(mod1) - beta) < 1e-10))
  expect_true(all((coef(mod2) - beta) < 1e-10))  
})

