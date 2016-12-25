context("matrix decomposition")

test_that("weight matrix", {
  # data
  V <- diag(rep(1:3, each = 2))
  
  # decompose  
  Si <- decompose_varcov_evd(V)
  S <- solve(Si) 
  
  expect_equal(tcrossprod(S), V)  
})

test_that("AR", {
  # data
  rho <- 0.31041 
  V <- diag(5)
  V <- rho^abs(row(V) - col(V))
  
  # decompose  
  Si <- decompose_varcov_evd(V)
  S <- solve(Si) 
  
  expect_equal(tcrossprod(S), V)      
})

test_that("decompose_varcov_chol on nuclear family", {
  # data
  V <- matrix(0.5, 5, 5)

  diag(V) <- 1

  V[1, 2] <- 0
  V[2, 1] <- 0  
  
  # decompose  
  Si <- decompose_varcov_chol(V)
  S <- solve(Si) 
  
  expect_equal(tcrossprod(S), V)
})

test_that("decompose_varcov_evd on nuclear family", {
  # data
  V <- matrix(0.5, 5, 5)

  diag(V) <- 1

  V[1, 2] <- 0
  V[2, 1] <- 0  
  
  # decompose  
  Si <- decompose_varcov_evd(V)
  S <- solve(Si) 
  
  expect_equal(tcrossprod(S), V)
})

test_that("(error) decompose_varcov_chol on family effect", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  val <- rep(1:3, each = 2)
  V <- Matrix(sapply(val, function(x) as.numeric(x == val)))

  # decompose  
  expect_error(decompose_varcov_chol(V), "not positive definite")
})

test_that("decompose_varcov_evd on family effect", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  val <- rep(1:3, each = 2)
  V <- Matrix(sapply(val, function(x) as.numeric(x == val)))

  # decompose  
  Si <- decompose_varcov_evd(V)
  S <- solve(Si) 
  
  expect_equal(as.numeric(tcrossprod(S)), as.numeric(V))        
})

