context("matrix decomposition")

test_that("weight matrix", {
  # data
  V <- diag(rep(1:3, each = 2))
  
  # decompose  
  Sti <- decompose_varcov_evd(V)
  St <- solve(Sti) 
  
  expect_equal(crossprod(St), V)  
})

test_that("AR", {
  # data
  rho <- 0.31041 
  V <- diag(5)
  V <- rho^abs(row(V) - col(V))
  
  # decompose  
  Sti <- decompose_varcov_evd(V)
  St <- solve(Sti) 
  
  expect_equal(crossprod(St), V)
})

test_that("decompose_varcov_chol on nuclear family", {
  # data
  V <- matrix(0.5, 5, 5)

  diag(V) <- 1

  V[1, 2] <- 0
  V[2, 1] <- 0  
  
  # decompose  
  Sti <- decompose_varcov_chol(V)
  St <- solve(Sti) 
  
  expect_equal(crossprod(St), V)
})

test_that("decompose_varcov_evd on nuclear family", {
  # data
  V <- matrix(0.5, 5, 5)

  diag(V) <- 1

  V[1, 2] <- 0
  V[2, 1] <- 0  
  
  # decompose  
  Sti <- decompose_varcov_evd(V)
  St <- solve(Sti) 
  
  expect_equal(crossprod(St), V)
})

test_that("(error) decompose_varcov_chol on family effect", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  val <- rep(1:3, each = 2)
  V <- Matrix(sapply(val, function(x) x * as.numeric(x == val)))

  # decompose  
  expect_error(suppressWarnings(decompose_varcov_chol(V)))
})

test_that("decompose_varcov_evd on family effect", {
  # inc
  stopifnot(require(Matrix))
  
  # data
  val <- rep(1:3, each = 2)
  V <- Matrix(sapply(val, function(x) x * as.numeric(x == val)))

  # decompose  
  Sti <- decompose_varcov_evd(V)
  #St <- solve(Sti) 
  
  expect_true(class(Sti) == "matrix")
})

