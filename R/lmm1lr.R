#-------------------------
# Main function `lmm1`
#-------------------------

#' LMM with a single random effect and residual random effect.
#'
#' @export
lmm1lr <- function(formula, data, zmat, REML = TRUE, 
  store_mat = FALSE,
  ..., 
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  stopifnot(!missing(zmat))

  ### ids
  if(is.null(rownames(data))) {
    ids <- as.character(1:nrow(data))
  } else {
    ids <- rownames(data)
  }
  stopifnot(!any(duplicated(ids)))
      
  ### extract model/response matrices
  X <- model.matrix(formula, data)
  y <- model.extract(model.frame(formula, data), "response")
  
  nobs_data <- nrow(data)
  nobs_model <- nrow(X)

  obs_model <- which(rownames(X) %in% ids)
  obs_omit <- which(!(rownames(X) %in% ids))

  ids_model <- ids[obs_model]
  
  ### check
  if(nrow(zmat) != nobs_data) {
    stop("zmat dimension")
  } else {
    if(!is.null(rownames(zmat))) {
      ids_zmat <- rownames(zmat)
      stopifnot(all(ids_zmat %in% ids))
          
      ind <- sapply(ids_model, function(x) which(x == ids_zmat))
      zmat <- zmat[ind, ]
    } else {
      zmat <- zmat[obs_model, ]
    }
  }
  
  ### optimize
  out <- optimize(lmm1_compute_lowrank_ll, c(0, 1), 
    y = y, X = X, Z = zmat, REML = REML, 
    maximum = TRUE)
  
  r2 <- out$maximum
  ll <- out$objective
  
  ### estimates
  est <- lmm1lr_effects_naive(gamma = r2, y = y, X = X, Z = zmat, REML = REML)
  
  coef <- data.frame(estimate = est$b, se = sqrt(diag(est$bcov)))
  
  ### return
  mod <- list(nobs_data = nobs_data, nobs_model = nobs_model,
    obs_model = obs_model, obs_omit = obs_omit,
    gamma = r2, s2 = est$s2,
    est = est, coef = coef,
    REML = REML, store_mat = store_mat)
  
  if(store_mat) {
    mod <- c(mod, list(y = y, X = X, Z = zmat))
  }
  
  mod$lmm <- list(r2 = r2, ll = ll, REML = REML)
  
  return(mod)
}

#-------------------------
# LogLik computation 
#-------------------------

lmm1lr_effects_naive <- function(model, gamma, y, X, Z, s2, REML = TRUE)
{
  ### args 
  missing_model <- missing(model)
  missing_s2 <- missing(s2)
  
  if(missing_model) {
    n <- length(y)
    k <- ncol(X)
    nk <- ifelse(REML, n - k, n)
    
    if(missing_s2) {
      # copmute effect sizes (`b`) with scaled V = gamma ZZ' + (1-gamma) I
      comp <- c(gamma, 1 - gamma)
  
      XV <- crossprod_inverse_woodburry(comp, Z, X) # crossprod(X, Sigma_inv)
      XVX <- XV %*% X
  
      b <- as.numeric(solve(XVX) %*% (XV %*% y))
  
      # comptes SE taking into account `s2`: V = s2 (gamma ZZ' + (1-gamma) I)
      r <- as.numeric(y - X %*% b)
      yPy <- crossprod_inverse_woodburry(comp, Z, r) %*% r # crossprod(r, Sigma_inv) %*% r
      s2 <- as.numeric(yPy / nk)
  
      comp <- s2 * c(gamma, 1 - gamma)
  
      XV <- crossprod_inverse_woodburry(comp, Z, X) # crossprod(X, Sigma_inv)
      XVX <- XV %*% X
      bcov <- solve(XVX)
    } else {
      comp <- s2 * c(gamma, 1 - gamma)
      XV <- crossprod_inverse_woodburry(comp, Z, X) # crossprod(X, Sigma_inv)
      XVX <- XV %*% X
      XVX_inv <- solve(XVX)
      
      b <- as.numeric(XVX_inv %*% (XV %*% y))
      bcov <- XVX_inv
    }
  } else {
    stop("not implemented")
  }
  
  ### return
  out <- list(s2 = s2, b = b, bcov = bcov)
}


#-------------------------
# LogLik computation 
#-------------------------

lmm1_compute_lowrank_ll <- function(gamma, y, X, Z, REML = TRUE)
{
  n <- length(y)
  k <- ncol(X)
  
  nk <- ifelse(REML, n - k, n)
  
  comp <- c(gamma, 1 - gamma)
  
  Sigma_det_log <- log_det_decomp(comp, Z) 
  
  XV <- crossprod_inverse_woodburry(comp, Z, X) # crossprod(X, Sigma_inv)
  XVX <- XV %*% X
  
  b <- solve(XVX) %*% (XV %*% y) 
  
  r <- as.numeric(y - X %*% b)
  yPy <- crossprod_inverse_woodburry(comp, Z, r) %*% r # crossprod(r, Sigma_inv) %*% r
  s2 <- yPy / nk

  ll <- -0.5*nk*(log(2*pi*s2) + 1) - 0.5*Sigma_det_log
  if(REML) {
    log_det_XVX <- determinant(XVX, log = TRUE)
    #log_det_XX <- determinant(crossprod(X), log = TRUE)
    
    ll <- ll - 0.5*as.numeric(log_det_XVX$modulus)
  }
  
  return(ll)
}

lmm1_compute_naive_ll <- function(gamma, y, X, G, REML = TRUE)
{
  n <- length(y)
  k <- ncol(X)
  
  nk <- ifelse(REML, n - k, n)
  
  Sigma <- gamma*G + (1 - gamma)*diag(n)
  Sigma_inv <- solve(Sigma)
  Sigma_det_log <- as.numeric(determinant(Sigma, log = TRUE)$modulus)
  
  XV <- crossprod(X, Sigma_inv)
  XVX <- XV %*% X
  b <- solve(XVX) %*% (XV %*% y) 
  
  r <- as.numeric(y - X %*% b)
  yPy <- crossprod(r, Sigma_inv) %*% r
  s2 <- yPy / nk

  ll <- -0.5*nk*(log(2*pi*s2) + 1) - 0.5*Sigma_det_log
  if(REML) {
    log_det_XVX <- determinant(XVX, log = TRUE)
    #log_det_XX <- determinant(crossprod(X), log = TRUE)
    
    ll <- ll - 0.5*as.numeric(log_det_XVX$modulus)
  }
  
  return(ll)
}

#-------------------------
# Support functions of linear algebra 
#-------------------------

### efficient inverse matrix calc.
inverse_woodburry <- function(comp, Z)
{ 
  # (1) Formula from https://en.wikipedia.org/wiki/Woodbury_matrix_identity
  # (A + UCV)- = A- - A- U (C- + VA-U)-VA-
  # (D + ZHZ')- = D- - D-Z (H- + Z'D-Z)-Z'D- = D- - D-ZL-Z'D-
  # where D = diag(comp1), H = diag(comp2), L = (H- + Z'D-Z)
  n <- nrow(Z)
  k <- ncol(Z)
  
  Li <- solve(diag(k) / comp[1] + crossprod(Z) / comp[2])
  diag(n) / comp[2] - Z %*% tcrossprod(Li, Z) / (comp[2] * comp[2])
}

### efficient calc. of X'V
crossprod_inverse_woodburry <- function(comp, Z, X)
{ 
  n <- nrow(Z)
  k <- ncol(Z)
  
  Li <- solve(diag(k) / comp[1] + crossprod(Z) / comp[2])
  t(X) / comp[2] - crossprod(X, Z) %*% tcrossprod(Li, Z) / (comp[2] * comp[2])
}

### efficient det calc.
log_det_decomp <- function(comp, Z)
{ 
  # (1) General formula from https://en.wikipedia.org/wiki/Matrix_determinant_lemma
  # |A + UCV| = |L| |C| |A|
  # where L = C- + VA-U
  # (2) For our specific case:
  # |D + ZHZ'| = |L| |H| |D|
  # D = diag(comp1), H = diag(comp2), L = (H- + Z'D-Z)
  n <- nrow(Z)
  k <- ncol(Z)
  
  L <- diag(k) / comp[1] + crossprod(Z) / comp[2]
  log_det_L <- determinant(L, log = TRUE)
  
  as.numeric(log_det_L$modulus) + n*log(comp[2]) + k*log(comp[1])
}











