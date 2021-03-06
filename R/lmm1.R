#-------------------------
# Main function `lmm1`
#-------------------------

#' LMM with a single random effect and residual random effect.
#'
#' @export
lmm1 <- function(formula, data, varcov, REML = TRUE, ..., 
  dtol = decompose_tol(), dmethod = "evd",
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  stopifnot(!missing(varcov))
  stopifnot(dmethod == "evd")

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
  if(!(class(varcov)[1] %in% c("list", "eigen"))) {
    if(nrow(varcov) != nobs_data || ncol(varcov) != nobs_data) {
      stop("varcov dimension")
    } else {
      if(!is.null(rownames(varcov))) {
        ids_varcov <- rownames(varcov)
        stopifnot(all(ids_varcov %in% ids))
          
        ind <- sapply(ids_model, function(x) which(x == ids_varcov))
        varcov <- varcov[ind, ind]
      } else {
        varcov <- varcov[obs_model, obs_model]
      }
    }
  }  
  
  ### process `varcov` argument & compute `decompose`!
  decompose <- decompose_varcov(varcov, method = dmethod, tol = dtol,
    output = "evd")
  stopifnot(length(decompose$values) == nobs_model)
  
  ### rotated y, X
  X_rotated <- crossprod(decompose$vectors, X)
  y_rotated <- as.numeric(crossprod(decompose$vectors, y))
  
  ### optimize
  out <- optimize(lmm1_compute_wls_ll, c(0, 1), 
    y = y_rotated, X = X_rotated, evalues = decompose$values, REML = REML, 
    maximum = TRUE) # , tol=tol
  
  r2 <- out$maximum
  ll <- out$objective
  
  ### call WLS
  decompose_varcov <- list(vectors = decompose$vectors,
    values = r2 * decompose$values + (1 - r2))
  
  mod <- wlm(formula, data, ..., varcov = decompose_varcov)
  
  ### return
  mod$lmm <- list(r2 = r2, ll = ll, REML = REML)
  
  return(mod)
}

#-------------------------
# Support functions `lmm1`
#-------------------------

lmm1_compute_wls_ll <- function(r2, y, X, evalues, REML = TRUE)
{
  n <- nrow(X)
  p <- ncol(X)

  ### weights in the error^2 term: W = diag(w)
  w_error2 <- r2 * evalues + (1 - r2)
  w <- 1 / w_error2
  W_inv <- diag(sqrt(w)) 

  # Update using weights
  y_uncorr <- as.numeric(crossprod(W_inv, y))
  X_uncorr <- crossprod(W_inv, X)
 
  # Estimate beta
  XX_uncorr_inv <- chol2inv(chol(crossprod(X_uncorr)))
  beta <- as.numeric(tcrossprod(y_uncorr, tcrossprod(XX_uncorr_inv, X_uncorr)))
  
  # Derive model statistics
  #r <- as.numeric(y_uncorr - tcrossprod(beta, X_uncorr))
  #rss <- sum(r^2)  
  r <- as.numeric(y - tcrossprod(beta, X))
  rss <- sum(w * r^2)
  
  if(!REML) {
    N <- n
    sigma2 <- rss / N

    ll <- -0.5 * (-sum(log(w)) + N * (log(2 * pi * sigma2) + 1)) 
  } else {
    N <- n - p
    sigma2 <- rss / N

    XX <- crossprod(X)
    logdetXX <- log(det(XX))

    XWX <- crossprod(crossprod(diag(sqrt(w)), X))
    logdetXWX <- log(det(XWX))

    ll <- -0.5 * (-sum(log(w)) + N * (log(2 * pi * sigma2) + 1) + logdetXWX - logdetXX)
  }
  
  return(ll)
}












