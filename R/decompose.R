
#-----------------------------
# Decompose var-covar matrix 
#----------------------------

#' The default tolerance for EVD-based decomposition.
#'
#' Tolerance for \code{decompose_varcov_evd} function.
#'
#' @return The default value.
#' @export
decompose_tol <- function() 1e-10

#' Decomposition of the var-covar matrix.
#'
#' @param varcov Variance-covariance (relative) matrix of residuals.
#' @param method One of the following method for decomposition:
#'    \code{"evd"}, \code{"chol_evd"} or \code{"chol"}.
#' @param tol Tolerance for \code{decompose_varcov_evd}.
#'    The default value is output of \code{decompose_tol} function.
#' @return Transformation matrix (in its trasnposed form) needed to pass from WLS to OLS.
#' @export
decompose_varcov <- function(varcov, method = c("evd", "chol_evd", "chol"), tol = decompose_tol())
{
  method <- match.arg(method)
  
  switch(method,
    "chol_evd" = {
      A <- try(decompose_varcov_chol(varcov))
      if(class(A)[1] == "try-error") {
        A <- decompose_varcov_evd(varcov, tol = tol)
      }
      
      return(A)
    },
    "chol" = decompose_varcov_chol(varcov),
    "evd" = decompose_varcov_evd(varcov, tol = tol),
    stop("switch"))
}
  
decompose_varcov_chol <- function(varcov)
{
  R <- chol(varcov)
  At <- backsolve(R, diag(nrow(R))) 

  return(At)
}

decompose_varcov_evd <- function(varcov, tol = decompose_tol())
{
  if(class(varcov) == "list") {
    if(all(c("values", "vectors") %in% names(varcov))) {
      At <- with(varcov, t(vectors) %*% diag(1/sqrt(values)) %*% vectors)
    } else {
      stop("`varcov` is a list, but it is not output from `eigen`")
    }
  } else {
    evd <- eigen(varcov, symmetric = TRUE)
    
    ind <- which(abs(evd$values) < tol)
    if(length(ind) > 0) {
      evd$values <- evd$values[-ind]
      evd$vectors <- evd$vectors[, -ind]
    }
    
    At <- t(with(evd, vectors %*% diag(1/sqrt(values)) %*% t(vectors)))
    
    ind <- (abs(At) < tol)
    At[ind] <- 0
  }
  
  return(At)
}
  
