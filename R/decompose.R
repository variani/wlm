
#-----------------------------
# Decompose var-covar matrix 
#----------------------------

#' The default method for decomposition.
#'
#' @return The default decompose method.
#' @export
decompose_method <- function() "evd"

#' The default tolerance for decomposition.
#'
#' Currently, it is used only for \code{decompose_varcov_evd} function.
#'
#' @return The default tolerance value.
#' @export
decompose_tol <- function() decompose_evd_tol()

#' The default tolerance for EVD-based decomposition.
#'
#' Tolerance for \code{decompose_varcov_evd} function.
#'
#' @return The default value.
#' @export
decompose_evd_tol <- function() 1e-10

#' Decomposition of the var-covar matrix.
#'
#' @param varcov Variance-covariance (relative) matrix of residuals.
#' @param method One of the following methods for decomposition:
#'    \code{"evd"}, \code{"chol_evd"} or \code{"chol"}.
#' @param tol Tolerance for \code{decompose_varcov_evd}.
#'    The default value is output of \code{decompose_evd_tol} function.
#' @param output Type of output. The transformtion matrix is returned if \code{"transform"}.
#'    More results of decomposition are returned if \code{"all"}.
#' @return Transformation matrix (in its trasnposed form) needed to pass from WLS to OLS.
#' @export
decompose_varcov <- function(varcov, 
  method = c("evd", "chol_evd", "chol"), tol = decompose_tol(),
  output = c("transform", "all"))
{
  ### args
  method <- match.arg(method)
  output <- match.arg(output)
  
  switch(method,
    "chol_evd" = {
      A <- try(decompose_varcov_chol(varcov, output))
      if(class(A)[1] == "try-error") {
        A <- decompose_varcov_evd(varcov, tol = tol, output)
      }
      
      return(A)
    },
    "chol" = decompose_varcov_chol(varcov, output),
    "evd" = decompose_varcov_evd(varcov, tol = tol, output),
    stop("switch"))
}
  
decompose_varcov_chol <- function(varcov, output = c("transform", "all"))
{
  ### args
  output <- match.arg(output)
  
  ### compute Chol.
  R <- chol(varcov)
  At <- backsolve(R, diag(nrow(R))) 

  ### return
  out <- switch(output,
    "transform" = At,
    "all" = list(transform = At, n = ncol(varcov)),
    stop("unknown value of `output`"))
  
  return(out)  
}

decompose_varcov_evd <- function(varcov, 
  tol = decompose_evd_tol(), output = c("transform", "all"))
{
  ### args
  output <- match.arg(output)
  
  ### compute EVD
  if(class(varcov) == "list") {
    if(all(c("values", "vectors") %in% names(varcov))) {
      vectors <- varcov$vectors
      values <- varcov$values
      
      At <- vectors %*% diag(1/sqrt(values)) %*% t(vectors) # `At` is symmetric
    } else {
      stop("`varcov` is a list, but it is not output from `eigen`")
    }
  } else {
    evd <- eigen(varcov, symmetric = TRUE)
    vectors <- evd$vectors
    values <- evd$values
    
    ind <- which(abs(evd$values) < tol)
    if(length(ind) > 0) {
      values <- values[-ind]
      vectors <- vectors[, -ind]
    }
    
    At <- vectors %*% diag(1/sqrt(values)) %*% t(vectors) # `At` is symmetric
    
    ind <- (abs(At) < tol)
    At[ind] <- 0
  }
  
  ### return
  out <- switch(output,
    "transform" = At,
    "all" = list(transform = At, vectors = vectors, values = values, 
      n = ncol(varcov), p = ncol(vectors)),
    stop("unknown value of `output`"))
  
  return(out)
}
  
