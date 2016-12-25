
#-----------------------------
# Decompose var-covar matrix 
#----------------------------

#' @export
decompose_varcov <- function(varcov, method = c("evd", "chol_evd", "chol"), tol = .Machine$double.eps)
{
  method <- match.arg(method)
  
  switch(method,
    "default" = {
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
  A <- t(At)
  
  return(A)
}

decompose_varcov_evd <- function(varcov, tol = .Machine$double.eps)
{
  if(class(varcov) == "list") {
    if(all(c("values", "vectors") %in% names(varcov))) {
      A <- with(varcov, vectors %*% diag(1/sqrt(values)) %*% t(vectors))
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
    
    A <- with(evd, vectors %*% diag(1/sqrt(values)) %*% t(vectors))
    
    ind <- (abs(A) < tol)
    A[ind] <- 0
  }
  
  return(A)
}
  
