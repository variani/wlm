#-------------------------
# Main function `lmm1`
#-------------------------

#' LMM with a single random effect and residual random effect.
#'
#' @export
lmm1 <- function(formula, data, ..., varcov = NULL,
  dtol = decompose_tol(), dmethod = decompose_method(),
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  stopifnot(!missing(varcov))
    
  ### extract model/response matrices
  X <- model.matrix(formula, data)
  Y <- model.extract(model.frame(formula, data), "response")
  
  nobs <- nrow(X)
  
  ### check
  if(class(varcov)[1] != "list") {
    if(nrow(varcov) != nobs || ncol(varcov) != nobs) {
      stop("varcov dimension")
    }
  }  
  
  ### process `varcov` argument
  decompose <- decompose_varcov(varcov, method = dmethod, tol = dtol,
    output = "all")
}
