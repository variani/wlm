#-------------------------
# Main function `wlm`
#-------------------------

#' GLS/WLS using lm.
#'
#' @param formula Formula.
#' @param data Data.
#' @param ... Arguments passed to \code{lm}.
#' @param varcov Variance-covariance (relative) matrix of residuals.
#' @param dtol Tolerance for \code{decompose_varcov}.
#'    The default value is output of \code{decompose_tol} function.
#' @param dmethod One of the following methods for decomposition:
#'    \code{"evd"}, \code{"chol_evd"} or \code{"chol"}.
#' @return A modified output of \code{lm}.
#'
#' @example inst/examples/function-wlm.R
#'
#' @export
wlm <- function(formula, data, ..., varcov, 
  dtol = decompose_tol(), dmethod = decompose_method())
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
   
  ### extract model/response matrices
  X <- model.matrix(formula, data)
  Y <- model.extract(model.frame(formula, data), "response")
  
  nobs <- nrow(X)
  
  ### process `varcov` argument
  decompose <- decompose_varcov(varcov, method = dmethod, tol = dtol,
    output = "all")
  
  ### create new `formula` and `data`
  names_x <- colnames(X)

  names_x <- gsub("\\(|\\)", "", names_x)
  colnames(X) <- names_x
  
  names_y <- as.character(formula)[2]
  tr_formula <- formula(paste(names_y, "~ -1 +", paste(names_x, collapse = " + ")))
  
  dat_Y <- as.data.frame(as.matrix(crossprod(decompose$transform, Y)))
  names(dat_Y) <- names_y
  
  dat_X <- as.data.frame(as.matrix(crossprod(decompose$transform, X)))
    
  tr_data <- cbind(dat_Y, dat_X)
  
  ### call lm
  mod <- lm(tr_formula, tr_data, ...)

  oldClass(mod) <- c("wlm", oldClass(mod))
  
  mod$varcov <- varcov
  mod$decompose <- decompose
  
  return(mod)
  
  # update call `mc` 
  num_elem <- length(mc)
  
  mc[[1]] <- quote(lm)
  mc[[2]] <- quote(.formula)
  mc[[3]] <- quote(.data)
  mc[[num_elem]] <- NULL  
  
  env$.formula <- formula_tr
  env$.data <- data_tr

  eval(mc, env)
}

