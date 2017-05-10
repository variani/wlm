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
wlm <- function(formula, data, ..., varcov = NULL, transform = NULL,
  dtol = decompose_tol(), dmethod = decompose_method(),
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  missing_transform <- missing(transform)
  missing_varcov <- missing(varcov)
    
  ### extract model/response matrices
  X <- model.matrix(formula, data)
  Y <- model.extract(model.frame(formula, data), "response")
  
  nobs <- nrow(X)
  
  ### check
  if(missing_transform) {
    if(class(varcov)[1] != "list") {
      if(nrow(varcov) != nobs || ncol(varcov) != nobs) {
        stop("varcov dimension")
      }
    }  
  } else {
    if(nrow(transform) != nobs || ncol(transform) != nobs) {
      stop("transform dimension")
    }
  }
  
  ### process `varcov` argument
  if(missing_transform) {
    decompose <- decompose_varcov(varcov, method = dmethod, tol = dtol, output = "all")
  } else {
    decompose <- list(transform = transform)
  }
  
  ### create new `formula` and `data`
  names_x <- colnames(X)

  names_x <- gsub("\\(|\\)", "", names_x) # rename the intecept: `(Intercept)`
  names_x <- gsub("\\:", "_", names_x) # rename inteaction terms: `g:e`  
  colnames(X) <- names_x
  
  names_y <- as.character(formula)[2]
  tr_formula <- formula(paste(names_y, "~ -1 +", paste(names_x, collapse = " + ")))
  
  dat_Y <- as.data.frame(as.matrix(crossprod(decompose$transform, Y)))
  names(dat_Y) <- names_y
  
  dat_X <- as.data.frame(as.matrix(crossprod(decompose$transform, X)))
    
  tr_data <- cbind(dat_Y, dat_X)
  
  ### call lm
  if(verbose) {
    cat(" * tr_formula:\n")
    print(tr_formula)
  }
  mod <- lm(tr_formula, tr_data, ...)

  oldClass(mod) <- c("wlm", oldClass(mod))
  
  if(missing_varcov) {
    mod$varcov <- varcov
  }
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

