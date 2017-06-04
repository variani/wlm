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
wlm <- function(formula, data, ..., ids = NULL, varcov = NULL, transform = NULL,
  dtol = decompose_tol(), dmethod = decompose_method(),
  verbose = 0)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
  
  ### args
  missing_transform <- missing(transform)
  missing_varcov <- missing(varcov)
  
  ### convert `data` to data.fame
  # - data_frame has no row names
  data <- as.data.frame(data)
  
  ### ids
  if(!is.null(ids)) {
    rownames(data) <- ids
  } else {
    if(is.null(rownames(data))) {
      ids <- as.character(1:nrow(data))
    } else {
      ids <- rownames(data)
    }
  }
  stopifnot(!any(duplicated(ids)))
    
  ### extract model/response matrices
  X <- model.matrix(formula, data)
  Y <- model.extract(model.frame(formula, data), "response")
  
  nobs_data <- nrow(data)
  nobs_model <- nrow(X)
  nobs_omit <- nobs_data - nobs_model
  
  obs_model <- which(ids %in% rownames(X))
  obs_omit <- which(!(ids %in% rownames(X)))
  
  ids_model <- ids[obs_model]
      
  ### check
  if(missing_transform) {
    if(!(class(varcov) %in% c("list", "eigen"))) {
      if(nrow(varcov) < nobs_model) {
        stop("varcov dimension: nrow(varcov) < nobs_model")   
      }
      if(!is.null(rownames(varcov))) {
        ids_varcov <- rownames(varcov)
        stopifnot(all(ids_varcov %in% ids))
          
        ind <- sapply(ids_model, function(x) which(x == ids_varcov))
        varcov <- varcov[ind, ind]
      } else {
        if(nrow(varcov) == nobs_data) {
          varcov <- varcov[obs_model, obs_model]
        } else {
          stop("varcov dimension: no rownames & nrow(varcov) != nobs_data")
        }
      }
    }  
  } else {
    if(nrow(transform) != nobs_model) {
      print(nrow(transform))
      print(nobs_model)
      stop("transform dimension: transform) != nobs_model")
    } else {
      if(nobs_omit) {
        stopifnot(!is.null(rownames(transform)))
      
        ids_transform <- rownames(transform)
        stopifnot(all(ids_transform %in% ids_model))

        ind <- sapply(ids_model, function(x) which(x == ids_transform))
      
        transform <- transform[ind, ind]
      }
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

