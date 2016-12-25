#-------------------------
# Main function `wlm`
#-------------------------

#' @export
wlm <- function(formula, data, ..., varcov, tol = 1e-10)
{
  ### call
  mc <- match.call()
  env <- parent.frame(1)
   
  ### extract model/response matrices
  X <- model.matrix(formula, data)
  Y <- model.extract(model.frame(formula, data), "response")
  
  nobs <- nrow(X)
  
  ### process `varcov` argument
  A <- decompose_varcov(varcov)
  
  ### create new `formula` and `data`
  names_x <- colnames(X)

  names_x <- gsub("\\(|\\)", "", names_x)
  colnames(X) <- names_x
  
  names_y <- as.character(formula)[2]
  tr_formula <- formula(paste(names_y, "~ -1 +", paste(names_x, collapse = " + ")))
  
  dat_Y <- as.data.frame(A %*% Y)
  names(dat_Y) <- names_y
  
  dat_X <- as.data.frame(A %*% X)
    
  tr_data <- cbind(dat_Y, dat_X)
  
  ### call lm
  mod <- lm(tr_formula, tr_data, ...)

  oldClass(mod) <- c("wlm", oldClass(mod))
  
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

