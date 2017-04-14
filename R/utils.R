
#' @export
mod_varcov <- function(mod)
{
  ### inc
  stopifnot(requireNamespace("lme4"))
  
  ### arg
  stopifnot(class(mod)[1] %in% c("lmerMod", "glmerMod"))
  
  # estimate varcov matrix
  n <- lme4::getME(mod, "n")
  s2 <- sigma(mod)^2 
  varcov <-  s2 * (tcrossprod(crossprod(lme4::getME(mod, "Zt"), 
    lme4::getME(mod, "Lambda"))) + Diagonal(n))

  return(varcov)
}

#' @export
mod_varcov_transform <- function(mod, tol = 1e-10)
{
  # varcov
  varcov <- mod_varcov(mod)
  
  # decompose
  decomp <- decompose_varcov_evd(varcov, output = "all")

  # compute transform
  transform <- decomp$transform
  transform[abs(transform) < tol] <- 0
  transform <- Matrix(transform)
  
  return(transform)
}
