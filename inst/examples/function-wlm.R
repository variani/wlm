# @ http://www.biostat.jhsph.edu/~iruczins/teaching/jf/ch5.pdf

# data
data(longley)
nobs <- nrow(longley)

rho <- 0.64417
V <- diag(nobs)
V <- rho^abs(row(V) - col(V))
  
mod <- wlm(Employed ~ GNP + Population, data = longley, varcov = V)
