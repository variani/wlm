#-------------------------
# Compuare relationship matrices
#-------------------------

#' @export
compute_relmat <- function(mat, center = TRUE, scale = TRUE,
  ids) 
{
  ### prepare the matrix of genotypes: to be centered / scaled
  if(class(mat)[1] != "matrix") {
    mat <- as.matrix(mat)
  }
  
  mat <- scale(mat, center = center, scale = scale)

  ### var
  M <- ncol(mat)
  N <- ncol(mat)
  
  ### compute the var-covar matrix
  relmat <- tcrossprod(mat) / M
  
  ### ids
  if(missing(ids)) {
   ids <- as.character(1:N)
  } 
    
  rownames(relmat) <- ids
  colnames(relmat) <- ids 
  
  return(relmat)
}











