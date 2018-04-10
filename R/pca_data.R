#' Conduct PCA
#'
#' @param df logged traits matrix
#' @return pca output
#' @importFrom stats princomp na.omit
#'
#' @export

pca_data <- function(df) {
  
  # calculate loadings 
  prin <- stats::princomp((stats::na.omit(df)), cor = TRUE, scores = TRUE)
  
  prin
}