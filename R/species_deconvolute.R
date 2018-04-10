#' Deconvolute species' TGA data
#'
#' @param list_item split species row as list item
#' @param tga_folder source folder for tga data
#' @return saved deconvolved plot and list of parameters and weights
#' @importFrom deconvolve process deconvolve fs_mixture
#' @importFrom reshape2 dcast
#' @importFrom utils read.csv
#'
#' @export

species_deconvolute <- function (list_item, tga_folder) {
  
  # extract species code
  x <- as.character(list_item$species_code)
  n_curves <- list_item$n_curves
  
  # turn NA's into NULL's
  if (is.na(n_curves)) n_curves = NULL
  
  # read raw TGA
  file <- paste0(tga_folder, x, '_TGA.csv')
  tmp <- process_raw_tga(file)
  
  # deconvolute TGA data
  output <- deconvolve::deconvolve(tmp, upper_temp = 650, n_curves = n_curves)
  
  # extract weights of components
  mean_weights <- data.frame(t(output$mean_weights))
  mean_weights$wt_type <- 'mean'
  
  # extract confidence intervals of component weights
  ci_weights <- data.frame(output$CI_weights)
  ci_weights$wt_type <- rownames(ci_weights)
  
  # combine means and confidence intervals of weights
  weights <- rbind(mean_weights, ci_weights)
  
  if (ncol(weights) == 4) {
    weights$HC_1 <- NA
    colnames(weights)[1] <- 'HC_2'
  }
  
  # set species of weight outputs
  weights$species_code <- x
  weights <- weights[, c('species_code', 'HC_1', 'HC_2', 'CL', 'LG', 'wt_type')]
  
  # set species of parameter outputs and make data wide
  params$species_code <- x
  params$parameter <- row.names(params)
  colnames(params) <- c('coefficient', 'species_code', 'parameter')
  params <- reshape2::dcast(params, species_code ~ parameter, value.var = 'coefficient')
  
  return(list(species_code = x, 
              output = output, 
              weights = weights, 
              params = params))
  
}