#' Prepare weights
#'
#' @param species_deconvoluted_list deconvoluted species data
#' @importFrom dplyr bind_rows
#' @return weights
#'
#' @export

load_tga_traits <- function (species_deconvoluted_list) {
  
  # combine weight estimates from all deconvoluted species data
  weight_estimates <- dplyr::bind_rows(lapply(1:length(species_deconvoluted_list), function(x) {
    return(species_deconvoluted_list[[x]]$weights)
  }))
  
  weight_estimates
  
}

