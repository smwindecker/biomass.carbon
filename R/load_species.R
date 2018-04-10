#' Prepare species data
#'
#' @param species_data raw species data
#' @return prepared species data
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#'
#' @export

load_species <- function(species_data) {
  
  species <- read.csv(species_data, header = T) %>%
    dplyr::filter(species_code != 'AA')
  
  species
  
}