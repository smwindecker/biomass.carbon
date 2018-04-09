#' Prepare species data
#'
#' @param species_data raw species data
#' @return prepared species data
#' @importFrom utils read.csv
#'
#' @export

prep_species <- function(species_data) {
  
  species <- read.csv(species_data, header = T) %>%
    filter(species_code != 'AA')
  
  species
  
}