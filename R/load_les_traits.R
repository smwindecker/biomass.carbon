#' Prepare LES traits
#'
#' @param trait_data file path for raw trait data
#' @param species_data file with species data
#' @return prepared trait data
#' @importFrom plyr ddply summarise
#' @importFrom utils read.csv
#'
#' @export

load_les_traits <- function(trait_data, species_data) {
  
  # read raw trait data
  traits <- read.csv(trait_data, header = T)
  
  # merge with the species data file
  trait <- merge(traits[!traits$species_code == 'AA',],
                 species_data[!species_data$species_code == 'AA', 
                              c('species_code', 'sp_abrev', 'species', 'family', 'plant_part', 'gf')],
                 by = c('species_code', 'plant_part'))
  
  # calculate SLA (m2/g) and DMC (mg/g) for each of ten samples
  trait$longSLA <- (trait$area/100)/trait$dry_weight
  trait$longDMC <- (trait$dry_weight*1000)/trait$wet_weight
  
  # calculate mean SLA and DMC of ten samples
  trait_1 <- plyr::ddply(trait, 'species_code', 
                         plyr::summarise, 
                         SLA = mean(longSLA), 
                         DMC = mean(longDMC))
  
  # merge with species data
  trt <- merge(trait_1, unique(trait[,c('species_code', 'sp_abrev', 'species', 'family', 'gf')]))
  trt
  
}