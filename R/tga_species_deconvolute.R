#' Prepare species' TGA data
#'
#' @param tga_folder folder where raw files are stored
#' @param species_data species detail file with growth form and full species name info
#' @importFrom dplyr bind_rows %>%
#' @return saved deconvolved plots and weights
#'
#' @export

tga_species_deconvolute <- function (tga_folder, species_data) {
  
  #species_data <- species
  species <- as.data.frame(unique(as.character(species_data$species_code))) %>%
    `colnames<-`('species_code')
  
  # species number of curves for those that need
  species$n_curves <- NULL
  three <- c('KK', 'M', 'X')
  species$n_curves[species[, 1] %in% three] <- 3
  species$n_curves[species[, 1] == 'CC'] <- 4
  
  # split each row into items of list
  species_list <- lapply(split(species, seq(nrow(species))), as.list)
  
  # apply species_deconvolute function to each list item 
  spp_deconvolute <- lapply(species_list[1:length(species_list)], species_deconvolute, 
                            tga_folder = tga_folder)
  
  spp_deconvolute
  
}
