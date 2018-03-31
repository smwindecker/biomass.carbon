#' Prepare species' TGA data
#'
#' @param species_data species detail file with growth form and full species name info
#' @param output_folder output folder
#' @importFrom dplyr bind_rows
#' @return saved deconvolved plot and weight
#'
#' @export

prep_tga <- function (species_data, output_folder) {
  
  #species_data <- species
  species <- as.data.frame(unique(as.character(species_data$species_code))) %>%
    `colnames<-`('species_code')

  # set up those with x axis labels
  species$xaxis <- FALSE
  xaxis_list <- c('BB', 'DD', 'G', 'JJ', 'KK', 'LL', 'MM', 'NN', 'S', 'V')
  species$xaxis[species[, 1] %in% xaxis_list] <- TRUE
  
  # set up those species with y axis labels
  species$yaxis <- FALSE
  yaxis_list <- c('BB', 'FF', 'H', 'II', 'J', 'JJ', 'K', 'L', 'LL', 'NN', 'Q', 'V')
  species$yaxis[species[, 1] %in% yaxis_list] <- TRUE
  
  # speices number of curves for those that need
  species$n_curves <- NULL
  three <- c('KK', 'M', 'X')
  species$n_curves[species[, 1] %in% three] <- 3
  species$n_curves[species[, 1] == 'CC'] <- 4
  
  # split each row into items of list
  species_list <- lapply(split(species, seq(nrow(species))), as.list)
  
  # apply species_deconvolute function to each list item 
  spp_deconvolve <- lapply(species_list[[1:2]], species_deconvolve, 
                           species_data = species_data,
                           output_folder = output_folder)
  
  # collate parameters and save tables
  tga_param_table(species_deconvolved_list = spp_deconvolve, 
                  species_data = species_data, 
                  output_folder = output_folder)
  
  # combine weight estimates from all deconvoluted species data
  weight_estimates <- dplyr::bind_rows(lapply(1:length(spp_deconvolve), function(x) {
    return(spp_deconvolve[[x]]$weights)
  }))
  
  weight_estimates
  
}