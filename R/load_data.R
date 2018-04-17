# Load data

#' Prepare C and N trait data
#'
#' @param leco_data raw leco data
#' @return prepared leco data
#' @importFrom utils read.csv
#'
#' @export

load_leco_traits <- function (leco_data) {
  
  # read leco data
  leco <- read.csv(leco_data, header = F, skip = 12)
  
  # exclude malformed sample and unused species. select columns for C and N
  leco <- leco[!(leco[, 1] == 'donotuse' | leco[, 1] == 'AA'), c(1,6,7)]
  colnames(leco) <- c('species_code', 'C', 'N')
  leco
  
}

#' Prepare LES traits
#'
#' @param trait_data file path for raw trait data
#' @param species_data file with species data
#' @return prepared trait data
#' @importFrom plyr ddply summarise
#' @importFrom utils read.csv
#'
#' @export

load_les_traits <- function (trait_data, species_data) {
  
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

#' Prepare species data
#'
#' @param species_data raw species data
#' @return prepared species data
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#'
#' @export

load_species <- function (species_data) {
  
  species <- read.csv(species_data, header = T) %>%
    dplyr::filter(species_code != 'AA')
  
  species
  
}

#' Wrapper for all species for TGA functions
#'
#' @param species_data species data
#' @param function_name function to apply
#' @param ... optional arguments
#' @return output of internal function
#'
#' @export

tga_wrapper <- function (species_data, function_name, ...) {

  input <- as.character(species_data$species_code)
  sapply(input, FUN = function_name, ..., simplify = FALSE, USE.NAMES = TRUE)

}

#' @param species_code species code to deconvolute
#' @param data_folder where raw data is saved
#' @importFrom deconvolve process deconvolve fs_mixture
#' @importFrom reshape2 dcast
#' @importFrom utils read.csv
#' 
#' @export

tga_deconvolute <- function (species_code, data_folder) {
  
  # extract species code
  x <- species_code
  
  three <- c('KK', 'M', 'X')
  
  if (x %in% three) {
    n_curves <- 3
  } else if (x == 'CC') {
    n_curves <- 4
  } else {
    n_curves = NULL
  }
  
  # read raw TGA
  file <- paste0(data_folder, x, '_TGA.csv')
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
  params <- as.data.frame(summary(output$minpack.lm)$coefficients[,1])
  params$species_code <- x
  params$parameter <- row.names(params)
  colnames(params) <- c('coefficient', 'species_code', 'parameter')
  params <- reshape2::dcast(params, species_code ~ parameter, value.var = 'coefficient')
  
  return(x = list(output = list(data = output$data, 
                                bounds = output$bounds, 
                                minpack.lm = output$minpack.lm, 
                                n_peaks = output$n_peaks,
                                CI_weights = output$CI_weights, 
                                mean_weights = output$mean_weights), 
                  weights = weights, 
                  params = params))
  
}

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

#' Load and process raw TGA data
#'
#' @param raw_file TGA raw data
#' @return processed TGA data
#' @importFrom deconvolve process
#' @importFrom utils read.csv
#'
#' @export

process_raw_tga <- function (raw_file) {
  
  df <- read.csv(raw_file, header = FALSE, skip = 29)
  names(df) <- c('temp', 'time', 'mass_loss')
  init_mass <- read.csv(raw_file, nrows = 1, header = FALSE, skip = 17)[1,2]
  tmp <- deconvolve::process(df, 'temp', 'mass_loss', init_mass)
  
  tmp
  
}