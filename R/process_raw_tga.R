#' Load and process raw TGA data
#'
#' @param raw_file TGA raw data
#' @return processed TGA data
#' @importFrom deconvolve process
#'
#' @export

process_raw_tga <- function(raw_file) {
  
  df <- read.csv(raw_file, header = FALSE, skip = 29)
  names(df) <- c('temp', 'time', 'mass_loss')
  init_mass <- read.csv(raw_file, nrows = 1, header = FALSE, skip = 17)[1,2]
  munge <- deconvolve::process(df, 'temp', 'mass_loss', init_mass)
  
  munge
  
}