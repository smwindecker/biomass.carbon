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