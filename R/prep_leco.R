prep_leco <- function (leco_data) {
  
  leco <- read.csv(leco_data, header = F, skip = 12)
  leco <- leco[!(leco[,1] == 'donotuse' | leco[,1] == 'AA'), c(1,6,7)]
  colnames(leco) <- c('species_code', 'C', 'N')
  leco
  
}