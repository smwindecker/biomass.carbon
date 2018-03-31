data_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames <- c('temp', 'deriv', 'species_code')
colnames(data_all) <- colnames

large_data <- function (x) {
  
  dat <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), header = F, skip = 29)
  
  # Add column names
  names(dat) <- c('temp', 'time', 'mass_loss')
  
  # Add a column with the species code
  dat$species_code <- x
  
  init_mass <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), nrows = 1, header = F, skip = 17)
  
  # get individual value then add to df.
  init_mass <- init_mass[1,2]
  
  munge <- deconvolve::process(dat, 'temp', 'mass_loss', init_mass)
  data <- munge$data[! (data$temp < 120 | data$temp > 620), c('temp', 'deriv')]
  data$species_code <- x
  
  data_all <- rbind(data_all, data)
  
}
  
  
species_codes <- c('A', 'B', 'BB', 'C', 'CC', 'DD', 'E', 'FF', 'G', 'H', 'I', 'II',
                   'J', 'JJ', 'K', 'KK', 'L', 'LL', 'M', 'MM', 'N', 'NN', 'Q', 'R', 
                   'S', 'T', 'V', 'X', 'Z')

data_all_list <- lapply(species_codes, large_data)

df <- do.call("rbind", data_all_list)

write.csv(df, 'all_tga.csv')


