
# RAW THERMOGRAMS AND CARBON PROPORTIONS FROM TGA
## many of them are improved. still issues? maybe reduce allowable skew even further? 
## it's much closer
## what needs to be done to prepare? 
## maybe read those articles? 
## and 


library(dplyr)
library(stringr)
library(plyr)
library(broom)
library(magrittr)
library(tidyr)
library(purrr)
library(ggplot2)
library(minpack.lm)
library(reshape2)

library(deconvolve)

tga_files = list.files(pattern = '*_TGA.csv', recursive = T)

fractions <- lapply(tga_files, function(x) {
  
  dat = read.csv(x, header = F, skip = 29)
  
  # Add column names
  names(dat) = c('temp', 'time', 'mass_loss')
  
  # Add a column with the species code
  dat$sp_code = substr(x, 9, 10)
  
  init_mass = read.csv(x, nrows = 1, header = F, skip = 17)
  
  # get individual value then add to df.
  init_mass = init_mass[1,2]
  
  munge <- process(dat, 'temp', 'mass_loss', init_mass, 'C')
  output <- deconvolve(munge)
  
  frac <- data.frame(t(output$mass_fractions))
  frac$species_code <- ModData(output)[1, 'sp_code']
  
  png(paste0('output/tga_fs_new/', frac$species[1], '.png'), 4, 4, 'in', res = 100)
  plot(output)
  dev.off()
  
  return(frac)
  
})


all_fractions <- bind_rows(fractions)




