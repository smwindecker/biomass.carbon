
# RAW THERMOGRAMS AND CARBON PROPORTIONS FROM TGA

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

source('R/paramSelect.R')
source('R/fraserSuzuki.R')

tga_files = list.files(pattern = '*_TGA.csv', recursive = T)

tga_data <- lapply(tga_files, function(x) {

  dat = read.csv(x, header = F, skip = 29)

  # Add column names
  names(dat) = c('temp', 'time', 'mass_loss')

  # Add a column with the species code
  dat$sp_code = substr(x, 9, 10)

  init_mass = read.csv(x, nrows = 1, header = F, skip = 17)

  # get individual value then add to df.
  init_mass = init_mass[1,2]

  dat$init_mass = init_mass

  return(dat)

})

tga_prep <- bind_rows(tga_data)

tga_prep$species_code <- as.factor(gsub('_', '', tga_prep$sp_code))
tga <- tga_prep[ ,c(1:3,5,6)]

tga$K <- tga$temp + 273
tga$temp_K <- round_any(tga$K, 1)

tga_1 <- tga[!duplicated(tga[,c('temp_K', 'species_code')]),]

species <- read.csv('raw/species.csv', header = TRUE)
tga_2 <- merge(tga_1[,c(3:6)], species[,c(1:2)])
tga_2$m_T <- tga_2$init_mass + tga_2$mass_loss

d <- -as.data.frame(diff(tga_2$mass_loss)/diff(tga_2$K))
x <- rep(NA, ncol(d))
deriv <- rbind(x, d)
colnames(deriv) <- 'deriv'
tga_d <- cbind(tga_2, deriv)
tga_d_trunc <- tga_d[!(tga_d$K < 400 | tga_d$K > 900),]

write.table(tga_d_trunc, 'munge/tga_derivs.txt')

# replace spaces with underscores in species names
tga_d_trunc$species <- gsub('([[:punct:]])|\\s+','_', tga_d_trunc$species)

# vector of all starting values - for optimiser
theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)
lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)

parameter_df <- data.frame()
proportion_df <- data.frame()
species_list <- unique(tga_d_trunc$species)

for (i in seq_along(species_list)) {
  
  sp.df = subset(tga_d_trunc, species == species_list[i])
  n <- nrow(sp.df)
  W <- sp.df$m_T
  
  obs <- sp.df['deriv']
  temp <- sp.df['K']
  params_opt <- paramSelect(theta, lb, fsTotal, temp, obs, restarts = 300)
  
  fs <- fs_model(sp.df, params_opt)
  fitted <- fs %>% broom::augment()
  fitted$species <- sp.df$species[1]
  results <- merge(fitted, sp.df)
  
  # get the parameter estimates
  parameters <- c(species_list[i], pEst(fs, 'h', '1'), pEst(fs, 'h', '2'), pEst(fs, 'h', '3'),
                  pEst(fs, 's', '1'), pEst(fs, 's', '2'), pEst(fs, 's', '3'),
                  pEst(fs, 'p', '1'), pEst(fs, 'p', '2'), pEst(fs, 'p', '3'),
                  pEst(fs, 'w', '1'), pEst(fs, 'w', '2'), pEst(fs, 'w', '3'))
  
  parameter_df <- rbind(parameter_df, data.frame(t(parameters), row.names = NULL))
  
  # get the proportions
  a_j <- vector(length = 3)
  
  for (j in 1:3) {
    
    f_j <- function (x) {
      
      h <- pEst(fs, 'h', j)
      s <- pEst(fs, 's', j)
      p <- pEst(fs, 'p', j)
      w <- pEst(fs, 'w', j)
      
      interior <- 2*s*((x - p) / w)
      exterior <- -log(2)/s^2
      
      ifelse(interior > -1, h*exp(exterior * (log(1 + interior)^2)), 0)
      
    }
    
    a_j[j] <- (integrate(Vectorize(f_j), lower = 400, upper = 900)$value) / (W[1] - W[n])
    
  }
  
  #a_j_list <- c(species_list[i], a_j)
  a_j_df <- data.frame(t(a_j))
  indx <- sapply(a_j_df, is.factor)
  a_j_df[indx] <- lapply(a_j_df[indx], function(x) as.numeric(as.character(x)))
  
  a_j_df$species <- species_list[i]
  a_j_df$mg_p1 <- (a_j_df[,1] * (W[1] - W[n])) / sp.df$init_mass[1]
  a_j_df$mg_p2 <- a_j_df[,2] * (W[1] - W[n]) / sp.df$init_mass[1]
  a_j_df$mg_p3 <- a_j_df[,3] * (W[1] - W[n]) / sp.df$init_mass[1]
  
  proportion_df <- rbind(proportion_df, a_j_df, row.names = NULL)
  
  png(paste0('output/tga_fs/', species_list[i], '.png'), 4, 4, 'in', res = 100)
  plot(sp.df$K, sp.df$deriv)
  curve(fsTotal2(x, pEst(fs, 'h', '1'), pEst(fs, 'h', '2'), pEst(fs, 'h', '3'),
                 pEst(fs, 's', '1'), pEst(fs, 's', '2'), pEst(fs, 's', '3'),
                 pEst(fs, 'p', '1'), pEst(fs, 'p', '2'), pEst(fs, 'p', '3'),
                 pEst(fs, 'w', '1'), pEst(fs, 'w', '2'), pEst(fs, 'w', '3')),
        from = 400, to = 900, add = TRUE, col = 'red')
  curve(fsFunc(x, pEst(fs, 'h', '1'), pEst(fs, 's', '1'), pEst(fs, 'p', '1'), pEst(fs, 'w', '1')),
        from = 400, to = 900, add = TRUE, col = 'blue')
  curve(fsFunc(x, pEst(fs, 'h', '2'), pEst(fs, 's', '2'), pEst(fs, 'p', '2'), pEst(fs, 'w', '2')),
        from = 400, to = 900, add = TRUE, col = 'green')
  curve(fsFunc(x, pEst(fs, 'h', '3'), pEst(fs, 's', '3'), pEst(fs, 'p', '3'), pEst(fs, 'w', '3')),
        from = 400, to = 900, add = TRUE, col = 'orange')
  dev.off()
  
}

colnames(proportion_df) <- c('pcomp_1', 'pcomp_2', 'pcomp_3', 'species',
                             'mg_p1', 'mg_p2', 'mg_p3')
colnames(parameter_df) <- c('species', 'h1', 'h2', 'h3', 's1', 's2',
                            's3', 'p1', 'p2', 'p3', 'w1', 'w2', 'w3')

write.table(proportion_df, 'munge/tga_proportions.txt')
write.table(parameter_df, 'munge/tga_parameters.txt')

