
rm(list = ls())
library(broom)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(minpack.lm)
library(reshape2)

source('R/paramSelect.R')

# load TGA data
tga <- read.table('munge/tga_derivs.txt')

# replace spaces with underscores in species names
tga$species <- gsub('([[:punct:]])|\\s+','_', tga$species)

# Frazer-Suzuki function
fsFunc <- function (x, h, s, p, w) {

  interior <- 2*s*((x - p) / w)
  exterior <- -log(2)/s^2

  ifelse(interior > -1, h*exp(exterior * (log(1 + interior)^2)), 0)

}

# combine the FS function three times for the three pseudo-components
fsTotal <- function (x, params) {

  fsTotalFunc <- fsFunc(x, params[1], params[4], params[7], params[10]) +
    fsFunc(x, params[2], params[5], params[8], params[11]) +
    fsFunc(x, params[3], params[6], params[9], params[12])

}

# create wrapper to separately identify the 12 parameters
fsTotal2 <- function (x, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3) {

  params <- c(h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3)
  fsTotal(x, params)

}

# vector of all starting values - for optimiser
theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)
lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)

# function to do the nls fit with the correct starting values
fs_model <- function (dataframe, params) {

  nlsLM(deriv ~ fsTotal2(K, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3),
        start = list(h1 = params[1], h2 = params[2], h3 = params[3],
                     s1 = params[4], s2 = params[5], s3 = params[6],
                     p1 = params[7], p2 = params[8], p3 = params[9],
                     w1 = params[10], w2 = params[11], w3 = params[12]),
        data = dataframe,
        control = nls.lm.control(maxiter = 1024, maxfev = 1e6))

}

# function to extract the parameter values
pEst <- function (fit, p, comp) {

  params <- fit %>% broom::tidy()
  params$parameter <- substr(params$term, 1, 1)
  params$component <- substr(params$term, 2, 2)

  value <- params$estimate[params$parameter == p & params$component == comp]
  print(value)

}

parameter_df <- data.frame()
proportion_df <- data.frame()
species_list <- unique(tga$species)

for (i in seq_along(species_list)) {

  sp.df = subset(tga, species == species_list[i])
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
