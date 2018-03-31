


rm(list = ls())
library(broom)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(minpack.lm)
library(reshape2)

# load TGA data
tga <- read.table('munge/tga_derivs.txt')

# replace spaces with underscores in species names
tga$species <- gsub('([[:punct:]])|\\s+','_', tga$species)

plot(tga$K, tga$m_T)

tga <- tga[tga$species_code == 'A',]

# Frazer-Suzuki function
fsFunc <- function (x, h, s, p, w) {

  interior <- 2*s*((x - p) / w)
  exterior <- -log(2)/s^2

  ifelse(interior > -1, h*exp(exterior * (log(1 + interior)^2)), 0)

}

# combine the FS function three times for the three pseudo-components
fsTotal <- function (x, hparams, sparams, pparams, wparams) {

  fsTotalFunc <- fsFunc(x, hparams[1], sparams[1], pparams[1], wparams[1]) +
  fsFunc(x, hparams[2], sparams[2], pparams[2], wparams[2]) +
  fsFunc(x, hparams[3], sparams[3], pparams[3], wparams[3])

}

# create wrapper to separately identify the 12 parameters
fsTotal2 <- function (x, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3) {

  hparams <- c(h1, h2, h3)
  sparams <- c(s1, s2, s3)
  pparams <- c(p1, p2, p3)
  wparams <- c(w1, w2, w3)

  fsTotal(x, hparams, sparams, pparams, wparams)

}

# single starting values
h <- c(.015, .013, .01)
s <- c(-.15, -.15, -.15)
p <- c(540, 600, 700)
w <- c(50, 30, 200)

# function to do the nls fit with the correct starting values
speciesModel <- function (dataframe, params) {

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
prop_list <- list()
species_list <- unique(tga$species)

theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)

for (i in seq_along(species_list)) {

  sp.df = subset(tga, species == species_list[i])
  n <- nrow(sp.df)
  W <- sp.df$m_T

  fs <- speciesModel(sp.df, theta)
  fitted <- fs %>% broom::augment()
  fitted$species <- sp.df$species[1]
  results <- merge(fitted, sp.df)

  # get the parameter estimates
  parameters <- c(species_list[i], pEst(fs, 'h', '1'), pEst(fs, 'h', '2'), pEst(fs, 'h', '3'),
                  pEst(fs, 's', '1'), pEst(fs, 's', '2'), pEst(fs, 's', '3'),
                  pEst(fs, 'p', '1'), pEst(fs, 'p', '2'), pEst(fs, 'p', '3'),
                  pEst(fs, 'w', '1'), pEst(fs, 'w', '2'), pEst(fs, 'w', '3'))

  parameter_df <- rbind(parameter_df, data.frame(t(parameters), row.names = NULL))
  colnames(parameter_df) <- c('species', 'h1', 'h2', 'h3', 's1', 's2', 's3', 'p1', 'p2', 'p3', 'w1', 'w2', 'w3')

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

  a_j_df <- as.data.frame(a_j)
  a_j_df$species <- species_list[i]
  a_j_df$component <- NA
  a_j_df$component[1] <- '1'
  a_j_df$component[2] <- '2'
  a_j_df$component[3] <- '3'
  # need to consider whether these are always going to be in the right order.. I guess yes because of the priors?

  prop_list[[j]] <- a_j_df

  png(paste0('output/tga_fs/', species_list[i], '.png'), 4, 4, 'in', res = 100)
  plot(tga$K, tga$deriv)
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

proportion_data <- dplyr::bind_rows(prop_list)

