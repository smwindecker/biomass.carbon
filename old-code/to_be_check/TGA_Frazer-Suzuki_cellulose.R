
rm(list = ls())

library(broom)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# load TGA data
cell <- read.table('munge/cellulose_deriv.txt')

fsFunc <- function (x, h, s, p, w) {

  interior <- 2*s*((x - p) / w)
  exterior <- -log(2)/s^2

  ifelse(interior > -1, Vectorize(h*exp(exterior * (log(1 + interior)^2))), 0)

}

speciesModel <- function (dataframe) {

  nls(deriv ~ fsFunc(K, h, s, p, w), data = dataframe, start = list(h = 0.022, s = -.015, p = 550, w = 30))

}

pEst <- function (fit, p) {
  params <- fit %>% broom::tidy()
  params$estimate[params$term == p]
}

fs <- speciesModel(cell)

params <- fs %>% broom::tidy()

fitted <- fs %>% broom::augment()
results <- merge(fitted, cell)

plot(cell$K, cell$deriv)

png('output/cellulose_fs_model.png')
ggplot(results, aes(K, deriv)) +
  geom_point() +
  geom_line(aes(K, .fitted), colour = 'red')
dev.off()


