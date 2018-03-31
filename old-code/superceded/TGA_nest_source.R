
# nested version
# neg exponential and weibull

rm(list = ls())
library(broom)
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(reshape2)

source('R/paramSelect.R')
source('R/fraserSuzuki.R')

# load decomp data
tga <- read.table('munge/tga_derivs.txt')

# replace spaces with underscores in species names
tga_d_trunc$species <- gsub('([[:punct:]])|\\s+','_', tga_d_trunc$species)

# vector of all starting values - for optimiser
theta <- c(.015, .013, .01, -.15, -.15, -.15, 540, 600, 700, 50, 30, 200)
lb <- c(0, 0, 0, -1, -1, -1, 0, 0, 0, 0, 0, 0)

tga_nest <-
  tga %>%
  group_by(species, species_code) %>%
  nest()

# sub_data is the tibble for each species
fs_workflow <- function (sub_data) {

  obs <- sub_data['deriv']
  temp <- sub_data['K']
  params_opt <- paramSelect(theta, lb, fsTotal, temp, obs, restarts = 300)
  fs <- fs_model(sub_data, params_opt)

  return(fs)

}

# nested models
list_model <-
  list(
    fraser_suzuki <- fs_workflow
  )

fn_model <- function (.model, df) {
  df$model <- map(df$data, possibly(.model, NULL))
  df
}

model_nest_new <-
  list_model %>%
  map_df(fn_model, n_nest, .id = 'id_model') %>%
  mutate(is_null = map_lgl(model, is.null)) %>%
  filter(!is_null) %>%
  select(-is_null)

pred_nest_new <-
  model_nest_new %>%
  mutate(pred = map2(model, data, predict))

pred_unnest <-
  pred_nest_new %>%
  unnest(data, pred)

pred_tall_new <-
  pred_nest_new %>%
  unnest(data, pred) %>%
  rename(modeled = pred, measured = prop) %>%
  gather('type', 'mass_loss', modeled, measured)
write.table(pred_tall_new, 'munge/nest_model_predictions.txt')

resid <-
  model_nest_new %>%
  mutate(resid = map(model, resid)) %>%
  unnest(data, resid)

resid %>%
  ggplot(aes(x = days, y = resid)) +
  geom_line(aes(color = species)) +
  facet_grid(id_model ~ .)

model_parameters <-
  model_nest_new %>%
  select(id_model, species, model) %>%
  mutate(tidy = map(model, tidy)) %>%
  select(-model) %>%
  unnest()
write.table(model_parameters, 'munge/nest_model_parameters.txt')

