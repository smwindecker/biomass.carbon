## get this in order


library(magrittr)
library(plyr)
library(dplyr)
library(deconvolve)
source('R/fraser-suzuki_functions.R')

species_df <- read.csv('raw/species.csv', header = TRUE)

write.table(t_mean, 'raw/all_traits.txt')
trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
phylogeny(t_mean, 
          genbank_accessions_file = 'raw/GenBankAccessions.txt', 
          nwk_file = 'raw/phylo_tree.nwk', 
          output_folder = 'docs/')
for (i in trait_list) boxplot(t_mean, trait = i, output_folder = 'figs/')

# prep log traits for pairplot and pca
log_traits <- t_mean

for (i in unique(trait_list)) {
  log_traits[, i] <- log(log_traits[, i])
}
rownames(log_traits) <- log_traits[,'sp_abrev']

cov <- log_traits[, names(log_traits) %in% trait_list]
cov <- cov[, c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')]

pca_gf(cov, 
       figure_folder = 'output/',
       table_folder = 'docs/')

pair_plot(cov, 
          output_file = 'figs/pairplot.png')

# Fraser-Suzuki parameter simulation
fs_simulate(output_folder = 'figs/')

