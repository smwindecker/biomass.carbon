
library(dplyr)

t_mean <- read.table('raw/all_traits.txt')
phylogeny(t_mean, 
          genbank_accessions_file = 'raw/GenBankAccessions.txt', 
          nwk_file = 'raw/phylo_tree.nwk', 
          output_folder = 'docs/')

# prep log traits for pairplot and pca
cov <- t_mean %>%
  set_rownames(t_mean[, 'sp_abrev']) %>%
  select(SLA, DMC, N, C, HC, CL, LG)
boxplot(cov, output_folder = 'figs/')

cov[] <- sapply(cov, log)

pca_gf(cov, 
       figure_folder = 'output/',
       table_folder = 'docs/')

pair_plot(cov, 
          output_file = 'figs/pairplot.png')

# Fraser-Suzuki parameter simulation
fs_simulate(output_folder = 'figs/')

######################
single_deconvolve(raw_file = 'raw/raw_biomass/CL.csv', 
                  subfig = 'a', 
                  output_file = 'figs/raw_CL.png')
single_deconvolve(raw_file = 'raw/raw_biomass/CL.csv', 
                  subfig = 'b', 
                  output_file = 'figs/raw_LG.png')

species_df <- read.csv('raw/species.csv', header = TRUE)




