# recreate my only few TGA plots, recheck legend script. 
# make this into a remake. 

library(deconvolve)
library(biomass.traits)
library(dplyr)

# set output folder
# dir.create('manuscript/figs')
output_folder = 'manuscript/figs/'

# load raw species data
species <- read.csv('data-raw/species.csv', header = T) %>%
  filter(species_code != 'AA')

# load prepared C and N trait data
cn <- prep_leco(leco_data = 'data-raw/leco.csv')

# load prepared LES trait data
trt <- prep_les(trait_data = 'data-raw/traits.csv', 
                species_data = species)

# load prepared TGA data
tga <- prep_tga(species_data = species,
                output_folder)

# create legend for TGA plot
tga_plots_legend(sample_data_file = 'data-raw/TGA/T_TGA.csv', 
                 output_file = 'manuscript/figs/tga_legend.png')

# combine traits
t <- dplyr::full_join(trt, cn, by = 'species_code') %>%
  dplyr::full_join(., tga, by = 'species_code') %>%
  dplyr::arrange(., species) %>%
  .[, c(1,4:7,2,3,9,8,10:14)]

# tidy this function 
traits_table(traits_df = t, 
             output_file = 'manuscript/figs/traits_table.tex')

# get only means of biomass traits
t_mean <- t %>%
  dplyr::filter(wt_type == 'mean') %>%
  dplyr::mutate(HC = HC_1 + HC_2)
t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
utils::write.table(t_mean, 'data/all_traits.txt')
  
# create phylogeny plots and tables
phylogeny(df = t_mean, 
          genbank_accessions_file = 'data-raw/GenBankAccessions.txt', 
          nwk_file = 'data-raw/phylo_tree.nwk', 
          output_folder)

# create boxplots
trait_boxplot(df = t_mean, 
              output_folder)

# create covariate matrix of traits
cov <- t_mean %>%
  magrittr::set_rownames(t_mean[, 'sp_abrev']) %>%
  dplyr::select(SLA, DMC, N, C, HC, CL, LG) 
cov[] <- log(cov[])

# create PCA
pca_gf(df = cov, 
       species_data = species,
       output_folder)

# create pair plot
pair_plot(df = cov, 
          output_file = 'manuscript/figs/pairplot.png')

# Fraser-Suzuki parameter simulation
fs_simulate(output_file = 'manuscript/figs/fs_simulate.png')

# J. amabilis theory curves
j_amabilis(species_code = 'A', 
           output_folder)

# test deconvolve on raw samples
single_deconvolute(raw_file = 'data-raw/raw_biomass/CL.csv', 
                  subfig = 'a', 
                  output_file = 'manuscript/figs/raw_CL.png')
single_deconvolute(raw_file = 'data-raw/raw_biomass/LG.csv', 
                  subfig = 'b', 
                  output_file = 'manuscript/figs/raw_LG.png')

