

# create legend for TGA plot
tga_plots_legend(sample_data_file = 'data-raw/TGA/T_TGA.csv', 
                 output_file = 'manuscript/figs/tga_legend.png')

# combine traits
t <- dplyr::full_join(trt, cn, by = 'species_code') %>%
  dplyr::full_join(., tga, by = 'species_code') %>%
  arrange(., species) %>%
  .[, c(1,4:7,2,3,9,8,10:14)]

# tidy this function 
traits_table(traits_df = t, 
             output_file = 'manuscript/figs/traits_table.tex')

### change t_3 to t!!!
write.csv(t, 'temp.csv')

t <- read.csv('temp.csv')
t_mean <- t_3[t_3$wt_type == 'mean',]
t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
utils::write.table(t_mean, 'data/all_traits.txt')

# create phylogeny plots and tables
phylogeny(df = t_mean, 
          genbank_accessions_file = 'data-raw/GenBankAccessions.txt', 
          nwk_file = 'data-raw/phylo_tree.nwk', 
          output_folder)

# create covariate matrix of traits
cov <- t_mean %>%
  dplyr::set_rownames(t_mean[, 'sp_abrev']) %>%
  dplyr::select(SLA, DMC, N, C, HC, CL, LG)

# create boxplots
trait_boxplot(df = cov, 
        output_folder)

# log all columns of covariates dataframe
cov[] <- sapply(cov, log)

# create PCA
pca_gf(df = cov, 
       species_data = species,
       output_folder)

# create pair plot
pair_plot(df = cov, 
          output_file = 'manuscript/figs/pairplot.png')

# Fraser-Suzuki parameter simulation
fs_simulate(output_folder)

# J. amabilis theory curves
j_amabilis(species_code = 'A', 
           output_folder)

# test deconvolve on raw samples
single_deconvolve(raw_file = 'data-raw/raw_biomass/CL.csv', 
                  subfig = 'a', 
                  output_file = 'manuscript/figs/raw_CL.png')
single_deconvolve(raw_file = 'data-raw/raw_biomass/CL.csv', 
                  subfig = 'b', 
                  output_file = 'manuscript/figs/raw_LG.png')

