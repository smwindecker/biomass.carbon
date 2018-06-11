## figure out how to get .Rnw to sweave. check Jimmy's additional options and build latex functions
## go over all the other scripts. 
## get sweave to work. -- fix the names of plots in the document. 


library(deconvolve)
library(dplyr)

source('R/analyse_data.R')
source('R/figures.R')
source('R/load_data.R')
source('R/tables.R')
source('R/manuscript_functions.R')

# set output folder
# dir.create('figs')

# ------ load data --------
# prepare species data
species <- load_species(species_data = 'data-raw/species.csv')

# prepare C and N trait data
carbon_nitrogen <- load_leco_traits(leco_data = 'data-raw/leco.csv')

# prepare LES trait data
economic_traits <- load_les_traits(trait_data = 'data-raw/traits.csv', 
                                   species_data = species)

# prepare TGA data
tga_output <- tga_wrapper(species, tga_deconvolve, 'data-raw/tga/')
biomass_traits <- load_tga_traits(tga_output)

# ----- modify data ----
# combine traits
combined_traits <- traits_combine(species = species,
                             cn = carbon_nitrogen,
                             trt = economic_traits, 
                             tga = biomass_traits)

# get only means of biomass traits
mean_traits <- traits_mean_only(combined_traits)

# log of mean traits
logged_trait_matrix <- traits_log(mean_traits)

# prepare PCA
pca_output <- pca_data(logged_trait_matrix)

# prepare phylo data
phylo_tree <- phylo_readtree("data-raw/phylo_tree.nwk")
phylo_trts <- phylo_traits(phylo_tree, mean_traits)


raw_cl <- single_deconvolve("data-raw/raw_biomass/CL.csv")
raw_lg <- single_deconvolve("data-raw/raw_biomass/LG.csv")

tga_params <- extract_parameters(tga_output, species)

# ----- figures -----
# J. amabilis theory curves
png('figs/theory_plot.png', width = 1500, height = 480)
tga_theory_plots('data-raw/TGA/A_TGA.csv')
dev.off()

png('figs/raw_tga_three.png', width = 1500, height = 500)
tga_plot_three(tga_output, species, species_names = c('T', 'LL', 'KK'))
dev.off()

png('figs/tga_ar.png', width = 1500, height = 980)
tga_plot_ar(tga_output, species, subfig = 'a', gf = 'AR')
dev.off()

png('figs/tga_at.png', width = 1500, height = 1940)
tga_plot_at(tga_output, species, subfig = 'b', gf = 'AT')
dev.off()

png('figs/tga_tda.png', width = 1500, height = 1460)
tga_plot_tda(tga_output, species, subfig = 'c', gf = 'Tda')
dev.off()

figs/tga_tdr.png:
  command: tga_plot_tdr(tga_output, species, subfig = I('d'), gf = I('Tdr'))
plot: 
  width: 1500
height: 980

png('figs/boxplot.png', width = 1200, height = 1050)
box_plot(mean_traits)
dev.off()

# create PCA
png('figs/raw_pca.png', width = 1000, height = 950)
pca(pca_output, logged_trait_matrix, species)
dev.off()

# create pair plot
png('figs/pairplot.png', width = 1000, height = 1000)
pair_plot(logged_trait_matrix)
dev.off()

# Fraser-Suzuki parameter simulation
png('figs/fs_simulate.png', width = 800, height = 700)
simulate_fraser_suzuki()
dev.off()

# test deconvolve on raw samples
png('figs/tga_pure_samples.png', width = 1200, height = 600)
tga_raw_plots(raw_cl_sample, raw_lg_sample)
dev.off()

png('figs/phylo_all.png', width = 1500, height = 1000)
phylo_plot(phylo_tree, phylo_trts)
dev.off()


# ----- tables ------
tga_param_table(tga_parameters, 
                'figs/tga_param_table.tex')

traits_table(traits_df = combined_traits, 
             output_file = 'figs/traits_table.tex')

phylo_accessions("data-raw/GenBankAccessions.txt", 'figs/gen_bank_accessions.tex')
phylo_mantel(phylo_tree, phylo_trts, "figs/mantel_results.tex")

pca_loadings(pca_output, 'figs/pca_loadings.tex')
