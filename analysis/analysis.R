## figure out how to get .Rnw to sweave. check Jimmy's additional options and build latex functions
## go over all the other scripts. 
## get sweave to work. -- fix the names of plots in the document. 


library(deconvolve)
library(biomass.traits)
library(dplyr)

# set output folder
# dir.create('manuscript/figs')

# ------ load data --------
# prepare species data
species <- load_species(species_data = 'data-raw/species.csv')

# prepare C and N trait data
carbon_nitrogen <- load_leco_traits(leco_data = 'data-raw/leco.csv')

# prepare LES trait data
economic_traits <- load_les_traits(trait_data = 'data-raw/traits.csv', 
                                   species_data = species)

# prepare TGA data
tga_output <- tga_wrapper(species, tga_deconvolute, 'data-raw/tga/')
biomass_traits <- load_tga_traits(tga_output)

# ----- modify data ----
# combine traits
all_traits <- traits_combine(species = species,
                             cn = carbon_nitrogen,
                             trt = economic_traits, 
                             tga = biomass_traits)

# get only means of biomass traits
mean_traits <- traits_mean_only(all_traits = all_traits, 
                                output_file = 'data/all_traits.txt')

# log of mean traits
logged_trait_matrix <- traits_log(mean_traits)

# prepare PCA
pca_output <- pca_data(logged_trait_matrix)

# prepare phylo data
phylo_tree <- phylo_readtree("data-raw/phylo_tree.nwk")
phylo_trts <- phylo_traits(phylo_tree, mean_traits)


raw_cl_sample <- single_deconvolute("data-raw/raw_biomass/CL.csv")
raw_lg_sample <- single_deconvolute("data-raw/raw_biomass/LG.csv")


# ----- figures -----
# J. amabilis theory curves
png('manuscript/figs/theory_plot.png', width = 1500, height = 480)
tga_theory_plots('data-raw/TGA/A_TGA.csv')
dev.off()

png('manuscript/figs/raw_tga_three.png', width = 1500, height = 500)
tga_plot_three(tga_output, species, species_names = c('LL', 'MM', 'KK'))
dev.off()

png('manuscript/figs/tga_G.png', width = 1500, height = 1940)
tga_plot_gram(tga_output, species, subfig = 'a', gf = 'G') 
dev.off()

png('manuscript/figs/tga_F.png', width = 1500, height = 1460)
tga_plot_forb(tga_output, species, subfig = 'b', gf = 'F')
dev.off()

png('manuscript/figs/tga_TNVS.png', width = 1500, height = 980)
tga_plot_others(tga_output, species, subfigs = c('c', 'd', 'e'))
dev.off()

png('manuscript/figs/boxplot.png', width = 1200, height = 1050)
box_plot(mean_traits)
dev.off()

# create PCA
png('manuscript/figs/raw_pca.png', width = 1000, height = 950)
pca(pca_output, logged_trait_matrix, species)
dev.off()

# create pair plot
png('manuscript/figs/pairplot.png', width = 1000, height = 1000)
pair_plot(logged_trait_matrix)
dev.off()

# Fraser-Suzuki parameter simulation
png('manuscript/figs/fs_simulate.png', width = 800, height = 700)
simulate_fraser_suzuki()
dev.off()

# test deconvolve on raw samples
png('manuscript/figs/tga_pure_samples.png', width = 1200, height = 600)
tga_raw_plots(raw_cl_sample, raw_lg_sample)
dev.off()

png('manuscript/figs/phylo_all.png', width = 1500, height = 1000)
phylo_plot(phylo_tree, phylo_trts)
dev.off()


# ----- tables ------
tga_param_table(tga_output, species, 
                'manuscript/figs/tga_param_table.tex')

traits_table(traits_df = all_traits, 
             output_file = 'manuscript/figs/traits_table.tex')


phylo_accessions("data-raw/GenBankAccessions.txt", 'manuscript/figs/gen_bank_accessions.tex')
phylo_mantel(phylo_tree, phylo_trts, "manuscript/figs/mantel_results.tex")

pca_loadings(pca_output, 'manuscript/figs/pca_loadings.tex')

