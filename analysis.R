
library(mixchar)
library(dplyr)

R.utils::sourceDirectory('R/')

# ------ load data --------
# prepare species data
species <- load_species(species_data = 'data-raw/species.csv')

# prepare C and N trait data
carbon_nitrogen <- load_leco_traits(leco_data = 'data-raw/leco.csv')

# prepare LES trait data
economic_traits <- load_les_traits(trait_data = 'data-raw/traits.csv', 
                                   species_data = species)

# prepare TGA data
tga_output <- tga_wrapper(species, tga_deconvolve, 'data-raw/tga/', ranseed = 1)
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

# deconvolve raw samples
raw_cl <- single_deconvolve("data-raw/raw_biomass/CL.csv")
raw_lg <- single_deconvolve("data-raw/raw_biomass/LG.csv")

# extract output
tga_params <- extract_parameters(tga_output, species)

# ----- figures -----

png('figs/raw_tga_three.png', width = 1500, height = 500)
tga_plot_three(tga_output, species, species_names = c('T', 'LL', 'KK'))
dev.off()

png('figs/tga_ar.png', width = 1500, height = 980)
tga_plot_ar(tga_output, species, gf = 'AR')
dev.off()

png('figs/tga_at.png', width = 1500, height = 1940)
tga_plot_at(tga_output, species, gf = 'AT')
dev.off()

png('figs/tga_tda.png', width = 1500, height = 1460)
tga_plot_tda(tga_output, species, gf = 'Tda')
dev.off()

png('figs/tga_tdr.png', width = 1500, height = 980)
tga_plot_tdr(tga_output, species, gf = 'Tdr')
dev.off()

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

png('figs/phylo_all.png', width = 1500, height = 1000)
phylo_plot(phylo_tree, phylo_trts)
dev.off()

png('figs/tga_pure.png', width = 1200, height = 600)
tga_raw_plots(raw_cl, raw_lg)
dev.off()

# ----- tables ------
tga_param_table(tga_params, 
                'figs/tga_param_table.tex')

traits_table(traits_df = combined_traits, 
             output_file = 'figs/traits_table.tex')

phylo_accessions("data-raw/GenBankAccessions.txt", 'figs/gen_bank_accessions.tex')
phylo_mantel(phylo_tree, phylo_trts, "figs/mantel_results.tex")

pca_loadings(pca_output, 'figs/pca_loadings.tex')

# ------ knit article ------
knitr::knit("ms/manuscript.Rnw", output = "ms/manuscript.tex")
tinytex::pdflatex("ms/manuscript.tex")

knitr::knit("supplement.Rnw", output = "supplement.tex")
tinytex::pdflatex("supplement.tex")
