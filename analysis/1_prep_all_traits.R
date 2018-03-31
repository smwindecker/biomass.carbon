
# ALL TRAITS

# library(dplyr)
library(plyr)

species <- read.csv('raw/species.csv', header = T)
species <- species[!species$species_code == 'AA',]

cn <- prep_leco(leco_file = 'raw/leco.csv')

trt <- prep_leaf_traits(trait_data = 'raw/traits.csv', 
                        species_data = species)

tga <- tga_prep()
tga_plots_legend(sample_data_file = 'raw/TGA/T_TGA.csv', 
                 output_file = 'output/tga_legend.png')

t <- merge(trt, cn, by = 'species_code')
t_1 <- merge(t, tga, by = 'species_code')

t_2 <- t_1[order(t_1$species),]

# check this is right
t_3 <- t_2[, c(1,4:7,2,3,9,8,10:14)]

# tidy this function 
traits_table(traits_df = t_3, 
             output_folder = 'docs/')

t_mean <- t_3[t_3$wt_type == 'mean',]
t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
write.table(t_mean, 'raw/all_traits.txt')

