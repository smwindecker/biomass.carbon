
# ALL TRAITS

library(plyr)

# load raw species data
species <- read.csv('data-raw/species.csv', header = T)

# remove unused species
species <- species[!species$species_code == 'AA',]

# load prepared C and N trait data
cn <- prep_leco(leco_file = 'data-raw/leco.csv')

# load prepared LES trait data
trt <- prep_les(trait_data = 'data-raw/traits.csv', 
                species_data = species)

# load prepared TGA data
tga <- prep_tga(species_data = species,
                output_folder = 'manuscript/figs/')

# create legend for TGA plots
tga_plots_legend(sample_data_file = 'data-raw/TGA/T_TGA.csv', 
                 output_file = 'manuscript/figs/tga_legend.png')

# combine traits
t <- merge(trt, cn, by = 'species_code')
t_1 <- merge(t, tga, by = 'species_code')

# order by species
t_2 <- t_1[order(t_1$species),]

# check this is right
t_3 <- t_2[, c(1,4:7,2,3,9,8,10:14)]

# tidy this function 
traits_table(traits_df = t_3, 
             output_file = 'manuscript/figs/traits_table.tex')


t_mean <- t_3[t_3$wt_type == 'mean',]
t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
write.table(t_mean, 'data/all_traits.txt')

