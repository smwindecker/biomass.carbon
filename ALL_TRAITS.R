
# ALL TRAITS

library(dplyr)
library(plyr)


library(scales)



## prep leco data
leco <- read.csv('raw/leco.csv', header = F, skip = 12)
colnames(leco) <- c('species_code', 'sample', 'carousel_no', 'mass', 'measurement',
                    'pC', 'pN', 'pS', 'process_time', 'analysis_datetime')

leco$C <- leco$pC
leco$N <- leco$pN

cn <- leco[!(leco$species_code == 'donotuse' | leco$species_code == 'AA'),
           c('species_code', 'C', 'N')]

## prep numerical traits
traits <- read.csv('raw/traits.csv', header = T)
species <- read.csv('raw/species.csv', header = T) # information about the species collected

# merge with the species data file
trait <- merge(traits[!traits$species_code == 'AA',],
             species[!species$species_code == 'AA', 
                     c('species_code', 'sp_abrev', 'species', 'family', 'plant_part', 'gf')],
             by = c('species_code', 'plant_part'))

# calculate SLA (m2/g) and DMC (mg/g)
trait$longSLA <- (trait$area/100)/trait$dry_weight
trait$longDMC <- (trait$dry_weight*1000)/trait$wet_weight

trait_1 <- ddply(trait, ~ species_code, summarise, SLA = mean(longSLA), DMC = mean(longDMC))
trt <- merge(trait_1, unique(trait[,c('species_code', 'sp_abrev', 'species', 'family', 'gf')]))

# tga
## could add tga procedure here. 
## make some the rest of this more programmatic. hm... maybe I need a full dataset 
## script and a separate analysis scrpt. 

tga <- read.table('munge/tga_proportions.txt', stringsAsFactors = TRUE)

t <- merge(trt, cn, by = 'species_code')
t_1 <- merge(t, tga, by = 'species_code')
t_2 <- t_1[order(t_1$species),]

t_3 <- t_2[, c(1,4:7,2,3,9,8,10:14)]

traits_table(t_3, 
             output_file = 'docs/traits_table.tex')

t_mean <- t_3[t_3$wt_type == 'mean',]
t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
write.table(t_mean, 'raw/all_traits.txt')

