
# TRAITS

library(plyr)

# read the traits df's
traits <- read.csv('raw/traits.csv', header = T)
species <- read.csv('raw/species.csv', header = T) # information about the species collected

# merge with the species data file
trt <- merge(traits[!traits$species_code == 'J',],
                    species[,c('species_code', 'species', 'plant_part', 'gf')],
             by = c('species_code', 'plant_part'))

# calculate SLA (cm2/g) and LDMC (mg/g)
trt$SLA <- trt$area/trt$dry_weight
trt$LDMC <- (trt$dry_weight*1000)/trt$wet_weight

trt_1 <- ddply(trt, ~ species, summarise, mSLA = mean(SLA), mLDMC = mean(LDMC))

trt_2 <- merge(trt_1, unique(trt[,c('species_code', 'species', 'gf')]))

write.table(trt_2, 'munge/trait_data.txt')

