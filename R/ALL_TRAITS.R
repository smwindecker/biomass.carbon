
# ALL TRAITS

library(dplyr)
library(plyr)
library(xtable)
library(phytools)
library(ade4)
library(adephylo)
library(scales)

source('R/pair_plot.R')
source('R/boxplot.R')
source('R/pca_gf.R')
source('R/fs_simulate.R')
source('R/phylogeny.R')

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
tga <- read.table('munge/tga_proportions.txt', stringsAsFactors = TRUE)

t <- merge(trt, cn, by = 'species_code')
t_1 <- merge(t, tga, by = 'species_code')
t_2 <- t_1[order(t_1$species),]

t_3 <- t_2[, c(1,4:7,2,3,9,8,10:14)]

make_traits_table <- function (t_3) {
  
  t_3$SLA <- round(t_3$SLA, 2)
  t_3$DMC <- round(t_3$DMC, 0)
  t_3[,c(8:13)] <- round(t_3[,c(8:13)], 1)
  
  t_3$gf <- as.character(t_3$gf)
  t_3$gf[t_3$gf == 'G'] <- 'graminoid'
  t_3$gf[t_3$gf == 'F'] <- 'forb'
  t_3$gf[t_3$gf == 'NV'] <- 'nonvascular'
  t_3$gf[t_3$gf == 'S'] <- 'shrub'
  t_3$gf[t_3$gf == 'T'] <- 'tree'
  
  for (i in c('HC_1', 'HC_2', 'CL', 'LG')) {
    for (j in unique(as.character(t_3$species))) {
      
      if (!is.na(t_3[(t_3$wt_type == 'mean' & t_3$species == j), i])) {
        
        t_3[(t_3$wt_type == 'mean' & t_3$species == j), i] <- paste0(
          t_3[(t_3$wt_type == 'mean' & t_3$species == j), i], ' [',
          t_3[(t_3$wt_type == '2.5%' & t_3$species == j), i], ', ',
          t_3[(t_3$wt_type == '97.5%' & t_3$species == j), i], ']')
      }
    }
  }
  
  traits_table <- t_3[t_3$wt_type == 'mean', 3:13]
  traits_table$species <- paste0('\\textit{', traits_table$species, '}')
  cols <- c('SLA', 'DMC', 'N', 'C')  
  traits_table[, cols] <- apply(traits_table[, cols], 2, function(x) as.character(x))
  traits_table <- xtable(traits_table)
  
  print(traits_table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = 'docs/traits_table.tex')
}

make_traits_table(t_3)

t_mean <- t_3[t_3$wt_type == 'mean',]
t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
write.table(t_mean, '../THESIS.bioassay/raw/all_traits.txt')


trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
phylogeny(t_mean)
for (i in trait_list) boxplot(t_mean, i)

# prep log traits for pairplot and pca
log_traits <- t_mean

for (i in unique(trait_list)) {
  log_traits[, i] <- log(log_traits[, i])
}
rownames(log_traits) <- log_traits[,'sp_abrev']

cov <- log_traits[, names(log_traits) %in% trait_list]
cov <- cov[, c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')]

pca_gf(cov)
pair_plot(cov)

# Fraser-Suzuki parameter simulation
fs_simulate()

