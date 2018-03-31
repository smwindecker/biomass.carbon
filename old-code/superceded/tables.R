
## species, traits

# add family designations and number of samples to species names)
family <- read.csv('raw/species_family.csv', header = TRUE)
t_2 <- read.table('munge/all_traits.txt')
species <- read.csv('raw/species.csv', header = TRUE)

sp_table <- merge(family, t_2)
sp_table <- merge(sp_table, species)
sp_table <- sp_table[, c('family', 'species', 'location', 'lat', 'lon', 'mSLA', 
                         'mLDMC', 'mgC', 'mgN', 'mg_p1', 'mg_p2', 'mg_p3')]

write.csv(sp_table, 'output/species_table.csv', row.names = FALSE)