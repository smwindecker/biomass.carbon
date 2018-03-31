
# C/N RATIOS FROM LECO

leco <- read.csv('raw/leco.csv', header = F, skip = 12)

# here could bring in the second raw leco data, then rbind them.
# then average the 2 samples

colnames(leco) <- c('species_code', 'sample', 'carousel_no', 'mass', 'measurement',
                    'pC', 'pN', 'pS', 'process_time', 'analysis_datetime')
leco <- leco[!(leco$species_code == 'donotuse'),]
leco$mgC <- leco$mass * (leco$pC / 100) * 1000
leco$mgN <- leco$mass * (leco$pN / 100) * 1000
leco$mgS <- leco$mass * (leco$pS / 100) * 1000

write.table(leco[,c(1, 11:13)], 'munge/leco_data.txt')

