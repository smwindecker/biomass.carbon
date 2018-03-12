
library(deconvolve)
source('R/single_deconvolve.R')

# HC <- read.csv('raw/raw_biomass/HC.csv', header = F, skip = 29)
CL <- read.csv('raw/raw_biomass/CL.csv', header = F, skip = 29)
LG <- read.csv('raw/raw_biomass/LG.csv', header = F, skip = 29)
Mix1 <- read.csv('raw/raw_biomass/Mix1.csv', header = F, skip = 29)

# Add column names
# names(HC) <- c('temp', 'time', 'mass_loss')
names(CL) <- c('temp', 'time', 'mass_loss')
names(LG) <- c('temp', 'time', 'mass_loss')
names(Mix1) <- c('temp', 'time', 'mass_loss')

# init_mass_HC <- read.csv('raw/raw_biomass/HC.csv', nrows = 1, header = F, skip = 17)[1,2]
init_mass_CL <- read.csv('raw/raw_biomass/CL.csv', nrows = 1, header = F, skip = 17)[1,2]
init_mass_LG <- read.csv('raw/raw_biomass/LG.csv', nrows = 1, header = F, skip = 17)[1,2]
init_mass_Mix1 <- read.csv('raw/raw_biomass/Mix1.csv', nrows = 1, header = F, skip = 17)[1,2]

# munge_HC <- deconvolve::process(HC, 'temp', 'mass_loss', init_mass_HC)
munge_CL <- deconvolve::process(CL, 'temp', 'mass_loss', init_mass_CL)
munge_LG <- deconvolve::process(LG, 'temp', 'mass_loss', init_mass_LG)
munge_Mix1 <- deconvolve::process(Mix1, 'temp', 'mass_loss', init_mass_Mix1)

plot(munge_CL$data$temp_C, munge_CL$data$deriv)
plot(munge_LG$data$temp_C, munge_LG$data$deriv)

single_deconvolve(munge_CL, 'a', 'raw_CL')
single_deconvolve(munge_LG, 'b', 'raw_LG')

# plot(munge_Mix1$data$temp_C, munge_Mix1$data$deriv)
# decon_Mix1 <- deconvolve::deconvolve(munge_Mix1, n_curves = 3)
# plot(decon_Mix1)
