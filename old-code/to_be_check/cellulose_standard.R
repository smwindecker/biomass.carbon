
# cellulose standard

init_mass <- read.csv('raw/TGA/cellulose.csv', nrows = 1, header = F, skip = 17)

# get individual value then add to df.
init_mass <- init_mass[1,2]

cel <- read.csv('raw/TGA/cellulose.csv', header = F, skip = 29)
colnames(cel) <- c('temp', 'time', 'mass_loss')

cel$temp_K <- cel$temp + 273
cel$K <- as.factor(round(cel$temp_K, digits = 1))

cel_bin <- aggregate(.~K, cel, FUN=head, 1)

d <- -as.data.frame(diff(cel_bin$mass_loss)/diff(as.numeric(cel_bin$K)))

x <- rep(NA, ncol(d))
deriv <- rbind(x, d)
colnames(deriv) <- 'deriv'
cel_d <- cbind(cel_bin, deriv)
cel_trunc <- cel_d[!(cel_d$temp_K < 400 | cel_d$temp_K > 900),]
cel_trunc$init_mass <- init_mass
cel_trunc$m_T <- cel_trunc$init_mass + cel_trunc$mass_loss

write.table(cel_trunc, 'munge/cellulose_deriv.txt')

png('output/tga_cellulose.png', width = 600, height = 480, units = "px", pointsize = 12)
plot(cel_trunc$K, cel_trunc$deriv)
dev.off()
