


library(ggplot2)
png('output/tga_deriv.png', width = 600, height = 480, units = "px", pointsize = 12)
ggplot(tga_d_trunc, aes(temp, deriv, colour = species)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = 'none', #c(.8, .2),
        text = element_text(size = 16)) +
  xlab('Temp C') +
  ylab('% Mass Loss/Temp C')
dev.off()

ggplot(tga_d_trunc[tga_d_trunc$species == 'Eleocharis acuta',], aes(temp)) +
  geom_line(aes(y = deriv, colour = "black")) +
  theme_bw() +
  theme(legend.position = 'none', #c(.8, .2),
        text = element_text(size = 16)) +
  xlab('Temp C') +
  ylab('- % Mass Loss/Temp C')


png('output/tga_massloss_single.png', width = 600, height = 480, units = "px", pointsize = 12)
ggplot(tga[tga$species == 'Eleocharis acuta',], aes(temp, p_mass)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        text = element_text(size = 16)) +
  xlab('Temp C') +
  ylab('% Mass Remaining')
dev.off()

png('output/tga_deriv_single.png', width = 600, height = 480, units = "px", pointsize = 12)
ggplot(tga_d[tga_d$species == 'Eleocharis acuta',], aes(temp, -deriv)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = 'none', #c(.8, .2),
        text = element_text(size = 16)) +
  xlab('Temp C') +
  ylab('- % Mass Loss/Temp C') +
  scale_x_continuous(limits = c(35, 800))
dev.off()






png('output/tga_massloss.png', width = 600, height = 480, units = "px", pointsize = 12)
ggplot(tga, aes(temp, p_mass, colour = species)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = c(.8, .8),
        text = element_text(size = 16)) +
  xlab('Temp C') +
  ylab('% Mass Remaining')
dev.off()

png('output/tga_deriv.png', width = 600, height = 480, units = "px", pointsize = 12)
ggplot(tga_d, aes(temp, deriv, colour = species)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = 'none', #c(.8, .2),
        text = element_text(size = 16)) +
  xlab('Temp C') +
  ylab('% Mass Loss/Temp C')
dev.off()

png('output/tga_ind_deriv.png', width = 1000, height = 300, units = "px", pointsize = 12)
tga_sub <- tga_d[tga_d$species == c('Myriophyllum crispatum', 'Juncus usitatus',
                                    'Cyperus eragrostis', 'Alternanthera denticulata', 'Triglochin procera'),]
ggplot(tga_sub, aes(temp, deriv, colour = species)) +
  geom_line() +
  theme_bw() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  xlab('Temp C') +
  ylab('% Mass Remaining/Temp C') +
  facet_wrap( ~ species, nrow = 1)
dev.off()

#
d <- read.table('munge/decomposition_data.txt', stringsAsFactors = T)
trt <- read.table('munge/trait_data.txt', stringsAsFactors = T)
cn <- read.table('munge/leco_data.txt', stringsAsFactors = T)

m <- merge(d, trt, by = 'species_code')
m_1 <- merge(m, cn, by = 'species_code')
tga_m <- merge(m_1, tga_d)




png('output/tga_ind_ldmc.png', width = 900, height = 800, units = "px", pointsize = 12)
tga_m2 <- transform(tga_m, species = reorder(species, LDMC))
ggplot(tga_m2, aes(temp, deriv, colour = LDMC)) +
  geom_line() +
  scale_colour_gradient(guide=guide_colourbar(reverse = TRUE),
                        low="#5EB7F8", high="#1A334B") +
  theme_bw() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab('Temp C') +
  ylab('% Mass Remaining/Temp C') +
  facet_wrap( ~ species, nrow = 6)
dev.off()

png('output/tga_ind_cn.png', width = 900, height = 800, units = "px", pointsize = 12)
tga_m2 <- transform(tga_m, species = reorder(species, CN))
ggplot(tga_m2, aes(temp, deriv, colour = CN)) +
  geom_line() +
  scale_colour_gradient(guide=guide_colourbar(reverse = TRUE),
                        low="#5EB7F8", high="#1A334B") +
  theme_bw() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab('Temp C') +
  ylab('% Mass Remaining/Temp C') +
  facet_wrap( ~ species, nrow = 6)
dev.off()

png('output/tga_ind_sla.png', width = 900, height = 800, units = "px", pointsize = 12)
tga_m2 <- transform(tga_m, species = reorder(species, SLA))
ggplot(tga_m2, aes(temp, deriv, colour = SLA)) +
  geom_line() +
  scale_colour_gradient(guide=guide_colourbar(reverse = TRUE),
                        low="#5EB7F8", high="#1A334B") +
  theme_bw() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab('Temp C') +
  ylab('% Mass Remaining/Temp C') +
  facet_wrap( ~ species, nrow = 6)
dev.off()

library(ggplot2)

ggplot(tga_m2, aes(temp, deriv, colour = SLA)) +
  geom_point() +
  scale_colour_gradient(guide=guide_colourbar(reverse = TRUE),
                        low="#5EB7F8", high="#1A334B") +
  theme_bw() +
  theme(text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab('Temp C') +
  ylab('% Mass Remaining/Temp C') +
  facet_wrap( ~ species, nrow = 6)
dev.off()


