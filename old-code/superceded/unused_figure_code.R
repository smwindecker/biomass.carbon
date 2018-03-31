
## unused figures

## ternary plot

t_data <- t_2[,c('species', 'P.HC', 'P.CL', 'P.LG')]

png('output/ternaryplot.png', 8, 8, 'in', res = 100)
ternary_plot <- ggtern(data = t_data, aes(x = P.HC, y = P.CL, z = P.LG)) +
  geom_point(aes(fill = species), size = 6, shape = 21, color = 'black') +
  ggtitle('Proportions of lignocellulosic biomass components') +
  labs(fill = 'species') +
  theme_rgbw() +
  theme(legend.position = c(0,1), legend.justification = c(1, 1))
ternary_plot
dev.off()