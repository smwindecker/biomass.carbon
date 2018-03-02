
biomass_plot <- function (df, trait) {
  
  df <- t_3
  trait <- 'HC_2'
  
  
  order <- t
  t_3 <- t_3[order(t_3$species[,t_3$HC_2])]
  png('output/HC2_variation.png')
  plot(df$species, df$HC_2)
  dev.off()
  
  png('output/CL_variation.png')
  plot(df$species, df$CL)
  dev.off()
  
  png('output/HC2_variation.png')
  plot(df$species, df$LG)
  dev.off()
  
  
}