
phylo_traits <- function(phylo, t_mean) {
  
  tips <- as.data.frame(phylo$tip.label)
  colnames(tips)[1] <- 'tip_label'
  
  # list of traits
  trait_list <- c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')
  
  # create empty columns for each traits
  tips[, trait_list] <- NA
  
  # add genus
  t_mean$genus <- gsub(' .*$', '', t_mean$species)
  
  # overall HC
  t_mean$HC <- t_mean$HC_1 + t_mean$HC_2
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  # input trait value into matrix from dataframe
  for (j in trait_list) {
    for (i in unique(tips$tip_label)) {
      if (i %in% t_mean$species) {
        tips[i, j] <- t_mean[t_mean$species == i, j]
      }
      if (i %in% t_mean$genus) {
        tips[i, j] <- mean(t_mean[t_mean$genus == i, j])
      }
    }
  }
  
  tips <- tips[is.na(tips$tip_label), - which(names(tips) %in% 'tip_label')]
  
  tips
  
}