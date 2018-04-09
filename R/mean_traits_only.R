
mean_traits_only <- function(all_traits, output_file) {
  
  t_mean <- all_traits %>%
    dplyr::filter(wt_type == 'mean') %>%
    dplyr::mutate(HC = HC_1 + HC_2)
  
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  utils::write.table(t_mean, output_file)
  
  t_mean
}

