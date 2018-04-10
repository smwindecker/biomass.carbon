#' Select mean of traits
#'
#' @param all_traits dataframe of traits
#' @param output_file file path for output plot
#' @return mean traits only
#' @importFrom dplyr filter mutate
#' @importFrom utils write.table
#'
#' @export

traits_mean_only <- function(all_traits, output_file) {
  
  t_mean <- all_traits %>%
    dplyr::filter(wt_type == 'mean') %>%
    dplyr::mutate(HC = HC_1 + HC_2)
  
  t_mean$HC[is.na(t_mean$HC)] <- t_mean$HC_2[is.na(t_mean$HC)]
  
  utils::write.table(t_mean, output_file)
  
  t_mean
}

