#' Produce parameter values table
#'
#' @param species_deconvolved_list list of deconvolved 
#' @param species_df dataframe with species info
#' @param output_folder folder path for output plot
#' @return saved parameters tables
#' @importFrom xtable xtable
#'
#' @export

tga_param_table <- function(species_deconvolved_list, species_df, output_folder) {
  
  parameter_estimates <- dplyr::bind_rows(lapply(1:length(species_deconvolved_list), function(x) {
    return(species_deconvolved_list[[x]]$params)
  }))
  write.table(parameter_estimates, 'munge/tga_parameters.txt')
  
  parameter_estimates[, c('h1', 'h2', 'h3', 'h0')] <- signif(parameter_estimates[, c('h1', 'h2', 'h3', 'h0')], 4)
  parameter_estimates[, c('p1', 'p2', 'p3', 'p0')] <- signif(parameter_estimates[, c('p1', 'p2', 'p3', 'p0')], 3)
  parameter_estimates[, c('s1', 's2', 's3', 's0')] <- signif(parameter_estimates[, c('s1', 's2', 's3', 's0')], 2)
  parameter_estimates[, c('w1', 'w2', 'w3', 'w0')] <- signif(parameter_estimates[, c('w1', 'w2', 'w3', 'w0')], 2)
  
  parameters <- merge(parameter_estimates, species_df[,c('species_code', 'species')])
  
  parameters$species <- paste0('\\textit{', parameters$species, '}')
  parameters <- xtable::xtable(parameters)
  
  print(parameters,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = paste0(output_folder, 'tga_param_table.tex'))
  
}
