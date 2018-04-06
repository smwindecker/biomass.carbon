#' Produce parameter values table
#'
#' @param species_deconvoluted_list list of deconvolved 
#' @param species_data dataframe with species info
#' @param output_folder folder path for output plot
#' @return saved parameters tables
#' @importFrom xtable xtable
#' @importFrom dplyr bind_rows
#' @importFrom utils write.table
#'
#' @export

tga_param_table <- function(species_deconvoluted_list, species_data, output_folder) {
  
  # bind parameter outputs of species_deconvolved function
  parameter_estimates <- dplyr::bind_rows(lapply(1:length(species_deconvoluted_list), function(x) {
    return(species_deconvoluted_list[[x]]$params)
  }))
  
  # save parameter output
  write.table(parameter_estimates, 'data/tga_parameters.txt')
  
  # set significant digits
  parameter_estimates[, c('h1', 'h2', 'h3', 'h0')] <- signif(parameter_estimates[, c('h1', 'h2', 'h3', 'h0')], 4)
  parameter_estimates[, c('p1', 'p2', 'p3', 'p0')] <- signif(parameter_estimates[, c('p1', 'p2', 'p3', 'p0')], 3)
  parameter_estimates[, c('s1', 's2', 's3', 's0')] <- signif(parameter_estimates[, c('s1', 's2', 's3', 's0')], 2)
  parameter_estimates[, c('w1', 'w2', 'w3', 'w0')] <- signif(parameter_estimates[, c('w1', 'w2', 'w3', 'w0')], 2)
  
  # combine with species data
  parameters <- merge(parameter_estimates, species_data[,c('species_code', 'species')])
  
  # add italics latex code
  parameters$species <- paste0('\\textit{', parameters$species, '}')
  
  # produce xtable
  parameters <- xtable::xtable(parameters)
  
  # save as .tex file
  print(parameters,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = paste0(output_folder, 'tga_param_table.tex'))
  
}
