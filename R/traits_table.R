#' Produce output trait table
#'
#' @param traits_df dataframe of traits
#' @param output_file file path for output plot
#' @return saved trait table
#' @importFrom xtable xtable
#'
#' @export

traits_table <- function (traits_df, output_file) {
  
  # modify growth form labels
  traits_df$gf <- as.character(traits_df$gf)
  traits_df$gf[traits_df$gf == 'G'] <- 'graminoid'
  traits_df$gf[traits_df$gf == 'F'] <- 'forb'
  traits_df$gf[traits_df$gf == 'NV'] <- 'nonvascular'
  traits_df$gf[traits_df$gf == 'S'] <- 'shrub'
  traits_df$gf[traits_df$gf == 'T'] <- 'tree'
  
  # round trait values
  traits_df$SLA <- sprintf("%.2f", round(traits_df$SLA, 2))
  traits_df$DMC <- sprintf("%.0f", round(traits_df$DMC, 0))
  traits_df[,c(8,9)] <- lapply(round(traits_df[,c(8,9)], 1), sprintf, fmt = "%.1f")
  traits_df[,c(10:13)] <- round(traits_df[,c(10:13)], 1)
  
  # for each biomass trait, paste mean and ci of weights together
  for (i in c('HC_1', 'HC_2', 'CL', 'LG')) {
    
    for (j in unique(as.character(traits_df$species))) {
      
      if (!is.na(traits_df[(traits_df$wt_type == 'mean' & traits_df$species == j), i])) {
        
        traits_df[(traits_df$wt_type == 'mean' & traits_df$species == j), i] <- paste0(
          traits_df[(traits_df$wt_type == 'mean' & traits_df$species == j), i], ' [',
          traits_df[(traits_df$wt_type == '2.5%' & traits_df$species == j), i], ', ',
          traits_df[(traits_df$wt_type == '97.5%' & traits_df$species == j), i], ']')
        
      }
    }
  }
  
  # subset to only include modified mean rows
  traits_table <- traits_df[traits_df$wt_type == 'mean', 3:13]
  
  # add italics specification for latex
  traits_table$species <- paste0('\\textit{', traits_table$species, '}')
  
  # create xtable
  traits_table <- xtable::xtable(traits_table)
  
  print(traits_table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = output_file)
  
}