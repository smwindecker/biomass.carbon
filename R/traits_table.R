#' Produce output trait table
#'
#' @param df dataframe of traits
#' @param output_file file path for output plot
#' @return saved trait table
#' @importFrom xtable xtable
#'
#' @export

traits_table <- function (df, output_file) {
  
  df$SLA <- sprintf("%.2f", round(df$SLA, 2))
  df$DMC <- sprintf("%.0f", round(df$DMC, 0))
  df[,c(8,9)] <- lapply(round(df[,c(8,9)], 1), sprintf, fmt = "%.1f")
  
  df$gf <- as.character(df$gf)
  df$gf[df$gf == 'G'] <- 'graminoid'
  df$gf[df$gf == 'F'] <- 'forb'
  df$gf[df$gf == 'NV'] <- 'nonvascular'
  df$gf[df$gf == 'S'] <- 'shrub'
  df$gf[df$gf == 'T'] <- 'tree'
  
  df[,c(10:13)] <- round(df[,c(10:13)], 1)
  for (i in c('HC_1', 'HC_2', 'CL', 'LG')) {
    for (j in unique(as.character(df$species))) {
      
      if (!is.na(df[(df$wt_type == 'mean' & df$species == j), i])) {
        
        df[(df$wt_type == 'mean' & df$species == j), i] <- paste0(
          df[(df$wt_type == 'mean' & df$species == j), i], ' [',
          df[(df$wt_type == '2.5%' & df$species == j), i], ', ',
          df[(df$wt_type == '97.5%' & df$species == j), i], ']')
      }
    }
  }
  
  traits_table <- df[df$wt_type == 'mean', 3:13]
  traits_table$species <- paste0('\\textit{', traits_table$species, '}')
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