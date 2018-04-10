#' Produce pca plot and loadings table
#'
#' @param prin pca output
#' @param output_file file path for loadings table
#' @return saved loadings table
#' @importFrom xtable xtable
#'
#' @export

pca_loadings <- function(prin, output_file) {
  
  # isolate first two axes 
  loadings <- as.data.frame(prin$loadings[,1:2])
  
  # expand trait names 
  loadings$trait[row.names(loadings) == 'SLA'] <- 'Specific litter area'
  loadings$trait[row.names(loadings) == 'DMC'] <- 'Litter dry matter content'
  loadings$trait[row.names(loadings) == 'N'] <- 'Litter nitrogen'
  loadings$trait[row.names(loadings) == 'C'] <- 'Litter carbon'
  loadings$trait[row.names(loadings) == 'HC'] <- 'Litter hemicelluloses'
  loadings$trait[row.names(loadings) == 'CL'] <- 'Litter cellulose'
  loadings$trait[row.names(loadings) == 'LG'] <- 'Litter lignin'
  
  loadings <- loadings[,c('trait', 'Comp.1', 'Comp.2')]
  
  # create xtable
  pca_loadings <- xtable::xtable(loadings)
  
  print(pca_loadings,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        hline.after = NULL,
        file = output_file)
  
}