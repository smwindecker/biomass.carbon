#' Produce pca plot and loadings table
#'
#' @param prin pca data output
#' @param df logged traits matrix
#' @param species_data species dataframe so can get the abreviated species labels
#' @param output_file file path for pca plot
#' @return saved pca plot
#' @importFrom vegan envfit
#' @importFrom stats na.omit
#' @importFrom grDevices png dev.off 
#' @importFrom graphics plot mtext legend text
#'
#' @export

pca <- function (prin, df, species_data, output_file) {
  
  # first two axes' scores
  pc12 <- prin$scores[, 1:2]
  df_pc12 <- data.frame(pc12)
  df_pc12$sp_abrev <- rownames(df_pc12)
  
  # label with abreviations
  pc12_labeled <- merge(df_pc12, species_data[,c('sp_abrev', 'sp_pca_label', 'gf')])
  rownames(pc12_labeled) <- pc12_labeled[,'sp_pca_label']
  
  # get length of axes
  fit <- vegan::envfit(pc12, na.omit(df)) 
  
  vars <- prin$sdev^2
  prop_vars <- vars/sum(vars)
  
  png(output_file, width = 1000, height = 950)
  
  plot(pc12_labeled[, c('Comp.1', 'Comp.2')], ylab='', xlab='', xaxt = 'n', yaxt = 'n', ylim=c(-4, 4), xlim=c(-4.2, 5), 
       cex.axis = 1, cex = 1.8, pch = c(2, 20, 3, 8, 0)[as.numeric(pc12_labeled$gf)])
  plot(fit, cex = 2, col = 1, labels = list(vectors = c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')))
  mtext(text = paste0('Axis 1 (', (100*round(prop_vars[[1]], 2)), '%)'), side = 1, 
        cex = 2, padj = 1)
  mtext(text = paste0('Axis 2 (', (100*round(prop_vars[[2]], 2)), '%)'), side = 2, 
        cex = 2, padj = 0)
  legend('topright', 
         c('Forb', 'Graminoid', 'Non-vascular', 'Shrub', 'Tree'), 
         bty = 'n',
         pch = c(2, 20, 3, 8, 0), 
         cex = 1.8)
  text(x = pc12_labeled[, 'Comp.1'], y = pc12_labeled[, 'Comp.2'],
       labels = row.names(pc12_labeled), vfont = c('sans serif', 'italic'),
       cex = 1.5, pos = 4)
  
  dev.off()
}
