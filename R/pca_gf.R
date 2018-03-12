#' Produce pca plot and loadings table
#'
#' @param df dataframe of traits
#' @param figure_folder folder path for pca plot
#' @param table_folder folder path for pca table
#' @return saved pca plot and loadings table
#' @importFrom xtable xtable
#' @importFrom vegan envfit
#'
#' @export

pca_gf <- function (df, figure_folder, table_folder) {
  
  prin <- princomp((na.omit(df)), cor = TRUE, scores = TRUE)
  
  loadings <- as.data.frame(prin$loadings[,1:2])
  loadings$trait[row.names(loadings) == 'SLA'] <- 'Specific litter area'
  loadings$trait[row.names(loadings) == 'DMC'] <- 'Litter dry matter content'
  loadings$trait[row.names(loadings) == 'N'] <- 'Litter nitrogen'
  loadings$trait[row.names(loadings) == 'C'] <- 'Litter carbon'
  loadings$trait[row.names(loadings) == 'HC'] <- 'Litter hemicelluloses'
  loadings$trait[row.names(loadings) == 'CL'] <- 'Litter cellulose'
  loadings$trait[row.names(loadings) == 'LG'] <- 'Litter lignin'
  
  loadings <- loadings[,c('trait', 'Comp.1', 'Comp.2')]
  
  pca_loadings <- xtable::xtable(loadings)
  print(pca_loadings,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        #sanitize.text.function = identity,
        hline.after = NULL,
        file = paste0(table_folder, 'pca_loadings.tex'))
  
  pc12 <- prin$scores[,1:2]
  df_pc12 <- data.frame(pc12)
  df_pc12$sp_abrev <- rownames(pc12)
  pc12_labeled <- merge(df_pc12, log_traits[,c('sp_abrev', 'gf')])
  rownames(pc12_labeled) <- pc12_labeled[,'sp_abrev']

  fit <- vegan::envfit(pc12, na.omit(df)) # use envfit for drawing arrows, can be also done using trait loadings
  
  vars <- prin$sdev^2
  prop_vars <- vars/sum(vars)
  
  png("output/pca.png", width = 1000, height = 950)
  
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
  
  dev.off()
}
