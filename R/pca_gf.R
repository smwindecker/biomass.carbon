#' Produce pca plot and loadings table
#'
#' @param df dataframe of traits
#' @param output_folder folder path for pca plot
#' @return saved pca plot and loadings table
#' @importFrom xtable xtable
#' @importFrom vegan envfit
#' @importFrom stats princomp na.omit
#' @importFrom grDevices png dev.off 
#' @importFrom graphics plot mtext legend text
#'
#' @export

pca_gf <- function (df, species_data, output_folder) {
  
  # calculate loadins 
  prin <- stats::princomp((stats::na.omit(df)), cor = TRUE, scores = TRUE)
  
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
        #sanitize.text.function = identity,
        hline.after = NULL,
        file = paste0(output_folder, 'pca_loadings.tex'))
  
  # first two axes' scores
  pc12 <- prin$scores[, 1:2]
  df_pc12 <- data.frame(pc12)
  df_pc12$sp_abrev <- rownames(pc12)
  
  # label with abreviations
  pc12_labeled <- merge(df_pc12, species_data[,c('sp_pca_label', 'gf')])
  rownames(pc12_labeled) <- pc12_labeled[,'sp_pca_label']

  # get length of axes
  fit <- vegan::envfit(pc12, na.omit(df)) 
  
  vars <- prin$sdev^2
  prop_vars <- vars/sum(vars)
  
  png(paste0(output_folder, 'pca.png'), width = 1000, height = 950)
  
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
