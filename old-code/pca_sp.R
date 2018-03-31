pca_sp <- function (df) {

  library(xtable)
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
  
  pca_loadings <- xtable(loadings)
  print(pca_loadings,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        #sanitize.text.function = identity,
        hline.after = NULL,
        file = 'docs/pca_loadings.tex')
  
  pc12 <- prin$scores[,1:2]
  write.csv(pc12, '../THESIS.bioassay/raw/pca_species.csv')
  
  # so that if the axes are reversed, it goes the way I expect with lignin on top.
  if (pc12[row.names(pc12)=='E. camaldulensis',2] < 0) pc12[,2] <- pc12[,2]*1
  df_pc12 <- data.frame(pc12)
  df_pc12$sp_abrev <- rownames(pc12)
  pc12_labeled <- merge(df_pc12, log_traits[,c('sp_abrev', 'gf')])
  rownames(pc12_labeled) <- pc12_labeled[,'sp_abrev']
  require(vegan)
  fit <- vegan::envfit(pc12, na.omit(df)) # use envfit for drawing arrows, can be also done using trait loadings

  vars <- prin$sdev^2
  prop_vars <- vars/sum(vars)
  
  png('figs/pca_sp.png', width = 760, height = 760)
  
  plot(pc12_labeled[, c('Comp.1', 'Comp.2')], ylab='', xlab='', xaxt = 'n', yaxt = 'n', ylim=c(-4, 5), xlim=c(-5, 5), 
       cex.axis=0.75, cex = 1.2, pch = c(2, 20, 3, 8, 0)[as.numeric(pc12_labeled$gf)])
  
  plot(fit, cex = 1.5, col = 1)
  
  mtext(text = paste0('Axis 1 (', (100*round(prop_vars[[1]], 2)), '%)'), side = 1, 
        cex = 2, padj = 1)
  mtext(text = paste0('Axis 2 (', (100*round(prop_vars[[2]], 2)), '%)'), side = 2, 
        cex = 2, padj = 0)
  legend(2.7, 4.8, c('forb', 'graminoid', 'non-vascular', 'shrub', 'tree'), 
         pch = c(2, 20, 3, 8, 0), cex = 1.5)
  
  reg_labels <- pc12_labeled[!rownames(pc12_labeled) %in% c('M. squarrosa', 'L. australis', 'C. appressa', 
                                                            'C. procerum', 'J. amabilis', 'R. tetraphyllus', 
                                                            'P. australis', 'T. domingensis', 'G. filum', 
                                                            'B. articulata', 'B. rubiginosa', 'Sphagnum'), ]
  text(x = reg_labels[, 'Comp.1'], y = reg_labels[, 'Comp.2'], 
       labels = row.names(reg_labels), vfont = c('sans serif', 'italic'), 
       cex = 1.5, pos = 4)
  
  up_labels <- pc12_labeled[c('M. squarrosa', 'L. australis', 'C. appressa', 
                              'C. procerum', 'J. amabilis' ), ]
  text(x = up_labels[, 'Comp.1'], y = up_labels[, 'Comp.2']+0.2, 
       labels = row.names(up_labels), vfont = c('sans serif', 'italic'), 
       cex = 1.5, pos = 4)
  
  v.up_label <- pc12_labeled[c('R. tetraphyllus'), ]
  text(x = v.up_label['Comp.1'], y = v.up_label['Comp.2']+0.25, 
       labels = 'R. tetraphyllus', vfont = c('sans serif', 'italic'), 
       cex = 1.5, pos = 4)
  
  down_labels <- pc12_labeled[c('P. australis', 'T. domingensis', 'G. filum', 
                                'B. articulata', 'B. rubiginosa'), ]
  text(x = down_labels[, 'Comp.1'], y = down_labels[, 'Comp.2']-0.2, 
       labels = row.names(down_labels), vfont = c('sans serif', 'italic'), 
       cex = 1.5, pos = 4)
  
  l.down_labels <- pc12_labeled[c('Sphagnum'), ]
  text(x = l.down_labels[, 'Comp.1'], y = l.down_labels[, 'Comp.2']-0.15, 
       labels = row.names(l.down_labels), vfont = c('sans serif', 'italic'), 
       cex = 1.5, pos = 4)
  
  # right_label <- pc12_labeled['J. amabilis', ]
  # text(x = right_label['Comp.1'], y = right_label['Comp.2'], 
  #      labels = 'J. amabilis', vfont = c('sans serif', 'italic'), 
  #      cex = 1.5, pos = 4, offset = 0.9)
  
  dev.off()
}
