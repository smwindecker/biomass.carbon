
# RAW THERMOGRAMS AND CARBON PROPORTIONS FROM TGA





# create legend


save_tga <- function () {
  
  A <- fractions('A', 'graminoids')
  B <- fractions('B', 'graminoids')
  BB <- fractions('BB', 'trees', xaxis = 'yes', yaxis = 'yes')
  C <- fractions('C', 'graminoids')
  CC <- fractions('CC', 'graminoids', n_curves = 4)
  DD <- fractions('DD', 'graminoids', xaxis = 'yes')
  E <- fractions('E', 'forbs')
  FF <- fractions('FF', 'graminoids', yaxis = 'yes')
  G <- fractions('G', 'forbs', xaxis = 'yes')
  H <- fractions('H', 'forbs', yaxis = 'yes')
  I <- fractions('I', 'graminoids')
  II <- fractions('II', 'graminoids', yaxis = 'yes')
  J <- fractions('J', 'graminoids', yaxis = 'yes')
  JJ <- fractions('JJ', 'nonvascular', xaxis = 'yes', yaxis = 'yes')
  K <- fractions('K', 'graminoids', yaxis = 'yes')
  KK <- fractions('KK', 'trees', xaxis = 'yes', n_curves = 3)
  L <- fractions('L', 'forbs', yaxis = 'yes')
  LL <- fractions('LL', 'graminoids', xaxis = 'yes', yaxis = 'yes')
  M <- fractions('M', 'forbs', n_curves = 3)
  MM <- fractions('MM', 'forbs', xaxis = 'yes')
  N <- fractions('N', 'graminoids')
  NN <- fractions('NN', 'shrubs', xaxis = 'yes', yaxis = 'yes', n_curves = 3)
  Q <- fractions('Q', 'trees', yaxis = 'yes')
  R <- fractions('R', 'trees')
  S <- fractions('S', 'graminoids', xaxis = 'yes')
  T_ <- fractions('T', 'forbs')
  V <- fractions('V', 'forbs', xaxis = 'yes', yaxis = 'yes')
  X <- fractions('X', 'forbs', n_curves = 3)
  Z <- fractions('Z', 'graminoids')
  
  all_weights <- as.matrix(rbind(A[[1]], B[[1]], BB[[1]], C[[1]], CC[[1]], DD[[1]], E[[1]], FF[[1]], 
                                 G[[1]], H[[1]], I[[1]], II[[1]], J[[1]], JJ[[1]], K[[1]], KK[[1]],
                                 L[[1]], LL[[1]], M[[1]], MM[[1]], N[[1]], NN[[1]], Q[[1]], R[[1]],
                                 S[[1]], T_[[1]], V[[1]], X[[1]], Z[[1]])) 
  write.table(all_weights, 'munge/tga_proportions.txt')
  
  all_params <- rbind(A[[2]], B[[2]], BB[[2]], C[[2]], CC[[2]], DD[[2]], E[[2]], FF[[2]],
                      G[[2]], H[[2]], I[[2]], II[[2]], J[[2]], JJ[[2]], K[[2]], KK[[2]],
                      L[[2]], LL[[2]], M[[2]], MM[[2]], N[[2]], NN[[2]], Q[[2]], R[[2]],
                      S[[2]], T_[[2]], V[[2]], X[[2]], Z[[2]]) %>%
    as.matrix.data.frame() %>%
    `colnames<-`(c('species_code', 'h_0', 'h_1', 'h_2', 'h_3',
                   'p_0', 'p_1', 'p_2', 'p_3',
                   's_0', 's_1', 's_2', 's_3',
                   'w_0', 'w_1', 'w_2', 'w_3')) %>%
    merge(species_df[, c('species_code', 'species')]) %>%
    select(species, 2:18) 
  t_allparams <- as.matrix(as.data.frame(all_params))
  t_allparams[, c(2:5)] <- signif(as.numeric(as.character(t_allparams[, c(2:5)])), 5)
  t_allparams[, c(6:17)] <- signif(as.numeric(as.character(t_allparams[, c(6:17)])), 3)
  t_allparams <- data.frame(t_allparams)
  t_allparams <- t_allparams[order(t_allparams$species),]
  write.table(t_allparams, 'munge/tga_parameters.txt')

  # t_allparams <- read.table('munge/tga_parameters.txt')
  tga_param_table <- t_allparams
  tga_param_table$species <- paste0('\\textit{', tga_param_table$species, '}')
  tga_param_table <- xtable(tga_param_table)
  
  print(tga_param_table,
        include.rownames = FALSE,
        include.colnames = FALSE,
        only.contents = TRUE,
        comment = FALSE,
        sanitize.text.function = identity,
        hline.after = NULL,
        file = 'docs/tga_param_table.tex')

  species_df <- read.csv('raw/species.csv', header = TRUE)
  species_df$species_name <- gsub(' ', '_', species_df$species)
  
  fs_params <- merge(t_allparams, species_df[, c('species', 'gf')])
  
  for(i in c(2:17)) {
    fs_params[,i] <- as.numeric(as.character(fs_params[,i]))
  }
  
  fs_gf <- ddply(fs_params, ~ gf, summarise, meanh1 = mean(h_1), 
                 meanh2 = mean(h_2),
                 meanh3 = mean(h_3),
                 meanp1 = mean(p_1),
                 meanp2 = mean(p_2),
                 meanp3 = mean(p_3),
                 meanw1 = mean(w_1),
                 meanw2 = mean(w_2),
                 meanw3 = mean(w_3)
  )
  
  write.table(fs_gf, 'munge/tga_gf.txt')
}

save_tga()

prop <- read.table('munge/tga_proportions.txt')
species_df <- read.csv('raw/species.csv', header = TRUE)
species_df$species_name <- gsub(' ', '_', species_df$species)

prop <- merge(prop, species_df[, c('species_code', 'gf')])
prop <- prop[prop$wt_type == 'mean',]

prop_gf <- ddply(prop, ~ gf, summarise, meanHC = mean(HC_2), 
               meanCL = mean(CL),
               meanLG = mean(LG))


params <- read.table('munge/tga_parameters.txt')
mean(params$p_0, na.rm = TRUE)

gf <- read.table('munge/tga_gf.txt')
