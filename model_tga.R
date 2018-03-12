
# RAW THERMOGRAMS AND CARBON PROPORTIONS FROM TGA



fractions <- function (x, gf, 
                       xaxis = 'no', 
                       yaxis = 'no', 
                       output_folder = 'output/', 
                       n_curves = NULL) {

  dat <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), header = F, skip = 29)

  # Add column names
  names(dat) <- c('temp', 'time', 'mass_loss')

  # Add a column with the species code
  dat$species_code <- x

  init_mass <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), nrows = 1, header = F, skip = 17)

  # get individual value then add to df.
  init_mass <- init_mass[1,2]

  munge <- deconvolve::process(dat, 'temp', 'mass_loss', init_mass)
  output <- deconvolve::deconvolve(munge, upper_temp = 650, n_curves = n_curves)
  
  species_df$species_name <- gsub(' ', '_', species_df$species)
  output$data <- merge(output$data, species_df)
  
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
  fit <- output$minpack.lm  
  data <- output$data
  sp_name <- data$species_name[data$species_code == x][1]
  spname <- data$sp_abrev[data$species_code == x][1]
  
  png(paste0('output/tga_fs/', gf, '/', sp_name, '.png'), width = 560, height = 480, 'px', bg = 'transparent')
  
  if (xaxis == 'yes' && yaxis == 'no') {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
    axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
    
  } else if (xaxis == 'no' && yaxis == 'yes') {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 400, 650), labels = FALSE)
    axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
         labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
    
  } else if (xaxis == 'yes' && yaxis == 'yes') {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
    axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
         labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
    
  } else {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 400, 650), labels = FALSE)
    axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
    
  }
  
  if (output$n_peaks == 4) {
    
    y1 <- fs_mixture_wrap_4(temp,
                            single_param(fit, 'h', '1'), single_param(fit, 'h', '2'),
                            single_param(fit, 'h', '3'), single_param(fit, 'h', '4'),
                            single_param(fit, 's', '1'), single_param(fit, 's', '2'),
                            single_param(fit, 's', '3'), single_param(fit, 's', '4'),
                            single_param(fit, 'p', '1'), single_param(fit, 'p', '2'),
                            single_param(fit, 'p', '3'), single_param(fit, 'p', '4'),
                            single_param(fit, 'w', '1'), single_param(fit, 'w', '2'),
                            single_param(fit, 'w', '3'), single_param(fit, 'w', '4'))
    lines(temp, y1, lty = 1, lwd = 2)
    
    y2 <- fs_function(temp,
                      single_param(fit, 'h', '1'), single_param(fit, 's', '1'),
                      single_param(fit, 'p', '1'), single_param(fit, 'w', '1'))
    lines(temp, y2, lty = 6, lwd = 3.5, col = 'orange')
    
    y3 <- fs_function(temp,
                      single_param(fit, 'h', '2'), single_param(fit, 's', '2'),
                      single_param(fit, 'p', '2'), single_param(fit, 'w', '2'))
    lines(temp, y3, lty = 3, lwd = 3.5, col = 'red')
    
    y4 <- fs_function(temp,
                      single_param(fit, 'h', '3'), single_param(fit, 's', '3'),
                      single_param(fit, 'p', '3'), single_param(fit, 'w', '3'))
    lines(temp, y4, lty = 4, lwd = 3.5, col = 'green3')
    
    y5 <- fs_function(temp,
                      single_param(fit, 'h', '4'), single_param(fit, 's', '4'),
                      single_param(fit, 'p', '4'), single_param(fit, 'w', '4'))
    lines(temp, y5, lty = 5, lwd = 2.5, col = 'blue')
    
  } else {
    
    y1 <- fs_mixture_wrap(temp,
                          single_param(fit, 'h', '1'), single_param(fit, 'h', '2'),
                          single_param(fit, 'h', '3'), single_param(fit, 's', '1'),
                          single_param(fit, 's', '2'), single_param(fit, 's', '3'),
                          single_param(fit, 'p', '1'), single_param(fit, 'p', '2'),
                          single_param(fit, 'p', '3'), single_param(fit, 'w', '1'),
                          single_param(fit, 'w', '2'), single_param(fit, 'w', '3'))
    lines(temp, y1, lty = 1, lwd = 2)
    
    y2 <- fs_function(temp,
                      single_param(fit, 'h', '1'), single_param(fit, 's', '1'),
                      single_param(fit, 'p', '1'), single_param(fit, 'w', '1'))
    lines(temp, y2, lty = 3, lwd = 3.5, col = 'red')
    
    y3 <- fs_function(temp,
                      single_param(fit, 'h', '2'), single_param(fit, 's', '2'),
                      single_param(fit, 'p', '2'), single_param(fit, 'w', '2'))
    lines(temp, y3, lty = 4, lwd = 3.5, col = 'green3')
    
    y4 <- fs_function(temp,
                      single_param(fit, 'h', '3'), single_param(fit, 's', '3'),
                      single_param(fit, 'p', '3'), single_param(fit, 'w', '3'))
    lines(temp, y4, lty = 5, lwd = 2.5, col = 'blue')
    
  }
  
  legend(650, .01,
         xjust = 1,
         legend = spname, 
         text.font = 3,
         cex = 3.1,
         bty = 'n')
  
  dev.off()
  
  mean_weights <- data.frame(t(output$mean_weights))
  mean_weights$wt_type <- 'mean'
  ci_weights <- data.frame(output$CI_weights)
  ci_weights $wt_type <- rownames(ci_weights)
  weights <- rbind (mean_weights, ci_weights)
  
  if (ncol(weights) == 4) {
    weights$HC_1 <- NA
    colnames(weights)[1] <- 'HC_2'
  }

  weights$species_code <- x
  weights <- weights[, c('species_code', 'HC_1', 'HC_2', 'CL', 'LG', 'wt_type')]
  
  h_vals <- list(h_0 = NA, h_1 = NA, h_2 = NA, h_3 = NA)
  p_vals <- list(p_0 = NA, p_1 = NA, p_2 = NA, p_3 = NA)
  s_vals <- list(s_0 = NA, s_1 = NA, s_2 = NA, s_3 = NA)
  w_vals <- list(w_0 = NA, w_1 = NA, w_2 = NA, w_3 = NA)
  
  if (output$n_peaks == 3) {
    for (i in 1:output$n_peaks) {
      
      h_vals[1+i] <- single_param(fit, 'h', i)
      names(h_vals)[1+i] <- paste('h', i)
      p_vals[1+i] <- single_param(fit, 'p', i)
      names(p_vals)[1+i] <- paste('p', i)
      s_vals[1+i] <- single_param(fit, 's', i)
      names(s_vals)[1+i] <- paste('s', i)
      w_vals[1+i] <- single_param(fit, 'w', i)
      names(w_vals)[1+i] <- paste('w', i)
      
    }
  }
  
  if (output$n_peaks == 4) {
    for (i in 1:output$n_peaks) {
      
      h_vals[i] <- single_param(fit, 'h', i)
      names(h_vals)[i] <- paste('h', i)
      p_vals[i] <- single_param(fit, 'p', i)
      names(p_vals)[i] <- paste('p', i)
      s_vals[i] <- single_param(fit, 's', i)
      names(s_vals)[i] <- paste('s', i)
      w_vals[i] <- single_param(fit, 'w', i)
      names(w_vals)[i] <- paste('w', i)
      
    }
    
  }
  
  params <- c(x, h_vals, p_vals, s_vals, w_vals)
  names(params) <- c('species_code', 'h_0', 'h_1', 'h_2', 'h_3',
                     'p_0', 'p_1', 'p_2', 'p_3',
                     's_0', 's_1', 's_2', 's_3',
                     'w_0', 'w_1', 'w_2', 'w_3')

  return(list(weights, params))

}

# create legend
legend <- function () {
  
  dat <- read.csv(paste0('raw/TGA/T_TGA.csv'), header = F, skip = 29)
  names(dat) <- c('temp', 'time', 'mass_loss')
  munge <- process(dat, 'temp', 'mass_loss', 16)
  output <- deconvolve(munge, n_curves = 4)
  data <- output$data
  
  png(paste0('output/tga_fs/legend.png'), width = 560, height = 480, 'px', bg = 'transparent')
  par(xpd = TRUE, mar = c(10, 2, 2, 2))
  plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, max(data$deriv) + 0.06*max(data$deriv)),
       ylab = '', xlab = '', pch = 20, cex = 0.3, cex.axis = 1.8)
  rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
  
  legend(120, 0.008,
         xjust = 0,
         legend = c('data', 'total DTG', 'HC-1', 'HC-2', 'CL', 'LG'),
         ncol = 2,
         cex = 1.4,
         bty = 'n',
         lty = c(NA, 1, 6, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', 'orange', 'red', 'green3', 'blue'),
         lwd = 2)
  
  dev.off()
}

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
