
# RAW THERMOGRAMS AND CARBON PROPORTIONS FROM TGA

library(stargazer)
library(magrittr)
library(plyr)
library(deconvolve)
source('R/fs_function.R')
source('R/fs_mixture.R')
source('R/fs_mixture_wrap.R')
source('R/fs_mixture_4.R')
source('R/fs_mixture_wrap_4.R')
source('R/single_param.R')

species_df <- read.csv('raw/species.csv', header = TRUE)
species_df$species_name <- gsub(' ', '_', species_df$species)

fractions <- function (x, gf, xaxis = 'no', yaxis = 'no', n_curves) {

  dat <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), header = F, skip = 29)
  
  # Add column names
  names(dat) <- c('temp', 'time', 'mass_loss')
  
  # Add a column with the species code
  dat$species_code <- x
  
  init_mass <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), nrows = 1, header = F, skip = 17)
  
  # get individual value then add to df.
  init_mass <- init_mass[1,2]
  
  munge <- deconvolve::process(dat, 'temp', 'mass_loss', init_mass)
  
  if (gf == 'forbs' & n_curves == 3) {
    
    output <- deconvolve::deconvolve(munge, n_curves = 3, upper_temp = 620,
                                     start_vec = c(0.003, 0.006, 0.001, -0.15, -0.15, -0.15,
                                                   250, 330, 390, 80, 50, 200))
  }
  
  if (n_curves == 4) {
  
    output <- deconvolve::deconvolve(munge, n_curves = 4, upper_temp = 620,
                                     start_vec = c(0.002, 0.003, 0.006, 0.001, -0.15, -0.15, -0.15, -0.15,
                                                   210, 250, 330, 390, 50, 70, 60, 200))
    
  }
  
  if ((gf == 'graminoids' | gf == 'shrubs' | gf == 'nonvascular') & n_curves == 3) {
    
    output <- deconvolve::deconvolve(munge, n_curves = 3, upper_temp = 620,
                                     start_vec = c(0.003, 0.006, 0.001, -0.15, -0.15, -0.15,
                                                   240, 330, 390, 70, 60, 180))
    
  }
  
  if (gf == 'trees' & n_curves == 3){
    
    output <- deconvolve::deconvolve(munge, n_curves = 3, upper_temp = 620,
                                     start_vec = c(0.003, 0.006, 0.001, -0.15, -0.15, -0.15,
                                                   250, 330, 390, 80, 60, 220))
    
  }

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
    axis(side = 1, at = c(150, 375, 600), cex.axis = 2.2, labels = c(150, 375, 600))
    
  } else if (xaxis == 'no' && yaxis == 'yes') {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 375, 600), labels = FALSE)
    axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
         labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
    
  } else if (xaxis == 'yes' && yaxis == 'yes') {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 375, 600), cex.axis = 2.2, labels = c(150, 375, 600))
    axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
         labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
    
  } else {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 375, 600), labels = FALSE)
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
  
  legend(620, .01,
         xjust = 1,
         legend = spname, 
         text.font = 3,
         cex = 3.1,
         bty = 'n')
  
  dev.off()
  
  frac <- data.frame(t(output$mass_fractions))
  
  if (ncol(frac) == 3) {
    frac$HC_1 <- NA
    colnames(frac)[1] <- 'HC_2'
  }
  
  frac$species_code <- x
  frac <- frac[, c('species_code', 'HC_1', 'HC_2', 'CL', 'LG')]
  
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
  
  return(list(frac, params))
  
}

A <- fractions('A', 'graminoids', n_curves = 3)
B <- fractions('B', 'graminoids', n_curves = 4)
BB <- fractions('BB', 'trees', xaxis = 'yes', yaxis = 'yes', n_curves = 3)
C <- fractions('C', 'graminoids', n_curves = 3)
CC <- fractions('CC', 'graminoids', n_curves = 4)
DD <- fractions('DD', 'graminoids', xaxis = 'yes', n_curves = 3)
E <- fractions('E', 'forbs', n_curves = 3)
FF <- fractions('FF', 'graminoids', yaxis = 'yes', n_curves = 3)
G <- fractions('G', 'forbs', xaxis = 'yes', n_curves = 3)
H <- fractions('H', 'forbs', yaxis = 'yes', n_curves = 4)
I <- fractions('I', 'graminoids', n_curves = 3)
II <- fractions('II', 'graminoids', yaxis = 'yes', n_curves = 3)
J <- fractions('J', 'graminoids', yaxis = 'yes', n_curves = 3)
JJ <- fractions('JJ', 'nonvascular', xaxis = 'yes', yaxis = 'yes', n_curves = 3)
K <- fractions('K', 'graminoids', yaxis = 'yes', n_curves = 3)
KK <- fractions('KK', 'trees', xaxis = 'yes', n_curves = 3)
L <- fractions('L', 'forbs', yaxis = 'yes', n_curves = 3)
LL <- fractions('LL', 'graminoids', xaxis = 'yes', yaxis = 'yes', n_curves = 3)
M <- fractions('M', 'forbs', n_curves = 3)
MM <- fractions('MM', 'forbs', xaxis = 'yes', n_curves = 3)
N <- fractions('N', 'graminoids', n_curves = 3)
NN <- fractions('NN', 'shrubs', xaxis = 'yes', yaxis = 'yes', n_curves = 3)
Q <- fractions('Q', 'trees', yaxis = 'yes', n_curves = 3)
R <- fractions('R', 'trees', n_curves = 3)
S <- fractions('S', 'graminoids', xaxis = 'yes', n_curves = 3)
T_ <- fractions('T', 'forbs', n_curves = 3)
V <- fractions('V', 'forbs', xaxis = 'yes', yaxis = 'yes', n_curves = 3)
X <- fractions('X', 'forbs', n_curves = 3)
Z <- fractions('Z', 'graminoids', n_curves = 3)

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

all_fractions <- rbind(A[[1]], B[[1]], BB[[1]], C[[1]], CC[[1]], DD[[1]], E[[1]], FF[[1]], 
                       G[[1]], H[[1]], I[[1]], II[[1]], J[[1]], JJ[[1]], K[[1]], KK[[1]],
                       L[[1]], LL[[1]], M[[1]], MM[[1]], N[[1]], NN[[1]], Q[[1]], R[[1]],
                       S[[1]], T_[[1]], V[[1]], X[[1]], Z[[1]]) %>% 
  as.matrix %>%
  write.table('munge/tga_proportions_withpackage.txt')

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
t_allparams[, c(2:17)] <- signif(as.numeric(as.character(t_allparams[, c(2:17)])), 3)
t_allparams <- data.frame(t_allparams)
t_allparams <- t_allparams[order(t_allparams$species),]

write.table(t_allparams, 'munge/tga_parameters_withpackage.txt') 
stargazer(t_allparams, summary = FALSE, rownames = FALSE)

fs_params <- read.table('munge/tga_parameters_withpackage.txt')
fs_params2 <- merge(fs_params, species[, c('species', 'gf')])
summary(fs_params2)

fs_gf <- ddply(fs_params2, ~gf, summarise, meanh1 = mean(h_1), 
               meanh2 = mean(h_2),
               meanh3 = mean(h_3),
               meanp1 = mean(p_1),
               meanp2 = mean(p_2),
               meanp3 = mean(p_3),
               meanw1 = mean(w_1),
               meanw2 = mean(w_2),
               meanw3 = mean(w_3)
)
