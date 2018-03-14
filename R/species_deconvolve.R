#' Deconvolve species' TGA data
#'
#' @param x species code in file name
#' @param species_data species detail file with growth form and full species name info
#' @param output_folder output folder
#' @param xaxis logical option whether to include x-axis label
#' @param yaxis logical option whether to include y-axis label
#' @param n_curves optional to manually specify number of curves
#' @return saved deconvolved plot and weight
#' @import deconvolve
#'
#' @export

species_deconvolve <- function (x, species_data, output_folder, 
                                xaxis = FALSE, 
                                yaxis = FALSE, 
                                n_curves = NULL) {
  
  file <- paste0('raw/TGA/', x, '_TGA.csv')
  munge <- process_raw_tga(file)
  
  output <- deconvolve::deconvolve(munge, upper_temp = 650, n_curves = n_curves)
  
  species_data$species_name <- gsub(' ', '_', species_data$species)
  data <- merge(output$data, species_data)
  sp_name <- data$species_name[data$species_code == x][1]
  spname <- data$sp_abrev[data$species_code == x][1]
  gf <- species_df$gf[species_df$species_code == x][1]
  
  fit <- output$minpack.lm  
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))

  png(paste0(output_folder, gf, '/', sp_name, '.png'), width = 560, height = 480, 'px', bg = 'transparent')
  
  if (isFALSE(xaxis) && isFALSE(yaxis)) {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
    axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
    
  } else if (isFALSE(xaxis) && isTRUE(yaxis)) {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 400, 650), labels = FALSE)
    axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
         labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
    
  } else if (isTRUE(xaxis) && isTRUE(yaxis)) {
    
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
    
    y1 <- fs_mixture(x = temp,
                     h1 = params['h1',], s1 = params['s1',],
                     p1 = params['p1',], w1 = params['w1',],
                     h2 = params['h2',], s2 = params['s2',],
                     p2 = params['p2',], w2 = params['w2',],
                     h3 = params['h3',], s3 = params['s3',],
                     p3 = params['p3',], w3 = params['w3',],
                     h4 = params['h4',], s4 = params['s4',],
                     p4 = params['p4',], w4 = params['w4',])
    
    y5 <- fs_function(x = temp,
                      h = params['h4',], s = params['s4',],
                      p = params['p4',], w = params['w4',])

    lines(temp, y5, lty = 5, lwd = 2.5, col = 'blue')
    
  } 
  
  if (output$n_peaks == 3) {
    
    y1 <- fs_mixture(x = temp,
                     h1 = params['h1',], s1 = params['s1',],
                     p1 = params['p1',], w1 = params['w1',],
                     h2 = params['h2',], s2 = params['s2',],
                     p2 = params['p2',], w2 = params['w2',],
                     h3 = params['h3',], s3 = params['s3',],
                     p3 = params['p3',], w3 = params['w3',])
  }
  
  lines(temp, y1, lty = 1, lwd = 2)
  
  y2 <- fs_function(x = temp,
                    h = params['h1',], s = params['s1',],
                    p = params['p1',], w = params['w1',])
  lines(temp, y2, lty = 6, lwd = 3.5, col = 'orange')
  
  y3 <- fs_function(x = temp,
                    h = params['h2',], s = params['s2',],
                    p = params['p2',], w = params['w2',])
  lines(temp, y3, lty = 3, lwd = 3.5, col = 'red')
  
  y4 <- fs_function(x = temp,
                    h = params['h3',], s = params['s3',],
                    p = params['p3',], w = params['w3',])
  lines(temp, y4, lty = 4, lwd = 3.5, col = 'green3')

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
  ci_weights$wt_type <- rownames(ci_weights)
  weights <- rbind(mean_weights, ci_weights)
  
  if (ncol(weights) == 4) {
    weights$HC_1 <- NA
    colnames(weights)[1] <- 'HC_2'
  }
  
  weights$species_code <- x
  weights <- weights[, c('species_code', 'HC_1', 'HC_2', 'CL', 'LG', 'wt_type')]
  
  return(list(weights, params))
  
}