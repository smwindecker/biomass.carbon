#' Plot species' TGA data
#'
#' @param species_deconvoluted_list deconvoluted species data list output
#' @param species_data species detail file with growth form and full species name info
#' @param output_folder output folder
#' @return saved deconvolved plots
#' @importFrom deconvolve process deconvolve fs_mixture
#' @importFrom reshape2 dcast
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot axis lines par legend rect
#'
#' @export

tga_plot <- function (species_deconvoluted_list, species_data, output_folder) {
  
  xaxis_list <- c('BB', 'DD', 'G', 'JJ', 'KK', 'LL', 'MM', 'NN', 'S', 'V')
  yaxis_list <- c('BB', 'FF', 'H', 'II', 'J', 'JJ', 'K', 'L', 'LL', 'NN', 'Q', 'V')
  
  x <- species_deconvoluted_list$species_code
  output <- species_deconvoluted_list$output
  
  # extract species name, genus abreviation, and growth form
  sp_name <- gsub(' ', '_', species_data$species[species_data$species_code == x][1])
  spname <- species_data$sp_abrev[species_data$species_code == x][1]
  gf <- species_data$gf[species_data$species_code == x][1]
  
  # extract parameters from mixture model fit
  fit <- output$minpack.lm
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  # temperature bounds for plots
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
  
  # isolate data
  data <- output$data
  
  # plot
  png(paste0(output_folder, 'tga_', gf, '_', sp_name, '.png'), width = 560, height = 480, 'px', bg = 'transparent')
  
  if (!isTRUE(x %in% xaxis_list) && !isTRUE(x %in% yaxis_list)) {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
    axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
    
  } else if (!isTRUE(x %in% xaxis_list) && isTRUE(x %in% yaxis_list)) {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 1, at = c(150, 400, 650), labels = FALSE)
    axis(side = 2, at = c(0, 0.005, 0.01), cex.axis = 2.2,
         labels = c(sprintf("%.0f", 0), sprintf("%.3f", c(0.005, 0.01))))
    
  } else if (isTRUE(x %in% xaxis_list) && isTRUE(x %in% yaxis_list)) {
    
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
    
    y1 <- deconvolve::fs_mixture(x = temp,
                                 h1 = params['h1',], s1 = params['s1',],
                                 p1 = params['p1',], w1 = params['w1',],
                                 h2 = params['h2',], s2 = params['s2',],
                                 p2 = params['p2',], w2 = params['w2',],
                                 h3 = params['h3',], s3 = params['s3',],
                                 p3 = params['p3',], w3 = params['w3',],
                                 h0 = params['h0',], s0 = params['s0',],
                                 p0 = params['p0',], w0 = params['w0',])
    
    y5 <- deconvolve::fs_function(x = temp,
                                  h = params['h0',], s = params['s0',],
                                  p = params['p0',], w = params['w0',])
    
    lines(temp, y5, lty = 5, lwd = 2.5, col = 'orange')
    
  } 
  
  if (output$n_peaks == 3) {
    
    y1 <- deconvolve::fs_mixture(x = temp,
                                 h1 = params['h1',], s1 = params['s1',],
                                 p1 = params['p1',], w1 = params['w1',],
                                 h2 = params['h2',], s2 = params['s2',],
                                 p2 = params['p2',], w2 = params['w2',],
                                 h3 = params['h3',], s3 = params['s3',],
                                 p3 = params['p3',], w3 = params['w3',])
  }
  
  lines(temp, y1, lty = 1, lwd = 2)
  
  y2 <- deconvolve::fs_function(x = temp,
                                h = params['h1',], s = params['s1',],
                                p = params['p1',], w = params['w1',])
  lines(temp, y2, lty = 6, lwd = 3.5, col = 'blue')
  
  y3 <- deconvolve::fs_function(x = temp,
                                h = params['h2',], s = params['s2',],
                                p = params['p2',], w = params['w2',])
  lines(temp, y3, lty = 3, lwd = 3.5, col = 'red')
  
  y4 <- deconvolve::fs_function(x = temp,
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
  
}