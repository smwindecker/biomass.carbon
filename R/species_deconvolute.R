#' Deconvolute species' TGA data
#'
#' @param list_item split species row as list item
#' @param species_data species detail file with growth form and full species name info
#' @param output_folder output folder
#' @return saved deconvolved plot and list of parameters and weights
#' @importFrom deconvolve process deconvolve
#' @importFrom reshape2 dcast
#' @importFrom grDevices dev.off png
#' @importFrom graphics axis legend plot rect
#' @importFrom utils read.csv
#'
#' @export

species_deconvolute <- function (list_item, species_data, output_folder) {
  
  # extract species code, axis label option, and number of curves
  x <- list_item$species_code
  xaxis <- list_item$xaxis
  yaxis <- list_item$yaxis
  n_curves <- list_item$n_curves
  
  # turn NA's into NULL's
  if (is.na(n_curves)) n_curves = NULL
  
  # read raw TGA
  file <- paste0('data-raw/TGA/', x, '_TGA.csv')
  df <- read.csv(file, header = FALSE, skip = 29)
  names(df) <- c('temp', 'time', 'mass_loss')
  init_mass <- read.csv(file, nrows = 1, header = FALSE, skip = 17)[1,2]
  
  # process TGA data
  tmp <- deconvolve::process(df, 'temp', 'mass_loss', init_mass)
  
  # deconvolute TGA data
  output <- deconvolve::deconvolve(tmp, upper_temp = 650, n_curves = n_curves)
  
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
  
  png(paste0(output_folder, 'tga_', gf, '_', sp_name, '.png'), width = 560, height = 480, 'px', bg = 'transparent')
  
  if (!isTRUE(xaxis) && !isTRUE(yaxis)) {
    
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
    rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')
    plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
         ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
    axis(side = 2, at = c(0, 0.005, 0.01), labels = FALSE)
    axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
    
  } else if (!isTRUE(xaxis) && isTRUE(yaxis)) {
    
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
                     h0 = params['h0',], s0 = params['s0',],
                     p0 = params['p0',], w0 = params['w0',])
    
    y5 <- fs_function(x = temp,
                      h = params['h0',], s = params['s0',],
                      p = params['p0',], w = params['w0',])
    
    lines(temp, y5, lty = 5, lwd = 2.5, col = 'orange')
    
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
  lines(temp, y2, lty = 6, lwd = 3.5, col = 'blue')
  
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
  
  # extract weights of components
  mean_weights <- data.frame(t(output$mean_weights))
  mean_weights$wt_type <- 'mean'
  
  # extract confidence intervals of component weights
  ci_weights <- data.frame(output$CI_weights)
  ci_weights$wt_type <- rownames(ci_weights)
  
  # combine means and confidence intervals of weights
  weights <- rbind(mean_weights, ci_weights)
  
  if (ncol(weights) == 4) {
    weights$HC_1 <- NA
    colnames(weights)[1] <- 'HC_2'
  }
  
  # set species of parameter outputs and make data wide
  params$species_code <- x
  params$parameter <- row.names(params)
  colnames(params) <- c('coefficient', 'species_code', 'parameter')
  params <- reshape2::dcast(params, species_code ~ parameter, value.var = 'coefficient')
  
  # set species of weight outputs
  weights$species_code <- x
  weights <- weights[, c('species_code', 'HC_1', 'HC_2', 'CL', 'LG', 'wt_type')]
  
  return(list(weights = weights, params = params))
  
}