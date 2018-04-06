#' J. amabilis individual curves for TGA explanation figure
#'
#' @param list_item split species row as list item
#' @param species_data species detail file with growth form and full species name info
#' @param output_folder output folder
#' @return saved deconvolved plot and list of parameters and weights
#' @importFrom deconvolve fs_function fs_mixture
#' @importFrom grDevices dev.off png
#' @importFrom graphics par axis legend plot
#'
#' @export

j_amabilis <- function (species_code, output_folder) {
  
  # read raw TGA
  file <- paste0('data-raw/TGA/', species_code, '_TGA.csv')
  tmp <- process_raw_tga(file)
  
  # plot TG curve
  png(paste0(output_folder, 'theory_TG.png'), width = 560, height = 480, 'px', bg = 'transparent')
  par(mar = c(6, 6, 2, 2))
  plot(tmp$data$temp_C, tmp$data$mass_T, yaxs = 'i', ylim = c(0, 22), xlim = c(0, 900),
       xaxs = 'i', ylab = 'Mass (mg)', xlab = 'Temperature (C)', xaxt = 'n', yaxt = 'n', 
       pch = 20, cex = 0.3, cex.axis = 1.8, cex.lab = 1.8)
  axis(side = 1, at = c(0, 200, 400, 600, 800), cex.axis = 1.8, labels = c(0, 200, 400, 600, 800))
  axis(side = 2, at = c(0, 10, 20), cex.axis = 1.8,
       labels = c(0, 10, 20))
  legend('topleft',
         legend = '(a)', 
         bty = 'n', 
         cex = 1.8)
  dev.off()
  
  # plot DTG curve
  png(paste0(output_folder, 'theory_DTG.png'), width = 560, height = 480, 'px', bg = 'transparent')
  par(mar = c(6, 6, 2, 2))
  plot(tmp$data$temp_C, tmp$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
       xaxs = 'i', ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = 'Temperature (C)', xaxt = 'n', yaxt = 'n', 
       pch = 20, cex.axis = 1.8, cex.lab = 1.8, cex = 0.9)
  axis(side = 1, at = c(0, 200, 400, 600, 800), cex.axis = 1.8, labels = c(0, 200, 400, 600, 800))
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 1.8,
       labels = c(0, 0.004, 0.008))
  legend('topleft',
         legend = '(b)', 
         bty = 'n', 
         cex = 1.8)
  dev.off()
  
  # deconvolute data
  output <- deconvolve::deconvolve(tmp, upper_temp = 650, n_curves = NULL)
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
  fit <- output$minpack.lm  
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  # plot mixture model outcome on DTG data
  png(paste0(output_folder, 'theory_DTG-mixture.png'), width = 560, height = 480, 'px', bg = 'transparent')
  par(mar = c(6, 6, 2, 2))
  plot(output$data$temp_C, output$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = 'Temperature (C)', 
       xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.9, cex.axis = 1.8, cex.lab = 1.8)
  axis(side = 1, at = c(200, 400, 600, 800), cex.axis = 1.8, labels = c(200, 400, 600, 800))
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 1.8,
       labels = c(0, 0.004, 0.008))
  
  y1 <- fs_mixture(x = temp,
                   h1 = params['h1',], s1 = params['s1',],
                   p1 = params['p1',], w1 = params['w1',],
                   h2 = params['h2',], s2 = params['s2',],
                   p2 = params['p2',], w2 = params['w2',],
                   h3 = params['h3',], s3 = params['s3',],
                   p3 = params['p3',], w3 = params['w3',])

  y2 <- fs_function(x = temp,
                    h = params['h1',], s = params['s1',],
                    p = params['p1',], w = params['w1',])
  
  y3 <- fs_function(x = temp,
                    h = params['h2',], s = params['s2',],
                    p = params['p2',], w = params['w2',])
  
  y4 <- fs_function(x = temp,
                    h = params['h3',], s = params['s3',],
                    p = params['p3',], w = params['w3',])
  
  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = 'red')
  lines(temp, y3, lty = 4, lwd = 3.5, col = 'green3')
  lines(temp, y4, lty = 5, lwd = 2.5, col = 'blue')
  
  legend('topright',
         legend = c('DTG data', 'DTG modelled', 'HC', 'CL', 'LG'), 
         ncol = 1,
         cex = 1.5,
         bty = 'n',
         lty = c(NA, 1, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA),
         col = c('black', 'black', 'red', 'green3', 'blue'),
         lwd = 2)
  legend('topleft',
         legend = '(c)', 
         bty = 'n', 
         cex = 1.8)
  dev.off()
  
}