#' Produce TGA figure legend
#'
#' @param sample_data_file file path to sample tga csv data for legend construction
#' @param output_file file path for output plot
#' @return saved TGA legend
#'
#' @export

tga_plots_legend <- function (sample_data_file, output_file) {
  
  dat <- read.csv(sample_data_file, header = F, skip = 29)
  names(dat) <- c('temp', 'time', 'mass_loss')
  munge <- process(dat, 'temp', 'mass_loss', 16)
  output <- deconvolve(munge, n_curves = 4)
  data <- output$data
  
  png(output_file, width = 560, height = 480, 'px', bg = 'transparent')
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