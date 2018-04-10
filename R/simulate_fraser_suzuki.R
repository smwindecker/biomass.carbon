#' Produce parameter simulation of Fraser-Suzuki function
#'
#' @param output_file file path for output plot
#' @importFrom deconvolve fs_function
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot axis lines par legend 
#' @return saved simulated Fraser-Suzuki plot
#'
#' @export


simulate_fraser_suzuki <- function (output_file) {
  
  x <- seq(200, 700)
  h1 <- deconvolve::fs_function(x, 0.004, -0.25, 400, 60)
  h2 <- deconvolve::fs_function(x, 0.006, -0.25, 400, 60)
  h3 <- deconvolve::fs_function(x, 0.008, -0.25, 400, 60)
  h4 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 60)
  
  s1 <- deconvolve::fs_function(x, 0.010, -0.55, 400, 60)
  s2 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 60)
  s3 <- deconvolve::fs_function(x, 0.010, 0.25, 400, 60)
  s4 <- deconvolve::fs_function(x, 0.010, 0.55, 400, 60)
  
  p1 <- deconvolve::fs_function(x, 0.010, -0.25, 350, 60)
  p2 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 60)
  p3 <- deconvolve::fs_function(x, 0.010, -0.25, 450, 60)
  p4 <- deconvolve::fs_function(x, 0.010, -0.25, 500, 60)
  
  w1 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 30)
  w2 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 60)
  w3 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 90)
  w4 <- deconvolve::fs_function(x, 0.010, -0.25, 400, 120)
  
  png(output_file, width = 800, height = 700)
  par(mfrow = c(2, 2))
  
  par(mar = c(1,6,6,1))
  plot(x, h4, type = 'l', lty = 4, xlab = '', xaxt = 'n', yaxt = 'n', cex = 1.6, cex.lab = 1.6,
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')))
  axis(side = 2, at = c(0, 0.002, 0.004, 0.006, 0.008, 0.010), cex.axis = 1.2,
       labels = c(0, 0.002, 0.004, 0.006, 0.008, 0.010))
  lines(x, h2, lty = 2)
  lines(x, h3, lty = 3)
  lines(x, h1, lty = 1)
  legend('topleft', legend = '(a)', bty = 'n', cex = 1.6)
  legend('topright', legend = c(expression(paste('h = 0.004 C'^'-1')), 
                                expression(paste('h = 0.006 C'^'-1')),
                                expression(paste('h = 0.008 C'^'-1')), 
                                expression(paste('h = 0.010 C'^'-1')),
                                's = -0.25',
                                'p = 400 C',
                                'w = 60 C'),
         bty = 'n', cex = 1.2,
         lty = c(1, 2, 3, 4, NA, NA, NA)
  )
  
  par(mar = c(1,1,6,6))
  plot(x, s1, type = 'l', lty = 1, xlab = '', xaxt = 'n', yaxt = 'n', cex = 1.6, 
       cex.lab = 1.6, ylab = '')
  lines(x, s2, lty = 2)
  lines(x, s3, lty = 3)
  lines(x, s4, lty = 4)
  legend('topleft', legend = '(b)', bty = 'n', cex = 1.6)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')), 
                                's = -0.5',
                                's = -0.25',
                                's = 0.25',
                                's = 0.55',
                                'p = 400 C',
                                'w = 60 C'),
         bty = 'n', cex = 1.2,
         lty = c(NA, 1, 2, 3, 4, NA, NA)
  )
  
  par(mar = c(6,6,1,1))
  plot(x, p1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6, cex.lab = 1.6,
       xlab = 'Temperature (C)', 
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')))
  axis(side = 1, at = c(200, 300, 400, 500, 600, 700), cex.axis = 1.2,
       labels = c(200, 300, 400, 500, 600, 700))
  axis(side = 2, at = c(0, 0.002, 0.004, 0.006, 0.008, 0.010), cex.axis = 1.2,
       labels = c(0, 0.002, 0.004, 0.006, 0.008, 0.010))
  lines(x, p2, lty = 2)
  lines(x, p3, lty = 3)
  lines(x, p4, lty = 4)
  legend('topleft', legend = '(c)', bty = 'n', cex = 1.6)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')), 
                                's = -0.25',
                                'p = 350 C',
                                'p = 400 C',
                                'p = 450 C', 
                                'p = 500 C',
                                'w = 60 C'),
         bty = 'n', cex = 1.2,
         lty = c(NA, NA, 1, 2, 3, 4, NA)
  )
  
  par(mar = c(6,1,1,6))
  plot(x, w1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6, cex.lab = 1.6,
       xlab = 'Temperature (C)', 
       ylab = '')
  axis(side = 1, at = c(200, 300, 400, 500, 600, 700), cex.axis = 1.2,
       labels = c(200, 300, 400, 500, 600, 700))
  lines(x, w2, lty = 2)
  lines(x, w3, lty = 3)
  lines(x, w4, lty = 4)
  legend('topleft', legend = '(d)', bty = 'n', cex = 1.6)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')), 
                                's = -0.25',
                                'p = 400 C',
                                'w = 30 C', 
                                'w = 60 C',
                                'w = 90 C',
                                'w = 120'),
         bty = 'n', cex = 1.2,
         lty = c(NA, NA, NA, 1, 2, 3, 4)
  )
  
  dev.off()
}