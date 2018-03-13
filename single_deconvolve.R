#' Deconvolve a single TGA curve
#'
#' @param raw_file_path TGA raw data
#' @param subfig sub figure label ID
#' @param output_file output file name
#' @return saved deconvolved plot and weight
#'
#' @export

single_deconvolve <- function (raw_file, subfig, output_file) {

  df <- read.csv(raw_file, header = FALSE, skip = 29)
  names(df) <- c('temp', 'time', 'mass_loss')
  init_mass <- read.csv(raw_file, nrwos = 1, header = FALSE, skip = 17)[1,2]
  munge <- deconvolve::process(df, 'temp', 'mass_loss', init_mass)
  
  mod_df <- munge$data

  # crop dataset at bounds
  mod_df <- mod_df[!(mod_df$temp_C < 120 | mod_df$temp_C > 650),]

  # name variables
  temp <- mod_df$temp_C
  obs <- mod_df$deriv

  # init mass
  mass_init <- munge$mass_init
  W <- mod_df$mass_T
  n <- length(W)

  start_vec <- c(0.003, -0.15, 390, 200)
  lb <- c(0, -0.3, 0, 0)
  ub <- c(0.1, 0.3, 900, 900)

  fit <- fs_model(mod_df, start_vec, lb, ub)

  mass_frac <- list('HC' = NA, 'CL' = NA, 'LG' = NA)

  coef <- as.data.frame(summary(fit)$coefficients[,1])

  h <- coef[row.names(coef) == 'h', 1]
  s <- coef[row.names(coef) == 's', 1]
  p <- coef[row.names(coef) == 'p', 1]
  w <- coef[row.names(coef) == 'w', 1]

  f_j <- function (x) {
    fs_function(x, h, s, p, w)
  }

  # area under the curves
  mass_frac <- (integrate(Vectorize(f_j), lower = 120,
                             upper = 650)$value) * 100

  png(output_folder, width = 760, height = 760)
  par(mar=c(5,8,4,1)+.1)
  plot(temp, obs, xlab = 'Temperature (C)', ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')),
       yaxs = 'i', ylim = c(0, 0.012),
       pch = 20, cex = 1.1, cex.axis = 2.1, cex.lab = 2.1)
  y <- fs_function(temp, h, s, p, w)
  lines(temp, y, lty = 1, lwd = 2)

  legend('topright',
         legend = c('DTG data', 'DTG modelled'), 
         ncol = 1,
         cex = 1.5,
         bty = 'n',
         lty = c(NA, 1),
         pch = c(20, NA),
         lwd = 2)
  legend('topleft',
         legend = paste0('(', subfig, ')'), 
         bty = 'n', 
         cex = 1.8)

  legend
  
  dev.off()
  mass_frac
  
}
