
x <- 'A'
n_curves = NULL
source('../deconvolve/R/fs_function.R')
source('../deconvolve/R/fs_mixture.R')
source('../deconvolve/R/fs_mixture_wrap.R')
source('../deconvolve/R/fs_mixture_4.R')
source('../deconvolve/R/fs_mixture_wrap_4.R')
source('../deconvolve/R/single_param.R')

dat <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), header = F, skip = 29)

# Add column names
names(dat) <- c('temp', 'time', 'mass_loss')

# Add a column with the species code
dat$species_code <- x

init_mass <- read.csv(paste0('raw/TGA/', x, '_TGA.csv'), nrows = 1, header = F, skip = 17)

# get individual value then add to df.
init_mass <- init_mass[1,2]

munge <- deconvolve::process(dat, 'temp', 'mass_loss', init_mass)

png('figs/TG.png', width = 560, height = 480, 'px', bg = 'transparent')
par(mar = c(6, 6, 2, 2))
plot(munge$data$temp_C, munge$data$mass_T, yaxs = 'i', ylim = c(0, 22), xlim = c(0, 900),
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

output <- deconvolve::deconvolve(munge, upper_temp = 650, n_curves = n_curves)

temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
fit <- output$minpack.lm  
data <- output$data

png('figs/DTG.png', width = 560, height = 480, 'px', bg = 'transparent')
par(mar = c(6, 6, 2, 2))
plot(munge$data$temp_C, munge$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
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

png('figs/DTG-mixture.png', width = 560, height = 480, 'px', bg = 'transparent')
par(mar = c(6, 6, 2, 2))
plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.009),
     ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = 'Temperature (C)', 
     xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.9, cex.axis = 1.8, cex.lab = 1.8)
axis(side = 1, at = c(200, 400, 600, 800), cex.axis = 1.8, labels = c(200, 400, 600, 800))
axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 1.8,
     labels = c(0, 0.004, 0.008))
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

png('output/mixture_only.png', width = 560, height = 480, 'px', bg = 'transparent')
par(mar = c(6, 6, 2, 2))
plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')

y1 <- fs_mixture_wrap(temp,
                      single_param(fit, 'h', '1'), single_param(fit, 'h', '2'),
                      single_param(fit, 'h', '3'), single_param(fit, 's', '1'),
                      single_param(fit, 's', '2'), single_param(fit, 's', '3'),
                      single_param(fit, 'p', '1'), single_param(fit, 'p', '2'),
                      single_param(fit, 'p', '3'), single_param(fit, 'w', '1'),
                      single_param(fit, 'w', '2'), single_param(fit, 'w', '3'))
lines(temp, y1, lty = 1, lwd = 4)
dev.off()

png('output/hc_only.png', width = 560, height = 480, 'px', bg = 'transparent')
plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')

y2 <- fs_function(temp,
                  single_param(fit, 'h', '1'), single_param(fit, 's', '1'),
                  single_param(fit, 'p', '1'), single_param(fit, 'w', '1'))
lines(temp, y2, lty = 3, lwd = 5.5, col = 'red')
dev.off()

png('output/cl_only.png', width = 560, height = 480, 'px', bg = 'transparent')
plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')

y3 <- fs_function(temp,
                  single_param(fit, 'h', '2'), single_param(fit, 's', '2'),
                  single_param(fit, 'p', '2'), single_param(fit, 'w', '2'))
lines(temp, y3, lty = 4, lwd = 5.5, col = 'green3')
dev.off()

png('output/lg_only.png', width = 560, height = 480, 'px', bg = 'transparent')
plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')

y4 <- fs_function(temp,
                  single_param(fit, 'h', '3'), single_param(fit, 's', '3'),
                  single_param(fit, 'p', '3'), single_param(fit, 'w', '3'))
lines(temp, y4, lty = 5, lwd = 4.5, col = 'blue')
dev.off()

png('output/sim_curve.png', width = 560, height = 480, 'px', bg = 'transparent')
plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
     ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3, cex.axis = 1.8)
rect(par('usr')[1], par('usr')[3], par('usr')[2], par('usr')[4], col = 'white')

y2 <- fs_function(temp, 0.008, -0.45, 400, 80)
lines(temp, y2, lty = 1, lwd = 4.5, col = 'black')
dev.off()
