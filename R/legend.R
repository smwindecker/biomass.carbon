legend(mean(x$bounds[1], x$bounds[2]), max(data$deriv) + 0.1*max(data$deriv),
       yjust = 0,
       legend = c('Total DTG', 'P-SC', 'P-HC', 'P-CL', 'P-LG'),
       ncol = 5,
       cex = 0.4,
       bty = 'n',
       col = c('black', 'red', 'blue', 'green', 'orange'),
       lty = 1, lwd = 2)