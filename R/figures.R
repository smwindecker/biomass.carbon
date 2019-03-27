## Figures

# Produce parameter simulation of Fraser-Suzuki function
simulate_fraser_suzuki <- function () {
  
  x <- seq(200, 700)
  height_1 <- mixchar::fs_function(x, 0.004, -0.25, 400, 60)
  height_2 <- mixchar::fs_function(x, 0.006, -0.25, 400, 60)
  height_3 <- mixchar::fs_function(x, 0.008, -0.25, 400, 60)
  h4 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  
  skew_1 <- mixchar::fs_function(x, 0.010, -0.55, 400, 60)
  skew_2 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  skew_3 <- mixchar::fs_function(x, 0.010, 0.25, 400, 60)
  s4 <- mixchar::fs_function(x, 0.010, 0.55, 400, 60)
  
  position_1 <- mixchar::fs_function(x, 0.010, -0.25, 350, 60)
  position_2 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  position_3 <- mixchar::fs_function(x, 0.010, -0.25, 450, 60)
  p4 <- mixchar::fs_function(x, 0.010, -0.25, 500, 60)
  
  width_1 <- mixchar::fs_function(x, 0.010, -0.25, 400, 30)
  width_2 <- mixchar::fs_function(x, 0.010, -0.25, 400, 60)
  width_3 <- mixchar::fs_function(x, 0.010, -0.25, 400, 90)
  w4 <- mixchar::fs_function(x, 0.010, -0.25, 400, 120)
  
  par(oma = c(5, 3, 0, 2), mar = c(1, 3, 2, 0), mfrow = c(2, 2))
  
  plot(x, h4, type = 'l', lty = 4, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
  axis(side = 2, at = c(0.00, 0.005, 0.009), cex.axis = 1.8,
       labels = c(0.001, 0.005, 0.009))
  lines(x, height_2, lty = 2)
  lines(x, height_3, lty = 3)
  lines(x, height_1, lty = 1)
  legend('topleft', legend = '(a)', bty = 'n', cex = 2)
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
  
  plot(x, skew_1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
  lines(x, skew_2, lty = 2)
  lines(x, skew_3, lty = 3)
  lines(x, s4, lty = 4)
  legend('topleft', legend = '(b)', bty = 'n', cex = 2)
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
  
  plot(x, position_1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
  axis(side = 1, at = c(200, 400, 600), cex.axis = 1.8,
       labels = c(200, 400, 600))
  axis(side = 2, at = c(0.00, 0.005, 0.009), cex.axis = 1.8,
       labels = c(0.001, 0.005, 0.009))
  lines(x, position_2, lty = 2)
  lines(x, position_3, lty = 3)
  lines(x, p4, lty = 4)
  legend('topleft', legend = '(c)', bty = 'n', cex = 2)
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
  
  plot(x, width_1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
  axis(side = 1, at = c(200, 400, 600), cex.axis = 1.8,
       labels = c(200, 400, 600))
  lines(x, width_2, lty = 2)
  lines(x, width_3, lty = 3)
  lines(x, w4, lty = 4)
  legend('topleft', legend = '(d)', bty = 'n', cex = 2)
  legend('topright', legend = c(expression(paste('h = 0.010 C'^'-1')), 
                                's = -0.25',
                                'p = 400 C',
                                'w = 30 C', 
                                'w = 60 C',
                                'w = 90 C',
                                'w = 120'),
         bty = 'n', cex = 1.2,
         lty = c(NA, NA, NA, 1, 2, 3, 4))
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 2.1, 
        outer = TRUE,
        cex = 1.9)
  mtext(text = expression(paste('Rate of mass loss (C'^'-1', ')')), 
        side = 2,
        line = 0.2,
        outer = TRUE, 
        cex = 1.9)
}

# Individual curves for TGA theory explanation figure
tga_theory_plots <- function (tga_data) {
  
  # read raw TGA
  tmp <- process_raw_tga(tga_data)
  
  # plot TG curve
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(8, 5, 0, 2), mar = c(3, 6, 3, 3))
  
  plot(tmp$data$temp_C, tmp$data$mass_T, yaxs = 'i', ylim = c(0, 22), xlim = c(33, 800),
       xaxs = 'i', ylab = 'Mass (mg)', xlab = '', xaxt = 'n', yaxt = 'n', 
       pch = 20, cex = 0.3, cex.lab = 3.3)
  axis(side = 1, at = c(200, 400, 600), cex.axis = 2.8, 
       labels = c(200, 400, 600), 
       padj = 1)
  axis(side = 2, at = c(0, 10, 20), cex.axis = 2.8,
       labels = c(0, 10, 20))
  legend('topleft',
         legend = '(a)', 
         bty = 'n', 
         cex = 3)
  
  arrows(x0 = 266, y0 = 19, x1 = 266, y1 = 17, lwd = 2, length = 0.1)
  arrows(x0 = 317, y0 = 15, x1 = 317, y1 = 13, lwd = 2, length = 0.1)
  arrows(x0 = 340, y0 = 11.5, x1 = 340, y1 = 9.5, lwd = 2, length = 0.1)
  
  # plot DTG curve
  plot(tmp$data$temp_C, tmp$data$deriv, yaxs = 'i', ylim = c(0, 0.01),
       xaxs = 'i', ylab = expression(paste('Rate of mass loss (C'^'-1', ')')), xlab = '', xaxt = 'n', yaxt = 'n', 
       pch = 20, cex.lab = 3.3, cex = 0.9)
  axis(side = 1, at = c(200, 400, 600), cex.axis = 2.8, 
       labels = c(200, 400, 600), 
       padj = 1)
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 2.8,
       labels = c(0, 0.004, 0.008))
  legend('topleft',
         legend = '(b)', 
         bty = 'n', 
         cex = 3)
  
  segments(x0 = 40, y0 = 0.0018, x1 = 40, y1 = 0.002, lwd = 3.5, col = 'darkgrey')
  segments(x0 = 40, y0 = 0.002, x1 = 120, y1 = 0.002, lwd = 3.5, col = 'darkgrey')
  segments(x0 = 120, y0 = 0.0018, x1 = 120, y1 = 0.002, lwd = 3.5, col = 'darkgrey')
  text(x = ((120-40)/2+40), y = 0.0024, '1', cex = 3.3, col = 'darkgrey')
  
  segments(x0 = 120, y0 = 0.0078, x1 = 120, y1 = 0.008, lwd = 3.5, col = 'darkgrey')
  segments(x0 = 120, y0 = 0.008, x1 = 650, y1 = 0.008, lwd = 3.5, col = 'darkgrey')
  segments(x0 = 650, y0 = 0.0078, x1 = 650, y1 = 0.008, lwd = 3.5, col = 'darkgrey')
  text(x = ((650-120)/2+120), y = 0.0084, '2', cex = 3.3, col = 'darkgrey')
  
  segments(x0 = 650, y0 = 0.0008, x1 = 650, y1 = 0.001, lwd = 3.5, col = 'darkgrey')
  segments(x0 = 650, y0 = 0.001, x1 = 790, y1 = 0.001, lwd = 3.5, col = 'darkgrey')
  segments(x0 = 790, y0 = 0.0008, x1 = 790, y1 = 0.001, lwd = 3.5, col = 'darkgrey')
  text(x = ((790-650)/2+650), y = 0.0014, '3', cex = 3.3, col = 'darkgrey')
  
  # deconvolve data
  output <- mixchar::deconvolve(tmp, upper_temp = 650, n_peaks = NULL)
  temp <- seq(output$temp_bounds[1], output$temp_bounds[2], length.out = nrow(output$data))
  fit <- output$model_fit  
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  # plot mixture model outcome on DTG data
  plot(output$data$temp_C, output$data$deriv, yaxs = 'i', ylim = c(0, 0.01),
       ylab = expression(paste('Rate of mass loss (C'^'-1', ')')), xlab = '', 
       xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.9, cex.lab = 3.3)
  axis(side = 1, at = c(200, 400, 600), cex.axis = 3, 
       labels = c(200, 400, 600), 
       padj = 1)
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 3,
       labels = c(0, 0.004, 0.008))
  arrows(x0 = 266, y0 = 0.0062, x1 = 266, y1 = 0.0055, lwd = 3, length = 0.1)
  arrows(x0 = 317, y0 = 0.0087, x1 = 317, y1 = 0.008, lwd = 3, length = 0.1)
  arrows(x0 = 365, y0 = 0.0022, x1 = 365, y1 = 0.0015, lwd = 3, length = 0.1)
  
  y1 <- mixchar::fs_mixture(temp = temp,
                            height_1 = params['height_1',], skew_1 = params['skew_1',],
                            position_1 = params['position_1',], width_1 = params['width_1',],
                            height_2 = params['height_2',], skew_2 = params['skew_2',],
                            position_2 = params['position_2',], width_2 = params['width_2',],
                            height_3 = params['height_3',], skew_3 = params['skew_3',],
                            position_3 = params['position_3',], width_3 = params['width_3',])
  
  y2 <- mixchar::fs_function(temp = temp,
                             height = params['height_1',], skew = params['skew_1',],
                             position = params['position_1',], width = params['width_1',])
  
  y3 <- mixchar::fs_function(temp = temp,
                             height = params['height_2',], skew = params['skew_2',],
                             position = params['position_2',], width = params['width_2',])
  
  y4 <- mixchar::fs_function(temp = temp,
                             height = params['height_3',], skew = params['skew_3',],
                             position = params['position_3',], width = params['width_3',])
  
  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
  lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
  lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')
  
  legend('topright',
         legend = c('DTG data', 'DTG modelled', 'Hemicelluloses', 'Cellulose', 'Lignin'), 
         ncol = 1,
         cex = 2.8,
         bty = 'n',
         lty = c(NA, 1, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA),
         col = c('black', 'black', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2)
  legend('topleft',
         legend = '(c)', 
         bty = 'n', 
         cex = 3)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 5, 
        outer = TRUE,
        cex = 3)
  
}


# Produce boxplot
box_plot <- function (df) {
  
  # create functions to specify how to round
  mfloor <- function (x, base) { 
    base*floor(x/base) 
  } 
  mround <- function (x, base) { 
    base*round(x/base) 
  } 
  mceiling <- function (x, base) { 
    base*ceiling(x/base) 
  } 
  
  par(oma = c(2, 3, 0, 2), mar = c(4, 6, 1, 1), mfrow = c(3, 3))
  
  low <- mfloor(min(df$LAM), .05)
  high <- mceiling(max(df$LAM), .05)
  mid <- mround((low + high)/2, .01)
  plot(df$gf, df$LAM, ylab = expression(paste('Litter area per mass (m'^'2', '/g)')), 
       xlab = '', yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$LAM)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.2f", c(low, mid, high)))
  legend('topleft', '(a)', bty = 'n', cex = 2)
  
  low <- mfloor(min(df$DMC), 5)
  high <- mceiling(max(df$DMC), 5)
  mid <- mround((low + high)/2, 1)
  plot(df$gf, df$DMC, ylab = 'Litter dry matter content (mg/g)', 
       xlab = '', yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$DMC)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.0f", c(low, mid, high)))
  legend('topleft', '(b)', bty = 'n', cex = 2)
  
  low <- mfloor(min(df$N), .05)
  high <- mceiling(max(df$N), .05)
  mid <- mround((low + high)/2, .01)
  plot(df$gf, df$N, ylab = 'Litter nitrogen content (wt%)',
       yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$N)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.2f", c(low, mid, high)))
  legend('topleft', '(c)', bty = 'n', cex = 2)     
  
  low <- mfloor(min(df$C), 1)
  high <- mceiling(max(df$C), 1)
  mid <- mround((low + high)/2, 1)
  plot(df$gf, df$C, ylab = 'Litter carbon content (wt%)', 
       xlab = '', yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$C)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.0f", c(low, mid, high)))
  legend('topleft', '(d)', bty = 'n', cex = 2)
  
  low <- mfloor(min(df$HC), .05)
  high <- mceiling(max(df$HC), .05)
  mid <- mround((low + high)/2, .01)
  plot(df$gf, df$HC, ylab = 'Litter hemicelluloses (wt%)',
       yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$HC)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.2f", c(low, mid, high)))
  legend('topleft', '(e)', bty = 'n', cex = 2) 
  
  low <- mfloor(min(df$CL), .05)
  high <- mceiling(max(df$CL), .05)
  mid <- mround((low + high)/2, .01)
  plot(df$gf, df$CL, ylab = 'Litter cellulose (wt%)',
       yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$CL)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.2f", c(low, mid, high)))
  legend('topleft', '(f)', bty = 'n', cex = 2)
  
  low <- mfloor(min(df$LG), .05)
  high <- mceiling(max(df$LG), .05)
  mid <- mround((low + high)/2, .01)
  plot(df$gf, df$LG, ylab = 'Litter lignin (wt%)',
       yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$LG)
  axis(side = 2, at = c(low, mid, high), cex.axis = 2,
       labels = sprintf("%.2f", c(low, mid, high)))
  legend('topleft', '(g)', bty = 'n', cex = 2) 
  
}

# Produce pair plot of traits
pair_plot <- function (df) {
  
  panel.cor <- function (x, y, digits = 2, prefix = "", cex.cor = 1.8, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.7/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
    
    p <- stats::cor.test(x, y)$p.value
    if (p < 0.05) sym <- 8
    if (p < 0.01) sym <- c(8,8)
    if (p <0.001) sym <- c(8,8,8)
    if (p < 0.05) legend('topright', legend = '', pch = sym, bty = 'n')
  }
  
  # Customize upper panel
  upper.panel<-function(x, y){
    points(x, y, xlab = '', ylab = '', cex = 2.2)
    mylm <- lm(y ~ x)
    abline(mylm, col = 'red', cex = 2.2)
    newx <- seq(min(x), max(x), length.out = 500)
    prd <- predict(mylm, newdata = data.frame(x = newx), interval = c('confidence'),
                   level = 0.90, type = 'response')
    lines(newx, prd[, 2], col = 'black', lty = 2, cex = 2.2)
    lines(newx, prd[, 3], col = 'black', lty = 2, cex = 2.2)
    
  }
  
  # Create the plot
  par(cex.axis = 2.7)
  pairs(df,
        lower.panel = panel.cor,
        upper.panel = upper.panel, 
        cex.labels = 5)
  
}

# Produce phylo plot 
phylo_plot <- function (phylo, tips) {
  
  tips[] <- scale(tips)
  
  n <- length(tips)
  PRGn <- c('#762a83', '#af8dc3', '#e7d4e8', '#f7f7f7', 
            '#d9f0d3', '#7fbf7b', '#1b7837')
  
  viri <- c('#440154FF', '#453781FF', '#33638DFF', '#238A8DFF', '#20A387FF', '#55C667FF', '#DCE319FF')
  
  # change colour scheme
  colors <- grDevices::colorRampPalette(viri)(n)
  
  phytools::phylo.heatmap(phylo, tips, fsize = c(1.5, 1.5, 1), colors = colors)
}

# Call TGA plot for AR
tga_plot_ar <- function (species_deconvolved_list, species_data, gf) {
  
  sorted_species <- species_data[order(species_data$species),]
  arp_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == gf]))
  
  layout(matrix(c(1,2,3,4,5,0,6,6,6), nrow = 3, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.2))
  par(oma = c(3, 8, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(arp_species[1], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(arp_species[2], species_deconvolved_list, species_data)
  tga_plot(arp_species[3], species_deconvolved_list, species_data)
  
  tga_plot(arp_species[4], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  tga_plot(arp_species[5], species_deconvolved_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 1, 
        outer = TRUE,
        cex = 3.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 3.5, 
        adj = 0.55)
  
  legend_four_curves_horizontal()
  
}

# Call TGA plot for AT
tga_plot_at <- function (species_deconvolved_list, species_data, gf) {
  
  sorted_species <- species_data[order(species_data$species),]
  ate_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == gf]))
  
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,0,0,11,11,11), nrow = 5, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.8, 0.8, 0.2))
  par(oma = c(3, 8, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(ate_species[1], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(ate_species[2], species_deconvolved_list, species_data)
  tga_plot(ate_species[3], species_deconvolved_list, species_data)
  
  tga_plot(ate_species[4], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(ate_species[5], species_deconvolved_list, species_data)
  tga_plot(ate_species[6], species_deconvolved_list, species_data)
  
  tga_plot(ate_species[7], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(ate_species[8], species_deconvolved_list, species_data)
  tga_plot(ate_species[9], species_deconvolved_list, species_data)
  
  tga_plot(ate_species[10], species_deconvolved_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 1, 
        outer = TRUE,
        cex = 3.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 3.5, 
        adj = 0.55)
  
  legend_four_curves_horizontal()
  
}

# Call TGA plot for Tda
tga_plot_tda <- function (species_deconvolved_list, species_data, gf) {
  
  sorted_species <- species_data[order(species_data$species),]
  tda_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == gf]))
  
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,10,10), nrow = 4, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.8, 0.2))
  par(oma = c(3, 8, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(tda_species[1], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(tda_species[2], species_deconvolved_list, species_data)
  tga_plot(tda_species[3], species_deconvolved_list, species_data)
  
  tga_plot(tda_species[4], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(tda_species[5], species_deconvolved_list, species_data)
  tga_plot(tda_species[6], species_deconvolved_list, species_data)
  
  tga_plot(tda_species[7], species_deconvolved_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(tda_species[8], species_deconvolved_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  tga_plot(tda_species[9], species_deconvolved_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 1, 
        outer = TRUE,
        cex = 3.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 3.5, 
        adj = 0.55)
  
  legend_four_curves_horizontal()
  
}

# Call TGA plot for TDr
tga_plot_tdr <- function (species_deconvolved_list, species_data, gf) {
  
  sorted_species <- species_data[order(species_data$species),]
  tdr_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == gf]))
  
  layout(matrix(c(1,2,3,4,5,0,6,6,6), nrow = 3, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.2))
  par(oma = c(3, 8, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(tdr_species[1], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(tdr_species[2], species_deconvolved_list, species_data)
  tga_plot(tdr_species[3], species_deconvolved_list, species_data)
  
  tga_plot(tdr_species[4], species_deconvolved_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 3.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  tga_plot(tdr_species[5], species_deconvolved_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 3.2, labels = c(150, 400, 650), padj = 1)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 1, 
        outer = TRUE,
        cex = 3.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 3.5, 
        adj = 0.55)
  
  legend_four_curves_horizontal()
  
}

# Call TGA plot for three emblem species
tga_plot_three <- function (species_deconvoluted_list, species_data, species_names) {
  
  layout(matrix(c(1,2,3,4,4,4), nrow = 2, ncol = 3, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(3, 6, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(species_names[1], species_deconvoluted_list, species_data, sp_legend = FALSE)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.5, labels = c(150, 400, 650), padj = 1)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.5,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  legend_subfig('a', cex = 2.6)
  
  tga_plot(species_names[2], species_deconvoluted_list, species_data, sp_legend = FALSE)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.5, labels = c(150, 400, 650), padj = 1)
  legend_subfig('b', cex = 2.6)
  
  tga_plot(species_names[3], species_deconvoluted_list, species_data, sp_legend = FALSE)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.5, labels = c(150, 400, 650), padj = 1)
  legend_subfig('c', cex = 2.6)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2.5)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 2.2, 
        adj = 0.75)
  
  legend_three_curves_horizontal()
  
}

# Plot single species' TGA data
tga_plot <- function (species_code, species_deconvolved_list, species_data, sp_legend = TRUE) {
  
  x <- species_code
  
  output <- species_deconvolved_list[[x]]
  spname <- species_data$sp_abrev[species_data$species_code == x][1]
  
  # extract parameters from mixture model fit
  fit <- output$model_fit
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  # temperature bounds for plots
  temp <- seq(output$temp_bounds[1], output$temp_bounds[2], length.out = nrow(output$data))
  
  # isolate data
  data <- output$data
  
  # plot
  plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
       ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
  
  if (output$n_peaks == 4) {
    
    y1 <- mixchar::fs_mixture(temp = temp,
                              height_1 = params['height_1',], skew_1 = params['skew_1',],
                              position_1 = params['position_1',], width_1 = params['width_1',],
                              height_2 = params['height_2',], skew_2 = params['skew_2',],
                              position_2 = params['position_2',], width_2 = params['width_2',],
                              height_3 = params['height_3',], skew_3 = params['skew_3',],
                              position_3 = params['position_3',], width_3 = params['width_3',],
                              height_0 = params['height_0',], skew_0 = params['skew_0',],
                              position_0 = params['position_0',], width_0 = params['width_0',])
    
    y5 <- mixchar::fs_function(temp = temp,
                               height = params['height_0',], skew = params['skew_0',],
                               position = params['position_0',], width = params['width_0',])
    
    lines(temp, y5, lty = 6, lwd = 2.5, col = '#33638DFF')
    
  } 
  
  if (output$n_peaks == 3) {
    
    y1 <- mixchar::fs_mixture(temp = temp,
                              height_1 = params['height_1',], skew_1 = params['skew_1',],
                              position_1 = params['position_1',], width_1 = params['width_1',],
                              height_2 = params['height_2',], skew_2 = params['skew_2',],
                              position_2 = params['position_2',], width_2 = params['width_2',],
                              height_3 = params['height_3',], skew_3 = params['skew_3',],
                              position_3 = params['position_3',], width_3 = params['width_3',])
  }
  
  y2 <- mixchar::fs_function(temp = temp,
                             height = params['height_1',], skew = params['skew_1',],
                             position = params['position_1',], width = params['width_1',])
  
  y3 <- mixchar::fs_function(temp = temp,
                             height = params['height_2',], skew = params['skew_2',],
                             position = params['position_2',], width = params['width_2',])
  
  y4 <- mixchar::fs_function(temp = temp,
                             height = params['height_3',], skew = params['skew_3',],
                             position = params['position_3',], width = params['width_3',])
  
  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = '#440154FF')
  lines(temp, y3, lty = 4, lwd = 3.5, col = '#B8DE29FF')
  lines(temp, y4, lty = 5, lwd = 3.5, col = '#3CBB75FF')
  
  if (isTRUE(sp_legend)) {legend_species_tga(spname)}
}

# Produce pca plot and loadings table
pca <- function (prin, df, species_data) {
  
  # first two axes' scores
  pc12 <- prin$scores[, 1:2]
  df_pc12 <- data.frame(pc12)
  df_pc12$sp_abrev <- rownames(df_pc12)
  
  # label with abreviations
  pc12_labeled <- merge(df_pc12, species_data[,c('sp_abrev', 'sp_pca_label', 'gf', 'gf_old')])
  rownames(pc12_labeled) <- pc12_labeled[,'sp_pca_label']
  
  # get length of axes
  fit <- vegan::envfit(pc12, na.omit(df)) 
  
  vars <- prin$sdev^2
  prop_vars <- vars/sum(vars)
  
  par(oma = c(2, 2, 0, 2))
  plot(pc12_labeled[, c('Comp.1', 'Comp.2')], ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', 
       ylim = c(-4, 4), xlim = c(-4.2, 5), 
       cex.axis = 1, cex = 1.8, pch = c(2, 20, 8, 0)[as.numeric(pc12_labeled$gf)])
  plot(fit, cex = 2, col = 1, labels = list(vectors = c('LAM', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')))
  
  mtext(text = paste0('Axis 1 (', (100*round(prop_vars[[1]], 2)), '%)'), 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2)
  mtext(text = paste0('Axis 2 (', (100*round(prop_vars[[2]], 2)), '%)'), 
        side = 2,
        line = 0,
        outer = TRUE, 
        cex = 2)
  
  axis(side = 1, at = c(-4, -2, 0, 2, 4), cex.axis = 1.8, labels = c(-4, -2, 0, 2, 4))
  axis(side = 2, at = c(-4, -2, 0, 2, 4), cex.axis = 1.8, labels = c(-4, -2, 0, 2, 4))
  legend(-4, 4,
         c('Amph. fluct-responders', 'Amph. fluct-tolerators', 'Terr. damp', 'Terr. dry'), 
         bty = 'n',
         pch = c(2, 20, 8, 0), 
         cex = 1.8)
  
  reg_labels <- pc12_labeled[!rownames(pc12_labeled) %in% c('Sph', 
                                                            'P.pro', 
                                                            'A.den', 
                                                            'M.cri', 
                                                            'L.aus',
                                                            'J.ama',
                                                            'P.dis'), ]
  
  text(x = reg_labels[, 'Comp.1'], y = reg_labels[, 'Comp.2'],
       labels = row.names(reg_labels), vfont = c('sans serif', 'bold italic'),
       cex = 1.5, pos = 4, col = 'black')
  
  up_labels <- pc12_labeled[c('A.den', 'M.cri', 'L.aus', 'J.ama', 'P.dis'), ]
  
  text(x = up_labels[, 'Comp.1'], y = up_labels[, 'Comp.2']+0.16, 
       labels = row.names(up_labels), vfont = c('sans serif', 'bold italic'), 
       cex = 1.5, pos = 4, col = 'black')
  
  down_labels <- pc12_labeled[c('Sph', 'P.pro'), ]
  
  text(x = down_labels[, 'Comp.1'], y = down_labels[, 'Comp.2']-0.16, 
       labels = row.names(down_labels), vfont = c('sans serif', 'bold italic'),
       cex = 1.5, pos = 4, col = 'black')
}

## Custom legends

# Species name legend
legend_species_tga <- function (spname) {
  legend(650, .01,
         xjust = 1,
         legend = spname, 
         text.font = 3,
         cex = 4.2,
         bty = 'n')
}

# Four curves legend
legend_four_curves <- function () {
  legend(120, 0.008,
         xjust = 0,
         legend = c('data', 'total DTG', 'Hemicelluloses-1', 'Hemicelluloses-2', 'Cellulose', 'Lignin'),
         ncol = 2,
         cex = 1.8,
         bty = 'n',
         lty = c(NA, 1, 6, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', '#33638DFF', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2)  
}

# Subfigure legend
legend_subfig <- function (subfig, cex = 2.2) {
  legend('topleft', paste0('(', subfig, ')'), bty = 'n', cex = cex)
}

# Three four horizontal legend
legend_four_curves_horizontal <- function () {
  
  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  legend(x = "top", inset = 0,
         legend = c('DTG data', 'DTG modelled', 'Hemicelluloses-1', 'Hemicelluloses-2', 'Cellulose', 'Lignin'),
         horiz = TRUE,
         cex = 2.8,
         bty = 'n',
         lty = c(NA, 1, 6, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', '#33638DFF', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2) 
}

# Three three horizontal legend
legend_three_curves_horizontal <- function () {
  
  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
  
  legend(x = "top", inset = 0, 
         legend = c('DTG data', 'DTG modelled', '', 'Hemicelluloses', 'Cellulose', 'Lignin'),
         horiz = TRUE,
         cex = 2.8,
         bty = 'n',
         lty = c(NA, 1, NA, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', 'black', '#440154FF', '#B8DE29FF', '#3CBB75FF'),
         lwd = 2) 
}

# Deconvolve raw materials plot
tga_raw_plots <- function (item_1, item_2) {
  
  layout(matrix(c(1,2,3,3), nrow = 2, ncol = 2, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(4, 4, 0, 2), mar = c(3, 6, 2, 0))
  
  plot(item_1$temp, item_1$obs, 
       xlab = '', 
       ylab = '',
       yaxs = 'i',
       yaxt = 'n', 
       xaxt = 'n',
       ylim = c(0, 0.012),
       pch = 20, 
       cex = 0.3)
  axis(side = 1, at = c(200, 400, 600), cex.axis = 2.2,
       labels = c(200, 400, 600), padj = 1)
  axis(side = 2, at = c(0.002, 0.006, 0.010), cex.axis = 2.2,
       labels = c(0.002, 0.006, 0.010))
  
  y1 <- mixchar::fs_function(item_1$temp, 
                             item_1$height, 
                             item_1$skew, 
                             item_1$position, 
                             item_1$width)
  
  lines(item_1$temp, y1, lty = 1, lwd = 3)
  
  legend_subfig('a', cex = 2.7)
  legend('topright',
         legend = c(paste('h =', round(item_1$height, digits = 4)),
                    paste('s =', round(item_1$skew, digits = 3)),
                    paste('p =', round(item_1$position, digits = 0)),
                    paste('w =', round(item_1$width, digits = 0))),
         bty = 'n',
         ncol = 1,
         cex = 2.2)
  
  plot(item_2$temp, item_2$obs, 
       xlab = '', 
       ylab = '',
       yaxs = 'i', 
       yaxt = 'n',
       xaxt = 'n',
       ylim = c(0, 0.012),
       pch = 20, 
       cex = 0.3)
  axis(side = 1, at = c(200, 400, 600), cex.axis = 2.2,
       labels = c(200, 400, 600), padj = 1)
  
  y2 <- mixchar::fs_function(item_2$temp, 
                             item_2$height, 
                             item_2$skew, 
                             item_2$position, 
                             item_2$width)
  
  lines(item_2$temp, y2, lty = 1, lwd = 3)
  
  legend_subfig('b', cex = 2.7)
  legend('topright',
         legend = c(paste('h =', round(item_2$height, digits = 4)),
                    paste('s =', round(item_2$skew, digits = 3)),
                    paste('p =', round(item_2$position, digits = 0)),
                    paste('w =', round(item_2$width, digits = 0))),
         bty = 'n',
         ncol = 1,
         cex = 2.2)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2.7)
  mtext(text = expression(paste('Rate of mass loss (C'^'-1', ')')), 
        side = 2,
        line = 0,
        outer = TRUE, 
        cex = 2.7,
        adj = 0.75)
  
  # empty plot to get the legend on the bottom
  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')
  legend(x = "top", inset = 0,
         legend = c('DTG data', 'DTG modelled'),
         horiz = TRUE,
         cex = 2.5,
         bty = 'n',
         lty = c(NA, 1),
         pch = c(20, NA),
         lwd = 2) 
}