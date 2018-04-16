# Figures

#' Produce boxplot
#'
#' @param df dataframe of traits
#' @importFrom graphics points axis legend par plot 
#' @return saved box plot
#'
#' @export

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
  
  low <- mfloor(min(df$SLA), .05)
  high <- mceiling(max(df$SLA), .05)
  mid <- mround((low + high)/2, .01)
  plot(df$gf, df$SLA, ylab = expression(paste('Specific litter area (m'^'2', '/g)')), 
       xlab = '', yaxt = 'n', 
       ylim = c(0.99*low, (high + 0.15*(high-low))),
       cex.axis = 1.8, cex.lab = 2.2)
  points(df$gf, df$SLA)
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

#' Deconvolute raw materials plot
#'
#' @param item_1 first deconvolve raw object
#' @param item_2 second deconvolve raw object
#' @return saved deconvolved plot of both raw materials
#'
#' @export

tga_raw_plots <- function (item_1, item_2) {
  
  par(oma = c(2, 3, 0, 2), mar = c(3, 3, 2, 0), mfrow = c(1, 2))
  
  plot(item_1$temp, item_1$obs, 
       xlab = '', 
       ylab = '',
       yaxs = 'i',
       yaxt = 'n', 
       xaxt = 'n',
       ylim = c(0, 0.012),
       pch = 20, 
       cex = 1.1)
  axis(side = 1, at = c(200, 300, 400, 500, 600, 700), cex.axis = 1.6,
       labels = c(200, 300, 400, 500, 600, 700))
  axis(side = 2, at = c(0, 0.002, 0.004, 0.006, 0.008, 0.010), cex.axis = 1.6,
       labels = c(0, 0.002, 0.004, 0.006, 0.008, 0.010))
  
  y1 <- deconvolve::fs_function(item_1$temp, 
                               item_1$h, 
                               item_1$s, 
                               item_1$p, 
                               item_1$w)
  
  lines(item_1$temp, y1, lty = 1, lwd = 2)
  
  legend('topleft',
         legend = '(a)', 
         bty = 'n', 
         cex = 1.8)
  
  plot(item_2$temp, item_2$obs, 
       xlab = '', 
       ylab = '',
       yaxs = 'i', 
       yaxt = 'n',
       xaxt = 'n',
       ylim = c(0, 0.012),
       pch = 20, 
       cex = 1.1)
  axis(side = 1, at = c(200, 300, 400, 500, 600, 700), cex.axis = 1.6,
       labels = c(200, 300, 400, 500, 600, 700))
  
  y2 <- deconvolve::fs_function(item_2$temp, 
                               item_2$h, 
                               item_2$s, 
                               item_2$p, 
                               item_2$w)
  
  lines(item_2$temp, y2, lty = 1, lwd = 2)
  
  legend('topleft',
         legend = '(b)', 
         bty = 'n', 
         cex = 1.8)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 0,
        outer = TRUE, 
        cex = 2)
  legend('topright',
         legend = c('DTG data', 'DTG modelled'), 
         ncol = 1,
         cex = 1.8,
         bty = 'n',
         lty = c(NA, 1),
         pch = c(20, NA),
         lwd = 2)
  
}

#' Produce pair plot of traits
#'
#' @param df logged trait matrix
#' @importFrom stats cor lm cor.test
#' @importFrom grDevices png dev.off 
#' @importFrom graphics par text points abline strwidth
#' @return saved pair plot
#'
#' @export

pair_plot <- function (df) {
  
  panel.cor <- function(x, y, digits = 2, prefix = "", 2, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- stats::cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
    
    p <- stats::cor.test(x, y)$p.value
    if (p < 0.05) sym <- 8
    if (p < 0.01) sym <- c(8,8)
    if (p <0.001) sym <- c(8,8,8)
    if (p < 0.05) legend('topright', legend = '', pch = sym, bty = 'n')
  }
  
  # Customize upper panel
  upper.panel<-function(x, y){
    points(x, y, xlab = '', ylab = '', cex = 1.8)
    mylm <- lm(y ~ x)
    abline(mylm, col = 'red', cex = 1.8)
    newx <- seq(min(x), max(x), length.out = 500)
    prd <- predict(mylm, newdata = data.frame(x = newx), interval = c('confidence'),
                   level = 0.90, type = 'response')
    lines(newx, prd[, 2], col = 'black', lty = 2, cex = 1.8)
    lines(newx, prd[, 3], col = 'black', lty = 2, cex = 1.8)
    
  }
  
  # Create the plot
  pairs(df, 
        lower.panel = panel.cor,
        upper.panel = upper.panel)
  
}

#' Produce phylo plot 
#'
#' @param phylo phylogenetic tree data
#' @param tips phylogenetic traits
#' @return saved pca plot
#' @importFrom vegan envfit
#' @importFrom phytools phylo.heatmap
#' @importFrom stats na.omit
#' @importFrom grDevices png dev.off 
#' @importFrom graphics plot mtext legend text
#'
#' @export

phylo_plot <- function (phylo, tips) {
  
  tips[] <- scale(tips)
  
  n <- length(tips)
  PRGn <- c('#762a83', '#af8dc3', '#e7d4e8', '#f7f7f7', 
            '#d9f0d3', '#7fbf7b', '#1b7837')
  # change colour scheme
  colors <- grDevices::colorRampPalette(PRGn)(n)
  
  phytools::phylo.heatmap(phylo, tips, fsize = c(1.5, 1.5, 1), colors = colors)
  
}

#' Produce parameter simulation of Fraser-Suzuki function
#'
#' @importFrom deconvolve fs_function
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot axis lines par legend 
#' @return saved simulated Fraser-Suzuki plot
#'
#' @export

simulate_fraser_suzuki <- function () {
  
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
  
  par(oma = c(5, 5, 0, 2), mar = c(1, 3, 2, 0), mfrow = c(2, 2))
  
  plot(x, h4, type = 'l', lty = 4, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
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
  
  plot(x, s1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
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
  
  plot(x, p1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
       ylab = '')
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
  
  plot(x, w1, type = 'l', lty = 1, xaxt = 'n', yaxt = 'n', cex = 1.6,
       xlab = '', 
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
         lty = c(NA, NA, NA, 1, 2, 3, 4))

  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 2.1, 
        outer = TRUE,
        cex = 1.8)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 0.9,
        outer = TRUE, 
        cex = 1.8)
  
}

#' Individual curves for TGA theory explanation figure
#'
#' @param tga_data species code to use in theory plot
#' @return three theory plots
#' @importFrom deconvolve deconvolve fs_function fs_mixture
#' @importFrom graphics par axis legend plot lines
#'
#' @export

tga_theory_plots <- function (tga_data) {
  
  # read raw TGA
  tmp <- process_raw_tga(tga_data)
  
  # plot TG curve
  layout(matrix(c(1,2,3), nrow = 1, ncol = 3, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(5, 3, 0, 2), mar = c(3, 6, 3, 3))

  plot(tmp$data$temp_C, tmp$data$mass_T, yaxs = 'i', ylim = c(0, 22), xlim = c(0, 900),
       xaxs = 'i', ylab = 'Mass (mg)', xlab = '', xaxt = 'n', yaxt = 'n', 
       pch = 20, cex = 0.3, cex.lab = 3)
  axis(side = 1, at = c(0, 200, 400, 600, 800), cex.axis = 2.5, labels = c(0, 200, 400, 600, 800))
  axis(side = 2, at = c(0, 10, 20), cex.axis = 2.5,
       labels = c(0, 10, 20))
  legend('topleft',
         legend = '(a)', 
         bty = 'n', 
         cex = 2.5)
  
  # plot DTG curve
  plot(tmp$data$temp_C, tmp$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
       xaxs = 'i', ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = '', xaxt = 'n', yaxt = 'n', 
       pch = 20, cex.lab = 3, cex = 0.9)
  axis(side = 1, at = c(0, 200, 400, 600, 800), cex.axis = 2.5, labels = c(0, 200, 400, 600, 800))
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 2.5,
       labels = c(0, 0.004, 0.008))
  legend('topleft',
         legend = '(b)', 
         bty = 'n', 
         cex = 2.5)
  
  # deconvolute data
  output <- deconvolve::deconvolve(tmp, upper_temp = 650, n_curves = NULL)
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
  fit <- output$minpack.lm  
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  # plot mixture model outcome on DTG data
  plot(output$data$temp_C, output$data$deriv, yaxs = 'i', ylim = c(0, 0.009),
       ylab = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), xlab = '', 
       xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.9, cex.lab = 3)
  axis(side = 1, at = c(200, 400, 600, 800), cex.axis = 2.5, labels = c(200, 400, 600, 800))
  axis(side = 2, at = c(0, 0.004, 0.008), cex.axis = 2.5,
       labels = c(0, 0.004, 0.008))
  
  y1 <- deconvolve::fs_mixture(x = temp,
                               h1 = params['h1',], s1 = params['s1',],
                               p1 = params['p1',], w1 = params['w1',],
                               h2 = params['h2',], s2 = params['s2',],
                               p2 = params['p2',], w2 = params['w2',],
                               h3 = params['h3',], s3 = params['s3',],
                               p3 = params['p3',], w3 = params['w3',])
  
  y2 <- deconvolve::fs_function(x = temp,
                                h = params['h1',], s = params['s1',],
                                p = params['p1',], w = params['w1',])
  
  y3 <- deconvolve::fs_function(x = temp,
                                h = params['h2',], s = params['s2',],
                                p = params['p2',], w = params['w2',])
  
  y4 <- deconvolve::fs_function(x = temp,
                                h = params['h3',], s = params['s3',],
                                p = params['p3',], w = params['w3',])
  
  lines(temp, y1, lty = 1, lwd = 2)
  lines(temp, y2, lty = 3, lwd = 3.5, col = 'red')
  lines(temp, y3, lty = 4, lwd = 3.5, col = 'green3')
  lines(temp, y4, lty = 5, lwd = 2.5, col = 'blue')
  
  legend('topright',
         legend = c('DTG data', 'DTG modelled', 'HC', 'CL', 'LG'), 
         ncol = 1,
         cex = 2.2,
         bty = 'n',
         lty = c(NA, 1, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA),
         col = c('black', 'black', 'red', 'green3', 'blue'),
         lwd = 2)
  legend('topleft',
         legend = '(c)', 
         bty = 'n', 
         cex = 2.5)
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 2, 
        outer = TRUE,
        cex = 2.2)
  
}

#' Call TGA plot for graminoids
#'
#' @param species_deconvoluted_list deconvoluted species data list item
#' @param species_data species detail file with growth form and full species name info
#' @param subfig subfig label
#' @param gf growth form to plot species of
#' @return saved multi-panel TGA plot
#'
#' @export

tga_plot_gram <- function (species_deconvoluted_list, species_data, subfig, gf) {
  
  sorted_species <- species_data[order(species_data$species),]
  gram_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == gf]))
  
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,0,12,12,12), nrow = 5, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.8, 0.8, 0.2))
  par(oma = c(3, 8, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(gram_species[1], species_deconvoluted_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.5,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  legend_subfig(subfig, cex = 2.8)
  tga_plot(gram_species[2], species_deconvoluted_list, species_data)
  tga_plot(gram_species[3], species_deconvoluted_list, species_data)
  
  tga_plot(gram_species[4], species_deconvoluted_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.5,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(gram_species[5], species_deconvoluted_list, species_data)
  tga_plot(gram_species[6], species_deconvoluted_list, species_data)
  
  tga_plot(gram_species[7], species_deconvoluted_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.5,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(gram_species[8], species_deconvoluted_list, species_data)
  tga_plot(gram_species[9], species_deconvoluted_list, species_data)
  
  tga_plot(gram_species[10], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.5, labels = c(150, 400, 650))
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.5,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(gram_species[11], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.5, labels = c(150, 400, 650))
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2.8)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 2.8, 
        adj = 0.55)
  
  legend_three_curves_horizontal()
  
}

#' Call TGA plot for forbs
#'
#' @param species_deconvoluted_list deconvoluted species data list item
#' @param species_data species detail file with growth form and full species name info
#' @param subfig subfig label
#' @param gf growth form to plot species of
#' @return saved multi-panel TGA plot
#'
#' @export

tga_plot_forb <- function (species_deconvoluted_list, species_data, subfig, gf) {
  
  sorted_species <- species_data[order(species_data$species),]
  forb_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == gf]))
  
  layout(matrix(c(1,2,3,4,5,6,7,8,9,10,10,10), nrow = 4, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.8, 0.2))
  par(oma = c(3, 6, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(forb_species[1], species_deconvoluted_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  legend_subfig(subfig, cex = 2.5)
  tga_plot(forb_species[2], species_deconvoluted_list, species_data)
  tga_plot(forb_species[3], species_deconvoluted_list, species_data)
  
  tga_plot(forb_species[4], species_deconvoluted_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(forb_species[5], species_deconvoluted_list, species_data)
  tga_plot(forb_species[6], species_deconvoluted_list, species_data)
  
  tga_plot(forb_species[7], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(forb_species[8], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  tga_plot(forb_species[9], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2.2)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 2.2, 
        adj = 0.55)
  
  legend_three_curves_horizontal()
  
}

#' Call TGA plot for tree, nonvascular, and shrub
#'
#' @param species_deconvoluted_list deconvoluted species data list item
#' @param species_data species detail file with growth form and full species name info
#' @param subfigs subfigure labels
#' @return saved multi-panel TGA plot
#'
#' @export

tga_plot_others <- function (species_deconvoluted_list, species_data, subfigs) {
  
  sorted_species <- species_data[order(species_data$species),]
  tree_species <- as.character(unique(sorted_species$species_code[sorted_species$gf == 'T']))
  nv <- as.character(unique(species_data$species_code[species_data$gf == 'NV']))
  s <- as.character(unique(species_data$species_code[species_data$gf == 'S']))
  
  layout(matrix(c(1,2,3,4,5,6,7,7,7), nrow = 3, ncol = 3, byrow = TRUE), heights = c(0.8, 0.8, 0.2))
  par(oma = c(3, 6, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(tree_species[1], species_deconvoluted_list, species_data)
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  legend_subfig(subfigs[1])
  tga_plot(tree_species[2], species_deconvoluted_list, species_data)
  tga_plot(nv, species_deconvoluted_list, species_data)
  legend_subfig(subfigs[2])
  
  tga_plot(tree_species[3], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  tga_plot(tree_species[4], species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  tga_plot(s, species_deconvoluted_list, species_data)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  legend_subfig(subfigs[3])
  
  mtext(text = 'Temperature (C)', 
        side = 1, 
        line = 0, 
        outer = TRUE,
        cex = 2.2)
  mtext(text = expression(paste('Rate of mass loss (-dm/dT) (C'^'-1', ')')), 
        side = 2,
        line = 2,
        outer = TRUE, 
        cex = 2.2, 
        adj = 0.6)
  
  legend_three_curves_horizontal()
  
}

#' Call TGA plot for three emblem species
#'
#' @param species_deconvoluted_list deconvoluted species data list item
#' @param species_data species detail file with growth form and full species name info
#' @param species_names to plot
#' @return saved multi-panel TGA plot
#'
#' @export

tga_plot_three <- function (species_deconvoluted_list, species_data, species_names) {
  
  layout(matrix(c(1,2,3,4,4,4), nrow = 2, ncol = 3, byrow = TRUE), heights = c(0.8, 0.2))
  par(oma = c(3, 6, 0, 2), mar = c(3, 3, 2, 0))
  
  tga_plot(species_names[1], species_deconvoluted_list, species_data, legend_species = FALSE)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  axis(side = 2, at = c(0.001, 0.005, 0.009), cex.axis = 2.2,
       labels = c(sprintf("%.3f", c(0.001, 0.005, 0.009))))
  legend_subfig('a', cex = 2.6)
  tga_plot(species_names[2], species_deconvoluted_list, species_data, legend_species = FALSE)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
  legend_subfig('b', cex = 2.6)
  tga_plot(species_names[3], species_deconvoluted_list, species_data, legend_species = FALSE)
  axis(side = 1, at = c(150, 400, 650), cex.axis = 2.2, labels = c(150, 400, 650))
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
        cex = 1.8, 
        adj = 0.75)
  
  legend_three_curves_horizontal()
  
}

#' Plot single species' TGA data
#'
#' @param species_code code of species to plot
#' @param species_deconvoluted_list deconvoluted species data list item
#' @param species_data species detail file with growth form and full species name info
#' @param legend_species logical when to add species name to plot
#' @return saved deconvolved plot
#' @importFrom deconvolve process deconvolve fs_mixture fs_function
#' @importFrom reshape2 dcast
#' @importFrom grDevices png dev.off
#' @importFrom graphics plot axis lines par legend rect
#'
#' @export

tga_plot <- function (species_code, species_deconvoluted_list, species_data, legend_species = TRUE) {
  
  x <- species_code
  
  list_item <- species_deconvoluted_list[[x]]
  spname <- species_data$sp_abrev[species_data$species_code == x][1]
  
  output <- list_item$output
  
  # extract parameters from mixture model fit
  fit <- output$minpack.lm
  params <- as.data.frame(summary(fit)$coefficients[,1])
  
  # temperature bounds for plots
  temp <- seq(output$bounds[1], output$bounds[2], length.out = nrow(output$data))
  
  # isolate data
  data <- output$data
  
  # plot
  plot(data$temp_C, data$deriv, yaxs = 'i', ylim = c(0, 0.01),
       ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', pch = 20, cex = 0.3)
  
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
  

  if (isTRUE(legend_species)) legend_species(spname)
  
}


#' Produce pca plot and loadings table
#'
#' @param prin pca data output
#' @param df logged traits matrix
#' @param species_data species dataframe so can get the abreviated species labels
#' @return saved pca plot
#' @importFrom vegan envfit
#' @importFrom stats na.omit
#' @importFrom grDevices png dev.off 
#' @importFrom graphics plot mtext legend text
#'
#' @export

pca <- function (prin, df, species_data) {
  
  # first two axes' scores
  pc12 <- prin$scores[, 1:2]
  df_pc12 <- data.frame(pc12)
  df_pc12$sp_abrev <- rownames(df_pc12)
  
  # label with abreviations
  pc12_labeled <- merge(df_pc12, species_data[,c('sp_abrev', 'sp_pca_label', 'gf')])
  rownames(pc12_labeled) <- pc12_labeled[,'sp_pca_label']
  
  # get length of axes
  fit <- vegan::envfit(pc12, na.omit(df)) 
  
  vars <- prin$sdev^2
  prop_vars <- vars/sum(vars)
  
  par(oma = c(2, 2, 0, 2))
  plot(pc12_labeled[, c('Comp.1', 'Comp.2')], ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', 
       ylim = c(-4, 4), xlim = c(-4.2, 5), 
       cex.axis = 1, cex = 1.8, pch = c(2, 20, 3, 8, 0)[as.numeric(pc12_labeled$gf)])
  plot(fit, cex = 2, col = 1, labels = list(vectors = c('SLA', 'DMC', 'N', 'C', 'HC', 'CL', 'LG')))
  
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
  legend('topright',
         c('Forb', 'Graminoid', 'Non-vascular', 'Shrub', 'Tree'), 
         bty = 'n',
         pch = c(2, 20, 3, 8, 0), 
         cex = 1.8)
  
  reg_labels <- pc12_labeled[!rownames(pc12_labeled) %in% c('Sph', 
                                                            'P.pro', 
                                                            'A.den', 
                                                            'M.cri', 
                                                            'L.aus',
                                                            'J.ama',
                                                            'P.dis'), ]
  
  text(x = reg_labels[, 'Comp.1'], y = reg_labels[, 'Comp.2'],
       labels = row.names(reg_labels), vfont = c('sans serif', 'italic'),
       cex = 1.5, pos = 4)
  
  up_labels <- pc12_labeled[c('A.den', 'M.cri', 'L.aus', 'J.ama', 'P.dis'), ]
  
  text(x = up_labels[, 'Comp.1'], y = up_labels[, 'Comp.2']+0.16, 
       labels = row.names(up_labels), vfont = c('sans serif', 'italic'), 
       cex = 1.5, pos = 4)
 
  down_labels <- pc12_labeled[c('Sph', 'P.pro'), ]
  
  text(x = down_labels[, 'Comp.1'], y = down_labels[, 'Comp.2']-0.16, 
       labels = row.names(down_labels), vfont = c('sans serif', 'italic'),
       cex = 1.5, pos = 4)

}

#' Custom legends
#'
#' @param spname species name for legend
#'
#' @export

legend_species <- function (spname) {
  
  legend(650, .01,
         xjust = 1,
         legend = spname, 
         text.font = 3,
         cex = 3.1,
         bty = 'n')
  
}

#' @export

legend_three_curves <- function () {
  
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
  
}

#' Subfigure legend
#' @param subfig subfig label
#' @param cex size of subfigure label
#' @export

legend_subfig <- function (subfig, cex = 2.2) {
  
  legend('topleft', paste0('(', subfig, ')'), bty = 'n', cex = cex)
  
}

#' @export 

legend_three_curves_horizontal <- function () {
  
  plot(1, type = 'n', axes = FALSE, xlab = '', ylab = '')

  legend(x = "top", inset = 0,
         legend = c('data', 'total DTG', 'HC-1', 'HC-2', 'CL', 'LG'),
         horiz = TRUE,
         cex = 2.4,
         bty = 'n',
         lty = c(NA, 1, 6, 3, 4, 5),
         pch = c(20, NA, NA, NA, NA, NA),
         col = c('black', 'black', 'orange', 'red', 'green3', 'blue'),
         lwd = 2) 
  
}

