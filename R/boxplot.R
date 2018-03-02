boxplot <- function (df, trait) {
  
  mfloor <- function (x, base) { 
    base*floor(x/base) 
  } 
  mround <- function (x, base) { 
    base*round(x/base) 
  } 
  mceiling <- function (x, base) { 
    base*ceiling(x/base) 
  } 
  
  if (trait == 'SLA') {
    
    png('figs/SLA_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$SLA), .05)
    high <- mceiling(max(df$SLA), .05)
    mid <- mround((low + high)/2, .01)
    plot(df$gf, df$SLA, ylab = expression(paste('Specific litter area (m'^'2', '/g)')), 
         xlab = '', yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$SLA)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.2f", c(low, mid, high)))
    legend('topleft', '(a)', bty = 'n', cex = 1.8)
    
    dev.off()
  }
  
  if (trait == 'DMC') {
    
    png('figs/DMC_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$DMC), 5)
    high <- mceiling(max(df$DMC), 5)
    mid <- mround((low + high)/2, 1)
    plot(df$gf, df$DMC, ylab = 'Litter dry matter content (mg/g)', 
         xlab = '', yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$DMC)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.0f", c(low, mid, high)))
    legend('topleft', '(b)', bty = 'n', cex = 1.8)
    
    dev.off()
  }
  
  if (trait == 'N') {
    
    png('figs/N_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$N), .05)
    high <- mceiling(max(df$N), .05)
    mid <- mround((low + high)/2, .01)
    plot(df$gf, df$N, ylab = 'Litter nitrogen content (wt%)',
         yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$N)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.2f", c(low, mid, high)))
    legend('topleft', '(c)', bty = 'n', cex = 1.8)     
    
    dev.off()
  }
  
  if (trait == 'C') {
    
    png('figs/C_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$C), 1)
    high <- mceiling(max(df$C), 1)
    mid <- mround((low + high)/2, 1)
    plot(df$gf, df$C, ylab = 'Litter carbon content (wt%)', 
         xlab = '', yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$C)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.0f", c(low, mid, high)))
    legend('topleft', '(d)', bty = 'n', cex = 1.8)
    
    dev.off()
  }

  if (trait == 'HC') {
    
    png('figs/HC_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$HC), .05)
    high <- mceiling(max(df$HC), .05)
    mid <- mround((low + high)/2, .01)
    plot(df$gf, df$HC, ylab = 'Litter hemicelluloses (wt%)',
         yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$HC)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.2f", c(low, mid, high)))
    legend('topleft', '(e)', bty = 'n', cex = 1.8) 
    
    dev.off()
  }

  if (trait == 'CL') {
    
    png('figs/CL_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$CL), .05)
    high <- mceiling(max(df$CL), .05)
    mid <- mround((low + high)/2, .01)
    plot(df$gf, df$CL, ylab = 'Litter cellulose (wt%)',
         yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$CL)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.2f", c(low, mid, high)))
    legend('topleft', '(f)', bty = 'n', cex = 1.8)
    
    dev.off()
  }

  if (trait == 'LG') {
    
    png('figs/LG_boxplot.png', width = 500, height = 480)
    
    par(mar = c(4,6,1,1))
    low <- mfloor(min(df$LG), .05)
    high <- mceiling(max(df$LG), .05)
    mid <- mround((low + high)/2, .01)
    plot(df$gf, df$LG, ylab = 'Litter lignin (wt%)',
         yaxt = 'n', 
         ylim = c(0.99*low, (high + 0.15*(high-low))),
         cex.axis = 1.6, cex.lab = 1.8)
    points(df$gf, df$LG)
    axis(side = 2, at = c(low, mid, high), cex.axis = 1.6,
         labels = sprintf("%.2f", c(low, mid, high)))
    legend('topleft', '(g)', bty = 'n', cex = 1.8) 
    
    dev.off()
  }

}