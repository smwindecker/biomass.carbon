library(lattice)
library(latticeExtra)
library(hexbin)

pairsFunction <- function(covData) {
  
  ct <- custom.theme(
    symbol = c("black", brewer.pal(n = 8, name = "Dark2")),
    fill = brewer.pal(n = 12, name = "Set3"),
    region = brewer.pal(n = 11, name = "Spectral"),
    reference = "#e8e8e8",
    bg = "transparent", fg = "black",
    lwd=2, pch=16
  )
  ct$axis.text$cex = 1.4
  ct$par.xlab.text$cex = 1.4
  ct$par.ylab.text$cex = 1.4
  
  cr <- colorRampPalette(c('grey80', 'grey0'))

  splom(~df,
        pscales = 0, #don't show axes,
        par.settings = ct,
        #upper.panel = panel.hexbinplot,  # use hexbinplot
        xbins = 15,                     # number of bins
        trans = log10, inv=function(x) 10^x, # density color scale transformation
        colramp = cr,
        # show correlation coefficient in lower panel
        # diag.panel = function(x, ...){
        #   yrng <- current.panel.limits()$ylim
        #   d <- density(x, na.rm=TRUE)
        #   d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y) )
        #   panel.lines(d)
        #   diag.panel.splom(x, ...)
        # },
        lower.panel = function(x,  y, ...) {
          panel.fill(col = brewer.pal(10, "PRGn")[round(cor(x, y) *  4 + 5)])
          panel.text(sum(range(x))/2, sum(range(y))/2, round(cor(x, y), 2), font = 2)
        },
        varname.cex = 0.9
  )
}