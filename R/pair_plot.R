#' Produce pair plot of traits
#'
#' @param df dataframe of traits
#' @param output_file file path for output plot
#' @return saved pair plot
#'
#' @export

pair_plot <- function (df, output_file) {

  panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * abs(r))
    
    p <- cor.test(x, y)$p.value
    if (p < 0.05) sym <- 8
    if (p < 0.01) sym <- c(8,8)
    if (p <0.001) sym <- c(8,8,8)
    
    if (p < 0.05) legend('topright', legend = '', pch = sym, bty = 'n')
  }
  
  # Customize upper panel
  upper.panel<-function(x, y){
    points(x, y, xlab = '', ylab = '')
    mylm <- lm(y ~ x)
    abline(mylm, col = 'red')
    newx <- seq(min(x), max(x), length.out = 500)
    prd <- predict(mylm, newdata = data.frame(x = newx), interval = c("confidence"),
                   level = 0.90, type="response")
    lines(newx, prd[, 2], col = 'black', lty = 2)
    lines(newx, prd[, 3], col = 'black', lty = 2)
    
  }
  # Create the plots
  png(output_file, 8, 8, 'in', res = 100)
  pairs(df, 
        lower.panel = panel.cor,
        upper.panel = upper.panel)
  dev.off()

}


# # Correlation panel
# panel.cor <- function(x, y){
#   usr <- par("usr"); on.exit(par(usr))
#   par(usr = c(0, 1, 0, 1))
#   r <- round(cor(x, y), digits=2)
#   txt <- paste0("R = ", r)
#   cex.cor <- 0.8/strwidth(txt)
#   
#   #panel.fill(col = brewer.pal(10, 'Greys')[r *  4 + 5])
#   panel.text(sum(range(x))/2, sum(range(y))/2, paste0('R =', r), font = 2)
#   text(0.5, 0.5, txt, cex = cex.cor * abs(r))
# }  