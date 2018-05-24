## Manuscript functions

# Growth form mean and sd function for traits
extract <- function (df, variable, fxn, gf = NULL) {

  if (variable %in% c('p0', 'p1', 'p2', 'p3', 'w0', 'w1', 'w2', 'w3', 'DMC')) digits <- 0
  if (variable %in% c('N', 'C', 'HC_1', 'HC_2', 'HC', 'CL', 'LG')) digits <- 1
  if (variable %in% c('LAM')) digits <- 2
  if (variable %in% c('s0', 's1', 's2', 's3')) digits <- 3
  if (variable %in% c('h0', 'h1', 'h2', 'h3')) digits <- 4
  
  if (!is.null(gf)) subset <- df[df$gf == gf, variable]
  if (is.null(gf)) subset <- df[, variable]
  
  if (fxn == 'mean') {
    value <- round(mean(subset, na.rm = TRUE), digits = digits)
  }
  if (fxn == 'sd') {
    value <- round(sd(subset, na.rm = TRUE), digits = digits)
  }
  if (fxn == 'min') {
    value <- round(min(subset, na.rm = TRUE), digits = digits)
  }
  if (fxn == 'max') {
    value <- round(max(subset, na.rm = TRUE), digits = digits)
  }
  
  value
}

# pca axis loading values
loading <- function (pca, axis) {
  
  vars <- pca$sdev^2
  prop_var <- vars/sum(vars)
  100*round(prop_var[[axis]], 2)
  
}

# trait R2 correlation values
tcor <- function (df, trait_1, trait_2) {
  
  value <- round(stats::cor(df[trait_1], df[trait_2]), digits = 2)
  value
}

# extract parameter or weight values
raw <- function (df, param) {
  
  if (param %in% c('p', 'w')) digits <- 0
  if (param == 's') digits <- 3
  if (param == 'h') digits <- 4
  if (param == 'weight') digits <- 1
  
  value <- round(df[[param]], digits = digits)
  value
  
}  
  
  
