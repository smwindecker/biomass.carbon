## Manuscript functions

# extract parameter or weight values
raw <- function (df, param) {
  
  if (param %in% c('position', 'width')) digits <- 0
  if (param == 'skew') digits <- 3
  if (param == 'height') digits <- 4
  if (param == 'weight') digits <- 1
  
  value <- round(df[[param]], digits = digits)
  value
  
} 

# Growth form mean and sd function for traits
extract <- function (df, variable, fxn, gf = NULL) {
  
  if (variable %in% c('position_0', 'position_1', 'position_2', 'position_3', 
                      'width_0', 'width_1', 'width_2', 'width_3', 'DMC')) digits <- 0
  if (variable %in% c('N', 'C', 'HC_1', 'HC_2', 'HC', 'CL', 'LG')) digits <- 1
  if (variable %in% c('LAM')) digits <- 2
  if (variable %in% c('skew_0', 'skew_1', 'skew_2', 'skew_3')) digits <- 3
  if (variable %in% c('height_0', 'height_1', 'height_2', 'height_3')) digits <- 4
  
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

# Deviance for model
dev <- function (model_file) {
  
  dev <- median(2*model_file$mod_specs$neg_loglik)
  dev
  
}
