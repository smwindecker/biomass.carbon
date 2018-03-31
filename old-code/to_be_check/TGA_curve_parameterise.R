
# load TGA data
tga <- read.table('munge/tga_derivs.txt')

# replace spaces with underscores in species names
tga$species <- gsub('([[:punct:]])|\\s+','_', tga$species)

tga <- tga[tga$species_code == 'A',]

gaussian <- function (x, params) {
  params[1] * exp(-0.5 * ((x - params[2]) / params[3]) ^ 2)
}

mixture <- function (x, params) {
  y1 <- gaussian(x, params[1:3])
  y2 <- gaussian(x, params[4:6])
  y3 <- gaussian(x, params[7:9])
  y1 + y2 + y3
}

rmse <- function (pred, obs) {
  sqrt(sum((obs - pred) ^ 2))
}

transform_a0 <- function (theta_a0) {
  params_a0 <- plogis(theta_a0 - 5)
  params_a0
}

transform_a1 <- function (theta_a1) {
  params_a1 <- theta_a1 * 0

  # first one around 200
  params_a1[1] <- exp(theta_a1[1] / 2) * 200

  # number to add to the subsequent ones (around 100)
  addition <- exp(theta_a1[2:3] / 2) * 100

  # add them
  params_a1[2:3] <- c(params_a1[1] + addition[1],
                      params_a1[1] + addition[1] + addition[2])
  params_a1
}

transform_a2 <- function (theta_a2) {
  params_a2 <- exp(theta_a2 + 3)

  params_a2
}

reparam <- function (theta) {

  # blank version of params to fill in
  params <- theta * 0

  # transform a0s (heights)
  a0s_idx <- c(1, 4, 7)
  params[a0s_idx] <- transform_a0(theta[a0s_idx])

  # transform a1s (centrepoints)
  a1s_idx <- c(2, 5, 8)
  params[a1s_idx] <- transform_a1(theta[a1s_idx])

  # transform a2s (widths)
  a2s_idx <- c(3, 6, 9)
  params[a2s_idx] <- transform_a2(theta[a2s_idx])

  params

}

objective <- function (theta, temp, obs) {

  # reparameterise params to make it identifiable
  params <- reparam(theta)

  # predict the curve
  pred <- mixture(temp, params)

  # see how good it is
  target <- rmse(pred, obs)

  target

}

fit_peaks <- function (obs, temp, restarts = 300) {

  require(nloptr)
  opts <- list("algorithm"="NLOPT_LN_BOBYQA",
               "xtol_rel"=1.0e-12)

  # fit the model `restarts` times with different starting locations
  o_list <- replicate(restarts,
                      nloptr(x0 = rnorm(9),
                             eval_f = objective,
                             temp = temp,
                             obs = obs,
                             opts = opts),
                      simplify = FALSE)

  # find the best one
  fits <- vapply(o_list,
                 function (x) x$objective,
                FUN.VALUE = 0)
  best <- which.min(fits)
  o <- o_list[[best]]

  reparam(o$solution)

}

int_list <- list()

for (i in unique(tga$species)) {
  temp <- tga[tga$species == i, 2]
  obs <- tga[tga$species == i, 'deriv']

  params_opt <- fit_peaks(obs, temp, restarts = 300)

  y0_pred <- gaussian(temp, params_opt[1:3])
  y1_pred <- gaussian(temp, params_opt[4:6])
  y2_pred <- gaussian(temp, params_opt[7:9])
  y <- mixture(temp, params_opt)

  # add the predicted values to tga dataframe
  tga$y0_pred[tga$species == i] <- y0_pred
  tga$y1_pred[tga$species == i] <- y1_pred
  tga$y2_pred[tga$species == i] <- y2_pred
  tga$y[tga$species == i] <- y

  # integrals under the three peaks
  y0_int <- sum(y0_pred[-1] * diff(temp))
  y1_int <- sum(y1_pred[-1] * diff(temp))
  y2_int <- sum(y2_pred[-1] * diff(temp))

  int_dat <- data.frame(y0_int, y1_int, y2_int)
  int_dat$i <- i
  int_list[[i]] <- int_dat

}

integral_data <- dplyr::bind_rows(int_list)



library(ggplot2)
# plot each species curve into separate file
species_list <- unique(tga$species)
for (i in seq_along(species_list)) {
  df = subset(tga, species == species_list[i])
  tga_masterplot <- ggplot(df, aes(temp)) +
    geom_line(aes(y = y0_pred), col = 'green') +
    geom_line(aes(y = y1_pred), col = 'red') +
    geom_line(aes(y = y2_pred), col = 'blue') +
    geom_line(aes(y = y), col = 'black') +
    geom_line(aes(y = deriv), col = 'grey') +
    ggtitle(species_list[i])
  png(paste0('output/tga_individual/', species_list[i], '.png'), 4, 4, 'in', res = 100)
  print(tga_masterplot)
  dev.off()
}

############################################################################


# plot(obs ~ temp, type = 'l', col = 'grey')
# lines(y1_pred ~ temp, lty = 2, col = 'green')
# lines(y2_pred ~ temp, lty = 2, col = 'blue')
# lines(y3_pred ~ temp, lty = 2, col = 'red')
# lines(y ~ temp, col = 'black')


