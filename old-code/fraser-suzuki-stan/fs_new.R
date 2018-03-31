

library(rstan)

data <- read.csv('all_tga.csv')

# create data as list for Stan, and fit model:
fs_data <- list(d = data$deriv,
                N = nrow(data),
                t = data$temp
)

fs <- stan(file = "fs.stan", data = fs_data,
           iter = 2000, chains = 4, cores = 4)

png('traceplot.png')
traceplot(fs)
dev.off()

extract_diagnostics <- function (fit) {
  
  fit_summary <- summary(fit)$summary
  abs_rhat <- max(abs(fit_summary[,'Rhat'] - 1))
  neff_min <- min(fit_summary[,'n_eff'])
  sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
  sum_div <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
  max_treedepth <- sapply(sampler_params, function(x) max(x[, "treedepth__"]))
  diagnostics[(diagnostics[, 1] == i & diagnostics[, 2] == j), ] <- 
    c(i, j, abs_rhat, neff_min, sum_div, max_treedepth)
}
not_converged <- diag[(diag$abs_rhat > 1.1 |
                         diag$neff_min/nrow(n) < 0.001 |
                         diag$sum_div > 0 |
                         diag$max_tree_c1 > 10 |
                         diag$max_tree_c2 > 10 |
                         diag$max_tree_c3 > 10) ,
                      ]
not_converged

}

