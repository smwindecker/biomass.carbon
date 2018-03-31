

rmse <- function (pred, obs) {
  sqrt(sum((obs - pred) ^ 2))
}

objective <- function (params, model, x, obs) {

  pred <- model(x, params)

  # see how good it is
  target <- rmse(pred, obs)

  target

}

# create params_opt with this function
paramSelect <- function (theta, lb, model, x, obs, restarts = 300) {

  require(nloptr)
  opts <- list("algorithm"="NLOPT_LN_BOBYQA",
               "xtol_rel"=1.0e-12)

  # fit the model `restarts` times with different starting locations
  o_list <- replicate(restarts,
                      nloptr(x0 = theta,
                             eval_f = objective,
                             lb = lb,
                             model = model,
                             x = x,
                             obs = obs,
                             opts = opts),
                      simplify = FALSE)

  # find the best one
  fits <- vapply(o_list,
                 function (x) x$objective,
                 FUN.VALUE = 0)
  best <- which.min(fits)
  o <- o_list[[best]]
  o$solution

}
