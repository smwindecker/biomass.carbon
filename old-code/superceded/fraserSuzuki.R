
# Frazer-Suzuki function
fsFunc <- function (x, h, s, p, w) {
  
  interior <- 2*s*((x - p) / w)
  exterior <- -log(2)/s^2
  
  ifelse(interior > -1, h*exp(exterior * (log(1 + interior)^2)), 0)
  
}

# combine the FS function three times for the three pseudo-components
fsTotal <- function (x, params) {
  
  fsTotalFunc <- fsFunc(x, params[1], params[4], params[7], params[10]) +
    fsFunc(x, params[2], params[5], params[8], params[11]) +
    fsFunc(x, params[3], params[6], params[9], params[12])
  
}

# create wrapper to separately identify the 12 parameters
fsTotal2 <- function (x, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3) {
  
  params <- c(h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3)
  fsTotal(x, params)
  
}

# function to do the nls fit with the correct starting values
fs_model <- function (dataframe, params) {
  
  nlsLM(deriv ~ fsTotal2(K, h1, h2, h3, s1, s2, s3, p1, p2, p3, w1, w2, w3),
        start = list(h1 = params[1], h2 = params[2], h3 = params[3],
                     s1 = params[4], s2 = params[5], s3 = params[6],
                     p1 = params[7], p2 = params[8], p3 = params[9],
                     w1 = params[10], w2 = params[11], w3 = params[12]),
        data = dataframe,
        control = nls.lm.control(maxiter = 1024, maxfev = 1e6))
  
}

# function to extract the parameter values
pEst <- function (fit, p, comp) {
  
  params <- fit %>% broom::tidy()
  params$parameter <- substr(params$term, 1, 1)
  params$component <- substr(params$term, 2, 2)
  
  value <- params$estimate[params$parameter == p & params$component == comp]
  print(value)
  
}