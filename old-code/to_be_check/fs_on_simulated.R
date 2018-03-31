
library(cubature)

# activation energy
E_0 <- 185.4
sigma <- 3.5

E_lower <- E_0 - 3*sigma
E_upper <- E_0 + 3*sigma
E <- seq(from = E_lower, to = E_upper, by = 0.01)

#f_E <- dnorm(E, mean = E_0, sd = sigma)
f_E <- function (x) {
  dnorm(x, mean = E_0, sd = sigma)
}

A <- 10^13.64
beta <- 20
R <- 8.314
T <- seq(from = 400, to = 900, by = 0.01)
T_0 <- T[1]
T_f <- tail(T, n=1)

integrand_small <- function (A, beta, E, R, T) {
  A/beta * exp(- (E/R*T))
}

integrate(integrand_small(A, beta, 175, R, T), lower = T_0, upper = T_f)

* f_E(E)






integrate(function(y) {
  sapply(y, function(y) {
    integrate(function(x) myfun(x,y), llim, ulim)$value
  })
}, llim, ulim)






f_T <- f_Temp(A, beta, E, R, T)

int.fun <- function (x) {
  1 + x^2
}

integrate(int.fun, 1, -1)


d_alpha.d_T <- function () {
  * f_E
}


A/beta * exp(- (E/R*T) - int) * f_E











#draws <- sample(temp, size = 1000000, replace = TRUE, prob = temp)



fs <- function (x, params) {
  params[1] * exp( - (log(2) / params[2]^2) *
                            (log (1 + 2 * params[2] *
                                    ((x - params[3]) / params[4])))^2 )
}

p <- c(.015, 600, -.3, 40)

fs_test <- fs(temp, p)

plot(fs_test ~ temp)








