
library(cubature)

# activation energy
E_mu <- 185.4
sigma <- 3.5
E_0 <- E_mu - 3*sigma
E_f <- E_0 + 3*sigma
#E <- seq(from = E_lower, to = E_upper, by = 0.01)

f_E <- function (E) {
  dnorm(E, mean = E_0, sd = sigma)
}

A <- 10^13.64
beta <- 20
R <- 8.314

T_i <- seq(from = 400, to = 900, by = 0.01)
T_0 <- T_i[1]


for (i in T_i) {

  integrand <- function (E, T) {

    A/beta * exp(- (E/R*i) - (A/beta * exp(- (E/R*T)))) * f_E(E)

  }

  integrated <- hcubature(integrand, lowerLimit = c(E_0, T_0), upperLimit = c(E_f, i))

  d_alpha_d_T <- integrated$integral

}




hcubature(int, lowerLimit = c(0, 3), upperLimit = c(5, 8))

##########################


for (T in T_i) {
integral <- function (A, beta, R, f_E) {
integrate(f = function(E_vec) sapply(E_vec, function(E) {
  integrate (f = function(T) {
    A/beta * exp(-E/R*T)
    }, lower = T_0, upper = T)$value
}) * f_E(E_vec), lower = E_0, upper = E_f)$value
}
}

integral(A = A, beta = beta, R = R, f_E = f_E)


