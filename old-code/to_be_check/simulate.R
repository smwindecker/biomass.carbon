
# second attempt


#T_i <- seq(from = 400, to = 900, by = 0.01)

T_i <- seq(from = 400, to = 900, by = 1)
T_0 <- T_i[1]
E_0 <- 185.4
sigma <- 3.5
E_lower <- E_0 - 3*sigma
E_upper <- E_0 + 3*sigma
A <- 1.5^9
beta <- 20
R <- 8.314


f_E <- function (E) {
  dnorm(E, mean = E_0, sd = sigma)
}


# try it with the f_E instead... should be same?
alpha <- NULL

for (i in 1:length(T_i)) {

  f <- function (E) {
#E <- 190
#Te <- 450
    u <- E/(R*T_i[1])
    u_0 <- E/(R*T_0)
    p_u <- (exp(-u) / u^2) * ((0.99962*u + 0.60642) / (u + 2.56879))
    p_u_0 <- (exp(-u_0) / u_0^2) * ((0.99962*u_0 + 0.60642) / (u_0 + 2.56879))

    approx <- (E / R) * (p_u - p_u_0)
    I <- (beta/A)
    f_E(E) * exp(I)

  }

  ind_alpha <- integrate (f, lower = E_lower, upper = E_upper)$value

  alpha <- rbind(alpha, data.frame(ind_alpha))

}

##########################




# alpha <- NULL
#
# for (i in T_i) {
#
#     f <- function (E) {
#
#       u <- E/R*i
#       u_0 <- E/R*T_0
#       p_u <- (exp(-u) / u^2) * ((0.99962*u + 0.60642) / (u + 2.56879))
#       p_u_0 <- (exp(-u_0) / u_0^2) * ((0.99962*u_0 + 0.60642) / (u_0 + 2.56879))
#
#       pre <- 1 / (sigma * sqrt(2 * pi))
#       int <- E / R * (p_u - p_u_0)
#       act <- (E - E_0)^2 /  2 * sigma^2
#
#       pre * exp(-int - act)
#
#       ind_alpha <- integrate (f, lower = E_lower, upper = E_upper)$value
#
#     }
#
#      alpha <- rbind(alpha, data.frame(ind_alpha))
# }

