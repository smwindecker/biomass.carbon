

#T_i <- seq(from = 400, to = 900, by = 0.01)

T_i <- seq(from = 400, to = 900, by = 1)
T_0 <- T_i[1]
E_0 <- 125
sigma <- 10
E_lower <- E_0 - 3*sigma
E_upper <- E_0 + 3*sigma
A <- 1.5^9
beta <- 20
n <- 2
R <- 8.314

f_E <- function (E) {
  dnorm(E, mean = E_0, sd = sigma)
}

# try it with the f_E instead... should be same?
alpha <- vector(length=length(T_i))

for (i in 1:length(T_i)) {

  f <- function (E) {
    u <- E/(R*T_i[i])
    u_0 <- E/(R*T_0)
    p_u <- (exp(-u) / u^2) * ((0.99962*u + 0.60642) / (u + 2.56879))
    p_u_0 <- (exp(-u_0) / u_0^2) * ((0.99962*u_0 + 0.60642) / (u_0 + 2.56879))

    int <- E / R * (p_u - p_u_0)
    f_E(E) * (1 - (1-n) * (A / beta) * int)^(1/(1-n))

  }

  alpha[i] <- integrate(f, lower = 0, upper = Inf)$value

}

plot(T_i,alpha)


