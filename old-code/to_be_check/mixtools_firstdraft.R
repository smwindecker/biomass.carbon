
library(mixtools)

tga_d_trunc <- read.table('munge/tga_derivs.txt')
sp_a <- tga_d_trunc[tga_d_trunc$species_code == 'A',]


# this bit comes from Orfao et al 1999
# number of records
n <- nrow(sp_a)

# mass
W <- sp_a$p_mass

## global W to global alpha
# W_0 is initial total mass (in this case, at t = 125C)
# W is mass at temp t
# W_inf is final mass (in this case, at t = 650 -- the char)
W_to_alpha <- function (W, W_0, W_inf) {
  (W_0 - W) / (W_0 - W_inf)
}

# degree of transformation
alpha <- W_to_alpha(W, W[1], W[n])

# temperature
T <- sp_a$temp

# proportion of total mass in experiment that was lost in any component
X_inf <- (W[1] - W[n]) / W[1]

plot(sp_a$deriv ~ sp_a$temp, type = 'l')

draws <- sample(T, size = 1000000, replace = TRUE, prob = sp_a$deriv)
hist(draws, breaks = 10000)

m <- normalmixEM(draws, k = 3)
summary(m)

plot(m, density = TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8)

# get mass loss under each component
a_j_0 <- rep(NA, 3)
a_j_inf <- rep(NA, 3)
for (j in 1:3) {
  a_j_0[j] <- pnorm(sp_a$temp[1], mean = m$mu[j], sd = m$sigma[j])
  a_j_inf[j] <- pnorm(sp_a$temp[nrow(sp_a)], mean = m$mu[j], sd = m$sigma[j])
}

# function to be used for M of pseudo-component j
alpha_to_W_j <- function (alpha, W_0, W_inf) {
  - alpha * (W_0 - W_inf) + W_0
}

W_j_0 <- alpha_to_W_j(a_j_0, W[1], W[n]) * m$lambda
W_j_inf <- alpha_to_W_j(a_j_inf, W[1], W[n]) * m$lambda

# sum(M_j_0)
# M[1]
#
# sum(M_j_inf)
# M[n]

num <- W_j_0 - W_j_inf
denom <- W[1] - W[n]
z_j_0 <- num / denom

# proportions of each in what burned?
y_1_0 <- z_j_0[1] * X_inf
y_2_0 <- z_j_0[2] * X_inf
y_3_0 <- 1 - y_1_0 - y_2_0


c(y_1_0, y_2_0, y_3_0)




gaussian <- function (x, params) {
  params[1] * exp(-0.5 * ((x - params[2]) / params[3]) ^ 2)
}

mixture <- function (x, params) {
  y1 <- gaussian(x, params[1:3])
  y2 <- gaussian(x, params[4:6])
  y3 <- gaussian(x, params[7:9])
  y1 + y2 + y3
}

params <- c(m$lambda[1], m$mu[1], m$sigma[1],
            m$lambda[2], m$mu[2], m$sigma[2],
            m$lambda[3], m$mu[3], m$sigma[3])

a <- gaussian(T, params = params[1:3])
b <- gaussian(T, params = params[4:6])
c <- gaussian(T, params = params[7:9])

total <- mixture(T, params = params)
plot(total ~ T, type = 'l')
lines(a ~ T, col = 'red')
lines(b ~ T, col = 'green')
lines(c ~ T, col = 'blue')

deriv2 <- max(total) * sp_a$deriv / max(sp_a$deriv)

lines(deriv2 ~ T, col = 'grey')
