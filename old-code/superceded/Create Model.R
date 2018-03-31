# Mixture model
#==========================================================================

# Load packages
library(ggplot2)		# For ggplot()
library(R2jags)			# For calling JAGS; jags()
library(reshape2)		# For melt()
library(gridExtra)		# For grid.arrange()

# Filepaths
fp.wd <- "H:/My Folder"		# Change as required
setwd(fp.wd)
fp.data <- paste0(fp.wd, "/Data.csv")

#==========================================================================
# Data
#==========================================================================

# Data
# data <- read.csv(fp.data)
data(juncus)
munge <- process(juncus, 'temp_C', 'mass_loss', init_mass, 'C')
data <- ModData(munge)

# crop dataset at bounds
#data <- data[!(data$temp_K < 400 | data$temp_K > 900),]

Temp <- data$temp_K
Rate <- data$deriv
plot(Temp, Rate)

# Pseudo data
# We need samples from the distribution rather than measurements of pdf
# Here we use the idea of "inverse transform sampling method" to generate
# data as if drawn from the density represented by the pdf.  Since the 
# data are measured at (approximately) regular intervals, the result will
# be similarly discrete.  By adding small perturbations to each observation,
# the pseudo data is transformed to continuous (optional).

# Create cumulative density function (CDF)
cdf <- cumsum(Rate)
cdf <- cdf / max(cdf)
plot(cdf)

# Draw N uniform random variates on [0, 1]
N <- 2000 		# Sample size - reasonable to be large
u <- runif(N, 0, 1)

# Create pseudo data
y <- rep(NA, N)
for(i in 1:N){
	y[i] <- Temp[which.min(abs(u[i] - cdf))]
}
plot(density(y))		# Compare with plot(Rate)

# Add small perturbations to make data "more" continuous
y <- y + rnorm(N, 0, 0.15)		# +/- between 0 and 0.5 degrees Kelvin

# Remove data before 400 degrees Kelvin
rem <- which(y < 400)
y <- y[-rem]
N <- length(y)

# Nice plot of density of pseudo data
dat <- data.frame(x = y)
ggplot(dat, aes(x = x)) + 
	geom_density(fill = "black", alpha = 0.5, color = NA) +
	scale_x_continuous("Temp") +
	scale_y_continuous("Pseudo density")

#==========================================================================
# Fitting the model in JAGS
#==========================================================================

# Specify number of components
K <- 3

# Create data list to be passed to JAGS
jags.dat <- list(N = N, y = y, K = K)

# Initial values
inits <- function() {list(
	mu = rnorm(K, 570, 10),
	sigma.sq = rnorm(K, 80, 5),
	z = sample(1:K, N, replace = TRUE),
	w = (1:K)/K
)}

# Parameters to monitor
parameters <- c("mu", "sigma.sq", "z", "w")

# Call JAGS
M.burnin <- 2000
M <- 3000
n.chains <- 1
MCMC.jags <- jags(
	data = jags.dat, 
	inits = inits, 
	parameters.to.save = parameters, 
	n.chains = n.chains, 
	n.iter = M + M.burnin,
	n.burnin = M.burnin, 
	n.thin = 1,
	DIC = TRUE,
	model.file = "R/JAGS Model.txt"
)

# Remove unnecessary output formats before exporting
MCMC.jags$BUGSoutput$sims.matrix <- NULL
MCMC.jags$BUGSoutput$sims.array <- NULL
save(MCMC.jags, file = "MCMC Output.RData")		# Optional

#==========================================================================
# Posterior summary plots
#==========================================================================

pars <- MCMC.jags$BUGSoutput$sims.list
names(pars)

smooth <- 1					# Bandwitdh smoothing parameter
DPI <- 200					# Resolution

par.num <- length(pars) - 1
gg.post <- vector("list", par.num)

# -------------------

# Classify each observation into a component based on the number of 
# times out of N that z_i equals k, for k = 1, ..., K.

dat <- pars$z
freq <- matrix(0, N, K)
for (i in 1:N){
	for (k in 1: K){
		freq[i,k] <- sum(dat[,i][which(dat[,i] == k)])/k
	}
}
max <- apply(freq, 1, max)
group.all <- rep(0, N)
for (i in 1:N){
	group.all[i] <- which(freq[i,] == max[i])[1]
}

# Convert the frequencies into probabilities
probs <- freq
for (i in 1:N){
	probs[i,] <- freq[i,]/sum(freq[i,])
}

rm(group.all)
rm(freq)

# -------------------

# Plot for mu
dat <- melt(pars$mu)
gg.post[[1]] <- ggplot(dat, aes(x = value, fill = factor(Var2))) + 
	geom_density(alpha = 0.5, color = NA, adjust = smooth) +
	scale_x_continuous("") +
	scale_y_continuous(bquote("Posterior density for "~mu)) +
	scale_fill_discrete("Component/nmembership",limits = 1:K, guide = FALSE)

# Plot for sigma.sq
dat <- melt(pars$sigma.sq)
gg.post[[2]] <- ggplot(dat, aes(x = value, fill = factor(Var2))) + 
	geom_density(alpha = 0.5, color = NA, adjust = smooth) +
	scale_x_continuous("") +
	scale_y_continuous(bquote("Posterior density for "~sigma^2)) +
	scale_fill_discrete("Component/nmembership",limits = 1:K, guide = FALSE)

# Plot for w
dat <- melt(pars$w)
gg.post[[3]] <- ggplot(dat, aes(x = value, fill = factor(Var2))) + 
	geom_density(alpha = 0.5, color = NA, adjust = smooth) +
	scale_x_continuous("") +
	scale_y_continuous(bquote("Posterior density for "~w)) +
	scale_fill_discrete("Component/nmembership",limits = 1:K, guide = FALSE)

# Plot for z

names(dat) <- c("x.val", "K", "z")

dat <- melt(probs)
dat$Var1 <- rep(1:N, K)
gg.post[[4]] <- ggplot(dat, aes(x = Var1, y = value, fill = factor(Var2))) + 
	geom_bar(stat = "identity", position = "stack", color = NA, width = 1) +
	scale_x_continuous(expression(z[i])) +
	scale_y_continuous("Posterior frequency") +
	scale_fill_discrete("Component/nmembership",limits = 1:K, guide = FALSE)

# -------------------

# Export plots
png(filename = "Posterior Plots.png", 
	width = 500*DPI/72, height = 300*DPI/72, pointsize = 15, res = DPI)
grid.arrange(gg.post[[1]], gg.post[[2]], gg.post[[3]], gg.post[[4]], ncol = 2)
dev.off()

# EOF
