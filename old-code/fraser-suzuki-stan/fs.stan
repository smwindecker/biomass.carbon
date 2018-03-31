
data {
  int<lower=1> N;                         // number of data points
  real d[N];                             // mass loss rate at temperature
  real<lower=0> t[N];                  // temperature
}

parameters {
  real h1;
  real h2;
  real h3;
  real s1;
  real s2;
  real s3;
  real p1;
  real p2;
  real p3;
  real w1;
  real w2;
  real w3;
  
  real<lower=0> sigma_obs; 
}

model {
  real c1[N];
  real c2[N];
  real c3[N];
  
  // priors
  sigma_obs ~ normal(0, 200);
  h1 ~ normal(0.003, 0.1);
  h2 ~ normal(0.006, 0.1);
  h3 ~ normal(0.001, 0.1);
  s1 ~ normal(-0.15, 1);
  s2 ~ normal(-0.15, 1);
  s3 ~ normal(-0.15, 1);
  p1 ~ normal(270, 50);
  p2 ~ normal(330, 50);
  p3 ~ normal(410, 50);
  w1 ~ normal(50, 50);
  w2 ~ normal(30, 50);
  w3 ~ normal(200, 50);

  // likelihood
  for (i in 1:N) {
    c1[i] = h1 * exp(-log(2)*((log(1 + 2*s1*((t[i] - p1)/w1)))/s1)^2);
    c2[i] = h2 * exp(-log(2)*((log(1 + 2*s2*((t[i] - p2)/w2)))/s2)^2);
    c3[i] = h3 * exp(-log(2)*((log(1 + 2*s3*((t[i] - p3)/w3)))/s3)^2);
    d[i] ~ normal(c1[i] + c2[i] + c3[i], sigma_obs);
    
  }
}

