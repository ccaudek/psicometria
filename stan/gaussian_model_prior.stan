data {
  int<lower=0> N;            // number of observations
}
generated quantities {
  real mu;                   // parameter of interest
  real<lower=0> sigma;       // known standard deviation of y
  array[N] real y_rep;       // prior predictive samples

  // Priors
  mu = normal_rng(175, 5);
  sigma = 10;

  // Generate prior predictive samples
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu, sigma);
  }
}
