data {
  int<lower=1> N; // numero totale di osservazioni 
  vector[N] y; // variabile di risposta
  vector[N] x; // variabile predittore
}
parameters {
  real alpha; // intercetta
  real beta; // coefficiente angolare
  real<lower=0> sigma; // deviazione standard residua
}
model {
  // verosimiglianza
  y ~ normal(alpha + beta * x, sigma);
}
generated quantities {
  vector[N] y_rep; // variabili predette
  
  for (n in 1 : N) {
    y_rep[n] = normal_rng(alpha + beta * x[n], sigma);
  }
}
