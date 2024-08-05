data {
  int<lower = 0> N;
  int <lower =0> n_S; //number of state in df
  int <lower =0> n_P; //number of pol_parties in df
  int<lower = 1, upper = 2> sex[N];
  int<lower = 1, upper = 4> age[N];
  int<lower = 1, upper = n_S> state[N];
  int<lower = 1, upper = n_P> party[N];
  
  int<lower = 0> y[N];
  
  // fixed effects
  int D_statecov;
  matrix[N, D_statecov] X_statecov;

}
parameters {
  real alpha;
  
  real<lower = 0> sigma_beta;
  vector<multiplier = sigma_beta>[2] beta;
  real<lower = 0> sigma_gamma;
  vector<multiplier = sigma_gamma>[4] gamma;
  real<lower = 0> sigma_delta;
  vector<multiplier = sigma_delta>[n_S] delta;
  real<lower = 0> sigma_zeta;
  vector<multiplier = sigma_zeta>[n_P] zeta;
  
  vector[D_statecov] eta; 
  
}
model {
 // for (i in 1:N) {
 //   real omega_i;
 //   omega_i = alpha + beta[sex[i]] + gamma[age[i]] + delta[state[i]] + zeta[party[i]] + dot_product(X_statecov[i], eta);
 //   target += weight[i] * bernoulli_logit_lpmf(y[i] | omega_i);
 // }
  
  y ~ bernoulli_logit(alpha + 
  beta[sex] + 
  gamma[age] + 
  delta[state] + 
  zeta[party] + 
  X_statecov * eta); 
  
  alpha ~ normal(0, 1);
  
  beta ~ normal(0, sigma_beta);
  gamma ~ normal(0, sigma_gamma);
  delta ~ normal(0, sigma_delta);
  zeta ~ normal(0, sigma_zeta);
  
  sigma_beta ~ normal(0, 1);
  sigma_gamma~ normal(0, 1);
  sigma_delta ~ normal(0, 1);
  sigma_zeta ~ normal(0, 1);
  
  eta ~ normal(0,1);
  
}


