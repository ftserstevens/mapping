data {
  
  int N; // number of observations
  int K; // number of response categories
  int<lower=1, upper=K> y[N]; // outcomes
  


//CTXT: topics matrix
  int D_ctxt; // number of predictors PC's
  row_vector[D_ctxt] x_ctxt[N]; // predictors for PC'

  int D_ctxtbyparty;
  row_vector[D_ctxtbyparty] x_ctxtbyparty[N]; 
  
//covariates R effects
  int<lower=1> M_id; // number of tweet fe
  int <lower =1, upper=M_id> g_id[N]; // map observation to tweet ids
  
  int<lower=1> M_party;
  int  <lower =1, upper=M_party> g_party[N];
  
  int<lower=1> M_age;
  int  <lower =1, upper=M_age> g_age[N];
  
  int<lower=1> M_sex;
  int  <lower =1, upper=M_sex> g_sex[N];
  
  int<lower=1> M_state;
  int  <lower =1, upper=M_state> g_state[N];

}



parameters {
  
  ordered[K-1] theta;
  
  //fixed effects
  real alpha; // intercept
  
  
  // random effects
  real <lower = 0> sigma_ctxt;
  vector[D_ctxt] ctxt; 

  real <lower = 0> sigma_CTXTparty;
  vector[D_ctxtbyparty] lambda_CTXTparty;
  
  real<lower=0> sigma_party;
  vector[M_party] party;
  
  real<lower=0> sigma_id;
  vector[M_id] id;
  
  real<lower=0> sigma_age;
  vector[M_age] age;
  
  real<lower=0> sigma_sex;
  vector[M_sex] sex;
  
  real<lower=0> sigma_state;
  vector[M_state] state;

}

transformed parameters {
  
}






model {
  //priors
  alpha ~ normal(0,1); // pop
  
  ctxt ~ normal(0,1); // pc's
  sigma_ctxt ~ normal(0,1); // 
  
  
  // Reffects priors
  id ~ normal(0,1); 
  sigma_id ~ normal(0,1);
  
  party ~ normal(0,1);
  sigma_party ~ normal(0,1);
  
  age ~ normal(0,1);
  sigma_age ~ normal(0,1);
  
  sex ~ normal(0,1);
  sigma_sex ~ normal(0,1);

  state ~ normal(0,1);
  sigma_state ~ normal(0,1);
  
  //Intercations priors
  lambda_CTXTparty ~ normal(0,1);
  sigma_CTXTparty ~ normal(0,1);
  
    

  
  for(n in 1:N) {
    y[n] ~ ordered_logistic( 
    
    alpha + // intercept
    id[g_id[n]]*sigma_id + // Tweet residuals
    // Reviewer ME
    (party[g_party[n]]*sigma_party + 
    age[g_age[n]]* sigma_age +
    sex[g_sex[n]]* sigma_sex +
    state[g_state[n]]* sigma_state) +
    /// context ME
    (x_ctxt[n] * ctxt*sigma_ctxt) + 
    //Interactions
    (x_ctxtbyparty[n] * lambda_CTXTparty * sigma_CTXTparty),
    //cutoff points
    theta);
  }
}


generated quantities {
  vector[N] y_pred;
  
  for (n in 1:N) {
    y_pred[n] = ordered_logistic_rng(
          alpha + // intercept
    id[g_id[n]]*sigma_id + // Tweet residuals
    // Reviewer ME
    (party[g_party[n]]*sigma_party + 
    age[g_age[n]]* sigma_age +
    sex[g_sex[n]]* sigma_sex +
    state[g_state[n]]* sigma_state) +
    /// context ME
    (x_ctxt[n] * ctxt*sigma_ctxt) + 
    //Interactions
    (x_ctxtbyparty[n] * lambda_CTXTparty * sigma_CTXTparty),
    //cutoff points
    theta);
    
  }
}

