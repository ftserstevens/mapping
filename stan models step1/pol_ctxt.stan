data {
  
  int N; // number of observations
  int K; // number of response categories
  int<lower=1, upper=K> y[N]; // outcomes
  

//base X matrix
  int D; // number of predictors
  row_vector[D] x[N]; // predictors 



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
  
  int<lower=1> M_inc;
  int  <lower =1, upper=M_inc> g_inc[N];
  
  int<lower=1> M_edu;
  int  <lower =1, upper=M_edu> g_edu[N];
  
  int<lower=1> M_state;
  int  <lower =1, upper=M_state> g_state[N];
  
  int<lower=1> M_seen;
  int  <lower =1, upper=M_seen> g_seen[N];
}



parameters {
  
  ordered[K-1] theta;
  
  //fixed effects
  vector[D] beta; // pop level coef.
  
  
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
  
  real<lower=0> sigma_inc;
  vector[M_inc] inc;

  real<lower=0> sigma_edu;
  vector[M_edu] edu;
  
  real<lower=0> sigma_state;
  vector[M_state] state;

  real<lower=0> sigma_seen;
  vector[M_seen] seen;
}

transformed parameters {
  vector[M_id] net_score;
  
  net_score = beta[1] + id * sigma_id + dot_product(ctxt, rep_vector(sigma_ctxt, D_ctxt));

}
  


model {
  //priors
  beta ~ normal(0,1); // pop
  
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
  
  inc ~ normal(0,1);
  sigma_inc ~ normal(0,1);
  
  edu ~ normal(0,1);
  sigma_edu ~ normal(0,1);
  
  state ~ normal(0,1);
  sigma_state ~ normal(0,1);
  
  seen ~ normal(0,1);
  sigma_seen ~ normal(0,1);

  //Intercations priors

  lambda_CTXTparty ~ normal(0,1);
  sigma_CTXTparty ~ normal(0,1);
  
    

    
  
  for(n in 1:N) {
    y[n] ~ ordered_logistic( 
    
    id[g_id[n]]*sigma_id + // Aplha estimation
    x[n] * beta +  // fixed effects
    
    
    // estiamte impact of naive reviewer bias.
    (party[g_party[n]]*sigma_party + 
    age[g_age[n]]* sigma_age +
    sex[g_sex[n]]* sigma_sex +
    inc[g_inc[n]]* sigma_inc +
    edu[g_edu[n]]* sigma_edu +
    state[g_state[n]]* sigma_state +
    seen[g_seen[n]]* sigma_seen) +

    (x_ctxt[n] * ctxt*sigma_ctxt) + 

    (x_ctxtbyparty[n] * lambda_CTXTparty * sigma_CTXTparty),
  
    //cutoff points
    theta);
  }
}

