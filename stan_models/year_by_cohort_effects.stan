// add cohort by year random effects 
data {
  // data 
  int<lower=0> N; // sample size
  int<lower=0> M; //number of covariates 
  int<lower=0> N_brood_year; // nuer of brood years
  vector[N] y_length; // observed data
  vector[N] release_length;
  vector[N] sigma;
  int<lower=0> stock[N]; // stock ID 
  int<lower=0> age[N]; 
  int<lower=0> brood_year[N]; 
  int<lower=0> release_age[N];
  int<lower=0> fishery[N];
  int<lower=0> n_fishery; // number of fisheries
  int<lower=0> n_stock; // number of stocks
  int<lower=0> n_year_max; // number of years in the time series
  int<lower=0> n_year[n_stock];
  matrix[n_year_max, M] X; // covaraites
  
  // priors
  real<lower=0> B_sd;
  real<lower=0> alpha_disp;
  real<lower=0> beta_disp;
  real<lower=0> alpha_devs;
  real<lower=0> beta_devs;
  real<lower=0> alpha_RE;
  real<lower=0> beta_RE;
}

parameters {
  
  vector[M] B;
  
  vector[n_stock] intercept;
  
  vector[n_stock] slopes_trans; // growth - year - weights

  real<lower=0> devs_stock_sd;
  vector[sum(n_year)] devs_stock;
  real rho_trans;
  real<lower=0> disp;
  vector[5] RE[N_brood_year];
  vector<lower=0>[5] sigma_RE;
  real FE_fishery[n_fishery];
  real FE_age[3];
}

transformed parameters {
  vector[N] pred; // predictions of size at age for all stock and year combinations 
  vector<lower=0,upper=0.2>[n_stock] slope; // transformed slope parameters
  vector[n_stock] growth_inc[n_year_max]; // growth conditions 
  vector[n_stock] epsilon[n_year_max];
  real<lower=0,upper=1> rho;

  
  // slopes
  rho = 1/(1+exp(-rho_trans));
  
  for(i in 1:n_stock){
    slope[i] = 0.2 /(1+exp(-slopes_trans[i])); 
  }
  
 
  for(i in 1:n_stock){
    epsilon[1,i] = devs_stock_sd*devs_stock[1];
    for(j in 2:n_year_max){
        if(j < n_year[i]+1){
          epsilon[j,i] = rho * epsilon[j-1,i] + devs_stock_sd*devs_stock[j];
        }else{
          epsilon[j,i] = 0;
        }
      }
    if(i > 1){
      epsilon[1,i] = devs_stock_sd*devs_stock[sum(n_year[1:(i-1)])+1];
      for(j in 2:n_year_max){
        if(j < n_year[i]+1){
          epsilon[j,i] = rho * epsilon[j-1,i] + devs_stock_sd*devs_stock[sum(n_year[1:(i-1)])+j];
        }else{
          epsilon[j,i] = 0;
        }
      }
    }
  }

  
  // Compute growth increment for each year and run combination  
  for(i in 1:n_year_max) {
    for(j in 1:n_stock) {
      if(i < n_year[j]+1){
        growth_inc[i, j] = intercept[j] + dot_product(X[i],B) + epsilon[i,j];
      }else{
        growth_inc[i,j] = 0;
      }
    }
  }
  

  // Compute the predictions of length for each age, year, run combination
  for(i in 1:N) {
    pred[i] = release_length[i]; // starting length
    for(a in release_age[i]:(age[i])) {
        pred[i] = pred[i] + (1.0 - slope[stock[i]]*a) *( growth_inc[brood_year[i]+a, stock[i]] + RE[brood_year[i]+1,a]); // add year effects 
    }
    pred[i] = pred[i] + FE_fishery[fishery[i]] + FE_age[age[i]-2];
  }
  
  
}

model {

  intercept ~ normal(1.0,2.0);
  rho_trans ~ normal(0,0.25);
  FE_fishery ~ normal(0,0.1);
  FE_age ~ normal(0,0.1);
  slopes_trans ~ normal(0.0, 0.1); // random effects
  sigma_RE ~ gamma(alpha_RE,beta_RE);
  disp ~ gamma(alpha_disp,beta_disp);
  devs_stock_sd ~ gamma(alpha_devs,beta_devs);
  devs_stock ~ normal(0,1);
  B ~ normal(0, B_sd);
  for(i in 1:N_brood_year){
    for(j in 1:5){
      RE[i,j] ~ normal(0, sigma_RE[j]);
    }
  }

  // likelihood of observations. y_length are log lengths. weight by effective number
  for(i in 1:N) {
    target += normal_lpdf(y_length[i] | pred[i], sigma[i]+disp);
  }

}
