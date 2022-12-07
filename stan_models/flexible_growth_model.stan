
data {
  // data 
  int<lower=0> N; // sample size
  int<lower=0> M; //number of covariates 
  vector[N] y_length; // observed data
  vector[N] release_length;
  vector[N] sigma;
  int<lower=0> stock[N]; // stock ID 
  int<lower=0> age[N]; 
  int<lower=0> brood_year[N]; 
  int<lower=0> release_age[N];
  int<lower=0> n_stock; // number of stocks
  int<lower=0> fishery[N];
  int<lower=0> n_fishery; // number of fisheries
  int<lower=0> n_year_max; // number of years in the time series
  int<lower=0> n_year[n_stock];
  matrix[n_year_max, M] X; // covaraites
  int<lower=0> a_min; // age at release 
  int<lower=0> n_age; // 5 - age at release
  // priors
  real<lower=0> B_sd;
  real<lower=0> alpha_disp;
  real<lower=0> beta_disp;
  real<lower=0> alpha_devs;
  real<lower=0> beta_devs;
}

parameters {
  
  vector[M] B;
  
  vector[n_stock] intercept;
  
  vector[n_stock] slopes_trans; // growth - year - weights
  vector<lower=0,upper=1>[n_stock] age_inc34;
  vector<lower=0,upper=1>[n_stock] age_inc45;
  
  real<lower=0> devs_stock_sd;
  vector[sum(n_year)] devs_stock;
  real rho_trans;
  real<lower=0> disp;
  real FE_fishery[n_fishery];
}

transformed parameters {
  vector[N] pred; // predictions of size at age for all stock and year combinations 
  vector[n_stock] slope; 
  vector[n_stock] growth_inc[n_year_max]; // growth conditions 
  vector[n_stock] age_inc[5]; 
  vector[n_stock] epsilon[n_year_max];
  real age_inc_scale;
  real<lower=0,upper=1> rho;
  
  rho = 1/(1+exp(-rho_trans));
  
  
  // slopes
  age_inc_scale = 1.0/(3-a_min); // age scale parameter insure no negative growth increments 
  for(i in 1:n_stock){
    slope[i] = age_inc_scale/(1+exp(-slopes_trans[i])); 
  }
  
  
  // age increments
  for(i in 1:n_stock){
    
    if(a_min == 2){
      age_inc[1,i] = 0.0; // if a_min is equal to one set incement from age zero to one to zero 
    }
    
    age_inc[a_min,i] = 1.0; // fist growth increment equals one w.l.o.g. 
    
    for(a in (a_min+1):3){ 
      age_inc[a,i] = (1-(a - a_min)*slope[i]); // increments decrease linearly until age 3. 
    }
    age_inc[3+1,i] = age_inc34[i]; // increments above age three are flexible 
    age_inc[4+1,i] = age_inc45[i]; // although I restricted their range to between zero and one. 
  }

  // growth increments random effects 
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

  
  // Compute growth increment env indecies  
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
        pred[i] = pred[i] + age_inc[a,stock[i]] * growth_inc[brood_year[i]+a, stock[i]]; // add year effects 
    }
    pred[i] = pred[i] + FE_fishery[fishery[i]]; 
  }
  
  
}

model {

  intercept ~ normal(1.0,2.0);
  rho_trans ~ normal(0,0.1);
  FE_fishery ~ normal(0,0.1);
  slopes_trans ~ normal(0.0, 0.25); 
  disp ~ gamma(alpha_disp,beta_disp);
  devs_stock_sd ~ gamma(alpha_devs,beta_devs);
  devs_stock ~ normal(0,1);
  B ~ normal(0, B_sd);
  

  // likelihood of observations. y_length are log lengths. weight by effective number
  for(i in 1:N) {
    target += normal_lpdf(y_length[i] | pred[i], sigma[i]+disp);
  }

}



generated quantities {
  // compute predictions without process errors
  vector[N] predicted;
  vector[n_stock] growth_inc_preds[n_year_max];
  real var_process;
  real var_obs;

  // growth increments
  for(i in 1:n_year_max) {
    for(j in 1:n_stock) {
      if(i < n_year[j]+1){
        growth_inc_preds[i, j] = intercept[j] + dot_product(X[i],B);
      }else{
        growth_inc_preds[i,j] = 0;
      }
    }
  }


  // predictions
  for(i in 1:N) {
    predicted[i] = release_length[i]; // starting length
    for(a in release_age[i]:(age[i])) {
        predicted[i] = predicted[i] + age_inc[a,stock[i]] * growth_inc_preds[brood_year[i]+a, stock[i]]; // add year effects
    }
    predicted[i] = predicted[i] + FE_fishery[fishery[i]];
  }

  var_obs = 0;
  for(i in 1:M){
    var_obs = var_obs + B[i]^2;
  }
  var_process = devs_stock_sd^2/(1-rho^2);
}
