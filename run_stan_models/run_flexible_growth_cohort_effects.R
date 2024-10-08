library(rstan)
library(shinystan)
library(dplyr)
setwd("~/Chinook_growth_repo")
source("run_stan_models/useful_functions.R")
### time series 

#######################################
####                               ####
####      Run all covariates       ####
####                               ####
#######################################
data_path <- "~/Chinook_growth_repo/transformed_data/increment_model_data.csv"
stan_file <- "~/Chinook_growth_repo/stan_models/flexible_growth_cohort_effects.stan"


data <- read.csv(data_path)
stock_group <- 1
run_analysis <- function(stock_group){
  
  # New inclusion criteria 
  data <- data %>%
    dplyr::filter(group==stock_group)%>%
    dplyr::group_by(stock,age)%>%
    dplyr::mutate(max_bood_year = max(brood_year),
                  min_brood_year = min(brood_year),
                  n_year = length(unique(brood_year)))%>%
    dplyr::ungroup()%>%
    dplyr::group_by(stock)%>%
    dplyr::mutate(n_year = max(n_year),
                  N_year = max(max_bood_year-min_brood_year))%>%
    dplyr::filter(N_year > 25,
                  n_year > 20)
  
  
  print(head(data))
  n_stocks <- length(unique(data$stock))
  
  data <- data %>% dplyr::filter(brood_year > 1977,
                                 brood_year < 2010)
  
  data <- data %>% dplyr::filter(!(is.na(sigma)),!(is.na(age)) )
  
  years_dat <- max_year_stock(data)
  years <- (years_dat$year_zero+1):years_dat$year_final
  N <- length(unique(data$stock))
  print(paste("the number of stocks in the analysis is:  ", N))
  
  
  ### covariates 
  
  
  covriates <- read.csv("~/Chinook_growth_repo/data/covariates.csv")
  
  X <- covriates %>% dplyr::select(
    year, PDO, WA_Pinks, ALPI, Pinks,        
    NPGO, MEI_summer, BI) # , BI, Pinks,WA_Pinks
  print(names(X))
  print(head(X))
  X <- X %>% dplyr::filter(year %in% years)
  X <- X %>% dplyr::select(-year)
  
  X <- scale(X)
  M <- ncol(X)
  
  
  N_brood_year = max(unique(data$brood_year - years_dat$year_zero)) - min(unique(data$brood_year - years_dat$year_zero))
  stan_data <- list(
    
    # Data
    N = nrow(data),
    M = 7,
    N_brood_year =N_brood_year +1,
    y_length = data$length/100,
    release_length = data$release_length/100,
    sigma = data$sigma/100,
    stock = as.numeric(as.factor(data$stock)),
    fishery = as.numeric(as.factor(data$fishery)),
    n_fishery = length(unique(data$fishery)),
    age = data$age, 
    brood_year = data$brood_year - years_dat$year_zero, 
    release_age = data$release_age,
    n_stock = length(unique(data$stock)),
    n_year_max =  max(years_dat$max_years),
    n_year = years_dat$max_years,
    X = as.matrix(X[1:max(years_dat$max_years),]),
    a_min = min(data$release_age),
    n_age = 5 - min(data$release_age),
    
    # Priors 
    B_sd = 0.25,
    alpha_disp = 2,
    beta_disp = 10,
    alpha_devs = 2,
    beta_devs = 1,
    alpha_RE = 2,
    beta_RE = 1
  )
  
  #options(mc.cores = parallel::detectCores())
  fit = stan(stan_file,
             data = stan_data,
             chains =1,
             iter = 1000,
             control=list(max_treedepth = 8, adapt_delta=0.50),
             pars = c("B", "disp", "devs_stock_sd", "rho", 
                      "pred","predicted","var_obs", 
                      "var_process", "age_inc","sigma_RE"))
  
  launch_shinystan(fit)
  samples <- as.data.frame(fit)
  priors <- c(stan_data$B_sd,stan_data$alpha_disp,
              stan_data$beta_disp, stan_data$alpha_devs,
              stan_data$beta_devs)
  
  write.csv(samples, paste("~/Chinook_growth_repo/model_output/HMC_samples/samples_flexible_C_", stock_group, ".csv", sep = ""))
  write.csv(data, paste("~/Chinook_growth_repo/model_output/HMC_samples/data_flexible_C_", stock_group, ".csv", sep = ""))
  write.csv(samples[sample(1:4000,500),], paste("~/Chinook_growth_repo/model_output/HMC_samples/short_samples_flexible_C_", stock_group, ".csv", sep = ""))
  write.csv(samples,"~/Chinook_growth_repo/model_output/HMC_samples/priors_flexible_C_1.csv")
  
}

run_analysis(1)
run_analysis(2)
run_analysis(3)
run_analysis(4)

















