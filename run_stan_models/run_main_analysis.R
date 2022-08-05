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
data_path <- "transformed_data/increment_model_data.csv"

stan_file <- "stan_models/base_model.stan"


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
  
  
  covriates <- read.csv("data/covariates.csv")
  
  X <- covriates %>% dplyr::select(
    year, PDO, WA_Pinks, ALPI, Pinks,        
    NPGO, MEI_summer, BI) # , BI, Pinks,WA_Pinks
  print(names(X))
  print(head(X))
  X <- X %>% dplyr::filter(year %in% years)
  X <- X %>% dplyr::select(-year)
  
  X <- scale(X)
  M <- ncol(X)
  
  
  
  stan_data <- list(
    
    # Data
    N = nrow(data),
    M = 7,
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
    
    # Priors 
    B_sd = 0.25,
    alpha_disp = 2,
    beta_disp = 10,
    alpha_devs = 2,
    beta_devs = 1
  )
  
  options(mc.cores = parallel::detectCores())
  fit = stan(stan_file,
             data = stan_data,
             chains =4,
             iter = 4000,
             control=list(max_treedepth = 12, adapt_delta=0.90),
             pars = c("B", "disp", "devs_stock_sd", "rho", "pred","predicted","var_obs", "var_process"))

  launch_shinystan(fit)
  samples <- as.data.frame(fit)
  priors <- c(stan_data$B_sd,stan_data$alpha_disp,
              stan_data$beta_disp, stan_data$alpha_devs,
              stan_data$beta_devs)

  write.csv(samples, paste("model_output/HMC_samples/samples_", stock_group, ".csv", sep = ""))
  
  write.csv(samples[sample(1:4000,500),], paste("model_output/HMC_samples/short_samples_", stock_group, ".csv", sep = ""))
  write.csv(samples,"model_output/HMC_samples/priors_1.csv")

}

run_analysis(1)
run_analysis(2)
run_analysis(3)
run_analysis(4)

##############################################
####                                      ####
####      Run NPGO + BI index model       ####
####                                      ####
##############################################

data_path <- "transformed_data/increment_model_data.csv"

stan_file <- "stan_models/base_model.stan"

data <- read.csv(data_path)

run_analysis_2 <- function(stock_group) {
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
  
  n_stocks <- length(unique(data$stock))
  
  data <- data %>% dplyr::filter(brood_year > 1977,
                                 brood_year < 2010)
  
  data <- data %>% dplyr::filter(!(is.na(sigma)),!(is.na(age)) )
  
  years_dat <- max_year_stock(data)
  years <- (years_dat$year_zero+1):years_dat$year_final
  
  
  
  ### covariates 
  
  
  covriates <- read.csv("data/covariates.csv")
  
  X <- covriates %>% dplyr::select(
    year,NPGO,BI) # , BI, Pinks,WA_Pinks
  X <- X %>% mutate(NPGO_BI = NPGO*BI)
  X <- X %>% dplyr::filter(year %in% years)
  X <- X %>% dplyr::select(-year)

  X <- scale(X)
  M <- ncol(X)


  stan_data <- list(

    # Data
    N = nrow(data),
    M = 3,
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

    # Priors
    B_sd = 0.25,
    alpha_disp = 2,
    beta_disp = 10,
    alpha_devs = 2,
    beta_devs = 1
  )

  options(mc.cores = parallel::detectCores())
  fit = stan(stan_file,
             data = stan_data,
             chains =4,
             iter = 4000,
             control=list(max_treedepth = 12, adapt_delta=0.95),
             pars = c("B", "disp", "devs_stock_sd", "rho", "pred","predicted","var_obs", "var_process"))

  launch_shinystan(fit)
  samples <- as.data.frame(fit)
  priors <- c(stan_data$B_sd,stan_data$alpha_disp,
              stan_data$beta_disp, stan_data$alpha_devs,
              stan_data$beta_devs)


  write.csv(samples, paste("model_output/HMC_samples/samples_NPGO_BI_", stock_group, ".csv", sep = ""))
  write.csv(samples[sample(1:4000,500),], paste("model_output/HMC_samples/short_samples_NPGO_BI_", stock_group, ".csv", sep = ""))
  write.csv(samples,"model_output/HMC_samples/priors_1.csv")

}


run_analysis_2(2)
run_analysis_2(4)















