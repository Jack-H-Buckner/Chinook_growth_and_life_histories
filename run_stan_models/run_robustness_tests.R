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

data <- read.csv(data_path)
stock_group <- 1
mod <- 
run_analysis <- function(stock_group,stan_file,tag, min_age){
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
  
  
  covriates <- read.csv("~/Chinook_growth_repo/data/covariates.csv")
  
  X <- covriates %>% dplyr::select(
    year, PDO, WA_Pinks, ALPI, Pinks,        
    NPGO, MEI_summer, BI) # , BI, Pinks,WA_Pinks
  
  X <- X %>% dplyr::filter(year %in% years)
  X <- X %>% dplyr::select(-year)
  
  X <- scale(X)
  M <- ncol(X)
  
  
  
  N_brood_year = max(unique(data$brood_year - years_dat$year_zero)) - min(unique(data$brood_year - years_dat$year_zero))
  stan_data <- list(
    
    # Data
    N = nrow(data),
    M = M,
    N_brood_year = N_brood_year + 1,
    y_length = data$length/100,
    release_length = data$release_length/100,
    fishery = as.numeric(as.factor(data$fishery)),
    n_fishery = length(unique(data$fishery)),
    sigma = data$sigma/100,
    stock = as.numeric(as.factor(data$stock)),
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
    beta_devs = 1,
    alpha_RE = 2,
    beta_RE = 1
  )
  print(stan_data$M)
  options(mc.cores = parallel::detectCores())
  fit = stan(stan_file,
             data = stan_data, 
             chains = 4, 
             iter = 4000,
             control=list(max_treedepth = 12, adapt_delta=0.95),
             pars = c("B", "disp", "devs_stock_sd", "rho", "sigma_RE"))
  
  launch_shinystan(fit)
  samples <- as.data.frame(fit)
  priors <- c(stan_data$B_sd,stan_data$alpha_disp,
              stan_data$beta_disp, stan_data$alpha_devs,
              stan_data$beta_devs)
  
  write.csv(samples, paste("model_output/HMC_samples/samples_", tag, stock_group, ".csv", sep = ""))
  write.csv(samples,"model_output/HMC_samples/priors_1.csv")
  
}

stan_file <- "~/Chinook_growth_repo/stan_models/cohort_effects.stan"
tag <- "cohort_RE_"
# run_analysis(1,stan_file,tag,3)
# run_analysis(2,stan_file,tag,3)
# run_analysis(3,stan_file,tag,3)
# run_analysis(4,stan_file,tag,3)



stan_file <- "~/Chinook_growth_repo/stan_models/year_by_cohort_effects.stan"
tag <- "year_cohort_RE_"
run_analysis(1,stan_file,tag,3)
run_analysis(2,stan_file,tag,3)
run_analysis(3,stan_file,tag,3)
run_analysis(4,stan_file,tag,3)






# 
# run_analysis_NPC <- function(stock_group,stan_file,tag, min_age){
#   data <- data %>%
#     dplyr::filter(group==stock_group)%>%
#     dplyr::group_by(stock,age)%>%
#     dplyr::mutate(max_bood_year = max(brood_year),
#                   min_brood_year = min(brood_year),
#                   n_year = length(unique(brood_year)))%>%
#     dplyr::ungroup()%>%
#     dplyr::group_by(stock)%>%
#     dplyr::mutate(n_year = max(n_year),
#                   N_year = max(max_bood_year-min_brood_year))%>%
#     dplyr::filter(N_year > 25,
#                   n_year > 20)
#   
#   n_stocks <- length(unique(data$stock))
#   
#   data <- data %>% dplyr::filter(brood_year > 1977,
#                                  brood_year < 2010)
#   
#   data <- data %>% dplyr::filter(!(is.na(sigma)),!(is.na(age)) )
#   
#   years_dat <- max_year_stock(data)
#   years <- (years_dat$year_zero+1):years_dat$year_final
#   
#   
#   
#   ### covariates 
#   
#   
#   covriates <- read.csv("data/covariates.csv")
#   covriates <- mutate(covriates, NPGOxBI = NPGO * BI)
#   X <- covriates %>% dplyr::select(
#     year, NPGO, BI, NPGOxBI) # , BI, Pinks,WA_Pinks
#   
#   X <- X %>% dplyr::filter(year %in% years)
#   X <- X %>% dplyr::select(-year)
#   
#   X <- scale(X)
#   M <- ncol(X)
#   
#   
#   
#   N_brood_year = max(unique(data$brood_year - years_dat$year_zero)) - min(unique(data$brood_year - years_dat$year_zero))
#   stan_data <- list(
#     
#     # Data
#     N = nrow(data),
#     M = M,
#     N_brood_year = N_brood_year + 1,
#     y_length = data$length/100,
#     release_length = data$release_length/100,
#     fishery = as.numeric(as.factor(data$fishery)),
#     n_fishery = length(unique(data$fishery)),
#     sigma = data$sigma/100,
#     stock = as.numeric(as.factor(data$stock)),
#     age = data$age, 
#     brood_year = data$brood_year - years_dat$year_zero, 
#     release_age = data$release_age,
#     n_stock = length(unique(data$stock)),
#     n_year_max =  max(years_dat$max_years),
#     n_year = years_dat$max_years,
#     X = as.matrix(X[1:max(years_dat$max_years),]),
#     
#     # Priors 
#     B_sd = 0.25,
#     alpha_disp = 2,
#     beta_disp = 10,
#     alpha_devs = 2,
#     beta_devs = 1,
#     alpha_RE = 2,
#     beta_RE = 1
#   )
#   
#   options(mc.cores = parallel::detectCores())
#   fit = stan(stan_file,
#              data = stan_data, 
#              chains =4, 
#              iter = 4000,
#              control=list(max_treedepth = 12, adapt_delta=0.95),
#              pars = c("B", "disp", "devs_stock_sd", "rho", 
#                       "pred","predicted","var_obs", "var_process",
#                       "FE_age","slope"))
#   
#   launch_shinystan(fit)
#   samples <- as.data.frame(fit)
#   priors <- c(stan_data$B_sd,stan_data$alpha_disp,
#               stan_data$beta_disp, stan_data$alpha_devs,
#               stan_data$beta_devs)
#   
#   write.csv(samples, paste("model_output/HMC_samples/samples_", tag, stock_group, ".csv", sep = ""))
#   write.csv(samples,"model_output/HMC_samples/priors_1.csv")
#   
# }
# 
# 
# 
# 
# 
# stan_file <- "stan_models/year_by_cohort_effects.stan"
# tag <- "year_cohort_RE_NPC"
# run_analysis_NPC(2,stan_file,tag,3)
# run_analysis_NPC(4,stan_file,tag,3)
# 
