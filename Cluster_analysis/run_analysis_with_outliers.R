library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")
source("Cluster_analysis/GAM_clusters.R")

################################################################################### 
#####                                                                         ##### 
#####    This script runs the cluaster analysis for the chinook growth        ##### 
#####    life histry paper. The seed is set to 141996 to insure the exact     #####  
#####    results can be reproduced. Scripts to generate the figures are in    #####  
#####    the make fugures file.                                               ##### 
#####                                                                         #####  
################################################################################## 

#set.seed(141996)

# load length at age data 
# These data are processed by the run_analysis.R 
# script in the process_data file
d <- read.csv( "~/Chinook_growth_repo/transformed_data/cluster_analysis_data_with_outliers.csv")


N_stocks <- length(unique(d$stock))
print(paste("the number of stocks in the analysis is:  ", N_stocks))



# character string and numberic stock labels. 
d$stock_char <- d$stock 
d$stock <- as.numeric(as.factor(d$stock))
N_iter <- 500


args_ls <- list(c(3,141996), c(3,161961),c(3,2101993),c(3,100))


fun <- function(x){gam_k_means_2(d, x[1],N_iter, temp, x[2])}

#options(mc.cores = parallel::detectCores())
options(mc.cores = 8)
output_ls <- parallel::mclapply(args_ls, fun) # 15
save(output_ls, file = "~/Chinook_growth_repo/model_output/clusters/data_list_with_outliers.Rdata")




save_dat <- function(ind, n, seed){
  # run algorithm for k in {2,3,4,5}
  mod_n2 <- output_ls[[ind]]
  write.csv(mod_n2$dat, 
            paste0("~/Chinook_growth_repo/model_output/clusters/data_n",
                   n, "with_outliers_age4_seed", seed, ".csv"))
  write.csv(mod_n2$likelihoods,
            paste0("~/Chinook_growth_repo/model_output/clusters/likelihoods_n",
                   n, "with_outliers_age4_seed", seed, ".csv"))
  write.csv(data.frame(MSE = c(mod_n2$MSE)),
            paste0("~/Chinook_growth_repo/model_output/clusters/MSE_n",
                   n, "with_outliers_seed", seed, ".csv"))
  ggplot(mod_n2$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
  ggsave(paste0("~/Chinook_growth_repo/model_output/clusters/EM_algorithm_n",
                n, "with_outliers_seed", seed, ".png"))
} 

load(file = "~/Chinook_growth_repo/model_output/clusters/data_list_with_outliers.Rdata")




# save data for n = 3
save_dat(1,3,1)
save_dat(2,3,2)
save_dat(3,3,3)
save_dat(4,3,4)


