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
d <- read.csv( "~/Chinook_growth_repo/transformed_data/cluster_analysis_data.csv")


N_stocks <- length(unique(d$stock))
print(paste("the number of stocks in the analysis is:  ", N_stocks))



# character string and numberic stock labels. 
d$stock_char <- d$stock 
d$stock <- as.numeric(as.factor(d$stock))
N_iter <- 500

#mod_k2 <- gam_k_means_2(d, 2, 500, temp, seed)


args_ls <- list(c(2,141996),c(3,141996), c(4,141996), c(5,141996), c(6,141996),
                c(2,161961),c(3,161961), c(4,161961), c(5,161961), c(6,161961),
                c(2,2101993),c(3,2101993), c(4,2101993), c(5,2101993), c(6,2101993),
                c(2,100),c(3,100), c(4,100), c(5,100), c(6,100))


args_ls <- list(c(2,141996),c(2,161961),c(2,2101993),
                c(2,100),c(2,250),c(2,3201961),c(2,8822),
                c(3,141996),c(3,161961),c(3,2101993),
                c(3,100),c(3,250),c(3,3201961),c(3,8822),
                c(4,141996),c(4,161961),c(4,2101993),
                c(4,100),c(4,250),c(4,3201961),c(4,8822),
                c(5,141996),c(5,161961),c(5,2101993),
                c(5,100),c(5,250),c(5,3201961),c(5,8822),
                c(6,141996),c(6,161961),c(6,2101993),
                c(6,100),c(6,250),c(6,3201961),c(6,8822)) 

fun <- function(x){gam_k_means_2(d, x[1],N_iter, temp, x[2])}

#options(mc.cores = parallel::detectCores())
options(mc.cores = 8)
output_ls <- parallel::mclapply(args_ls, fun) # 15
save(output_ls, file = "~/Chinook_growth_repo/model_output/clusters/data_list.Rdata")




save_dat <- function(ind, n, seed){
  # run algorithm for k in {2,3,4,5}
  mod_n2 <- output_ls[[ind]]
  write.csv(mod_n2$dat, 
            paste0("~/Chinook_growth_repo/model_output/clusters/data_n",
            n, "_age4_seed", seed, ".csv"))
  write.csv(mod_n2$likelihoods,
            paste0("~/Chinook_growth_repo/model_output/clusters/likelihoods_n",
                    n, "_age4_seed", seed, ".csv"))
  write.csv(data.frame(MSE = c(mod_n2$MSE)),
            paste0("~/Chinook_growth_repo/model_output/clusters/MSE_n",
                   n, "_seed", seed, ".csv"))
  ggplot(mod_n2$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
  ggsave(paste0("~/Chinook_growth_repo/model_output/clusters/EM_algorithm_n",
                n, "_seed", seed, ".png"))
} 

load(file = "~/Chinook_growth_repo/model_output/clusters/data_list.Rdata")


# save data for n = 2
save_dat(1,2,1)
save_dat(2,2,2)
save_dat(3,2,3)
save_dat(4,2,4)
save_dat(5,2,5)
save_dat(6,2,6)
save_dat(7,2,7)

# save data for n = 3
save_dat(8,3,1)
save_dat(9,3,2)
save_dat(10,3,3)
save_dat(11,3,4)
save_dat(12,3,5)
save_dat(13,3,6)
save_dat(14,3,7)


# save data for n = 4
save_dat(15,4,1)
save_dat(16,4,2)
save_dat(17,4,3)
save_dat(18,4,4)
save_dat(19,4,5)
save_dat(20,4,6)
save_dat(21,4,7)


# save data for n = 5
save_dat(22,5,1)
save_dat(23,5,2)
save_dat(24,5,3)
save_dat(25,5,4)
save_dat(26,5,5)
save_dat(27,5,6)
save_dat(28,5,7)


# save data for n = 6
save_dat(29,6,1)
save_dat(30,6,2)
save_dat(31,6,3)
save_dat(32,6,4)
save_dat(33,6,5)
save_dat(34,6,6)
save_dat(35,6,7)



