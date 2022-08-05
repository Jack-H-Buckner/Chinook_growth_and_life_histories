library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")
source("clusters/GAM_clusters.R")

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
dat_means <- read.csv("transformed_data/increment_model_data.csv")

# additional processing 
d <- dat_means %>%
  filter(age == 4)%>%  # get lengths at age 4
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  filter(max_bood_year-min_brood_year > 25,
         n_year > 20)%>%
  ungroup()%>%
  group_by(stock)%>% # standardize lengt hat age observations for each stock 
  mutate(length_unscaled = length,
         m = mean(length))%>%
  mutate(length = length - m)%>%
  ungroup()%>%
  mutate(s = sd(length))%>%
  mutate(length = length/s)


N_stocks <- length(unique(d$stock))
print(paste("the number of stocks in the analysis is:  ", N_stocks))



# character string and numberic stock labels. 
d$stock_char <- d$stock 
d$stock <- as.numeric(as.factor(d$stock))
N_iter <- 500


seed <- 141996

#mod_k2 <- gam_k_means_2(d, 2, 500, temp, seed)


args_ls <- list(c(2,141996),c(3,141996), c(4,141996), c(5,141996), c(6,141996),
                c(2,161961),c(3,161961), c(4,161961), c(5,161961), c(6,161961),
                c(2,2101993),c(3,2101993), c(4,2101993), c(5,2101993), c(6,2101993),
                c(2,100),c(3,100), c(4,100), c(5,100), c(6,100))

fun <- function(x){gam_k_means_2(d, x[1],N_iter, temp, x[2])}

#options(mc.cores = parallel::detectCores())
options(mc.cores = 8)
output_ls <- parallel::mclapply(args_ls, fun) # 15

mod_k2 <- output_ls[[1]]



# run algorithm for k in {2,3,4,5}
mod_n2 <- output_ls[[1]]
write.csv(mod_n2$dat, "model_output/clusters/data_n2_age4_seed1.csv")
write.csv(mod_n2$likelihoods, "model_output/clusters/likelihoods_n2_age4_seed1.csv")
write.csv(data.frame(MSE = c(mod_n2$MSE)),
          "model_output/clusters/MSE_n2_seed1.csv")
ggplot(mod_n2$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n2_seed1.png")

mod_n3 <- output_ls[[2]]
write.csv(mod_n3$dat, "model_output/clusters/data_n3_age4_seed1.csv")
write.csv(mod_n3$likelihoods, "model_output/clusters/likelihoods_n3_age4_seed1.csv")
write.csv(data.frame(MSE= c(mod_n3$MSE)),
          "model_output/clusters/MSE_n3_seed1.csv")
ggplot(mod_n3$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n3_seed1.png")

mod_n4 <- output_ls[[3]]
write.csv(mod_n4$dat, "model_output/clusters/data_n4_age4_seed1.csv")
write.csv(mod_n4$likelihoods, "model_output/clusters/likelihoods_n4_age4_seed1.csv")
write.csv(data.frame(MSE = c(mod_n4$MSE)),
          "model_output/clusters/MSE_n4_seed1.csv")
ggplot(mod_n4$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n4_seed1.png")

mod_n5 <- output_ls[[4]]
write.csv(mod_n5$dat, "model_output/clusters/data_n5_age4_seed1.csv")
write.csv(mod_n5$likelihoods, "model_output/clusters/likelihoods_n5_age4_seed1.csv")
write.csv(data.frame(MSE = c(mod_n5$MSE)),
          "model_output/clusters/MSE_n5_seed1.csv")
ggplot(mod_n4$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggplot(mod_n5$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n5_seed1.png")


mod_n6 <- output_ls[[5]]
write.csv(mod_n6$dat, "model_output/clusters/data_n6_age4_seed1.csv")
write.csv(mod_n6$likelihoods, "model_output/clusters/likelihoods_n6_age4_seed1.csv")
write.csv(data.frame(MSE = c(mod_n6$MSE)),
          "model_output/clusters/MSE_n6_seed1.csv")
ggplot(mod_n6$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n6_seed1.png")




# run algorithm for k in {2,3,4,5}
mod_n2 <- output_ls[[6]]
write.csv(mod_n2$dat, "model_output/clusters/data_n2_age4_seed2.csv")
write.csv(mod_n2$likelihoods, "model_output/clusters/likelihoods_n2_age4_seed2.csv")
write.csv(data.frame(MSE = c(mod_n2$MSE)),
          "model_output/clusters/MSE_n2_seed2.csv")
ggplot(mod_n2$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n2_seed2.png")

mod_n3 <- output_ls[[7]]
write.csv(mod_n3$dat, "model_output/clusters/data_n3_age4_seed2.csv")
write.csv(mod_n3$likelihoods, "model_output/clusters/likelihoods_n3_age4_seed2.csv")
write.csv(data.frame(MSE = c(mod_n3$MSE)),
          "model_output/clusters/MSE_n3_seed2.csv")
ggplot(mod_n3$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n3_seed2.png")

mod_n4 <- output_ls[[8]]
write.csv(mod_n4$dat, "model_output/clusters/data_n4_age4_seed2.csv")
write.csv(mod_n4$likelihoods, "model_output/clusters/likelihoods_n4_age4_seed2.csv")
write.csv(data.frame(MSE = c(mod_n4$MSE)),
          "model_output/clusters/MSE_n4_seed2.csv")
ggplot(mod_n4$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n4_seed2.png")

mod_n5 <- output_ls[[9]]
write.csv(mod_n5$dat, "model_output/clusters/data_n5_age4_seed2.csv")
write.csv(mod_n5$likelihoods, "model_output/clusters/likelihoods_n5_age4_seed2.csv")
write.csv(data.frame(MSE = c(mod_n5$MSE)),
          "model_output/clusters/MSE_n5_seed2.csv")
ggplot(mod_n5$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n5_seed2.png")


mod_n6 <- output_ls[[10]]
write.csv(mod_n6$dat, "model_output/clusters/data_n6_age4_seed2.csv")
write.csv(mod_n6$likelihoods, "model_output/clusters/likelihoods_n6_age4_seed2.csv")
write.csv(data.frame(MSE = c(mod_n6$MSE)),
          "model_output/clusters/MSE_n6_seed2.csv")
ggplot(mod_n6$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n6_seed2.png")





mod_n2 <- output_ls[[11]]
write.csv(mod_n2$dat, "model_output/clusters/data_n2_age4_seed3.csv")
write.csv(mod_n2$likelihoods, "model_output/clusters/likelihoods_n2_age4_seed3.csv")
write.csv(data.frame(MSE = c(mod_n2$MSE)),
          "model_output/clusters/MSE_n2_seed3.csv")
ggplot(mod_n2$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n2_seed3.png")

mod_n3 <- output_ls[[12]]
write.csv(mod_n3$dat, "model_output/clusters/data_n3_age4_seed3.csv")
write.csv(mod_n3$likelihoods, "model_output/clusters/likelihoods_n3_age4_seed3.csv")
write.csv(data.frame(MSE = c(mod_n3$MSE)),
          "model_output/clusters/MSE_n3_seed3.csv")
ggplot(mod_n3$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n3_seed3.png")

mod_n4 <- output_ls[[13]]
write.csv(mod_n4$dat, "model_output/clusters/data_n4_age4_seed3.csv")
write.csv(mod_n4$likelihoods, "model_output/clusters/likelihoods_n4_age4_seed3.csv")
write.csv(data.frame(MSE= c(mod_n4$MSE)),
          "model_output/clusters/MSE_n4_seed3.csv")
ggplot(mod_n4$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n4_seed3.png")

mod_n5 <- output_ls[[14]]
write.csv(mod_n5$dat, "model_output/clusters/data_n5_age4_seed3.csv")
write.csv(mod_n5$likelihoods, "model_output/clusters/likelihoods_n5_age4_seed3.csv")
write.csv(data.frame(MSE = c(mod_n5$MSE)),
          "model_output/clusters/MSE_n5_seed3.csv")
ggplot(mod_n5$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n5_seed3.png")


mod_n6 <- output_ls[[15]]
write.csv(mod_n6$dat, "model_output/clusters/data_n6_age4_seed3.csv")
write.csv(mod_n6$likelihoods, "model_output/clusters/likelihoods_n6_age4_seed3.csv")
write.csv(data.frame(MSE = c(mod_n6$MSE)),
          "model_output/clusters/MSE_n6_seed3.csv")
ggplot(mod_n6$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n6_seed3.png")









mod_n2 <- output_ls[[16]]
write.csv(mod_n2$dat, "model_output/clusters/data_n2_age4_seed4.csv")
write.csv(mod_n2$likelihoods, "model_output/clusters/likelihoods_n2_age4_seed4.csv")
write.csv(data.frame(MSE = c(mod_n2$MSE)),
          "model_output/clusters/MSE_n2_seed4.csv")
ggplot(mod_n2$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n2_seed4.png")

mod_n3 <- output_ls[[17]]
write.csv(mod_n3$dat, "model_output/clusters/data_n3_age4_seed4.csv")
write.csv(mod_n3$likelihoods, "model_output/clusters/likelihoods_n3_age4_seed4.csv")
write.csv(data.frame(MSE = c(mod_n3$MSE)),
          "model_output/clusters/MSE_n3_seed4.csv")
ggplot(mod_n3$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n3_seed4.png")

mod_n4 <- output_ls[[18]]
write.csv(mod_n4$dat, "model_output/clusters/data_n4_age4_seed4.csv")
write.csv(mod_n4$likelihoods, "model_output/clusters/likelihoods_n4_age4_seed4.csv")
write.csv(data.frame(MSE= c(mod_n4$MSE)),
          "model_output/clusters/MSE_n4_seed4.csv")
ggplot(mod_n4$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n4_seed4.png")

mod_n5 <- output_ls[[19]]
write.csv(mod_n5$dat, "model_output/clusters/data_n5_age4_seed4.csv")
write.csv(mod_n5$likelihoods, "model_output/clusters/likelihoods_n5_age4_seed4.csv")
write.csv(data.frame(MSE = c(mod_n5$MSE)),
          "model_output/clusters/MSE_n5_seed4.csv")
ggplot(mod_n5$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n5_seed4.png")


mod_n6 <- output_ls[[20]]
write.csv(mod_n6$dat, "model_output/clusters/data_n6_age4_seed4.csv")
write.csv(mod_n6$likelihoods, "model_output/clusters/likelihoods_n6_age4_seed4.csv")
write.csv(data.frame(MSE = c(mod_n6$MSE)),
          "model_output/clusters/MSE_n6_seed4.csv")
ggplot(mod_n6$covergance,aes(x = x, y=y))+geom_point()+geom_smooth()
ggsave("figures/EM_algorithm_convergend_n6_seed4.png")


