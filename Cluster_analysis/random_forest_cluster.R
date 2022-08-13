# setwd("~/Chinook_growth_repo")
# data_clusters <- read.csv("model_output/cluster_data/data_n3_age4_seed1.csv")
# likelihoods <- read.csv("model_output/cluster_data/likelihoods_n3_age4_seed1.csv")
# group_labs <- read.csv("transformed_data/increment_model_group_labels.csv")
# stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")

library(ggplot2)
library(dplyr)
library(randomForest)
library(reshape2)


####run for k=3
data_clusters_k3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n3_age4_seed2.csv")
# likelihoods <- read.csv("model_output/cluster_data/likelihoods_n4_age4_seed2.csv")
# group_labs <- read.csv("transformed_data/increment_model_group_labels.csv")
stock_characteristics <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")

# release_age <- c()
# for(i in 1:nrow(stock_characteristics)){
#   if(stock_characteristics$release_type[i] %in% c("one_YO_fall", "one_YO_summer")){
#     release_age <- append(release_age, 0)
#   }else{
#     release_age <- append(release_age, 1)
#   }
# }
# stock_characteristics$release_age <- release_age

dat <- data_clusters_k3 %>% 
  dplyr::group_by(stock_char) %>%
  dplyr::summarize(
    cluster = mean(cluster))%>%
  dplyr::mutate(stock = stock_char)%>%
  dplyr::select(-stock_char)

dat_factors <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::mutate(interior = 1*(release_location_rmis_region %in% c("LOCR","CECR","SNAK","UPCR")))





# one hot encoding 
d <- reshape2::dcast(dat_factors, 
                     stock ~ release_type, 
                     length)

d2 <- reshape2::dcast(dat_factors, stock ~ as.factor(run), length) %>%
  dplyr::select(-stock)

d <- cbind(d,d2)




d <- cbind(d,reshape2::dcast(dat_factors, stock ~ sex, length)%>%
             dplyr::select(-stock))

d <- cbind(d, dat_factors %>% select(interior))

dat_factors <- cbind(d, reshape2::dcast(dat_factors,  stock ~ release_location_rmis_region, 
                                        length)%>%
                       dplyr::select(-stock))


dat_ODI <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(ODI)


dat_release_age <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(release_age)


dat_x <- cbind(dat_factors,dat_ODI)
dat_x <- cbind(dat_x,dat_release_age)

dat_x$spring <- dat_x$`1`
dat_x$summer <- dat_x$`2`
dat_x$fall <- dat_x$`3`
dat_x$late_fall <- dat_x$`8`

dat <- dat %>%
  merge(dat_x, by = "stock")


dat_model <- dat %>% select(cluster, ODI,release_age,spring,summer,fall,late_fall,
                            interior,CECR,GRAY,LOCR,NOOR,NPS,
                            NWC,SKAG,SNAK,UPCR,WILP)

names(dat_model) <- c("cluster","ODI", "Release_age", "Spring", "Summer", "Fall", "Late_Fall",
                      "Interior", "CECR","GRAY","LOCR","NOOR","NPS","NWC","SKAG",          
                      "SNAK","UPCR","WILP" )
  
rf <- randomForest(formula = as.factor(cluster) ~ .
                   , data = dat_model,
                   ntree = 5000,mtry = 5,localImp = TRUE)




importance_vals <- importance(rf)
vars <- rownames(importance_vals)
importance_vals <- as.data.frame(importance_vals)
importance_vals$variable <- vars

importance_vals$var_type <- c(rep("Marine distribution",1),
                              rep("Release age", 1),
                              rep("Run timing", 4),
                              rep("RMIS region", 11)
                              )
importance_vals$`Variable type` <- c(rep("Marine distribution",1),
                                     rep("Release age", 1),
                                     rep("Run timing", 4),
                                     rep("RMIS region",11)
)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)

write.csv(importance_vals,"~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_n3.csv")

varImpPlot(rf)

rf


#### re run for k = 2 ####


data_clusters_k2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n2_age4_seed2.csv")

dat <- data_clusters_k2 %>% 
  dplyr::group_by(stock_char) %>%
  dplyr::summarize(
    cluster = mean(cluster))%>%
  dplyr::mutate(stock = stock_char)%>%
  dplyr::select(-stock_char)

dat_factors <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::mutate(interior = 1*(release_location_rmis_region %in% c("LOCR","CECR","SNAK","UPCR")))





# one hot encoding 
d <- reshape2::dcast(dat_factors, 
                     stock ~ release_type, 
                     length)

d2 <- reshape2::dcast(dat_factors, stock ~ as.factor(run), length) %>%
  dplyr::select(-stock)

d <- cbind(d,d2)




d <- cbind(d,reshape2::dcast(dat_factors, stock ~ sex, length)%>%
             dplyr::select(-stock))

d <- cbind(d, dat_factors %>% select(interior))

dat_factors <- cbind(d, reshape2::dcast(dat_factors,  stock ~ release_location_rmis_region, 
                                        length)%>%
                       dplyr::select(-stock))


dat_ODI <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(ODI)


dat_release_age <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(release_age)


dat_x <- cbind(dat_factors,dat_ODI)
dat_x <- cbind(dat_x,dat_release_age)

dat_x$spring <- dat_x$`1`
dat_x$summer <- dat_x$`2`
dat_x$fall <- dat_x$`3`
dat_x$late_fall <- dat_x$`8`

dat <- dat %>%
  merge(dat_x, by = "stock")


dat_model <- dat %>% select(cluster, ODI,release_age,spring,summer,fall,late_fall,
                            interior,CECR,GRAY,LOCR,NOOR,NPS,
                            NWC,SKAG,SNAK,UPCR,WILP)

names(dat_model) <- c("cluster","ODI", "Release_age", "Spring", "Summer", "Fall", "Late_Fall",
                      "Interior", "CECR","GRAY","LOCR","NOOR","NPS","NWC","SKAG",          
                      "SNAK","UPCR","WILP" )

rf <- randomForest(formula = as.factor(cluster) ~ .
                   , data = dat_model,
                   ntree = 5000,mtry = 5,localImp = TRUE)




importance_vals <- importance(rf)
vars <- rownames(importance_vals)
importance_vals <- as.data.frame(importance_vals)
importance_vals$variable <- vars

importance_vals$var_type <- c(rep("Marine distribution",1),
                              rep("Release age", 1),
                              rep("Run timing", 4),
                              rep("RMIS region", 11)
)
importance_vals$`Variable type` <- c(rep("Marine distribution",1),
                                     rep("Release age", 1),
                                     rep("Run timing", 4),
                                     rep("RMIS region", 11)
)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)

write.csv(importance_vals,"~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_n2.csv")

varImpPlot(rf)

rf




#### re run for k=4
data_clusters_k4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n4_age4_seed2.csv")

dat <- data_clusters_k4 %>% 
  dplyr::group_by(stock_char) %>%
  dplyr::summarize(
    cluster = mean(cluster))%>%
  dplyr::mutate(stock = stock_char)%>%
  dplyr::select(-stock_char)

dat_factors <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::mutate(interior = 1*(release_location_rmis_region %in% c("LOCR","CECR","SNAK","UPCR")))





# one hot encoding 
d <- reshape2::dcast(dat_factors, 
                     stock ~ release_type, 
                     length)

d2 <- reshape2::dcast(dat_factors, stock ~ as.factor(run), length) %>%
  dplyr::select(-stock)

d <- cbind(d,d2)




d <- cbind(d,reshape2::dcast(dat_factors, stock ~ sex, length)%>%
             dplyr::select(-stock))

d <- cbind(d, dat_factors %>% select(interior))

dat_factors <- cbind(d, reshape2::dcast(dat_factors,  stock ~ release_location_rmis_region, 
                                        length)%>%
                       dplyr::select(-stock))


dat_ODI <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(ODI)


dat_release_age <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(release_age)


dat_x <- cbind(dat_factors,dat_ODI)
dat_x <- cbind(dat_x,dat_release_age)

dat_x$spring <- dat_x$`1`
dat_x$summer <- dat_x$`2`
dat_x$fall <- dat_x$`3`
dat_x$late_fall <- dat_x$`8`

dat <- dat %>%
  merge(dat_x, by = "stock")


dat_model <- dat %>% select(cluster, ODI,release_age,spring,summer,fall,late_fall,
                            interior,CECR,GRAY,LOCR,NOOR,NPS,
                            NWC,SKAG,SNAK,UPCR,WILP)

names(dat_model) <- c("cluster","ODI", "Release_age", "Spring", "Summer", "Fall", "Late_Fall",
                      "Interior", "CECR","GRAY","LOCR","NOOR","NPS","NWC","SKAG",          
                      "SNAK","UPCR","WILP" )

rf <- randomForest(formula = as.factor(cluster) ~ .
                   , data = dat_model,
                   ntree = 5000,mtry = 5,localImp = TRUE)




importance_vals <- importance(rf)
vars <- rownames(importance_vals)
importance_vals <- as.data.frame(importance_vals)
importance_vals$variable <- vars

importance_vals$var_type <- c(rep("Marine distribution",1),
                              rep("Release age", 1),
                              rep("Run timing", 4),
                              rep("RMIS region", 11)
)
importance_vals$`Variable type` <- c(rep("Marine distribution",1),
                                     rep("Release age", 1),
                                     rep("Run timing", 4),
                                     rep("RMIS region", 11)
)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)

write.csv(importance_vals,"~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_n5.csv")

varImpPlot(rf)

rf





#######################################
####      k =4 with out outliers   ####
#######################################

#### re run for k=4
data_clusters <- read.csv("model_output/cluster_data/data_n4_age4_seed2_outliers.csv")
likelihoods <- read.csv("model_output/cluster_data/likelihoods_n4_age4_seed2_outliers.csv")
group_labs <- read.csv("transformed_data/increment_model_group_labels.csv")
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data_more_regions.csv")

release_age <- c()
for(i in 1:nrow(stock_characteristics)){
  if(stock_characteristics$release_type[i] %in% c("one_YO_fall", "one_YO_summer")){
    release_age <- append(release_age, 0)
  }else{
    release_age <- append(release_age, 1)
  }
}
stock_characteristics$release_age <- release_age

dat <- data_clusters %>% 
  dplyr::group_by(stock_char) %>%
  dplyr::summarize(
    cluster = mean(cluster))%>%
  dplyr::mutate(stock = stock_char)%>%
  dplyr::select(-stock_char)

dat_factors <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::mutate(interior = 1*(release_location_rmis_region %in% c("LOCR","CECR","SNAK","UPCR")))





# one hot encoding 
d <- reshape2::dcast(dat_factors, 
                     stock ~ release_type, 
                     length)

d2 <- reshape2::dcast(dat_factors, stock ~ as.factor(run), length) %>%
  dplyr::select(-stock)

d <- cbind(d,d2)




d <- cbind(d,reshape2::dcast(dat_factors, stock ~ sex, length)%>%
             dplyr::select(-stock))

d <- cbind(d, dat_factors %>% select(interior))

dat_factors <- cbind(d, reshape2::dcast(dat_factors,  stock ~ release_location_rmis_region, 
                                        length)%>%
                       dplyr::select(-stock))


dat_COG <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(COG)


dat_release_age <- stock_characteristics %>%
  dplyr::mutate(stock = paste(release_type,";",run,";",sex,";",release_location_rmis_basin))%>%
  dplyr::select(release_age)


dat_x <- cbind(dat_factors,dat_COG)
dat_x <- cbind(dat_x,dat_release_age)

dat_x$spring <- dat_x$`1`
dat_x$summer <- dat_x$`2`
dat_x$fall <- dat_x$`3`
dat_x$late_fall <- dat_x$`8`

dat <- dat %>%
  merge(dat_x, by = "stock")


dat_model <- dat %>% select(cluster, COG,release_age,spring,summer,fall,late_fall,
                            interior,CECR,LOCR,NOOR,NWC,SKAG,SNAK,UPCR)

names(dat_model) <- c("cluster","ODI", "Release_age", "Spring", "Summer", "Fall", "Late_Fall",
                      "Interior", "CECR", "LOOR", "NOOR", "NWC", "SKAG", "SNAK", "UPCR")

rf <- randomForest(formula = as.factor(cluster) ~ .
                   , data = dat_model,
                   ntree = 5000,mtry = 5,localImp = TRUE)




importance_vals <- importance(rf)
vars <- rownames(importance_vals)
importance_vals <- as.data.frame(importance_vals)
importance_vals$variable <- vars

importance_vals$var_type <- c(rep("Marine distribution",1),
                              rep("Release age", 1),
                              rep("Run timing", 4),
                              rep("RMIS region", 8)
)
importance_vals$`Variable type` <- c(rep("Marine distribution",1),
                                     rep("Release age", 1),
                                     rep("Run timing", 4),
                                     rep("RMIS region", 8)
)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)

write.csv(importance_vals,"model_output/cluster_data/RF_importnace_values_n4_outliers.csv")

varImpPlot(rf)

rf



