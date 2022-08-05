setwd("~/chinook_growth")
library(ggplot2)
library(dplyr)
library(mgcv)


#####################################################
####                K = 3                        ####
#####################################################

stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters <- read.csv("samples/clusters/data_n3_age4_seed1.csv")

cbbPalette <- c("#000000","#F0E442", "#009E73","#56B4E9","#E69F00", "#56B4E9",  "#0072B2", "#D55E00", "#CC79A7")

###############################
#### Plots cluster trends ####
##############################
mod <- gam(data=data_clusters , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))

dat_predict <- data_clusters %>% 
  group_by(cluster, brood_year) %>% 
  summarize(n = n())


dat_predict$preds <- predict( mod,dat_predict)


p <- ggplot(dat_predict, aes(x = brood_year, y = preds, color = as.factor(cluster)))+
  geom_line()+
  scale_color_brewer(palette = "Dark2", name = "Cluster")+
  xlab("Brood year")+
  ylab("Trend")+
  theme_classic()


ggsave(file = "figures/fitted_trends_n3_seed1.png",
       p,
       width = 6,
       height = 4)

# plot cluster trends
p <- ggplot(data_clusters %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, color = as.factor(cluster), group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2",
                                     "3" = "Cluster: 3")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_n3_seed1.png",
       p,
       width = 6,
       height = 10)




#####################################################
#### Plots cluster assignments against atributes ####
#####################################################


## organize data 
data_clusters <- data_clusters %>%
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  ungroup()%>%mutate(duration = max_bood_year - min_brood_year)


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -n, -run)
d<- merge(data_clusters,stock_characteristics, by = "stock_char")

d <- d %>% 
  dplyr::group_by(stock,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   p52.5 = 1-mean(p52.5),
                   COG = mean(COG),
                   release_age = mean(release_age),
                   run = mean(run), 
                   n_year = mean(n_year), 
                   duration = mean(duration))



p <- ggplot(d ,aes(x = as.factor(cluster), y = COG, fill = as.factor(cluster)))+
  geom_boxplot()+
  theme_test()+
  xlab("Cluster")+
  scale_fill_manual(values=cbbPalette)+
  ylab("Ocean Distribution Index")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.position = "none")


ggsave(file = "figures/ClustersByODI_n3_seed1.png",
       p,
       width = 6,
       height = 5)



p <- ggplot(d %>% 
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  # scale_fill_brewer(palette = "Dark2",
  #                   name = "Cluster assignment")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")


ggsave(file = "figures/ClustersByRun_n3_seed1.png",
       p,
       width = 6,
       height = 5)


p <- ggplot(d %>% 
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  ylab("Proportion")+
  xlab("Release age")+
  scale_x_discrete(label = c("0", "1"))+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/ClustersByReleaseAge_n3_seed1.png",
       p,
       width = 6,
       height = 5)




##############################
#### Plots random forest  ####
##############################
importance_vals <- read.csv("samples/clusters/RF_importnace_values_n3.csv")

importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)

p <- ggplot(importance_vals,
            aes(y = variable, x = MeanDecreaseGini,fill = var_type))+#
  geom_bar(stat = "identity")+
  ylab("Variable")+
  xlab("Mean Decreasing Gini")+
  theme_classic()+
  scale_fill_manual(values=cbbPalette, name = "Variable type")+
  #scale_fill_brewer(palette = "Set1", name = "Variable type")+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = c(0.6, 0.5),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"))

p
ggsave(
  filename = "figures/clustered_rf_importance_n3_seed1.png",
  plot = p,
  width = 4.5,
  height = 7.5,
  dpi = 300
)

#####################################################
####                K = 4                        ####
#####################################################
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters <- read.csv("samples/clusters/data_n4_age4_seed3.csv")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbbPalette <- c("#000000","#F0E442", "#009E73","#56B4E9","#E69F00", "#56B4E9",  "#0072B2", "#D55E00", "#CC79A7")

###############################
#### Plots cluster trends ####
##############################



# plot fitted trends 
mod <- gam(data=data_clusters , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))

dat_predict <- data_clusters %>% 
  group_by(cluster, brood_year) %>% 
  summarize(n = n())


dat_predict$preds <- predict( mod,dat_predict)


p <- ggplot(dat_predict, aes(x = brood_year, y = preds, color = as.factor(cluster)))+
  geom_line()+
  scale_color_brewer(palette = "Dark2", name = "Cluster")+
  xlab("Brood year")+
  ylab("Trend")+
  theme_classic()


ggsave(file = "figures/fitted_trends_n4_seed3.png",
       p,
       width = 6,
       height = 4)



# plot clustered stock time series  
p <- ggplot(data_clusters %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, color = as.factor(cluster), group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2",
                                     "3" = "Cluster: 3",
                                     "4" = "Cluster: 4")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-3.5,3.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_n4_seed3.png",
       p,
       width = 6,
       height = 10)


#####################################################
#### Plots cluster assignments against atributes ####
#####################################################


## organize data 
data_clusters <- data_clusters %>%
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  ungroup()%>%mutate(duration = max_bood_year - min_brood_year)


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -n, -run)
d<- merge(data_clusters,stock_characteristics, by = "stock_char")

d <- d %>% 
  dplyr::group_by(stock,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   p52.5 = 1-mean(p52.5),
                   COG = mean(COG),
                   release_age = mean(release_age),
                   run = mean(run), 
                   n_year = mean(n_year), 
                   duration = mean(duration))



p <- ggplot(d ,aes(x = as.factor(cluster), y = COG, fill = as.factor(cluster)))+
  geom_boxplot()+
  theme_test()+
  xlab("Cluster")+
  scale_fill_manual(values=cbbPalette)+
  ylab("Ocean Distribution Index")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.position = "none")


ggsave(file = "figures/ClustersByODI_n4_seed3.png",
       p,
       width = 6 ,
       height = 5 )



p <- ggplot(d %>% 
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)),
            size = 6)+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  # scale_fill_brewer(palette = "Dark2",
  #                   name = "Cluster assignment")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")


ggsave(file = "figures/ClustersByRun_n4_seed3.png",
       p,
       width = 6,
       height = 5)


p <- ggplot(d %>% 
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)),
            size = 6)+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  ylab("Proportion")+
  xlab("Release age")+
  scale_x_discrete(label = c("0", "1"))+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/ClustersByReleaseAge_n4_seed3.png",
       p,
       width = 6,
       height = 5)


##############################
#### Plots random forest  ####
##############################
importance_vals <- read.csv("samples/clusters/RF_importnace_values_n4.csv")
#library(ggpattern)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)
p <- ggplot(importance_vals,
            aes(y = variable, x = MeanDecreaseGini,fill = var_type))+#
  geom_bar(stat = "identity")+
  ylab("Variable")+
  xlab("Mean Decreasing Gini")+
  theme_classic()+
  scale_fill_manual(values=cbbPalette, name = "Variable type")+
  #scale_fill_brewer(palette = "Set1", name = "Variable type")+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = c(0.6, 0.5),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"))

p
ggsave(
  filename = "figures/clustered_rf_importance_n4_seed3.png",
  plot = p,
  width = 4.5,
  height = 7.5,
  dpi = 300
)





#####################################################
####                K = 5                        ####
#####################################################

data_clusters <- read.csv("samples/clusters/data_n5_age4_seed1.csv")
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
###############################
#### Plots cluster trends ####
##############################

# plot fitted trends 
mod <- gam(data=data_clusters , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))

dat_predict <- data_clusters %>% 
  group_by(cluster, brood_year) %>% 
  summarize(n = n())


dat_predict$preds <- predict( mod,dat_predict)


p <- ggplot(dat_predict, aes(x = brood_year, y = preds, color = as.factor(cluster)))+
  geom_line()+
  scale_color_brewer(palette = "Dark2", name = "Cluster")+
  xlab("Brood year")+
  ylab("Trend")+
  theme_classic()


ggsave(file = "figures/fitted_trends_n5_seed1.png",
       p,
       width = 6,
       height = 4)



# plot clustered stock time series  
p <- ggplot(data_clusters %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, color = as.factor(cluster), group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2",
                                     "3" = "Cluster: 3",
                                     "4" = "Cluster: 4",
                                     "5" = "Cluster: 5")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-3.5,3.5)+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_n5_seed1.png",
       p,
       width = 6,
       height = 10)


#####################################################
#### Plots cluster assignments against atributes ####
#####################################################


## organize data 
data_clusters <- data_clusters %>%
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  ungroup()%>%mutate(duration = max_bood_year - min_brood_year)


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -n, -run)
d<- merge(data_clusters,stock_characteristics, by = "stock_char")

d <- d %>% 
  dplyr::group_by(stock,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   p52.5 = 1-mean(p52.5),
                   COG = mean(COG),
                   release_age = mean(release_age),
                   run = mean(run), 
                   n_year = mean(n_year), 
                   duration = mean(duration))



p <- ggplot(d ,aes(x = as.factor(cluster), y = COG, fill = as.factor(cluster)))+
  geom_boxplot()+
  theme_test()+
  xlab("Cluster")+
  scale_fill_manual(values=cbbPalette)+
  ylab("Ocean Distribution Index")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.position = "none")


ggsave(file = "figures/ClustersByODI_n5_seed1.png",
       p,
       width = 6,
       height = 5)



p <- ggplot(d %>% 
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  # scale_fill_brewer(palette = "Dark2",
  #                   name = "Cluster assignment")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")


ggsave(file = "figures/ClustersByRun_n5_seed1.png",
       p,
       width = 6,
       height = 5)


p <- ggplot(d %>% 
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  ylab("Proportion")+
  xlab("Release age")+
  scale_x_discrete(label = c("0", "1"))+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/ClustersByReleaseAge_n5_seed1.png",
       p,
       width = 6,
       height = 5)



##############################
#### Plots random forest  ####
##############################
importance_vals <- read.csv("samples/clusters/RF_importnace_values_n5.csv")
library(ggpattern)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)
p <- ggplot(importance_vals,
            aes(y = variable, x = MeanDecreaseGini,fill = var_type))+#
  geom_bar(stat = "identity")+
  ylab("Variable")+
  xlab("Mean Decreasing Gini")+
  theme_classic()+
  scale_fill_manual(values=cbbPalette, name = "Variable type")+
  #scale_fill_brewer(palette = "Set1", name = "Variable type")+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = c(0.6, 0.5),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 12, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"))


ggsave(
  filename = "figures/clustered_rf_importance_n5_seed1.png",
  plot = p,
  width = 4.5,
  height = 7,
  dpi = 300
)





###########################################
#### Plot Northern stocks but run type ####
###########################################



stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
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
  mutate(length = length/s)%>%
  select(-run)


dat <- merge(d,stock_characteristics,by = "stock")

ggplot(dat %>% filter(COG > 47.5), 
       aes(x = brood_year, y = length,
           color = as.factor(run),
           group = as.factor(stock)))+
  geom_smooth(aes(group = as.factor(run)))+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()


ggplot(dat %>% filter(COG > 48), 
       aes(x = brood_year, y = length,
           color = as.factor(run),
           group = as.factor(stock)))+
  geom_smooth(aes(group = as.factor(stock)),
              alpha = 0.01,
              size = 0.5)+
  geom_smooth(aes(group = as.factor(run)),
              linetype = 2)+
  scale_color_brewer(palette = "Dark2")+
  theme_classic()








#####################################################
####          K = 4     outliers                 ####
#####################################################
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters <- read.csv("samples/clusters/data_n4_age4_seed2_outliers.csv")
###############################
#### Plots cluster trends ####
##############################

# check convergence 
setwd("~/chinook_growth")
MSE1 <- read.csv("~/chinook_growth/samples/clusters/MSE_n4_seed1_outliers.csv")
MSE2 <- read.csv("~/chinook_growth/samples/clusters/MSE_n4_seed2_outliers.csv")
MSE3 <- read.csv("~/chinook_growth/samples/clusters/MSE_n4_seed_outliers.csv")
# see 2 is correct 
# plot fitted trends 
mod <- gam(data=data_clusters , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))

dat_predict <- data_clusters %>% 
  group_by(cluster, brood_year) %>% 
  summarize(n = n())


dat_predict$preds <- predict( mod,dat_predict)


p <- ggplot(dat_predict, aes(x = brood_year, y = preds, color = as.factor(cluster)))+
  geom_line()+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  xlab("Brood year")+
  ylab("Trend")+
  theme_classic()

p
ggsave(file = "figures/fitted_trends_n4_seed2_outliers.png",
       p,
       width = 6,
       height = 4)



# plot clustered stock time series  
p <- ggplot(data_clusters %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, color = as.factor(cluster), group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2",
                                     "3" = "Cluster: 3",
                                     "4" = "Cluster: 4")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-3.5,3.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")


p
ggsave(file = "figures/Clusters_n4_seed2_outliers.png",
       p,
       width = 6,
       height = 10)


#####################################################
#### Plots cluster assignments against atributes ####
#####################################################


## organize data 
data_clusters <- data_clusters %>%
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  ungroup()%>%mutate(duration = max_bood_year - min_brood_year)


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -n, -run)
d<- merge(data_clusters,stock_characteristics, by = "stock_char")

d <- d %>% 
  dplyr::group_by(stock,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   p52.5 = 1-mean(p52.5),
                   COG = mean(COG),
                   release_age = mean(release_age),
                   run = mean(run), 
                   n_year = mean(n_year), 
                   duration = mean(duration))



p <- ggplot(d ,aes(x = as.factor(cluster), y = COG, fill = as.factor(cluster)))+
  geom_boxplot()+
  theme_test()+
  xlab("Cluster")+
  scale_fill_manual(values=cbbPalette, name = "Cluster assignment")+
  ylab("Ocean Distribution Index")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.position = "none")


p
ggsave(file = "figures/ClustersByODI_n4_seed2_outliers.png",
       p,
       width = 6 ,
       height = 5 )



p <- ggplot(d %>% 
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 28, family = "Times New Roman"),
        legend.position = "bottom")

p
ggsave(file = "figures/ClustersByRun_n4_seed2_outliers.png",
       p,
       width = 6,
       height = 5)


p <- ggplot(d %>% 
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  ylab("Proportion")+
  xlab("Release age")+
  scale_x_discrete(label = c("0", "1"))+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 28, family = "Times New Roman"),
        legend.position = "bottom")
p
ggsave(file = "figures/ClustersByReleaseAge_n4_seed2_outliers.png",
       p,
       width = 6,
       height = 5)

##############################
#### Plots random forest  ####
##############################
importance_vals <- read.csv("samples/clusters/RF_importnace_values_n4_outliers.csv")
#library(ggpattern)
importance_vals$variable <- reorder(importance_vals$variable,importance_vals$MeanDecreaseGini)
p <- ggplot(importance_vals,
            aes(y = variable, x = MeanDecreaseGini,fill = var_type))+#
  geom_bar(stat = "identity")+
  ylab("Variable")+
  xlab("Mean Decreasing Gini")+
  theme_classic()+
  scale_fill_manual(values=cbbPalette, name = "Variable type")+
  #scale_fill_brewer(palette = "Set1", name = "Variable type")+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = c(0.7, 0.5),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"))

p
ggsave(
  filename = "figures/clustered_rf_importance_n4_seed3_outliers.png",
  plot = p,
  width = 4.5,
  height = 7.5,
  dpi = 300
)









#####################################################
####          K = 3     outliers                 ####
#####################################################
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters <- read.csv("samples/clusters/data_n3_age4_seed2_outliers.csv")
###############################
#### Plots cluster trends ####
##############################

# check convergence 
setwd("~/chinook_growth")
MSE1 <- read.csv("~/chinook_growth/samples/clusters/MSE_n3_seed1_outliers.csv")
MSE2 <- read.csv("~/chinook_growth/samples/clusters/MSE_n3_seed2_outliers.csv")
MSE3 <- read.csv("~/chinook_growth/samples/clusters/MSE_n3_seed3_outliers.csv")
# see 2 is correct 
# plot fitted trends 
mod <- gam(data=data_clusters , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))

dat_predict <- data_clusters %>% 
  group_by(cluster, brood_year) %>% 
  summarize(n = n())


dat_predict$preds <- predict( mod,dat_predict)


p <- ggplot(dat_predict, aes(x = brood_year, y = preds, color = as.factor(cluster)))+
  geom_line()+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  xlab("Brood year")+
  ylab("Trend")+
  theme_classic()

p
ggsave(file = "figures/fitted_trends_n3_seed2_outliers.png",
       p,
       width = 6,
       height = 4)



# plot clustered stock time series  
p <- ggplot(data_clusters %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, color = as.factor(cluster), group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2",
                                     "3" = "Cluster: 3",
                                     "4" = "Cluster: 4")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-3.5,3.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")


p
ggsave(file = "figures/Clusters_n3_seed2_outliers.png",
       p,
       width = 6,
       height = 10)


#####################################################
#### Plots cluster assignments against atributes ####
#####################################################


## organize data 
data_clusters <- data_clusters %>%
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  ungroup()%>%mutate(duration = max_bood_year - min_brood_year)


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -n, -run)
d<- merge(data_clusters,stock_characteristics, by = "stock_char")

d <- d %>% 
  dplyr::group_by(stock,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   p52.5 = 1-mean(p52.5),
                   COG = mean(COG),
                   release_age = mean(release_age),
                   run = mean(run), 
                   n_year = mean(n_year), 
                   duration = mean(duration))



p <- ggplot(d ,aes(x = as.factor(cluster), y = COG, fill = as.factor(cluster)))+
  geom_boxplot()+
  theme_test()+
  xlab("Cluster")+
  scale_fill_manual(values=cbbPalette, name = "Cluster assignment")+
  ylab("Ocean Distribution Index")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.position = "none")


p
ggsave(file = "figures/ClustersByODI_n3_seed2_outliers.png",
       p,
       width = 6 ,
       height = 5 )



p <- ggplot(d %>% 
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 28, family = "Times New Roman"),
        legend.position = "bottom")

p
ggsave(file = "figures/ClustersByRun_n3_seed2_outliers.png",
       p,
       width = 6,
       height = 5)


p <- ggplot(d %>% 
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  ylab("Proportion")+
  xlab("Release age")+
  scale_x_discrete(label = c("0", "1"))+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 28, family = "Times New Roman"),
        legend.position = "bottom")
p
ggsave(file = "figures/ClustersByReleaseAge_n3_seed2_outliers.png",
       p,
       width = 6,
       height = 5)








#####################################################
####          K = 5     outliers                 ####
#####################################################
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters <- read.csv("samples/clusters/data_n5_age4_seed1_outliers.csv")
###############################
#### Plots cluster trends ####
##############################

# check convergence 
setwd("~/chinook_growth")
MSE1 <- read.csv("~/chinook_growth/samples/clusters/MSE_n5_seed1_outliers.csv")
MSE2 <- read.csv("~/chinook_growth/samples/clusters/MSE_n5_seed2_outliers.csv")
MSE3 <- read.csv("~/chinook_growth/samples/clusters/MSE_n5_seed3_outliers.csv")
# see 1 is correct 
# plot fitted trends 
mod <- gam(data=data_clusters , length ~ as.factor(cluster) + s(brood_year, by =as.factor(cluster) ))

dat_predict <- data_clusters %>% 
  group_by(cluster, brood_year) %>% 
  summarize(n = n())


dat_predict$preds <- predict( mod,dat_predict)


p <- ggplot(dat_predict, aes(x = brood_year, y = preds, color = as.factor(cluster)))+
  geom_line()+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  xlab("Brood year")+
  ylab("Trend")+
  theme_classic()

p
ggsave(file = "figures/fitted_trends_n5_seed2_outliers.png",
       p,
       width = 6,
       height = 4)



# plot clustered stock time series  
p <- ggplot(data_clusters %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, color = as.factor(cluster), group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2",
                                     "3" = "Cluster: 3",
                                     "4" = "Cluster: 4",
                                     "5" = "Cluster: 5")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-3.5,3.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  #scale_color_brewer(palette = "Dark2", name = "Cluster")+
  scale_color_manual(values=cbbPalette, name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")


p
ggsave(file = "figures/Clusters_n5_seed2_outliers.png",
       p,
       width = 6,
       height = 10)


#####################################################
#### Plots cluster assignments against atributes ####
#####################################################


## organize data 
data_clusters <- data_clusters %>%
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  ungroup()%>%mutate(duration = max_bood_year - min_brood_year)


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -n, -run)
d<- merge(data_clusters,stock_characteristics, by = "stock_char")

d <- d %>% 
  dplyr::group_by(stock,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   p52.5 = 1-mean(p52.5),
                   COG = mean(COG),
                   release_age = mean(release_age),
                   run = mean(run), 
                   n_year = mean(n_year), 
                   duration = mean(duration))



p <- ggplot(d ,aes(x = as.factor(cluster), y = COG, fill = as.factor(cluster)))+
  geom_boxplot()+
  theme_test()+
  xlab("Cluster")+
  scale_fill_manual(values=cbbPalette, name = "Cluster assignment")+
  ylab("Ocean Distribution Index")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.position = "none")


p
ggsave(file = "figures/ClustersByODI_n5_seed2_outliers.png",
       p,
       width = 6 ,
       height = 5 )



p <- ggplot(d %>% 
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 28, family = "Times New Roman"),
        legend.position = "bottom")

p
ggsave(file = "figures/ClustersByRun_n5_seed2_outliers.png",
       p,
       width = 6,
       height = 5)


p <- ggplot(d %>% 
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values=cbbPalette, name = "Cluster")+
  ylab("Proportion")+
  xlab("Release age")+
  scale_x_discrete(label = c("0", "1"))+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 28, family = "Times New Roman"),
        legend.position = "bottom")
p
ggsave(file = "figures/ClustersByReleaseAge_n5_seed2_outliers.png",
       p,
       width = 6,
       height = 5)

