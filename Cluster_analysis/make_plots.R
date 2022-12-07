library(ggplot2)
library(dplyr)
library(mgcv)
library(PNWColors)



#### Model selection plot

# plot the MSE values for each seed and select optimal value 
MSE2.1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed1.csv")
MSE2.2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed2.csv")
MSE2.3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed3.csv")
MSE2.4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed4.csv")
MSE2.5 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed5.csv")
MSE2.6 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed6.csv")
MSE2.7 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n2_seed7.csv")
plot(c(MSE2.1$MSE,MSE2.2$MSE,MSE2.3$MSE,MSE2.4$MSE,
       MSE2.5$MSE,MSE2.6$MSE,MSE2.7$MSE))

MSE3.1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed1.csv")
MSE3.2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed2.csv")
MSE3.3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed3.csv")
MSE3.4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed4.csv")
MSE3.5 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed5.csv")
MSE3.6 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed6.csv")
MSE3.7 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3_seed7.csv")
plot(c(MSE3.1$MSE,MSE3.2$MSE,MSE3.3$MSE,MSE3.4$MSE,
       MSE3.5$MSE,MSE3.6$MSE,MSE3.7$MSE))

MSE4.1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed1.csv")
MSE4.2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed2.csv")
MSE4.3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed3.csv")
MSE4.4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed4.csv")
MSE4.5 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed5.csv")
MSE4.6 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed6.csv")
MSE4.7 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n4_seed7.csv")
plot(c(MSE4.1$MSE,MSE4.2$MSE,MSE4.3$MSE,MSE4.4$MSE,
       MSE4.5$MSE,MSE4.6$MSE,MSE4.7$MSE))

MSE5.1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed1.csv")
MSE5.2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed2.csv")
MSE5.3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed3.csv")
MSE5.4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed4.csv")
MSE5.5 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed5.csv")
MSE5.6 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed6.csv")
MSE5.7 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n5_seed7.csv")
plot(c(MSE5.1$MSE,MSE5.2$MSE,MSE5.3$MSE,MSE5.4$MSE,
       MSE5.5$MSE,MSE5.6$MSE,MSE5.7$MSE))

MSE6.1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed1.csv")
MSE6.2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed2.csv")
MSE6.3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed3.csv")
MSE6.4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed4.csv")
MSE6.5 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed5.csv")
MSE6.6 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed6.csv")
MSE6.7 <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n6_seed7.csv")
plot(c(MSE6.1$MSE,MSE6.2$MSE,MSE6.3$MSE,MSE6.4$MSE,
       MSE6.5$MSE,MSE6.6$MSE,MSE6.7$MSE))


plot(2:6,c(MSE2.2$MSE,MSE3.2$MSE,MSE4.2$MSE,MSE5.5$MSE,MSE6.3$MSE))



#####################################################
####                K = 2                        ####
####################################################

# seed 2
stock_characteristics <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")
data_clusters_k2 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n2_age4_seed2.csv")


###############################
#### Plots cluster trends ####
##############################

p <- ggplot(data_clusters_k2  %>% 
              filter(age == 4) %>%
              group_by(brood_year, stock, cluster)%>%
              summarize(length = mean(length)),
            aes(x = brood_year+4, y = length, 
                color = as.factor(cluster),
                group = as.factor(stock)))+
  geom_point(size = 0.75, alpha = 0.5)+
  geom_line(size = 0.1)+
  geom_hline(aes(yintercept = -2),linetype = 2,size = 0.5)+
  geom_hline(aes(yintercept = 2),linetype = 2,size = 0.5)+
  facet_wrap(~cluster,
             ncol = 1,
             labeller = labeller(cluster = 
                                   c("1" = "Cluster: 1",
                                     "2" = "Cluster: 2")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  #ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = pnw_palette("Cascades", n=2), name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")
p
ggsave(file = "figures/Cluster_trends_n2_seed2.png",
       p,
       width = 6,
       height = 8)




# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k2 <- merge(data_clusters_k2 ,stock_characteristics, by = "stock_char")

p <- ggplot(d_merged_k2 %>% 
         group_by(stock_char)%>%
         summarize(cluster = mean(cluster),
                   ODI= mean(ODI)),
       aes(x = as.factor(cluster), 
           y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=2),
                     name = "Cluster")+
  xlab("Cluster")+
  ylab("Ocean Distribution Index (ODI)")+
  theme_classic()+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "none")

ggsave(file = "figures/Clusters_ODI_n2_seed2.png",
       p,
       width = 7,
       height = 6.0)

# p vlaue 
d_test <- d_merged_k2 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )



p <- ggplot(d_merged_k2  %>% 
         group_by(stock_char)%>%
         summarize(run = mean(run.x),
                   cluster = mean(cluster))%>%
         ungroup()%>%
         dplyr::group_by(run)%>%
         dplyr::mutate(m = n())%>%
         dplyr::ungroup()%>%
         dplyr::group_by(cluster, run)%>%
         dplyr::summarise(p = n()/mean(m),
                          m = mean(m)),
       aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=2), name = "Cluster")+
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
p
ggsave(file = "figures/Clusters_run_type_n2_seed2.png",
       p,
       width = 7,
       height = 6)

d_test <- d_merged  %>% 
  group_by(stock_char)%>%
  summarize(run = mean(run.x),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_chisq$run,d_chisq$cluster, simulate.p.value=T)



p <- ggplot(d_merged_k2  %>% 
         group_by(stock_char)%>%
         summarize(release_age = mean(release_age.x),
                   cluster = mean(cluster))%>%
         ungroup()%>%
         dplyr::group_by(release_age)%>%
         dplyr::mutate(m = n())%>%
         dplyr::ungroup()%>%
         dplyr::group_by(cluster, release_age)%>%
         dplyr::summarise(p = n()/mean(m),
                          m = mean(m)),
       aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=2), name = "Cluster")+
  ylab("Proporiton")+
  xlab("Release age")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

p
ggsave(file = "figures/Clusters_release_age_n2_seed2.png",
       p,
       width = 7,
       height = 6)



d_test <- d_merged  %>% 
  group_by(stock_char)%>%
  summarize(run = mean(run.x),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_chisq$run,d_chisq$cluster, simulate.p.value=T)


#####  random forest plot ######
var_imp_k2 <- read.csv(
         "~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_n2.csv")

var_imp_k2$variable <- reorder(var_imp_k2$variable,var_imp_k2$MeanDecreaseGini)

var_imp_k2$variable
p <- ggplot(var_imp_k2,
       aes(x = MeanDecreaseGini, 
           y =variable, 
           fill = var_type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = pnw_palette("Bay", n=4), 
                    name = "Variable")+
  theme_classic()+
  xlab("Mean Decreasing Gini")+
  ylab("Variable")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = c(0.75,0.5))


ggsave(file = "~/Chinook_growth_repo/figures/random_forest_k2.png",
       p,
       width = 9,
       height = 12)

#####################################################
####                K = 3                        ####
#####################################################

# using seed 2
stock_characteristics <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")
data_clusters_k3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n3_age4_seed2.csv")


# plot cluster trends
pal <- PNWColors::pnw_palette("Cascades", n=3)
pal[2]
p <- ggplot(data_clusters_k3 %>% 
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
  #ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = PNWColors::pnw_palette("Cascades", n=3), name = "Cluster")+
  theme(strip.text = element_text(size=24, family = "Times New Roman"),
        axis.title = element_text(size=32, family = "Times New Roman"),
        axis.text = element_text(size=28, family = "Times New Roman"),
        legend.title = element_text(size=32, family = "Times New Roman"),
        legend.text  = element_text(size=28, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "~/Chinook_growth_repo/figures/Clusters_trends_n3_seed2.png",
       p,
       width = 6,
       height = 10)

# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k3 <- merge(data_clusters_k3,stock_characteristics, by = "stock_char")

p <- ggplot(d_merged_k3 %>% 
         group_by(stock_char)%>%
         summarize(cluster = mean(cluster),
                   ODI= mean(ODI)),
       aes(x = as.factor(cluster), 
           y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=3),
                    name = "Cluster")+
  xlab("Cluster")+
  ylab("Ocean Distribution \n Index")+
  theme_classic()+
  theme(strip.text = element_text(size=24, family = "Times New Roman"),
        axis.title = element_text(size=32, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=24, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "none")

ggsave(file = "figures/Clusters_ODI_n3_seed2.png",
       p,
       width = 6,
       height = 5)



d_test <- d_merged_k3 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )


p <- ggplot(d_merged_k3  %>% 
         group_by(stock_char)%>%
         summarize(run = mean(run.x),
                   cluster = mean(cluster))%>%
         ungroup()%>%
         dplyr::group_by(run)%>%
         dplyr::mutate(m = n())%>%
         dplyr::ungroup()%>%
         dplyr::group_by(cluster, run)%>%
         dplyr::summarise(p = n()/mean(m),
                          m = mean(m)),
       aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=3), name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 32, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = "bottom")
p
ggsave(file = "figures/Clusters_run_type_n3_seed2.png",
       p,
       width = 6,
       height = 5)


d_test <- d_merged_k3  %>%
  group_by(stock_char)%>%
  summarize(run = mean(run.x),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_test$run,d_test$cluster, simulate.p.value=T)


d_merged_k3$release_age1 <- plyr::mapvalues(d_merged_k3$release_age.x, c(1,2), c(1,2))

p <- ggplot(d_merged_k3  %>% 
         group_by(stock_char)%>%
         summarize(release_age = mean(release_age1),
                   cluster = mean(cluster))%>%
         ungroup()%>%
         dplyr::group_by(release_age)%>%
         dplyr::mutate(m = n())%>%
         dplyr::ungroup()%>%
         dplyr::group_by(cluster, release_age)%>%
         dplyr::summarise(p = n()/mean(m),
                          m = mean(m)),
       aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = PNWColors::pnw_palette("Cascades", n=3), name = "Cluster")+
  #scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Release age")+
  theme_test()+
  theme(axis.title = element_text(size =32, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = "bottom")
p
ggsave(file = "~/Chinook_growth_repo/figures/Clusters_release_age_n3_seed2.png",
       p,
       width = 6,
       height = 5)

d_test <- d_merged_k3  %>%
  group_by(stock_char)%>%
  summarize(release_age = mean(release_age),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_test$release_age,d_test$cluster, simulate.p.value=T)



#####  random forest plot ######
var_imp_k3 <- read.csv(
  "~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_n3.csv")

var_imp_k3$variable <- reorder(var_imp_k3$variable,var_imp_k3$MeanDecreaseGini)


p <- ggplot(var_imp_k3,
            aes(x = MeanDecreaseGini, 
                y =variable, 
                fill = var_type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = pnw_palette("Bay", n=4), 
                    name = "Variable")+
  theme_classic()+
  xlab("Mean Decreasing Gini")+
  ylab("Variable")+
  theme(axis.title = element_text(size = 38, family = "Times New Roman"),
        axis.text = element_text(size = 28, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 34, family = "Times New Roman"),
        legend.position = c(0.75,0.5))


ggsave(file = "~/Chinook_growth_repo/figures/random_forest_k3.png",
       p,
       width = 10,
       height = 12)


#####################################################
####                K = 4                        ####
#####################################################

# using seed 2
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters_k4 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n4_age4_seed2.csv")


# plot cluster trends
p <- ggplot(data_clusters_k4 %>% 
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
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = pnw_palette("Cascades", n=4), name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_trends_n4_seed2.png",
       p,
       width = 6,
       height = 10)

# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k4 <- merge(data_clusters_k4,stock_characteristics, by = "stock_char")


p<-ggplot(d_merged_k4 %>% 
         group_by(stock_char)%>%
         summarize(cluster = mean(cluster),
                   ODI= mean(ODI)),
       aes(x = as.factor(cluster), 
           y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=4),
                    name = "Cluster")+
  xlab("Cluster")+
  ylab("Ocean Distribution Index (ODI)")+
  theme_classic()+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "none")


ggsave(file = "figures/Clusters_ODI_n4_seed2.png",
       p,
       width = 7,
       height = 6)


d_test <- d_merged_k4 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )



p<-ggplot(d_merged_k4  %>% 
         group_by(stock_char)%>%
         summarize(run = mean(run.x),
                   cluster = mean(cluster))%>%
         ungroup()%>%
         dplyr::group_by(run)%>%
         dplyr::mutate(m = n())%>%
         dplyr::ungroup()%>%
         dplyr::group_by(cluster, run)%>%
         dplyr::summarise(p = n()/mean(m),
                          m = mean(m)),
       aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=4), name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")


ggsave(file = "figures/Clusters_run_type_n4_seed2.png",
       p,
       width = 7,
       height = 6)


p <- ggplot(d_merged_k4  %>% 
         group_by(stock_char)%>%
         summarize(release_age = mean(release_age.x),
                   cluster = mean(cluster))%>%
         ungroup()%>%
         dplyr::group_by(release_age)%>%
         dplyr::mutate(m = n())%>%
         dplyr::ungroup()%>%
         dplyr::group_by(cluster, release_age)%>%
         dplyr::summarise(p = n()/mean(m),
                          m = mean(m)),
       aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=4), name = "Cluster")+
  ylab("Proporiton")+
  xlab("Release age")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")



ggsave(file = "figures/Clusters_release_age_n4_seed2.png",
       p,
       width = 7,
       height = 6)



#####  random forest plot ######
var_imp_k4 <- read.csv(
  "~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_n4.csv")

var_imp_k4$variable <- reorder(var_imp_k4$variable,var_imp_k4$MeanDecreaseGini)


p <- ggplot(var_imp_k4,
            aes(x = MeanDecreaseGini, 
                y =variable, 
                fill = var_type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = pnw_palette("Bay", n=4), 
                    name = "Variable")+
  theme_classic()+
  xlab("Mean Decreasing Gini")+
  ylab("Variable")+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = c(0.75,0.5))


ggsave(file = "~/Chinook_growth_repo/figures/random_forest_k4.png",
       p,
       width = 9,
       height = 12)



#####################################################
####                K = 5                        ####
#####################################################

# using seed 5
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters_k5 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n5_age4_seed5.csv")


# plot cluster trends
p <- ggplot(data_clusters_k5 %>% 
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
  #ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = pnw_palette("Cascades", n=5), name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_trends_n5_seed5.png",
       p,
       width = 6,
       height = 10)

# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k5 <- merge(data_clusters_k5,stock_characteristics, by = "stock_char")


p<-ggplot(d_merged_k5 %>% 
            group_by(stock_char)%>%
            summarize(cluster = mean(cluster),
                      ODI= mean(ODI)),
          aes(x = as.factor(cluster), 
              y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=5),
                    name = "Cluster")+
  xlab("Cluster")+
  ylab("Ocean Distribution Index (ODI)")+
  theme_classic()+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "none")

p
ggsave(file = "figures/Clusters_ODI_n5_seed5.png",
       p,
       width = 10,
       height = 8)


d_test <- d_merged_k5 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )



p<-ggplot(d_merged_k5  %>% 
            group_by(stock_char)%>%
            summarize(run = mean(run.x),
                      cluster = mean(cluster))%>%
            ungroup()%>%
            dplyr::group_by(run)%>%
            dplyr::mutate(m = n())%>%
            dplyr::ungroup()%>%
            dplyr::group_by(cluster, run)%>%
            dplyr::summarise(p = n()/mean(m),
                             m = mean(m)),
          aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=5), name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")


ggsave(file = "figures/Clusters_run_type_n5_seed5.png",
       p,
       width = 10,
       height = 8)


p <- ggplot(d_merged_k5  %>% 
              group_by(stock_char)%>%
              summarize(release_age = mean(release_age.x),
                        cluster = mean(cluster))%>%
              ungroup()%>%
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=5), name = "Cluster")+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")



ggsave(file = "figures/Clusters_release_age_n5_seed5.png",
       p,
       width = 10,
       height = 8)

#####################################################
#####################################################
####                                             ####
####            Make likelihood plots            ####
####                                             ####
#####################################################
#####################################################

#'Likelihoods_plot_data(likelihoods, nclusters = ncol(likelihoods), order = NA)
#'returns a long form data frame with the likelihoods
#'that each stock is associated with each cluster and
#'and ordering varible with maximum value of one 
#'the ordering variable lists the stocks in order the
#'probaiblity they are in each cluster in asceing order
#'likelihoods - likelihoos matrix from expectation maximization function
#'char - data frame with characteristics of stocks 
#'nclusters - number of clusters
#'order - a list c("Vi", "Vj", "Vk", ..."Vn")
Likelihoods_plot_data <- function(likelihoods, char, order ,nclusters = ncol(likelihoods)-1){
  x <- rep(0,nrow(likelihoods))
  for(i in 1:nclusters){
    print(order[i])
    x <- x + likelihoods[,order[i]]*i
  }
  x <- (x-min(x)+0.0001) /max((x-min(x)+0.001))
  print(head(x))
  likelihoods$x <- x + 0.000001*1:length(x)
  print(head(char))
  likelihoods <- merge(likelihoods, char, by = "stock")
  likelihoods_melt <- reshape2::melt(likelihoods, id.var = c("x", names(char)))
  return(likelihoods_melt)
}


add_blank_cols <- function(df, vars){
  combinations <- unique(df[vars])
  nms <- names(df)[!(names(df) %in% c(vars,"variable"))]
  combinations[nms] <- 0.00000
  combinations$variable <- "V1"
  return(rbind(df,combinations))
}

stock_characteristics <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")


stock_characteristics <- stock_characteristics %>% 
  dplyr::mutate(stock_char = paste(release_type,run,sex,release_location_rmis_basin))%>%
  dplyr::select(-stock, -run)

data_clusters <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n3_age4_seed2.csv")

d<- merge(data_clusters,stock_characteristics, 
          by = c("stock_char"))

d$run_text <- plyr::mapvalues(d$run, c(1,2,3,8),c("spring", "summer", "fall", "late fall"))
d <- d %>% mutate(stock_name=paste0(release_location_rmis_basin.y, " ", run_text, ".", 
                                release_age.x, " ", "(", sex, ")"))

d <- d %>% 
  dplyr::group_by(stock,stock_name,release_location_rmis_region)%>%
  dplyr::summarize(cluster = mean(cluster),
                   ODI = mean(ODI),
                   release_age = mean(release_age.x),
                   run = mean(run)) 

# plot likelihods for different cluster numbers 
char <- d %>% 
  dplyr::group_by(stock,stock_name)%>%
  dplyr::summarize(
    release_age = mean(release_age),
    ODI = mean(ODI),
    p52.5 = mean(ODI))%>%
  dplyr::mutate(p52.5 = 1*(ODI < 50.0))

d1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/likelihoods_n2_age4_seed2.csv") %>% 
  dplyr::select(-X)
names(d1) <- c("V2","V1","stock")
d1 <-Likelihoods_plot_data(d1,char,order = c("V1","V2"))
likli_n2 <- add_blank_cols(d1, c("release_age", "p52.5"))
likli_n2$variable <- ordered(likli_n2$variable, c("V1","V2"))
likli_n2$n <- 2

d1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/likelihoods_n3_age4_seed2.csv") %>%
  dplyr::select(-X)
names(d1) <- c("V1","V2","V3","stock")
d1 <-Likelihoods_plot_data(d1,char,order = c("V3","V1","V2"))

likli_n3 <- add_blank_cols(d1, c("release_age", "p52.5"))
likli_n3$variable <- ordered(likli_n3$variable, c("V1","V2","V3"))
likli_n3$n <- 3 
x1 <- likli_n3 %>% 
  dplyr::group_by(stock)%>%
  dplyr::summarise(x1 = mean(x))


d1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/likelihoods_n4_age4_seed2.csv") %>% 
  dplyr::select(-X)
names(d1) <- c("V2","V3","V1","V4","stock")
d1 <-Likelihoods_plot_data(d1,char,order = c("V1","V2","V4","V3"))
likli_n4 <- add_blank_cols(d1, c("release_age", "p52.5"))
likli_n4$n <- 4

d1 <- read.csv("~/Chinook_growth_repo/model_output/clusters/likelihoods_n5_age4_seed4.csv") %>% 
  dplyr::select(-X)
names(d1) <-c("V4","V3","V5","V2","V1","stock")

d1 <-Likelihoods_plot_data(d1,char,order = c("V1","V2","V3","V4","V5"))
likli_n5 <- add_blank_cols(d1, c("release_age", "p52.5"))
likli_n5$variable <- ordered(likli_n5$variable, c("V1","V2","V3","V4","V5"))
likli_n5$n <- 5

likli_n2 <- merge(likli_n2, x1, by = "stock")
likli_n3 <- merge(likli_n3, x1, by = "stock")
likli_n4 <- merge(likli_n4, x1, by = "stock")
likli_n5 <- merge(likli_n5, x1, by = "stock")

# force stock orderings to match patterns when k =

likeli <- rbind(likli_n2, likli_n3, likli_n4, likli_n5)
likeli$variable <- plyr::mapvalues(likeli$variable,
                                   c("V1","V2","V3","V4","V5"),
                                   c("1","2","3","4","5"))


## add group variables 
groups <- c()
for(i in 1:nrow(likeli)){
  group_i <- ""
  if(likeli$p52.5[i] == 1){
    group_i <- "South, "
  }else{ group_i <- "North, "}
  group_i <- paste(group_i, "age", likeli$release_age[i])
  groups <- append(groups, group_i)
}

likeli$group <- groups
likeli$stock_name <- reorder(likeli$stock_name ,likeli$x1)

p <- ggplot(likeli %>% dplyr::filter(x != 0),
            aes(x = stock_name,#as.factor(as.numeric(as.factor(x1))), 
                y = value, fill = variable))+
  geom_bar(stat = "identity")+
  facet_wrap(~n, ncol = 1)+
  theme_classic()+
  scale_fill_manual(values = pnw_palette("Bay", n =5),
                    name = "Cluster")+
  ylab("Probability")+
  xlab("Stock")+
  theme(axis.text.x = element_text(size = 11, 
                                   family = "Times New Roman",
                                   angle = 45,
                                   hjust = 1, vjust = 1),
        axis.title = element_text(size = 16, family = "Times New Roman"),
        strip.text = element_text(size = 12, family = "Times New Roman"),
        legend.title = element_text(size = 16, family = "Times New Roman"))

p
## note that the exact ordering of each cluster may change due to 
ggsave(
  filename = "~/Chinook_growth_repo/figures/likelihoods.png",
  plot = p,
  width = 11,
  height = 6,
  dpi = 300
)







### with outliers ###
MSE3.1_o <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3with_outliers_seed1.csv")
MSE3.2_o <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3with_outliers_seed2.csv")
MSE3.3_o <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3with_outliers_seed3.csv")
MSE3.4_o <- read.csv("~/Chinook_growth_repo/model_output/clusters/MSE_n3with_outliers_seed4.csv")
plot(c(MSE3.1_o$MSE,MSE3.2_o$MSE,MSE3.3_o$MSE,MSE3.4_o$MSE))



#####################################################
####                K = 3                        ####
#####################################################

# using seed 2
stock_characteristics <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")
data_clusters_k3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n3with_outliers_age4_seed2.csv")


# plot cluster trends
pal <- PNWColors::pnw_palette("Cascades", n=3)
pal[2]
p <- ggplot(data_clusters_k3 %>% 
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
  #ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = PNWColors::pnw_palette("Cascades", n=3), name = "Cluster")+
  theme(strip.text = element_text(size=24, family = "Times New Roman"),
        axis.title = element_text(size=32, family = "Times New Roman"),
        axis.text = element_text(size=28, family = "Times New Roman"),
        legend.title = element_text(size=32, family = "Times New Roman"),
        legend.text  = element_text(size=28, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "~/Chinook_growth_repo/figures/Clusters_trends_with_outliers_n3_seed2.png",
       p,
       width = 6,
       height = 10)

# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k3 <- merge(data_clusters_k3,stock_characteristics, by = "stock_char")

p <- ggplot(d_merged_k3 %>% 
              group_by(stock_char)%>%
              summarize(cluster = mean(cluster),
                        ODI= mean(ODI)),
            aes(x = as.factor(cluster), 
                y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=3),
                    name = "Cluster")+
  xlab("Cluster")+
  ylab("Ocean Distribution \n Index")+
  theme_classic()+
  theme(strip.text = element_text(size=24, family = "Times New Roman"),
        axis.title = element_text(size=32, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=24, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "none")

ggsave(file = "figures/Clusters_ODI_with_outliers_n3_seed2.png",
       p,
       width = 6,
       height = 5)



d_test <- d_merged_k3 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )


p <- ggplot(d_merged_k3  %>% 
              group_by(stock_char)%>%
              summarize(run = mean(run.x),
                        cluster = mean(cluster))%>%
              ungroup()%>%
              dplyr::group_by(run)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, run)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(run), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(run), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = pnw_palette("Cascades", n=3), name = "Cluster")+
  scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 32, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_run_type_with_outliers_n3_seed2.png",
       p,
       width = 6,
       height = 5)


d_test <- d_merged_k3  %>%
  group_by(stock_char)%>%
  summarize(run = mean(run.x),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_test$run,d_test$cluster, simulate.p.value=T)


d_merged_k3$release_age1 <- plyr::mapvalues(d_merged_k3$release_age.x, c(1,2), c(0,1))

p <- ggplot(d_merged_k3  %>% 
              group_by(stock_char)%>%
              summarize(release_age = mean(release_age1),
                        cluster = mean(cluster))%>%
              ungroup()%>%
              dplyr::group_by(release_age)%>%
              dplyr::mutate(m = n())%>%
              dplyr::ungroup()%>%
              dplyr::group_by(cluster, release_age)%>%
              dplyr::summarise(p = n()/mean(m),
                               m = mean(m)),
            aes(x = as.factor(release_age), y = p, fill = as.factor(cluster)))+
  geom_bar(stat = "identity")+
  geom_text(aes(x = as.factor(release_age), y = 1.025, label = paste("n =", m)))+
  scale_fill_manual(values = PNWColors::pnw_palette("Cascades", n=3), name = "Cluster")+
  #scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Release age")+
  theme_test()+
  theme(axis.title = element_text(size =32, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "~/Chinook_growth_repo/figures/Clusters_release_age_with_outliers_n3_seed2.png",
       p,
       width = 6,
       height = 5)




#####  random forest plot ######
var_imp_k3 <- read.csv(
  "~/Chinook_growth_repo/model_output/clusters/RF_importnace_values_with_outliers_n3.csv")

var_imp_k3$variable <- reorder(var_imp_k3$variable,var_imp_k3$MeanDecreaseGini)


p <- ggplot(var_imp_k3,
            aes(x = MeanDecreaseGini, 
                y =variable, 
                fill = var_type))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = pnw_palette("Bay", n=4), 
                    name = "Variable")+
  theme_classic()+
  xlab("Mean Decreasing Gini")+
  ylab("Variable")+
  theme(axis.title = element_text(size = 38, family = "Times New Roman"),
        axis.text = element_text(size = 28, family = "Times New Roman"),
        legend.text = element_text(size = 28, family = "Times New Roman"),
        legend.title = element_text(size = 34, family = "Times New Roman"),
        legend.position = c(0.75,0.5))


ggsave(file = "~/Chinook_growth_repo/figures/random_forest_with_outliers_k3.png",
       p,
       width = 10,
       height = 12)










