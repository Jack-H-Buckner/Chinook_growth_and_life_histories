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
  ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = pnw_palette("Cascades", n=2), name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")

ggsave(file = "figures/Clusters_n2_seed2.png",
       p,
       width = 6,
       height = 10)




# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k2 <- merge(data_clusters_k2 ,stock_characteristics, by = "stock_char")

ggplot(d_merged_k2 %>% 
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
        legend.position = "bottom")

# p vlaue 
d_test <- d_merged_k2 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )



ggplot(d_merged_k2  %>% 
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


d_test <- d_merged  %>% 
  group_by(stock_char)%>%
  summarize(run = mean(run.x),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_chisq$run,d_chisq$cluster, simulate.p.value=T)




ggplot(d_merged_k2  %>% 
         group_by(stock_char)%>%
         summarize(release_age = mean(release_age),
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
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

#####################################################
####                K = 3                        ####
#####################################################

# using seed 2
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
data_clusters_k3 <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n3_age4_seed2.csv")


# plot cluster trends
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
  ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = pnw_palette("Cascades", n=3), name = "Cluster")+
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

# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k3 <- merge(data_clusters_k3,stock_characteristics, by = "stock_char")

ggplot(d_merged_k3 %>% 
         group_by(stock_char)%>%
         summarize(cluster = mean(cluster),
                   ODI= mean(ODI)),
       aes(x = as.factor(cluster), 
           y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=3),
                    name = "Cluster")+
  xlab("Cluster")+
  ylab("Ocean Distribution Index (ODI)")+
  theme_classic()+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")


d_test <- d_merged %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )


ggplot(d_merged_k3  %>% 
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
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

d_test <- d_merged  %>%
  group_by(stock_char)%>%
  summarize(run = mean(run.x),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_test$run,d_test$cluster, simulate.p.value=T)



ggplot(d_merged_k3  %>% 
         group_by(stock_char)%>%
         summarize(release_age = mean(release_age),
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
  scale_fill_manual(values = pnw_palette("Cascades", n=3), name = "Cluster")+
  #scale_x_discrete(label = c("Spring", "Summer", "Fall", "Late Fall"))+
  ylab("Proporiton")+
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

d_test <- d_merged  %>%
  group_by(stock_char)%>%
  summarize(release_age = mean(release_age),
            cluster = mean(cluster))
print(paste("N = ",nrow(d_test)))
chisq.test(d_test$release_age,d_test$cluster, simulate.p.value=T)

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
                                     "4" = "Cluster: 3")))+
  geom_smooth( aes(group = as.factor(cluster)),alpha = 0.2)+
  theme_test()+
  ylim(-2.5,2.5)+
  ylab("Scaled lengths")+
  xlab("Year")+
  scale_color_manual(values = pnw_palette("Cascades", n=4), name = "Cluster")+
  theme(strip.text = element_text(size=20, family = "Times New Roman"),
        axis.title = element_text(size=28, family = "Times New Roman"),
        axis.text = element_text(size=24, family = "Times New Roman"),
        legend.title = element_text(size=28, family = "Times New Roman"),
        legend.text  = element_text(size=24, family = "Times New Roman"),
        legend.position = "bottom")
p

# ocean distribution 
stock_characteristics$stock_char <- stock_characteristics$stock
d_merged_k4 <- merge(data_clusters_k4,stock_characteristics, by = "stock_char")


ggplot(d_merged_k4 %>% 
         group_by(stock_char)%>%
         summarize(cluster = mean(cluster),
                   ODI= mean(ODI)),
       aes(x = as.factor(cluster), 
           y = ODI, fill = as.factor(cluster)))+
  geom_boxplot()+
  scale_fill_manual(values = pnw_palette("Cascades", n=4),
                    name = "Cluster")+
  theme_classic()


d_test <- d_merged_k4 %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster),
            ODI= mean(ODI))
print(paste("N = ",nrow(d_test)))
kruskal.test(as.factor(cluster) ~ ODI, 
             data = d_test )



ggplot(d_merged_k4  %>% 
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



ggplot(d_merged_k4  %>% 
         group_by(stock_char)%>%
         summarize(release_age = mean(release_age),
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
  xlab("Run timing")+
  theme_test()+
  theme(axis.title = element_text(size = 28, family = "Times New Roman"),
        axis.text = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 24, family = "Times New Roman"),
        legend.title = element_text(size = 22, family = "Times New Roman"),
        legend.position = "bottom")

































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



ggplot(d_merged  %>% 
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

