library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")
d <- read.csv( "~/Chinook_growth_repo/transformed_data/cluster_analysis_data.csv")
d_char <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")
d_merged <- merge(d,d_char, by = "stock")



ggplot(d_merged %>%
         filter(run.x %in% c(3,8))%>%
         group_by(brood_year,stock)%>%
         summarize(length = mean(length_unscaled),
                   ODI = mean(ODI)), 
       aes(x = brood_year, y = length, 
           color = ODI, group = ODI))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  viridis::scale_color_viridis()+
  theme_classic()


ggplot(d_merged %>%
         filter(run.x %in% c(1,2))%>%
         group_by(brood_year,stock)%>%
         summarize(length = mean(length_unscaled),
                   ODI = mean(ODI)), 
       aes(x = brood_year, y = length, 
           color = ODI, group = ODI))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  viridis::scale_color_viridis()+
  theme_classic()


# Clusters
dat <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n2_age4_seed1.csv")
d_char$stock_char <- d_char$stock
d_merged <- merge(dat,d_char, by = "stock_char")

ggplot(d_merged,
       aes(x = as.factor(cluster), y = ODI))+
  geom_boxplot()


ggplot(d_merged %>%
         group_by(sex)%>%
         mutate(n = n())%>%
         ungroup()%>%
         group_by(sex, cluster)%>%
         summarize(p = n()/n),
       aes(x = sex, y = p))+
  geom_bar(stat = identify)


d_merged %>% 
  group_by(stock_char)%>%
  summarize(cluster = mean(cluster))%>%
  ungroup()%>%
  group_by(cluster)%>%
  summarize(n())


