library(dplyr)
library(ggplot2)

dat <- readRDS("~/Chinook_growth_repo/data/final_dat.rds")



ggplot(dat, 
       aes(y = length, x = as.factor(length_code)))+
  geom_boxplot()


dat %>% group_by(length_code)%>%
  summarize(n = n())




