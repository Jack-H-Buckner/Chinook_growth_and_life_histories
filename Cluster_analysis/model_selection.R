library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")

# load MSE scores
N2.1 <- read.csv("model_output/clusters/MSE_n2_seed1.csv")
N2.2 <- read.csv("model_output/clusters/MSE_n2_seed2.csv")
N2.3 <- read.csv("model_output/clusters/MSE_n2_seed3.csv")
N2.4 <- read.csv("model_output/clusters/MSE_n2_seed4.csv")
N3.1 <- read.csv("model_output/clusters/MSE_n3_seed1.csv")
N3.2 <- read.csv("model_output/clusters/MSE_n3_seed2.csv")
N3.3 <- read.csv("model_output/clusters/MSE_n3_seed3.csv")
N3.4 <- read.csv("model_output/clusters/MSE_n3_seed4.csv")
N4.1 <- read.csv("model_output/clusters/MSE_n4_seed1.csv")
N4.2 <- read.csv("model_output/clusters/MSE_n4_seed2.csv")
N4.3 <- read.csv("model_output/clusters/MSE_n4_seed4.csv")
N4.4 <- read.csv("model_output/clusters/MSE_n4_seed3.csv")
N5.1 <- read.csv("model_output/clusters/MSE_n5_seed1.csv")
N5.2 <- read.csv("model_output/clusters/MSE_n5_seed2.csv")
N5.3 <- read.csv("model_output/clusters/MSE_n5_seed3.csv")
N5.4 <- read.csv("model_output/clusters/MSE_n5_seed4.csv")
N6.1 <- read.csv("model_output/clusters/MSE_n6_seed1.csv")
N6.2 <- read.csv("model_output/clusters/MSE_n6_seed2.csv")
N6.3 <- read.csv("model_output/clusters/MSE_n6_seed3.csv")
N6.4 <- read.csv("model_output/clusters/MSE_n6_seed4.csv")

vals <- c(N2.1$MSE[1],N2.2$MSE[1],N2.3$MSE[1],N2.4$MSE[1],
          N3.1$MSE[1],N3.2$MSE[1],N3.3$MSE[1],N3.4$MSE[1],
          N4.1$MSE[1],N4.2$MSE[1],N4.3$MSE[1],N4.4$MSE[1],
          N5.1$MSE[1],N5.2$MSE[1],N5.3$MSE[1],N5.4$MSE[1],
          N6.1$MSE[1],N6.2$MSE[1],N6.3$MSE[1],N6.4$MSE[1]) #N4.4$MSE[1],N4.5$MSE[1],N4.5$MSE[1],N4.7$MSE[1],N4.8$MSE[1],N4.9$MSE[1],

k <- c(2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6)

seed <- c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4)

ggplot(data.frame(k = k, MSE = vals, seed = seed) %>%
         group_by(k)%>%
         summarize(MSE = min(MSE)),
       aes(x = k, y = MSE))+
  geom_point()+
  geom_line()+
  theme_classic()

ggsave("figures/model_selection.png",
       height = 5,
       width = 7)



ggplot(data.frame(k = k, MSE = vals, seed = seed),
       aes(x = k, y = MSE, color= as.factor(seed)))+
  geom_point()+
  geom_line()+
  theme_classic()

ggsave("figures/model_selection_1.png",
       height = 5,
       width = 7)




