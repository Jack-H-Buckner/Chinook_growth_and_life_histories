library(dplyr)
library(ggplot2)
library(reshape2)

library(PNWColors)

samples_1 <- read.csv("~/Chinook_growth_repo/model_output/HMC_samples/samples_flexible_1.csv")
data_1 <- read.csv("~/Chinook_growth_repo/model_output/HMC_samples/data_flexible_3.csv")

samples_long_1 <- melt(samples_1, id.var = "X")



# Residuals as a function of age and time 
summarize_preds_1 <- samples_long_1 %>% 
  filter(substr(variable,1,5) == "pred.")%>%
  group_by(variable)%>%
  summarize(median = 100*quantile(value, 0.5),
            lower = 100*quantile(value, 0.25),
            upper = 100*quantile(value, 0.75))



residuals_df_1 <- cbind(data_1, summarize_preds_1) %>%
  mutate(resids = length - median)


p <- ggplot(residuals_df_1,
       aes(x = brood_year, y = resids))+
  facet_wrap(~age, ncol = 2, scales = "free_y")+
  geom_point(alpha = 0.5, size = 1.0)+
  geom_smooth()+
  #scale_color_manual(values = pnw_palette("Bay",n = 3), name = "Age")+
  theme_classic()+
  xlab("Brood year")+
  ylab("Residuals")

p

ggsave(p,
       file = "~/Chinook_growth_repo/figures/Residuals_flexible_growth_group_1.png",
       width = 7,
       height = 6)




## look at other parameters

samples_1 <- read.csv("~/Chinook_growth_repo/model_output/HMC_samples/samples_flexible_1.csv")
data_1 <- read.csv("~/Chinook_growth_repo/model_output/HMC_samples/data_flexible_3.csv")

samples_long_1 <- melt(samples_1, id.var = "X")



# get increments
summarize_1 <- samples_long_1 %>% 
  filter(substr(variable,1,7) == "age_inc")%>%
  group_by(variable)%>%
  summarize(median = 100*quantile(value, 0.5),
            lower = 100*quantile(value, 0.25),
            upper = 100*quantile(value, 0.75))


# compute length at age in constant environment 
dat <- samples_long_1 %>% 
  filter(substr(variable,1,7) == "age_inc")%>%
  mutate(age = as.numeric(substr(variable,9,9)),
         stock = as.numeric(substr(variable,11,12)))%>%
  group_by(variable)%>%
  summarize(median = 100*quantile(value, 0.5),
            lower = 100*quantile(value, 0.25),
            upper = 100*quantile(value, 0.75),
            age = mean(age),
            stock = mean(stock))

legnth_age <- c()
for(i in 1:nrow(dat)){
  d <- dat %>% filter(stock == floor(i/5)+1)
  legnth_age <- append(legnth_age, sum(d$median[1:(((i-1)%%5)+1)]))
}

dat$length <- legnth_age

ggplot(dat,
       aes(x = age, y = length, color = as.factor(stock)))+
  geom_point()+
  geom_line()





samples_1 <- read.csv("~/Chinook_growth_repo/model_output/HMC_samples/samples_flexible_YC_1_2.csv")

samples_long_1 <- melt(samples_1, id.var = "X")


library(dplyr)

samples_long_1 %>% 
  filter(substr(variable,1,8) == "sigma_RE")%>%
  group_by(variable)%>%
  summarize(median = quantile(value, 0.5),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75))





