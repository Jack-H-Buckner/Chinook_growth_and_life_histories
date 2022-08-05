
#### the code in this file may not run #### 

library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")
source("stan_model_plots/utils.R")



### tests 

stock_group <- 2
data_path <- "transformed_data/increment_model_data.csv"
data <- read.csv(data_path) %>% 
  dplyr::filter(group==stock_group)%>%
  dplyr::group_by(stock,age)%>%
  dplyr::mutate(n_year = length(unique(brood_year)))%>%
  dplyr::group_by(stock)%>%
  dplyr::group_by(n_year = median(n_year))%>%
  dplyr::filter(n_year > 25)

data <- data %>% dplyr::filter(brood_year > 1977,
                               brood_year < 2010)
data <- data %>% dplyr::filter(!(is.na(sigma)),!(is.na(age)) )

samples <- read.csv("model_output/HMC_samples/short_samples_2.csv") %>%
  reshape2::melt()%>%
  dplyr::filter(substr(variable,1,4) == "pred") %>%
  dplyr::mutate(pred_type = substr(variable,1,9) != "predicted")
samples1 <- samples %>% 
  dplyr::group_by(variable, pred_type) %>%
  dplyr::summarize(lower = quantile(value, 0.25)*100,
                   upper = quantile(value, 0.75)*100,
                   pred = median(value)*100)

dat1 <- cbind(rbind(data,data),samples1)



d <- dat1 %>% 
  group_by(stock) %>%
  mutate(n = n())%>%
  ungroup()%>%
  group_by()%>%
  mutate(max_n = max(n))%>%
  filter(n == max_n)%>%
  ungroup()%>%
  group_by(age,brood_year,pred_type)%>%
  summarize(pred = mean(pred),
            lower = mean(lower),
            upper = mean(upper),
            length = mean(length),
            sigma = sqrt(mean(sigma^2)))




ggplot(data = d,
       mapping = aes(x = brood_year, y = length, color = as.factor(age)),
       color = "black")+
  geom_point(size = 2)+
  geom_line(linetype = 2, alpha = 0.5)+
  geom_errorbar(aes(ymin = length - sigma, ymax = length + sigma))+
  geom_point(data = d %>% filter(!(pred_type)), 
             mapping = aes(x = brood_year, y = pred, 
                 group = as.factor(paste(age,pred_type))),
             color = "blue")+
  geom_line(data = d %>% filter(!(pred_type)), 
            mapping = aes(x = brood_year, y = pred, 
                          group = as.factor(paste(age,pred_type))),
            color = "blue")+
  geom_errorbar(data = d %>% filter(!(pred_type)), 
                mapping = aes(ymin = lower, ymax = upper),
                color = "blue")+
  theme_classic()
























d <- make_dat(2,
              1980, 20,
              "model_output/HMC_samples/short_samples_2.csv")


ggplot(dat1, 
       aes(x = brood_year, y = pred, 
           group = as.factor(paste(age,pred_type)), 
           color = pred_type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  scale_color_manual(
    name = "process errors",
    values = c("red","blue"))+
  theme_classic()


ggplot(dat1 %>%
         reshape2::dcast(age+brood_year~pred_type, value.var = "pred"),
       aes(x = brood_year, y = `TRUE` -`FALSE`))+
  geom_point()+
  geom_line()+
  facet_wrap(~age, nrow = 3)+
  theme_classic()







ggplot(dat1 %>% filter(stock == "one_YO_summer 3 M LEWI",
                      age == 4,
                      fishery ==54), 
       aes(x = brood_year, y = pred, 
           group = as.factor(paste(age,pred_type)), 
           color = pred_type))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = lower, ymax = upper))+
  scale_color_manual(
    name = "process errors",
    values = c("red","blue"))+
  geom_point(data = filter(dat1,pred_type,stock == "one_YO_summer 3 M LEWI",
                           age == 4,
                           fishery ==54),
             mapping = aes(x = brood_year, y = length),
             color = "black")+
  geom_line(data = filter(dat1,pred_type,stock == "one_YO_summer 3 M LEWI",
                          age == 4,
                          fishery ==54),
            mapping = aes(x = brood_year, y = length),
            color = "black")+
  geom_errorbar(data = filter(dat1,pred_type,stock == "one_YO_summer 3 M LEWI",
                              age == 4,
                              fishery ==54),
                mapping = aes(ymin = length - sigma, ymax = length + sigma),
                color = "black")+
  theme_classic()




ggplot(d$dat2, 
       aes(x = brood_year, y = pred, 
           group = as.factor(age)))+
  geom_point(color = "blue")+
  geom_line(color = "blue")+
  geom_errorbar(aes(ymin = lower, ymax = upper),color = "blue")+
  facet_wrap(~pred_type)+
  geom_point(mapping = aes(x = brood_year, y = length),
             color = "black")+
  geom_line(mapping = aes(x = brood_year, y = length),
            color = "black")+
  geom_errorbar(mapping = aes(ymin = length - sigma, ymax = length + sigma),
                color = "black")+
  facet_wrap(~pred_type)+
  theme_classic()



ggplot(d$dat2 %>%
         dplyr::filter(pred_type) %>%
         dcast(),
       aes(x = brood_year, y = length - pred))+
  geom_point()+
  geom_line()+
  facet_wrap(~age, nrow = 3)+
  theme_classic()

ggplot(d$dat2 %>%
         dplyr::filter(pred_type) %>%
         dplyr::mutate(resid = length - pred) %>%
         reshape2::dcast(brood_year ~ age, value.var = "resid"),
       aes(x = `3`, y = `4`))+
  geom_point()





raw_samples <- read.csv("samples/data/samples_2_priors2_short.csv")

varinace_components_estimates <- function(path){
  raw_samples <- read.csv(path)
  vars <- raw_samples %>%
    dplyr::mutate(var_disp = disp^2) %>%
    reshape2::melt()%>%
    dplyr::filter(substr(variable,1,1) %in% c("v"))%>%#"B","d","r",
    dplyr::group_by(variable)%>%
    dplyr::summarize(value = median(value))%>%
    dcast(1~variable)
  print(vars)
  
  R2_obs <- vars$var_obs/(vars$var_obs + vars$var_process)
  R2_process <- (vars$var_obs+vars$var_process)/(vars$var_obs+vars$var_process+vars$var_disp)
  
  return(list(R2_process = R2_process,  R2_obs = R2_obs))
}

varinace_components_estimates("samples/data/samples_2_priors2_short.csv")


varinace_components_largest_stock <- function(group,
                                              base_year,
                                              min_years,
                                              samples_path){
  d <- make_dat(group,
                base_year,
                min_years,
                samples_path)
  
  vars <- merge(d$dat2 %>% 
                  select(pred_type,length, pred, age, brood_year)%>%
                  filter(pred_type),
                d$dat2 %>% 
                  select(pred_type,pred, age, brood_year)%>%
                  filter(!pred_type),
                by = c("age", "brood_year"))
  
  
  MSE <- vars %>% 
    mutate(m = mean(length),
           m.x = mean(pred.x))%>%
    mutate(R0 = length - m,
           R1 = length - pred.y,
           R2 = length - pred.x,
           R0.x = pred.x - m.x,
           R1.x = pred.x - pred.y)%>%
    group_by(age)%>%
    summarize(MSE0 = sum(R0^2),
              MSE1 = sum(R1^2),
              MSE2 = sum(R2^2),
              MSE0.x = sum(R0.x^2),
              MSE1.x = sum(R1.x^2))
  
  R2_obs <- 1- MSE$MSE1.x/MSE$MSE0.x
  R2_process <- 1- MSE$MSE2/MSE$MSE0
  R2_pred <- 1- MSE$MSE1/MSE$MSE0
  return(list(R2_process = R2_process,  R2_obs = R2_obs, R2_pred = R2_pred))
}

varinace_components_largest_stock(2,
                                  1980, 20,
                                  "samples/data/samples_2_priors2_short.csv")

