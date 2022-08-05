library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/chinook_growth")

make_dat <- function(group,
                     base_year,
                     min_years,
                     samples_path){
  g <- group
  
  
  dat <- read.csv("transformed_data/increment_model_data.csv") %>%
    dplyr::filter(group==g, age > 2)%>%
    dplyr::group_by(stock,age)%>%
    dplyr::mutate(n_year = length(unique(brood_year)))%>%
    dplyr::group_by(stock)%>%
    dplyr::group_by(n_year = median(n_year))%>%
    dplyr::filter(n_year >  min_years) %>% 
    dplyr::filter(brood_year > 1977, brood_year < 2010) %>% 
    dplyr::filter(!(is.na(sigma)),!(is.na(age)) )
  
  
  
  samples <- read.csv(samples_path) %>%
    reshape2::melt()%>%
    dplyr::filter(substr(variable,1,4) == "pred") %>%
    dplyr::mutate(pred_type = substr(variable,1,9) != "predicted")
  
  print(head(samples))
  
  print(sum(is.na(samples$value)))
  
  samples1 <- samples %>% 
    dplyr::group_by(variable, pred_type) %>%
    dplyr::summarize(lower = quantile(value, 0.25)*100,
                     upper = quantile(value, 0.75)*100,
                     pred = median(value)*100)
  
  print("here !")
  
  print(nrow(rbind(dat,dat)))
  print(nrow(samples1))
  
  
  dat1 <- cbind(rbind(dat,dat),samples1)
  
  print("here !!!")
  
  d <- dat1%>%
    dplyr::group_by(stock,age,pred_type)%>%
    dplyr::mutate(n_year = length(unique(brood_year)))%>%
    dplyr::ungroup()%>%
    dplyr::mutate(n_year_max = max(n_year))%>%
    dplyr::filter(n_year == n_year_max) %>%
    dplyr::group_by(age, brood_year,pred_type)%>%
    dplyr::summarize(length = mean(length),
                     pred = mean(pred), 
                     lower = mean(lower),
                     upper = mean(upper),
                     sigma = min(sigma))
  if(g == 3){
    d <- dat1%>%
      dplyr::filter(stock %in% c("two_YO_release ; 1 ; F ; WILL",
                                 "two_YO_release ; 1 ; M ; WILL"))%>%
      dplyr::group_by(stock,age,pred_type)%>%
      dplyr::group_by(age, brood_year,pred_type)%>%
      dplyr::summarize(length = mean(length),
                       pred = mean(pred), 
                       lower = mean(lower),
                       upper = mean(upper),
                       sigma = min(sigma))
    
  }
  dat1 <- dat1 %>%
    dplyr::mutate(brood_year_base =brood_year - base_year)
  
  return(list(dat1 = dat1, dat2 = d))
  
  
}


