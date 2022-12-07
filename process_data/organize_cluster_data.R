library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")


# Load growth increment data 
dat_means <- read.csv("~/Chinook_growth_repo/transformed_data/increment_model_data.csv")


# Filter for age 4 
d <- dat_means %>%filter(age == 4)


# restrict to observations from escapement
d <- d %>% filter(fishery %in% 50:59 )


# remove stocks that do not have at least 20 observations over a 25 year period  
d <- d %>% group_by(stock,release_type, run,
           release_location_rmis_basin) %>%  
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  filter(max_bood_year-min_brood_year > 25, 
         n_year > 20)



d_with_outliers <- d %>% group_by(stock,release_type, run,
                    release_location_rmis_basin)%>% # standardize lengt hat age observations for each stock 
  mutate(length_unscaled = length,
         m = mean(length))%>%
  mutate(length = length - m)%>%
  ungroup()%>%
  mutate(s = sd(length))%>%
  mutate(length = length/s)

# standardize length data for each stock 
d <- d %>% group_by(stock,release_type, run,
           release_location_rmis_basin)%>% # standardize lengt hat age observations for each stock 
  mutate(length_unscaled = length,
         m = mean(length))%>%
  mutate(length = length - m)%>%
  ungroup()%>%
  mutate(s = sd(length))%>%
  mutate(length = length/s) %>%
  filter(length > - 4.0) # remove outliers in first revisions this was set to 6


# save data for use in other analyses
write.csv(d, "~/Chinook_growth_repo/transformed_data/cluster_analysis_data.csv")
write.csv(d_with_outliers, "~/Chinook_growth_repo/transformed_data/cluster_analysis_data_with_outliers.csv")


N_stocks <- length(unique(d$stock))
print(paste("the number of stocks in the analysis is:  ", N_stocks))



d %>% 
  dplyr::group_by(stock) %>%
  dplyr::summarize(n = n(),
                  n_year = length(unique(brood_year)))%>%
  kableExtra::kable()%>%
  kableExtra::save_kable("~/Chinook_growth_repo/figures/clusters_stocks_table.png")

ggplot(dat_means,aes(y=length,x=brood_year))+geom_point()



ggplot(d,aes(y=length,x=brood_year))+geom_point()
