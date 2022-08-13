
library(dplyr)
library(ggplot2)
library(rstan)
library(reshape2)
setwd("~/Chinook_growth_repo")


## load the relevant dat sets 
ODI_dat <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")
marine_dsn_dat <- read.csv("~/Chinook_growth_repo/transformed_data/marine_observations.csv")

## load cluster analysis data for stocks to look at 
cluster_dat <- read.csv("~/Chinook_growth_repo/transformed_data/cluster_analysis_data.csv")

#### Check data for robustness to biases in ocean distirubtion 
#### caused by variation in distirubtion by age 

# filter for stock used in cluster analysis

# create variable for stock not including sex 
cluster_dat <- cluster_dat %>%
  mutate(run_origin = paste(release_location_rmis_basin,run,release_type))

marine_dsn_dat <- marine_dsn_dat%>%
  mutate(run_origin = paste(release_location_rmis_basin,run,release_type))

marine_dsn_dat_clusters <-  marine_dsn_dat %>%
  filter(run_origin %in% unique(cluster_dat$run_origin))
  
ODI_dat <- ODI_dat %>%
  filter(stock %in% unique(cluster_dat$stock))

# Plot histogram of updated 
ggplot(ODI_dat,
       aes(x = ODI))+
  geom_histogram()+
  theme_classic()



# compute age composition by stocks 
ggplot(marine_dsn_dat_clusters %>%
         filter(!(is.na(latitude)), ocean_age > 1)%>%
         group_by(run_origin, brood_year)%>%
         summarize(n = n()), 
       aes(x = brood_year, y = n, group = run_origin))+
  geom_point()+
  geom_line()+
  geom_smooth(method = "lm")+
  facet_wrap(~run_origin, scales = "free_y")+
  theme_classic()

ggsave("~/Chinook_growth_repo/figures/ODI_samples_by_year.png",
       height = 10,
       width = 10)

# plot 
ggplot(marine_dsn_dat_clusters %>%
         filter(!(is.na(latitude)), ocean_age > 1),
       aes(x = ocean_age))+
  geom_histogram()+
  facet_wrap(~run_origin, scales = "free_y")+
  theme_classic()
ggsave("~/Chinook_growth_repo/figures/ODI_samples_by_age.png",
       height = 10,
       width = 10)

## comapre pre brood year 1990 ODI to post BY 1990
ggplot(marine_dsn_dat_clusters %>% 
  dplyr::filter(!(is.na(latitude)), ocean_age > 1) %>%
  mutate(range = (recovery_year < 1994) & (recovery_year > 1979))%>%
  group_by(run_origin, range) %>%
  summarize(ODI = mean(latitude)) %>%
  reshape2::dcast(run_origin ~ range ),
  aes(x = `FALSE`, y = `TRUE`))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  xlab("base index")+
  ylab("1979 - 1994")

ggsave("~/Chinook_growth_repo/figures/full_ODI_and_1974_1994.png")

# compute correlation time period 
d_corr <- marine_dsn_dat_clusters %>% 
  dplyr::filter(!(is.na(latitude)), ocean_age > 1) %>%
  mutate(range = (recovery_year < 1994) & (recovery_year > 1979))%>%
  group_by(run_origin, range) %>%
  summarize(ODI = mean(latitude)) %>%
  reshape2::dcast(run_origin ~ range )
cor(d_corr$`TRUE`, d_corr$`FALSE`)

# compare currnet ODI metric to just age 4 
ggplot(marine_dsn_dat_clusters %>% 
          dplyr::filter(!(is.na(latitude)), ocean_age > 1) %>%
          group_by(run_origin, ocean_age == 3) %>%
          summarize(ODI = mean(latitude)) %>%
          reshape2::dcast(run_origin ~ `ocean_age == 3` ),
       aes(x = `FALSE`, y = `TRUE`))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()+
  xlab("base Index")+
  ylab("ocean age 3 only")
ggsave("~/Chinook_growth_repo/figures/full_ODI_and_age_4_only.png")

d_corr_age <- marine_dsn_dat_clusters %>% 
  dplyr::filter(!(is.na(latitude)), ocean_age > 1) %>%
  group_by(run_origin, ocean_age == 3) %>%
  summarize(ODI = mean(latitude)) %>%
  reshape2::dcast(n+run_origin ~ `ocean_age == 3` )
cor(d_corr_age$`TRUE`, d_corr_age$`FALSE`)
### compare current ODI metric to weighted ODI by estimated number 

# First note that estimation level (the area over which the 
# sample is supposed to be representative) varies by location
# observaiton further north are agregated to a higher level. 
ggplot(marine_dsn_dat_clusters%>%
         filter(estimated_number < 20),
       aes(x = as.factor(estimation_level), y= estimated_number))+
  geom_boxplot()+
  theme_classic()

ggplot(marine_dsn_dat_clusters,
       aes(y = latitude, x=as.factor(estimation_level)))+
  geom_boxplot()+
  theme_classic()


ODI_EN <- marine_dsn_dat_clusters %>% 
  dplyr::filter(!(is.na(latitude)), 
                ocean_age > 1) %>%
  group_by(run_origin) %>%
  summarize(ODI_EN = sum(latitude*estimated_number)/sum(estimated_number))


ODI <- marine_dsn_dat_clusters %>% 
  dplyr::filter(!(is.na(latitude)), 
                ocean_age > 1) %>%
  group_by(run_origin) %>%
  summarize(ODI = mean(latitude))

ODI_estimate_number <- merge(ODI,ODI_EN, by = c("run_origin"))

## correlation estimated number

cor(ODI_estimate_number$ODI,ODI_estimate_number$ODI_EN)
ggplot(ODI_estimate_number,
       aes(x = ODI, y = ODI_EN))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_classic()


  




