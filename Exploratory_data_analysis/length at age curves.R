library(dplyr)
library(ggplot2)
library(rstan)
library(reshape2)
setwd("~/Chinook_growth_repo")



# set codes for non-terminal area fisheries in the RMIS data base
marine_fisheries <- c(10:19,
                      20,25,26,30,40,41,42,43,62,70,71,
                      72,74,79,80:89)

# laod data set 
dat = readRDS("data/joined_releases_recoveries_locations.rds")

# restrict to brood years, run timing and regions of interest. 
dat = dat %>% filter(release_location_rmis_region %in% c("LOCR","UPCR","CECR", "SNAK","NOOR",
                                                          "WILP","GRAY","NWC","SKAG","NPS"), 
                      brood_year > 1975, brood_year < 2015,
                      run %in% c(1,2,3,8))

# convert data values 
dat$recovery_date = lubridate::parse_date_time(paste(substr(dat$recovery_date,1,4), substr(dat$recovery_date,5,6), substr(dat$recovery_date,7,8)), orders = "ymd")
dat$release_date = lubridate::parse_date_time(paste(substr(dat$first_release_date,1,4), substr(dat$first_release_date,5,6), substr(dat$first_release_date,7,8)), orders = "ymd")

# convert dates to years and days
dat$recovery_year = lubridate::year(dat$recovery_date)
dat$recovery_day = lubridate::yday(dat$recovery_date)
dat$release_year = lubridate::year(dat$release_date)
dat$release_day = lubridate::yday(dat$release_date)

# Compute age starting in the brood year
dat$age = dat$recovery_year - dat$brood_year
dat$age_days = lubridate::time_length(lubridate::interval(dat$release_date, dat$recovery_date), unit = "day" )
dat$release_age = dat$release_year- dat$brood_year

# Restrict to observations in marine fisheries 
dat1 <- dat %>% filter(fishery %in% marine_fisheries)



ggplot(dat1 %>% 
         filter(fishery %in% 20:90,
                brood_year %in% 1980:1990 ,
                age %in% c(1,2,3,4,5),
                !(is.na(length)),
                run == 3)%>%
         group_by(age)%>%
         summarize(length_median = quantile(length,0.5),
                   length_upper = quantile(length,0.75),
                   length_lower = quantile(length,0.25)),
       aes(x = age, 
           y = length_median,
           ymin = length_lower,
           ymax = length_upper))+
  geom_point()+
  geom_line()+
  geom_errorbar()+
  theme_classic()+
  xlab("Age")+ylab("Length")+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))

ggsave("figures/length_at_age_marine.png")



ggplot(dat1 %>% 
         filter(fishery %in% 20:90,
                brood_year %in% 1980:1990 ,
                age %in% c(1,2,3,4,5),
                !(is.na(length)),
                run == 3),
       aes(x = age_days, 
           y = length))+
  geom_point()+
  geom_smooth()+
  theme_classic()+
  xlab("Age")+ylab("Length")+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 18))+
  geom_vline(aes(xintercept = 365))+
  geom_vline(aes(xintercept = 2*365))+
  geom_vline(aes(xintercept = 3*365))+
  geom_vline(aes(xintercept = 4*365))


lm(data = dat1 %>% 
  filter(fishery %in% 20:90,
         brood_year %in% 1980:1990 ,
         age %in% c(1,2,3,4,5),
         !(is.na(length)),
         (round(age_days/365)+1) > 1,
         length > 250,
         run == 3),
  formula = length~(as.factor(round(age_days/365)+1))*age_days)



ggplot(dat1 %>% 
         filter(fishery %in% 20:90,
                brood_year %in% 1980:1990 ,
                age %in% c(2,3,4,5),
                !(is.na(length)),
                  length > 250,
                run == 3,
                (round(age_days/365)+1) > 1),
       aes(x = age_days,y=length, color = as.factor(round(age_days/365)+1 )))+
  geom_point(size = 0.5, alpha = 0.25)+
  geom_smooth(method = "lm")+
  theme_classic()+
  xlab("Age (days since release)")+
  ylab("Length")+
  scale_color_discrete(name = "Age (years)")
ggsave("~/Chinook_growth_repo/figures/growth_rates_at_age.png")







d <-dat1 %>% 
  filter(fishery %in% 20:90,
         brood_year %in% 1980:1990 ,
         age %in% c(2,3,4,5),
         !(is.na(length)),
         length > 250,
         run == 3, release_age == 1) 


mean_release_length = mean(d$avg_length, na.rm = T)
d_early_age2 = d %>% filter(age_days < 365)
mean_early_age2 = mean(d_early_age2$length, na.rm = T)


mean_early_age2 - mean_release_length

 
lm(data = d %>% filter(round(age_days/365) == 1),
    formula = length ~age_days)
 

lm(data = d %>% filter(round(age_days/365) == 2),
   formula = length ~age_days)


mean_early_age2 - mean_release_length
0.8267*365/(mean_early_age2 - mean_release_length)
0.7099*365/(mean_early_age2 - mean_release_length)

0.7099*365 / (0.8267*365)
