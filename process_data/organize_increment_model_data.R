library(dplyr)
setwd("~/Chinook_growth_repo")

# load data 
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
dat = readRDS("data/final_dat.rds")

# restrict to study regions
# restrict to range with compelte brood years 
# restrict to terminal area fisheries 
# remove values with missing data for sex
# restrict to primary run types 
# restrict to primary age grops 
dat1 = dat %>% filter(release_location_rmis_region %in% c("LOCR","UPCR","CECR", "SNAK","NOOR","WILP","GRAY","NWC","SKAG","NPS"), 
                      brood_year > 1975, brood_year < 2015,
                      fishery %in% c(21,24,45,46,50:59),
                      length_code == 0, length > 110, # remove small outliers
                      sex %in% c("M","F"), 
                      run %in% c(1,2,3,8),
                      age > 2)


# compute average length at release for each rlease type 
release_lengths <- dat1%>% 
  filter(sex %in% c("M","F"),!(is.na(run)),!(is.na(release_type))  )%>%
  group_by(release_type, run, sex)%>%
  dplyr::summarize(release_length = sum(avg_length,na.rm=TRUE)/sum(!(is.na(avg_length))))


# add to data set 
dat1 <- merge(dat1, release_lengths, by = c("release_type", "run", "sex"))



# filter out stock-year-age-fishery combinations with fewer than 10 observations
min_obs <- 10
dat1 = dat1 %>%
  dplyr::group_by(sex,run,release_location_rmis_basin, release_type, age, brood_year, fishery) %>% 
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n > min_obs)


# filter out stocks with out at least 15 years of observations for one age class
min_yrs <- 15
dat1 = dat1 %>%
  dplyr::group_by(sex,run,release_location_rmis_basin, 
                  release_location_rmis_region, release_type,
                  age) %>% 
  dplyr::mutate(nyears = length(unique(brood_year))) %>%
  dplyr::ungroup()%>%
  dplyr::group_by(sex,run,release_location_rmis_basin, 
                  release_location_rmis_region, release_type) %>% 
  dplyr::mutate(nyears_max = max(nyears))%>%
  dplyr::filter(nyears_max > min_yrs)


dat1 <- dat1%>%
  dplyr::mutate(stock = paste(release_type, run,  sex,  release_location_rmis_basin),
                fishery_gear = paste(fishery, ";", gear))





### add ocean distribution data ####

# merge with means
d <- merge(dat1, stock_characteristics, by = 
             c("stock", "release_type", "run", "sex", "release_location_rmis_basin","release_age"))


# group stock by ocean distribution and release age
ODI_threshold <- 50.0 # original was 47.5, but removal of ocean age one increased ODI values 
# I used ODI = 50 as the cuttoff based on visual inspection of the cluster plots
dat1 <- d %>% mutate(marine_dsn = (ODI < ODI_threshold)) %>%  
  mutate(group = paste(release_age, ";", marine_dsn))
dat1$group <- as.numeric(as.factor(dat1$group))


# create data frame with groupings for each stock 
group_labs <- dat1 %>% mutate(marine_dsn = (ODI < ODI_threshold)) %>%
  mutate(group = paste(release_age, ";", marine_dsn))
group_labs$group_num <- as.numeric(as.factor(group_labs$group))
group_labs <- group_labs%>%dplyr::group_by(group_num, group)%>%dplyr::summarize(n =n())




### Define alternative groupins based on run timing
alt_grouping <- function(run,dsn){
  if(run %in% c(1,2)){
    return(1)
  }else if(dsn < ODI_threshold){
    return(2)
  }else{
    return(3)
  }
}

dat1 <- dat1 %>% 
  dplyr::mutate(alt_group = 1 + 1*(run %in% c(1,2)) + 2*((run %in% c(3,4) ) & (marine_dsn)))
alt_group_labs <- data.frame(val = c(1,2,3), nms = c("fall north", "spring", "fall south"))



# computre mean and variance of length measurements for each stock, age, year, fishery combination 
dat_means <- dat1 %>%
  dplyr::group_by(stock,brood_year, age, fishery,group,alt_group,
                  release_type, run,release_location_rmis_basin)%>% # added this line 8/5/22
  dplyr::mutate(mu = mean(length))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(stock,age,fishery,release_type, run,release_location_rmis_basin)%>%
  dplyr::mutate(sigma = sum((length - mu)^2)/(n()-1), n = n())%>%
  dplyr::ungroup()%>%
  dplyr::group_by(stock,brood_year,age,fishery,run,group,alt_group,
                  release_type,release_location_rmis_basin)%>% # added this line 8/5/22
  dplyr::summarize(length = mean(mu),
                   sigma = sqrt(mean(sigma)/n()),
                   release_length = mean(release_length),
                   y_n = n(),
                   release_age = median(release_age))


# calacuate groupng for each stock 
dat_groups <- dat_means%>%
  dplyr::group_by(stock)%>%
  dplyr::summarize(run = median(group))

dat_alt_groups <- dat_means%>%
  dplyr::group_by(stock)%>%
  dplyr::summarize(run = median(alt_group))

dat_nums <- dat_means %>%
  dplyr::group_by(stock,age,fishery,brood_year)%>%
  dplyr::summarize(y_n = mean(y_n))


#dat1$gear_char <- paste(dat1$gear, "a")
write.csv(dat1,"~/Chinook_growth_repo/transformed_data/increment_model_data_individual_observation.csv" )
write.csv(dat_means, "~/Chinook_growth_repo/transformed_data/increment_model_data.csv")
write.csv(group_labs, "~/Chinook_growth_repo/transformed_data/increment_model_group_labels.csv")
write.csv(alt_group_labs, "~/Chinook_growth_repo/transformed_data/increment_model_alt_group_labels.csv")
write.csv(dat_groups, "~/Chinook_growth_repo/transformed_data/increment_model_groups.csv")
write.csv(dat_nums, "~/Chinook_growth_repo/transformed_data/increment_model_num_obs.csv")



dat_means