library(dplyr)
setwd("~/Chinook_growth_repo")

# load data 
stock_characteristics <- read.csv("transformed_data/stock_characteristics_data.csv")
dat = readRDS("data/final_dat.rds")


dat1 = dat %>% filter(release_location_rmis_region %in% c("LOCR","UPCR","CECR", "SNAK","NOOR",
                                                          "WILP","GRAY","NWC","SKAG","NPS"), 
                      brood_year > 1975, brood_year < 2015,
                      fishery %in% c(21,24,45,46,50:59),
                      sex %in% c("M","F"), 
                      run %in% c(1,2,3,8),
                      age > 2)





release_lengths <- dat1%>% 
  filter(sex %in% c("M","F"),!(is.na(run)),!(is.na(release_type))  )%>%
  group_by(release_type, run, sex)%>%
  dplyr::summarize(release_length = sum(avg_length,na.rm=TRUE)/sum(!(is.na(avg_length))))


dat1 <- merge(dat1, release_lengths, by = c("release_type", "run", "sex"))




# filter outsotck year combinations with fewer than 2 observations
min_obs <- 2
dat1 = dat1 %>%
  dplyr::group_by(sex,run,release_location_rmis_basin, release_type, age, brood_year, fishery) %>% 
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n > min_obs)

# fileter out stocks with fewer than 15 years of observations for at least one age 
min_yrs <- 15
dat1 = dat1 %>%
  dplyr::group_by(sex,run,release_location_rmis_basin, release_location_rmis_region, release_type, age) %>% 
  dplyr::mutate(nyears = length(unique(brood_year))) %>%
  dplyr::filter(nyears > min_yrs)


# filter out stocks with fewer than two ages with sufficent observations
min_ages <- 1
dat1 = dat1 %>%
  dplyr::group_by(sex,run,release_location_rmis_basin, release_location_rmis_region, release_type) %>% 
  dplyr::mutate(nages = length(unique(age))) %>%
  dplyr::filter(nages > min_ages)


# filter out fisheirs with too few observaitons 
min_obs <- 50
dat1 = dat1 %>% 
  dplyr::mutate(fishery_gear = paste0(fishery,":",gear)) %>% 
  dplyr::group_by(fishery_gear) %>% 
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n > min_obs)%>%
  dplyr::ungroup()


dat1 <- dat1%>%
  dplyr::mutate(stock = paste(release_type, run,  sex,  release_location_rmis_basin),
                fishery_gear = paste(fishery, ";", gear))





### add ocean distribution data ####



# merge with means
d <- merge(dat1, stock_characteristics, by = 
                c("stock", "release_type", "run", "sex", "release_location_rmis_basin"))


# add group labels to means
dat1 <- d %>% mutate(marine_dsn = (COG < 47.5)) %>%
  mutate(group = paste(release_age, ";", marine_dsn))



# convert to ints
dat1$group <- as.numeric(as.factor(dat1$group))

# save group assignemts 
group_labs <- dat1 %>% mutate(marine_dsn = (COG < 47.5)) %>%
  mutate(group = paste(release_age, ";", marine_dsn))



group_labs$group_num <- as.numeric(as.factor(group_labs$group))


group_labs <- group_labs%>%dplyr::group_by(group_num, group)%>%dplyr::summarize(n =n())




### alternative groupings 
alt_grouping <- function(run,dsn){
  if(run %in% c(1,2)){
    return(1)
  }else if(dsn < 47.5){
    return(2)
  }else{
    return(3)
  }
}

dat1 <- dat1 %>% 
  dplyr::mutate(alt_group = 1 + 1*(run %in% c(1,2)) + 2*((run %in% c(3,4) ) & (marine_dsn)))


alt_group_labs <- data.frame(val = c(1,2,3), nms = c("fall north", "spring", "fall south"))


dat_means <- dat1 %>%
  dplyr::group_by(stock,brood_year, age, fishery,group,alt_group)%>%
  dplyr::mutate(mu = mean(length))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(stock,age,fishery)%>%
  dplyr::mutate(sigma = sum((length - mu)^2)/(n()-1), n = n())%>%
  dplyr::ungroup()%>%
  dplyr::group_by(stock,run,group,alt_group,age,fishery,brood_year)%>%
  dplyr::summarize(length = mean(mu),
                   sigma = sqrt(mean(sigma)/n()),
                   release_length = mean(release_length),
                   y_n = n(),
                   release_age = median(release_age))



data_means <- dat_means



dat_groups <- dat_means%>%
  dplyr::group_by(stock)%>%
  dplyr::summarize(run = median(group))

dat_alt_groups <- dat_means%>%
  dplyr::group_by(stock)%>%
  dplyr::summarize(run = median(alt_group))


dat_nums <- dat_means %>%
  dplyr::group_by(stock,age,fishery,brood_year)%>%
  dplyr::summarize(y_n = mean(y_n))


dat1$gear_char <- paste(dat1$gear, "a")
write.csv(dat1,"transformed_data/increment_model_data_individual_observation.csv" )
write.csv(dat_means, "transformed_data/increment_model_data.csv")
write.csv(group_labs, "transformed_data/increment_model_group_labels.csv")
write.csv(alt_group_labs, "transformed_data/increment_model_alt_group_labels.csv")
write.csv(dat_groups, "transformed_data/increment_model_groups.csv")
write.csv(dat_nums, "transformed_data/increment_model_num_obs.csv")

