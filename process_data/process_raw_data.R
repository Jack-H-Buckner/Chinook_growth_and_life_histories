# data for jack
library(dplyr)
library(lubridate)
options(mc.cores = parallel::detectCores())
setwd("~/chinook_growth_repo")

"change"

# read in raw data 
dat = readRDS("data/joined_releases_recoveries_locations.rds")

# regions used to filter stocks prior to 8/5/22
# c("CECR","FRTH","LOCR","NOOR","NWC","SKAG","SNAK","SOOR","UPCR")

# filter for study region and non missing values for dates 
dat = dplyr::filter(dat, !is.na(length), # sex != "", remove filter for missing sex obs 
                    release_location_rmis_region %in% c("LOCR","UPCR","CECR", "SNAK","NOOR","WILP","GRAY","NWC","SKAG","NPS"),
                    !is.na(recovery_date),
                    !is.na(first_release_date), 
                    nchar(recovery_date)==8,
                    nchar(first_release_date)==8)

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

# filter for primary age groups 
dat = dplyr::filter(dat, age %in% 2:5)

# filter very old brod years with limited data  
dat = dat %>%
  dplyr::filter(brood_year > 1970)

# group observations by release

define_release_type <- function(age,release_day){
  if(age == 2)
    return("two_YO_release" )
  if(age == 1 & release_day < 230)
    return("one_YO_summer")
  if(age == 1 & release_day > 230)
    return("one_YO_fall")
  else
    return("NA")
}

# add release type to data frame 
release_type <- rep(0,nrow(dat1))

for( i in 1:nrow(dat)){
  release_type[i] <- define_release_type(dat$release_age[i], dat$release_day[i])
}

dat$release_type <- release_type

dat = dat%>%dplyr::filter(release_type %in% c("two_YO_release","one_YO_summer","one_YO_fall"))

# save data
saveRDS(dat,file="data/final_dat.rds")






# limit fishery - gear combos to dominant ones
# ds = dplyr::filter(dat, gear!="", recovery_rmis_basin != "") %>% 
#   dplyr::group_by(fishery, recovery_rmis_basin, gear, age) %>% 
#   dplyr::summarize(n = n()) %>% 
#   dplyr::arrange(-n) %>%
#   dplyr::filter(n>25)#, n < 500) # restrict to combinations with at least 25 obs& n < 500

# ds$group = paste0(ds$fishery,ds$recovery_rmis_basin,ds$gear)
# print(length(unique(ds$group)))
# drop basins w/few groups
# dat = group_by(dat, release_location_rmis_basin) %>% 
#   dplyr::mutate(nbasin = n()) %>% 
#   dplyr::filter(nbasin >= 5000) %>% 
#   dplyr::select(-nbasin) %>% 
#   ungroup() %>% 
#   group_by(recovery_rmis_basin, gear, age) %>% 
#   dplyr::mutate(nbasin = n()) %>% 
#   dplyr::filter(nbasin >= 25)

#dat$group = paste0(dat$fishery,dat$recovery_rmis_basin,dat$gear)
#dat = dplyr::filter(dat, group%in%ds$group)

