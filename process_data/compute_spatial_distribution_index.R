library(dplyr)
library(ggplot2)
library(rstan)
library(reshape2)
setwd("~/Chinook_growth_repo")


## usful function 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}



# set codes for non-terminal area fisheries in the RMIS data base
marine_fisheries <- c(10:19,
                      20,25,26,30,40,41,42,43,62,70,71,
                      72,74,79,80:89)

# laod data set 
dat = readRDS("data/joined_releases_recoveries_locations.rds")

# restrict to brood years, run timing and regions of interest. 
dat1 = dat %>% filter(release_location_rmis_region %in% c("LOCR","UPCR","CECR", "SNAK","NOOR",
                                                          "WILP","GRAY","NWC","SKAG","NPS"), 
                      brood_year > 1975, brood_year < 2015,
                      run %in% c(1,2,3,8))

# Restrict to pbservations in marine fisheries 
dat1 <- dat1 %>% filter(fishery %in% marine_fisheries)

# remove entries without recovery dates 
dat1 = dplyr::filter(dat1,
                     !is.na(recovery_date), 
                     !is.na(first_release_date), 
                     nchar(recovery_date)==8, 
                     nchar(first_release_date)==8)

# Organize recovery and release dates 
dat1$recovery_date = lubridate::parse_date_time(paste(substr(dat1$recovery_date,1,4),
                                                      substr(dat1$recovery_date,5,6),
                                                      substr(dat1$recovery_date,7,8)), 
                                                orders = "ymd")
dat1$release_date = lubridate::parse_date_time(paste(substr(dat1$first_release_date,1,4),
                                                     substr(dat1$first_release_date,5,6), 
                                                     substr(dat1$first_release_date,7,8)), 
                                               orders = "ymd")
dat1$recovery_year = lubridate::year(dat1$recovery_date)
dat1$recovery_day = lubridate::yday(dat1$recovery_date)
dat1$release_year = lubridate::year(dat1$release_date)
dat1$release_day = lubridate::yday(dat1$release_date)


# Compute age from the recovery dates and brood years
dat1$age = dat1$recovery_year - dat1$brood_year
dat1$age_days = lubridate::time_length(lubridate::interval(dat1$release_date, dat1$recovery_date), unit = "day" )
dat1$release_age = dat1$release_year- dat1$brood_year
dat1$ocean_age = dat1$recovery_year - dat1$release_year
  
  
# filter out unreasanoble ages
dat1 <- dat1 %>% filter(age < 7, age > 0)

# define function to seperat stocks into one of three 
# release strategies 
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

for( i in 1:nrow(dat1)){
  release_type[i] <- define_release_type(dat1$release_age[i], dat1$release_day[i])
}
dat1$release_type <- release_type


# group observations into stocks to match 
# growth models 
dat1 <- dat1 %>% 
  group_by(release_location_rmis_basin, 
           sex,
           run,
           release_type
  ) %>%
  mutate(stock = paste(release_type, run, sex, release_location_rmis_basin),
         stock_ = paste(release_type, run, release_location_rmis_basin))


# load locaiton informaiton for each fishery 
locaitons <- read.csv("process_data/gis_file.csv")

fishery <- c(10, 11, 12, 15, 16, 17, 
             18, 20, 21, 22, 23, 25,
             26,28,40,41,42,43, 800,
             85,80, 82, 812,81,802)

fishery_name <- c(
  "Ocean Troll (non-treaty)",
  "Ocean Troll- Day Boat",
  "Ocean Troll- Trip",
  "Treaty Troll",
  "Terminal Troll",
  "Non-treaty/treaty Troll",
  "Aboriginal Troll",
  "Ocean Gillnet",
  "Columbia Gillnet",
  "Coastal Gillnet",
  "Mixed Net and Seine",
  "Commercial Seine",
  "Terminal Seine",
  "Other Net", 
  "Ocean Sport", 
  "Charter Sport",
  "Private Sport" ,
  "Jetty Sport",
  "Hake Trawl Shoreside (OR WA)",
  "Ocean Trawl By-catch",
  "Hake Trawl At Sea (CA OR WA)",
  "Groundfish Observer (Bering Sea)",
  "Rockfish Fishery (Gulf of AK)",
  "Groundfish Observer (Gulf AK)",
  "Rockfish Trawl (CA OR WA)" 
  
)

# collect location informaiton into a data frame 
fishery_fishery_names <- data.frame(fishery = fishery, fishery_name = fishery_name)

locations <- merge(locaitons,fishery_fishery_names, by = "fishery_name")

locations <- locations %>% 
  dplyr::group_by(fishery, rec_year) %>%
  dplyr::summarize(latitude = mean(latitude))


# itterate over observaitons
# all missing latitude values impute to the mean
# lititude for the fishery.
dat1$imputed_val <- rep(0,nrow(dat1))
n <- nrow(dat1)

for(i in 1:n){
  lat <- dat1$latitude[i]
  if(is.na(lat)){
    dat1$latitude[i] <-  as.numeric(locations[locations$rec_year == dat1$recovery_year[i] & locations$fishery == dat1$fishery[i], "latitude"])
    dat1$imputed_val[i] <- 1
  }
}



# remove stocks with fewer than 100 observations for ODI
dat1 <- dat1 %>% 
  group_by(release_location_rmis_basin,release_type,run) %>%
  mutate(n_obs = sum(ocean_age > 1))%>%
  filter(n_obs > 100)

## method used before 8/5/22 to group stock and calcaulte the ocena distirubton index
distribution_summary_old <- dat1 %>%
  dplyr::group_by(stock) %>%
  dplyr::filter(!(is.na(latitude)), sex %in% c("M","F")) %>%
  dplyr::summarize(
    p52.5 = sum(latitude <= 52.5)/n(),
    COG = mean(latitude),
    COG_check = sum(latitude*(1-imputed_val))/sum((1-imputed_val)),
    n = n(),
    run = Mode(run),
    sex = Mode(sex),
    release_type = Mode(release_type),
    release_location_rmis_region = Mode(release_location_rmis_region),
    release_location_rmis_basin = Mode(release_location_rmis_basin))%>%
  dplyr::filter(n > 10)


## new method used to calcualte ocean distributon index 
## calcualting ocean distribuiton by RMIS basin, run and release type. 
## not diferntiating by sex 

ODI <- dat1 %>%
  filter(!(is.na(latitude)), ocean_age > 1)%>%
  dplyr::group_by(release_location_rmis_basin,release_type,run) %>%
  dplyr::summarize(ODI = mean(latitude),
                   ODI_check = sum(latitude*(1-imputed_val))/sum((1-imputed_val)),
                   n_ODI = n())

# group stock characteristics differntiating by sex 
dat_stocks <- dat1 %>%
  dplyr::group_by(stock,release_location_rmis_basin,release_type,run) %>%
  dplyr::filter(!(is.na(latitude)), sex %in% c("M","F")) %>%
  dplyr::summarize(
    sex = Mode(sex),
    release_location_rmis_region = Mode(release_location_rmis_region))%>%
  dplyr::filter(n > 10)

## merge ocean distribution index and other stock characteristics 
distribution_summary <- merge(dat_stocks ,ODI, 
                              by = c("release_location_rmis_basin",
                                     "release_type",
                                     "run"))


ggplot(
  data.frame(Index = distribution_summary$ODI,
             Index_no_imp = distribution_summary$ODI_check),
  aes(x = Index, y = Index_no_imp))+
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle(paste("Correlation",
                     cor(distribution_summary$ODI,
                         distribution_summary$ODI_check)))+
  theme_classic()

ggsave("figures/ODI_imputation.png",
       height = 5,
       width = 5)

write.csv(distribution_summary , "transformed_data/stock_characteristics_data.csv")
write.csv(dat1 , "transformed_data/marine_observations.csv")







