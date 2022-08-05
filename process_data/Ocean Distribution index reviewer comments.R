


### Check the effects of keeping observations with missing vlaeus for sex
### added 8/5/22

# computre val old way removing missing values of sex
ODI_filter <- dat_marine1 %>%
  dplyr::filter(sex %in% c("M","F"),!(is.na(latitude))) %>%
  dplyr::group_by(release_location_rmis_basin,release_type,run) %>%
  dplyr::summarize(COG_filter = mean(latitude),
                   n_filter = n())

# compute value new way using these values 
ODI_ <- dat_marine1 %>%
  dplyr::filter(!(is.na(latitude))) %>%
  dplyr::group_by(release_location_rmis_basin,release_type,run) %>%
  dplyr::summarize(COG = mean(latitude),
                   n = n())

compare_dat <- merge(ODI_,ODI_filter, 
                     by = c("release_location_rmis_basin",
                            "release_type",
                            "run"))

ggplot(compare_dat,
       aes(x=as.numeric(COG_filter),y=as.numeric(COG)))+
  geom_point()+
  geom_smooth(method="lm")



#### Check data for robustness to biases in ocean distirubtion 
#### caused by variation in distirubtion by age 

# This check introduces some circularity in the code
# - The growth increment data file needs the ocena distribution index
# - 

# filter for stock used in cluster analysis

cluster_data <- read.csv("transformed_data/cluster_analysis_data.csv")

dat_marine_filtered <- dat_marine1 %>%
  filter(stock %in% unique(cluster_data$stock))

# compute age composition by stocks 

# compare age compositions 
