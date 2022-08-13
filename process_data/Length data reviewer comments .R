library(ggplot2)
library(dplyr)
# Load data 
dat <- read.csv("~/Chinook_growth_repo/transformed_data/increment_model_data_individual_observation.csv")

# Length by Fishery 
dat %>%
  group_by(fishery)%>%
  summarize(n = n())
# Length by Fishery 

dat_main_fishery <- dat %>%
  filter(fishery %in% c(21,24,50,52,54))

## plot differnces in size between fisheries 
dat_main_fishery$fishery <- plyr::mapvalues(dat_main_fishery$fishery,
                                            c(21,24,50,52,54),
                                            c("Columbia R. gill net","Fresh Water Net",
                                              "Hatchery","Fish trap","Spawning grounds"))
ggplot(dat_main_fishery,
       aes(x = fishery, y = length))+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(~age, ncol = 2)


# plot total observations in each fishery over time
dat_prop_fishery_over_time <- dat_main_fishery %>%
  group_by(brood_year, age)%>%
  mutate(n = n())%>%
  group_by(fishery,brood_year, age) %>%
  summarize(p = n()/mean(n))


ggplot(dat_prop_fishery_over_time,
       aes(x = brood_year, y = p, 
           group = fishery, fill = fishery))+
  geom_area()+
  theme_classic()+
  facet_wrap(~age, ncol = 2)


# plot total catch in each fishery over time
dat_prop_fishery_over_time <- dat_main_fishery %>%
  group_by(brood_year, age)%>%
  mutate(n = sum(estimated_number))%>%
  group_by(fishery,brood_year, age) %>%
  summarize(p = sum(estimated_number)/mean(n))


ggplot(dat_prop_fishery_over_time,
       aes(x = brood_year, y = p, 
           group = fishery, fill = fishery))+
  geom_area()+
  theme_classic()+
  facet_wrap(~age, ncol = 2)
       


# plot diffences in average size between fisheries in each year
#dat_diff_size <-
dat_diff_size <- dat_main_fishery %>%
  group_by(fishery, brood_year, age)%>%
  summarize(length = mean(length))%>%
  ungroup()%>%
  reshape2::dcast( brood_year+age~fishery, value.var = "length")%>%
  mutate(gill_hatchery = Hatchery - `Columbia R. gill net`,
         spawn_hatchery= Hatchery - `Spawning grounds`)%>%
  select(brood_year, age,gill_hatchery ,spawn_hatchery)%>%
  reshape2::melt(id.vars = c("brood_year", "age"))

ggplot(dat_diff_size,
       aes(x = variable, y = value, fill = brood_year > 1995))+
  geom_boxplot()+
  facet_wrap(~age, ncol = 2)+
  theme_classic()



