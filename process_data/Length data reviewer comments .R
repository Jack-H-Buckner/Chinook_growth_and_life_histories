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
  filter(fishery %in% c("Hatchery","Fish trap","Spawning grounds"))%>%
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


## plots for escapemt fisheries only 
dat_escapement_sampling <- dat %>%
  filter(fishery %in% c(50:59))

## plot differnces in size between fisheries 
dat_escapement_sampling$sampling_method <- plyr::mapvalues(dat_escapement_sampling $fishery,
                                            c(50:59),
                                            c("Hatchery","Other","Fish trap","Other","Spawning grounds",
                                              "Other","Other","Other","Other","Other"))

dat_escapement_sampling$sampling_method <- ordered(dat_escapement_sampling$sampling_method,
                                                 levels = c("Other", "Fish trap", "Hatchery", 
                                                            "Spawning grounds"))

dat_prop_sampling_over_time <- dat_escapement_sampling %>%
  group_by(brood_year, age)%>%
  mutate(n = sum(estimated_number))%>%
  group_by(sampling_method,brood_year, age) %>%
  summarize(p = sum(estimated_number)/mean(n))


p<-ggplot(dat_prop_sampling_over_time,
       aes(x = brood_year, y = p, 
           group = sampling_method, fill = sampling_method))+
  geom_bar(stat = "identity")+
  theme_classic()+
  scale_fill_manual(values = PNWColors::pnw_palette("Bay",n=4))+
  xlab("Brood year")+
  ylab("Fraction of samples")+
  facet_wrap(~age, ncol = 2)+
  theme(axis.text.x = element_text(size = 12, angle = 20, vjust = 0.9, hjust = 0.9),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))
p
ggsave(file = "~/Chinook_growth_repo/figures/trends_in_samples.png",
       p, height = 6, width = 9)

p<-ggplot(dat_escapement_sampling ,
       aes(x = sampling_method, y = length, fill = sampling_method))+
  geom_boxplot(outlier.size = 0.5)+
  scale_fill_manual(values = PNWColors::pnw_palette("Bay",n=4))+
  theme_classic()+
  xlab("Sapling method")+
  facet_wrap(~age, ncol = 2, scales = "free_y")+
  theme(axis.text.x = element_text(size = 12, angle = 20, vjust = 0.9, hjust = 0.9),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))
p
ggsave(file = "~/Chinook_growth_repo/figures/size_at_age_by_sampling.png",
       p, height = 6, width = 9)


ggplot(dat_escapement_sampling %>%
            group_by(age, brood_year, release_age, sampling_method)%>%
            summarize(length = mean(length)),
          aes(x = brood_year, y = length, color = sampling_method))+
  geom_point()+
  geom_line()+
  geom_smooth()+
  scale_color_manual(values = PNWColors::pnw_palette("Bay",n=4))+
  theme_classic()+
  xlab("Sapling method")+
  facet_grid(release_age~age, scales = "free")+
  theme(axis.text.x = element_text(size = 12, angle = 20, vjust = 0.9, hjust = 0.9),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16))

# make table

dat_escapement_sampling  %>%
  group_by(sampling_method,age)%>%
  summarize(n = n())%>%
  reshape2::dcast(sampling_method~age)
  

### focus on escapes only 

# plot total catch in each fishery over time
dat_prop_fishery_over_time <- dat_main_fishery %>%
  filter()%>%
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

ggplot(dat_prop_fishery_over_time,
       aes(x = brood_year, y = p, 
           group = fishery, fill = fishery))+
  geom_bar(stat = "identity")+
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



