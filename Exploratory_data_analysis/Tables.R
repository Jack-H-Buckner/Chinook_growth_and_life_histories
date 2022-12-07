library(dplyr)
library(flextable)
setwd("~/Chinook_growth_repo")



### stock table ###

data_path <- "~/Chinook_growth_repo/transformed_data/increment_model_data.csv"
data <- read.csv(data_path)
char <- read.csv("~/Chinook_growth_repo/transformed_data/stock_characteristics_data.csv")
clusters <- read.csv("~/Chinook_growth_repo/model_output/clusters/data_n3_age4_seed2.csv") %>%
  mutate(stock = stock_char)%>%
  group_by(stock)%>%
  summarise(cluster = median(cluster))


d <- data %>%filter(age==4)%>%
  dplyr::group_by(stock)%>%
  dplyr::mutate(max_bood_year = max(brood_year),
                min_brood_year = min(brood_year),
                n_year = length(unique(brood_year)))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(stock)%>%
  dplyr::mutate(n_year = max(n_year),
                N_year = max(max_bood_year-min_brood_year))%>%
  dplyr::filter(N_year > 25,
                n_year > 20) %>%
  dplyr::group_by(group, run, release_age, stock)%>%
  dplyr::summarize(n = n(),
                   max_bood_year = mean(max_bood_year),
                   min_brood_year = mean(min_brood_year))%>%
  dplyr::ungroup()%>%
  dplyr::select(group, stock,release_age,max_bood_year,min_brood_year)

d <- merge(d, char, by = "stock")%>%
  merge(clusters)

d <- d[order(d$group, d$run),names(d)]
d$release_age <- d$release_age.x
d <- d %>%dplyr::select(group,cluster,run,ODI,release_age,sex,
                        release_location_rmis_region,
                        release_location_rmis_basin,
                        min_brood_year,max_bood_year)

# # add release age
# release_age <- c()
# for(i in 1:nrow(d)){
#   if(d$release_type[i] == "two_YO_release"){
#     release_age<- append(release_age, 0)
#   }else{
#     release_age<- append(release_age, 1)
#   }
# }
d$`Release Age` <- d$release_age 
d$Number <- 1:nrow(d)
# rename 
d$`Run Num.` <- d$run
d$Run<- plyr::mapvalues(d$run, c(1,2,3,8), c("Spring", "Summer", "Fall", "Late Fall"))
d$Sex <- d$sex
d$Cluster <- d$cluster
d$`Min year` <- as.character(d$min_brood_year)
d$`Max year` <- as.character(d$max_bood_year)


d <- mutate(d, Name = paste0(release_location_rmis_basin, " ", Run, ".", 
                             release_age, " ", "(", sex, ")"))

d$`Release Region` <- plyr::mapvalues(d$release_location_rmis_region, 
                                      c("LOCR","UPCR","CECR", "SNAK","NOOR","WILP",
                                        "GRAY","NWC","SKAG","NPS"), 
                                      c("Lower Columbia River", "Upper Columbia river", 
                                        "Central Columbia river","Snake River","Nothern Oregon",
                                        "Willipa bay", "Gray's Harbor","Washington Coast", 
                                        "Skagit River", "North Puget Sound"))

d$`Release Basin (RMIS code)` <- d$release_location_rmis_basin
d$`Regression model group` <- d$group
d$`ODI (range)` <- d$ODI
ft <- flextable(d,
                col_keys = c("Name","Regression model group", "Cluster", "Run", "Release Age", 
                             "ODI (range)", "ODI",
                             "Sex", "Release Region",
                             "Release Basin (RMIS code)","Min year", "Max year")) %>%
  vline(j="Regression model group")%>%
  merge_v(j = c("Regression model group")) %>%
  hline(j = "Regression model group") %>%
  bg(i = ~`Regression model group` %% 2== 0 , bg = rgb(0.9,0.9,0.9), part = "body") %>%
  theme_booktabs()%>%
  compose(j = 6,
          value = as_paragraph(
            linerange(value = `ODI (range)`,
                      max = max(`ODI (range)`), height = .15)
          ),
          part = "body")
ft <- autofit(ft)

save_as_image(x = ft, path = "~/Chinook_growth_repo/figures/stock_table.png", zoom = 4)




### summary statistics - regions ###

dat_ind <- read.csv("transformed_data/increment_model_data_individual_observation.csv")
d <- dat_ind %>% 
  mutate(`RMIS code` = release_location_rmis_region.x)%>%
  group_by(`RMIS code`)%>%
  summarize(`N stocks`= length(unique(stock)),
            N = n())

d$`Release Location` <- plyr::mapvalues(d$`RMIS code`,
                                        c("LOCR","UPCR","CECR", "SNAK","NWC","NOOR","SKAG","NPS","WILP"), 
                                        c("Lower Columbia River", "Upper Columbia river", 
                                          "Central Columbia river", "Snake River",
                                          "Washington Coast", "Northern Oregon","Skagit River",
                                          "North Puget Sound", "Willapa Bay")) 
ft <-flextable(d, col_keys = c("Release Location", "RMIS code","N stocks","N")) %>% autofit()

save_as_image(x = ft, path = "~/Chinook_growth_repo/figures/regions_table.png")



### summary statistics -gear ###
dat_ind <- read.csv("transformed_data/increment_model_data_individual_observation.csv")
d <- dat_ind %>% 
  group_by(age, gear)%>%
  summarize(N = n())

ft <-flextable(d, col_keys = c("age", "gear","N")) %>% autofit()

save_as_image(x = ft, path = "~/Chinook_growth_repo/figures/gear_table.png")





dat_ind <- read.csv("transformed_data/increment_model_data_individual_observation.csv")
nrow(dat_ind)


