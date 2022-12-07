library(dplyr)
library(flextable)

setwd("~/Chinook_growth_repo")
#source("run_stan/simulation tests/useful_functions.R")
### time series 

#######################################
####                               ####
####      Run all covariates       ####
####                               ####
#######################################
data_path <- "transformed_data/increment_model_data.csv"
data <- read.csv(data_path)


stock_group <- 4

#original filtering code for cluster analysis 
data1 <- data %>%
  filter(age == 4)%>%  # get lengths at age 4
  group_by(stock) %>%  # filter for stocks with long enough time series 
  mutate(max_bood_year = max(brood_year),
         min_brood_year = min(brood_year),
         n_year = length(unique(brood_year)))%>%
  filter(max_bood_year-min_brood_year > 25,
         n_year > 20)



d <- data1 %>%dplyr::select(group,run,release_age,
                        stock)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

d <- data1 %>% group_by(stock)%>%
  summarize(group = Mode(group),
            run= Mode(run),
            release_age = Mode(release_age))

d <- d[order(d$group, d$run),names(d)]
d$Number <- 1:nrow(d)



ft <- flextable(d,
                col_keys = c("group", "run", 
                             "release_age","stock", "Number")) %>%
  vline(j="group")%>%
  merge_v(j = c("group")) %>%
  hline(j = "group") %>%
  bg(i = ~group %% 2 == 0 , bg = rgb(0.9,0.9,0.9), part = "body") %>%
  # color( i = ~cluster %in% c(1,2,3), ~cluster  , 
  #        color = function(x){return( cols[x])} ) %>%
  theme_booktabs()

ft <- autofit(ft)
ft
#ft <- tempfile(fileext = ".png")
save_as_image(x = ft, path = "~/Chinook_growth_repo/figures/stock_table.png")














# make table with included stocks, theri gorupings and key characteristics 


library(flextable)
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
d$`Min year` <- d$min_brood_year
d$`Max year` <- d$max_bood_year


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











# change to group by distribution and run timing, group all spring runs  
char <- read.csv("~/chinook_growth/transformed_data/stock_characteristics_data.csv")
clusters <- read.csv("~/chinook_growth/samples/clusters/data_n3_age4.csv") %>%
  mutate(stock = stock_char)%>%
  group_by(stock)%>%
  summarise(cluster = median(cluster))


d <- data %>%
  dplyr::group_by(stock,age)%>%
  dplyr::mutate(max_bood_year = max(brood_year),
                min_brood_year = min(brood_year),
                n_year = length(unique(brood_year)))%>%
  dplyr::ungroup()%>%
  dplyr::group_by(stock)%>%
  dplyr::mutate(n_year = max(n_year),
                N_year = max(max_bood_year-min_brood_year))%>%
  dplyr::filter(N_year > 25,
                n_year > 20) %>%
  dplyr::group_by(group, run, release_age, stock )%>%
  dplyr::summarize(n = n())%>%
  dplyr::ungroup()%>%
  dplyr::select(group, stock)
# dplyr::ungroup()%>%
# dplyr::mutate(RMIS_basin= substr(stock, nchar(stock)-4, nchar(stock)),
#               sex = substr(stock, nchar(stock)-6, nchar(stock)-5))

d <- merge(d, char, by = "stock")%>%
  merge(clusters)
d$`Ocean Dsn` <- d$COG


grouping <- function(run,dsn){
  if(run %in% c(1,2)){
    return(1)
  }else if(dsn < 47.5){
    return(2)
  }else{
    return(3)
  }
}
group <- c()

for( i in 1:nrow(d)){
  group <-append(group, grouping(d$run[i],d$`Ocean Dsn`[i]))
}

d$group <- group
d <- d[order(d$group, d$run),names(d)]
d <- d %>%dplyr::select(group,cluster,run,`Ocean Dsn`,release_type,sex,
                        release_location_rmis_region,
                        release_location_rmis_basin)
d$Number <- 1:nrow(d)


ft <- flextable(d,
                col_keys = c("group", "cluster","run", "Ocean Dsn", 
                             "sex", "release_location_rmis_region",
                             "release_location_rmis_basin")) %>%
  vline(j="group")%>%
  merge_v(j = c("group")) %>%
  hline(j = "group") %>%
  bg(i = ~group %% 2 == 0 , bg = rgb(0.9,0.9,0.9), part = "body") %>%
  color( i = ~cluster %in% c(1,2,3), ~cluster  , 
         color = function(x){return( cols[x])} ) %>%
  theme_booktabs()%>%
  compose(j = 4,
          value = as_paragraph(linerange(value = `Ocean Dsn`,
                                         max = max(`Ocean Dsn`), height = .15)),part = "body")
ft <- autofit(ft)
#ft <- tempfile(fileext = ".png")
save_as_image(x = ft, path = "~/chinook_growth/figures/stock_alt_groups_table.png")










# sex not included as goruping variable
ft <- d %>% 
  dplyr::ungroup()%>%
  dplyr::group_by(group,run,release_type,
                  release_location_rmis_region,
                  release_location_rmis_basin)%>%
  dplyr::summarize(`Ocean Dsn` = mean(`Ocean Dsn`)) %>%
  flextable(col_keys = c("group", "run", "Ocean Dsn", 
                         "release_type",
                         "release_location_rmis_region",
                         "release_location_rmis_basin")) %>%
  vline(j="group")%>%
  merge_v(j = c("group")) %>%
  hline(j = "group") %>%
  bg(i = ~group %% 2 == 0 , bg = rgb(0.9,0.9,0.9), part = "body") %>%
  theme_booktabs()%>%
  compose(j = 3,
          value = as_paragraph(
            linerange(value = `Ocean Dsn`,
                      max = max(`Ocean Dsn`), height = .15)))

save_as_image(x = ft, path = "~/chinook_growth/figures/stocks_table.png")







# plot contributions of RMIS regions 


dat_ind <- read.csv("transformed_data/increment_model_data_individual_observation.csv")
d <- dat_ind %>% 
  mutate(`RMIS code` = release_location_rmis_region.x)%>%
  group_by(`RMIS code`)%>%
  summarize(`N stocks`= length(unique(stock)),
            N = n())

d$`Release Location` <- plyr::mapvalues(d$`RMIS code`,
                                        c("LOCR","UPCR","CECR", "SNAK","NWC","NOOR","SKAG"), 
                                        c("Lower Columbia River", "Upper Columbia river", 
                                          "Central Columbia river", "Snake River",
                                          "Washington Coast", "Northern Oregon","Skagit River")) 
ft <-flextable(d, col_keys = c("Release Location", "RMIS code","N stocks","N")) %>% autofit()

save_as_image(x = ft, path = "~/chinook_growth/figures/regions_table.png")








gear_levels_num <- c(21,24,45,46,50,52, 
                     53,54,55,56,57,59)

gear_levels_char <-c("Gill Net", "Gill Net", "Sport","Sport", "Hatchery",
                     rep("Spawning Grounds", 7))

dat_ind$Gear <- plyr::mapvalues(dat_ind$fishery, gear_levels_num, gear_levels_char) 
d <- dat_ind %>% 
  group_by(Gear)%>%
  summarize(N = n())

  
 
ft <-flextable(d, col_keys = c("Gear","N")) %>% autofit()

save_as_image(x = ft, path = "~/chinook_growth/figures/Gear_table.png")




