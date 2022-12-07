library(dplyr)
library(rstan)
library(lubridate)
library(reshape2)
library(zoo)

header.true.row.true <- function(df) {
  df$index <- rownames(df)
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


setwd("~/Chinook_growth_repo")


###########################
###                     ###
###     Predictors      ###
###                     ###
###########################



# Pink Salmon Gulf of Alaska 


#Pinks <- read.csv("data/Pink_salmon.csv")
pinks_dat <- readxl::read_xlsx("~/Chinook_growth_repo/data/pink_data.xlsx")

# # organize pinks data 
# Pinks <- Pinks%>%
#   dplyr::mutate(Pink = as.numeric(Pink))%>%
#   dplyr::filter(!(is.na(Pink)))
# Pinks <- melt(Pinks, id.var = "Year")
# Pinks <- Pinks%>%dplyr::group_by(variable)%>%dplyr::mutate(value = value/max(value))
# Pinks <- Pinks %>% filter(variable == "Pink")

Pinks <- data.frame(year = pinks_dat$Year, Pinks = pinks_dat$northamerica_pink_catchescape)
Pinks$Pinks <- scale(Pinks$Pinks)





print("alaska pinks")



# Pink Salmon Washington state 


WA_Pinks<- readxl::read_excel("data/litz_2021_fig_3b_ver_2.xlsx") %>%
  dplyr::select(`Return year`,Total )%>%
  dplyr::mutate(`Return year` = round(as.numeric(`Return year`)),
                Total = as.numeric(Total))

names(WA_Pinks) <- c("year", "WA_Pinks")

# ipute 2015 to mean of odd years 
WA_Pinks$WA_Pinks[WA_Pinks$year == 2015] <- mean(WA_Pinks$WA_Pinks[WA_Pinks$year != 2015])

WA_Pinks <- rbind(WA_Pinks,data.frame(year = seq(1960,2017,2), WA_Pinks = rep(0,length(seq(1960,2016,2)))))
WA_Pinks$WA_Pinks <- (WA_Pinks$WA_Pinks - mean(WA_Pinks$WA_Pinks))/sd(WA_Pinks$WA_Pinks)




# Aleutian low pressure index

ALPI <- read.csv("data/ALPI.csv")
names(ALPI) <- c("year","ALPI")
ALPI$ALPI <- scale(ALPI$ALPI)










# Pacific Decadal Oscilation

PDO <- read.csv("data/PDO.csv")


PDO <- PDO%>%
  header.true.row.true()

names(PDO) <- c("Value","Date")


PDO <- PDO%>%
  dplyr::mutate(year = as.numeric(substr(Date,1,4)),
                month = as.numeric(substr(Date,5,6)))%>%
  dplyr::group_by(year)%>%
  dplyr::summarize(value = sum(as.numeric(Value))/n())

names(PDO) <- c("year", "PDO")


PDO$PDO <- scale(PDO$PDO)









# North Pacific Gyre Oscilations 

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


NPGO <- read.csv("data/NPGO.csv")

NPGO <- header.true(NPGO )

NPGO <- NPGO[c(2,3,4)]
names(NPGO) <- c("year", "month", "NPGO")


NPGO_data <- NPGO %>% dplyr::mutate(year = as.numeric(year),
                                    month = as.numeric(month),
                                    NPGO = as.numeric(NPGO))%>%
  
  dplyr::group_by(
    year
  )%>%
  dplyr::summarize(NPGO = mean(NPGO))%>%
  dplyr::ungroup()%>%
  dplyr::mutate(NPGO_5yr_rolling = zoo::rollapply(data = NPGO, 
                                                  width = 5, 
                                                  FUN = mean, 
                                                  align = "right", 
                                                  fill = NA, 
                                                  na.rm = T))






print("finished NPGO")


# North East Pacific Rim Sea Surface Tempreature 

# winter

SSTarc.winter <- read.csv("data/ersstArc.win.csv")

SSTarc.winter <- SSTarc.winter%>%dplyr::mutate(
  rolling_5yr = rollapply(data = ersstArc.win, 
                          width = 5, 
                          FUN = mean, 
                          align = "right", 
                          fill = NA, 
                          na.rm = T))
names(SSTarc.winter) <- c("year", "SSTarc_winter", "SSTarc_winter_5yr_rolling") 


# summer 

SSTarc.summer <- read.csv("data/ersstArc.sum.csv")

SSTarc.summer <- SSTarc.summer%>%dplyr::mutate(
  rolling_5yr = rollapply(data = ersstArc.sum, 
                          width = 5, 
                          FUN = mean, 
                          align = "right", 
                          fill = NA, 
                          na.rm = T))
names(SSTarc.summer) <- c("year", "SSTarc_summer", "SSTarc_summer_5yr_rolling") 


SSTarc.summer$SSTarc_summer <- scale(SSTarc.summer$SSTarc_summer)


### Columbia River Disharge Annual Averages ###


### Multivariate El-Nino index ###
MEI <- read.csv("data/meiv2.csv")

MEI <- melt(MEI, id.var = "Year") 

seasons <- c()
for(i in 1:nrow(MEI)){
  if(MEI$variable[i] %in% c("X1", "X2", "X3")){
    seasons <- append(seasons, "winter")
  }
  if(MEI$variable[i] %in% c("X4", "X5", "X6")){
    seasons <- append(seasons, "spring")
  }
  if(MEI$variable[i] %in% c("X7", "X8", "X9")){
    seasons <- append(seasons, "summer")
  }
  if(MEI$variable[i] %in% c("X10", "X11", "X12")){
    seasons <- append(seasons, "fall")
  }
  
}

MEI$season <- seasons 

# spring
MEI_spring <-  MEI %>% 
  dplyr::group_by(season, Year) %>%
  dplyr::summarize(m = mean(value)) %>%
  dplyr::filter(season == "spring")

MEI_spring <- MEI_spring[,2:3]
names(MEI_spring) <- c("year", "MEI_spring")




# summer
MEI_summer <-  MEI %>% 
  dplyr::group_by(season, Year) %>%
  dplyr::summarize(m = mean(value)) %>%
  dplyr::filter(season == "summer")


MEI_summer <- MEI_summer[,2:3]
names(MEI_summer) <- c("year", "MEI_summer")

# winter

MEI_winter <- MEI %>% 
  dplyr::group_by(season, Year) %>%
  dplyr::summarize(m = mean(value)) %>%
  dplyr::filter(season == "winter")


MEI_winter <- MEI_winter[,2:3]
names(MEI_winter) <- c("year", "MEI_winter")

### Bifrucation index ###

BI <- read.csv("data/BI.csv") %>% 
  header.true.row.true() %>% 
  dplyr::select(year, bifurcation_index)

names(BI) <- c("year", "BI")
BI$BI <- scale(as.numeric(BI$BI))
### total hatchery releases ###


# Arrange all covariates into a data frame 

df_list <-  list(SSTarc.summer, 
                 SSTarc.winter, 
                 PDO,
                 WA_Pinks,
                 ALPI,
                 Pinks,
                 NPGO_data,
                 MEI_spring,
                 MEI_summer,
                 MEI_winter,
                 BI
)


f <- function(d1, d2){ merge(d1, d2, by = "year")}
covriates <- Reduce(f, df_list)


X <- covriates %>% select(year,
                          SSTarc_summer,
                          SSTarc_winter,
                          PDO,
                          WA_Pinks,
                          ALPI,
                          Pinks,
                          NPGO,
                          MEI_summer,
                          MEI_winter,
                          BI)


write.csv(X,"~/Chinook_growth_repo/transformed_data/covariates.csv")


p <- ggplot(melt(X, id.var = "year"), aes(x = year, y = value))+
  geom_point()+geom_line()+facet_wrap(~variable, ncol = 1)+
  theme_test()+theme(text = element_text(size  = 20))
p
ggsave( "figures/covariates.png", p, width =10, height = 10)


