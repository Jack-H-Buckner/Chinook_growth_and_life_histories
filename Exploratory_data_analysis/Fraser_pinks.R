
library(dplyr)
library(ggplot2)
library(reshape2)

Fraser <- readxl::read_xlsx("~/Chinook_growth_repo/data/fraser_recruitment.xlsx")
Washington <- readxl::read_xlsx("~/Chinook_growth_repo/data/litz_2021_fig_3b_ver_2.xlsx")