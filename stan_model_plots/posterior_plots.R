library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")

process_samples <- function(path, group){
  samples1 <- read.csv(path)
  # order of var names
  
  samples_long <- melt(samples1)
  
  mean_effects <- samples_long %>% filter(substr(variable, 1,1) == "B")
  
  
  
  mean_effects$variable <- plyr::mapvalues(mean_effects$variable,
                                           c("B.1.","B.2.",
                                             "B.3.","B.4.",
                                             "B.5.","B.6.",
                                             "B.7."
                                             ),
                                           c("PDO", "WA_Pinks", "ALPI", "Pinks",        
                                             "NPGO", "MEI", "BI"))
  mean_effects$group = group
  return(mean_effects)
}


## plot main analysis 

path <- "model_output/HMC_samples/samples_1.csv"
group <- "Release age 1, Northern"
group1 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_2.csv"
group <- "Release age 1, Southern"
group2 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_3.csv"
group <- "Release age 2, Northern"
group3 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_4.csv"
group <- "Release age 2, Southern"
group4 <- process_samples(path,group)

group1$group <- "Release age 1, North"
group2$group <- "Release age 1, South"
group3$group <- "Release age 2, North"
group4$group <- "Release age 2, South"

mean_effects <- rbind(group1,group2,group3,group4)



sig_code <- function(p){
  if(p[1] < 0.001){
    return("***")
  }else if(p[1] < 0.01){
    return("**")
  }else if(p[1]<0.05){
    return("*")
  }
  return("")
}
p_vals <- mean_effects %>% 
  dplyr::group_by(variable, group) %>%
  dplyr::summarize(b = mean(value),
                   p1 = sum(value < 0)/n(),
                   p2 = sum(value > 0)/n())%>%
  dplyr::ungroup()%>%
  dplyr::group_by(variable, group) %>%
  dplyr::mutate(p = sig_code(min(p1,p2)))



p1 <- ggplot(mean_effects, aes(x = value, fill = variable))+
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  geom_text(data = p_vals, 
            aes(x = 0.05, y = 20, 
                label = p),
            color= "black")+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 16, family = "Times New Roman",
                                   angle = 30, hjust = 0.75),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        strip.text = element_text(size = 14, family = "Times New Roman"))

p1


ggsave(p1,file = "figures/main_regression_plot_larger_font.png",
       height = 8,
       width = 10)

# 
# table1 <- ggplot(mean_effects %>% 
#          dplyr::group_by(variable, group) %>%
#          dplyr::summarize(b = mean(value),
#                           p1 = sum(value < 0)/n(),
#                           p2 = sum(value > 0)/n())%>%
#          dplyr::ungroup()%>%
#          dplyr::group_by(variable, group) %>%
#          dplyr::mutate(p = min(p1,p2)),
#        aes(x = 0, y = 0.1, label = paste("b: ",round(b, 3)),
#            color = b > 0))+
#   scale_color_manual(
#     name = "process errors",
#     values = c("red","green"))+
#   geom_
#   geom_text()+
#   ggnewscale::new_scale("color")+
#   scale_color_manual(name = "p-val", values = c("red","black"))+
#   ylim(-0.3,0.3)+
#   geom_text(aes(y = -0.1, label = paste("p: ", round(p, 3)),
#                 color = p > 0.05))+
#   
#   facet_grid(variable~group)+
#   theme_void()
# 
# ggsave(table1,file = "figures/main_regression_table.png",
#        height = 8,
#        width = 10)

path <- "model_output/HMC_samples/samples_cohort_RE_1.csv"
group <- "Release age 1, North"
group1_RE <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_cohort_RE_2.csv"
group <- "Release age 1, South"
group2_RE <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_cohort_RE_3.csv"
group <- "Release age 2, North"
group3_RE <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_cohort_RE_4.csv"
group <- "Release age 2, South"
group4_RE <- process_samples(path,group)


mean_effects_RE <- rbind(group1_RE,group2_RE,group3_RE,group4_RE)

p2 <- ggplot(mean_effects_RE, aes(x = value, fill = variable))+
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()

ggsave(p2,file = "figures/Cohort_effects_regression_plot.png",
       height = 8,
       width = 10)





path <- "model_output/HMC_samples/samples_year_cohort_RE_1.csv"
group <- "Release age 1, North"
group1_RE2 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_year_cohort_RE_2.csv"
group <- "Release age 1, South"
group2_RE2 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_year_cohort_RE_3.csv"
group <- "Release age 2, North"
group3_RE2 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_year_cohort_RE_4.csv"
group <- "Release age 2, South"
group4_RE2 <- process_samples(path,group)


mean_effects_RE2 <- rbind(group1_RE2,group2_RE2,group3_RE2,group4_RE2)

p3 <- ggplot(mean_effects_RE2, aes(x = value, fill = variable))+
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()

ggsave(p3,file = "figures/year_cohort_effects_regression_plot.png",
       height = 8,
       width = 10)




mean_effects$Model <- "base"
mean_effects_RE$Model <- "Cohort_RE"
mean_effects_RE2$Model <- "Year_age_RE"
all_mods_effects <- rbind(mean_effects,mean_effects_RE,mean_effects_RE2)

p4 <- ggplot(all_mods_effects, aes(x = value, fill = Model))+
  geom_density(alpha = 0.6)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_brewer(name = "Index", palette = "BrBG" )+
  theme_classic()+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        strip.text = element_text(size = 14, family = "Times New Roman"))

ggsave(p4,file = "figures/all_models_regression_plot_larger_font.png",
       height = 8,
       width = 10)






process_samples_NPC <- function(path, group){
  samples1 <- read.csv(path)
  # order of var names
  
  samples_long <- melt(samples1)
  
  mean_effects <- samples_long %>% filter(substr(variable, 1,1) == "B")
  
  
  
  mean_effects$variable <- plyr::mapvalues(mean_effects$variable,
                                           c("B.1.","B.2.", "B.3."),
                                           c( "NPGO", "BI", "NPGO x BI"))
  mean_effects$group = group
  return(mean_effects)
}



path <- "model_output/HMC_samples/samples_NPGO_BI_2.csv"
group <- "Release age 1, South"
group2_NPC <- process_samples_NPC( path, group)
path <- "model_output/HMC_samples/samples_NPGO_BI_4.csv"
group <- "Release age 2, Southern"
group4_NPC <- process_samples_NPC( path,group)

mean_effects_NPC <- rbind(group2_NPC,group4_NPC)

p5 <- ggplot(mean_effects_NPC, aes(x = value, fill = variable))+
  geom_density(alpha = 0.6)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 14, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        strip.text = element_text(size = 14, family = "Times New Roman"))




ggsave(p5,file = "figures/NPC_regression_plot.png",
       height = 6,
       width = 8.5)







path <- "model_output/HMC_samples/samples_year_cohort_RE_NPC2.csv"
group <- "Ocean type, Southern"
group2_NPC <- process_samples_NPC( path, group)
path <- "model_output/HMC_samples/samples_year_cohort_RE_NPC4.csv"
group <- "Stream type, Southern"
group4_NPC <- process_samples_NPC( path,group)

mean_effects_NPC <- rbind(group2_NPC,group4_NPC)

ggplot(mean_effects_NPC, aes(x = value, fill = variable))+
  geom_density(alpha = 0.6)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()







### old robustness test not included in manusctipt ** code may not work *** 
#### grouped by run timing



path <- "model_output/HMC_samples/samples_alt_group1.csv"
group <- "Fall, Northern"
group1_alt <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_alt_group2.csv"
group <- "Fall, Southern"
group2_alt <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_alt_group3.csv"
group <- "Spring, Northern"
group3_alt <- process_samples(path,group)


group1_alt$group <- "Fall, Northern"
group2_alt$group <- "Spring, Summer"
group3_alt$group <- "Fall, Southern"


mean_effects_alt<- rbind(group1_alt,group2_alt,group3_alt)


# ggplot(mean_effects_alt, aes(x = value, fill = variable))+
#   geom_density(alpha = 0.6)+
#   geom_vline(aes(xintercept = 0), linetype = 2)+
#   facet_grid(variable~group)+
#   xlab("Effect size")+
#   ylab("Density")+
#   scale_fill_discrete(name = "Index")+
#   theme_classic()


#### grouped by run timing



path <- "model_output/HMC_samples/samples_base_alt_group1.csv"
group <- "Fall, Northern"
group1_alt <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_base_alt_group2.csv"
group <- "Fall, Southern"
group2_alt <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_base_alt_group3.csv"
group <- "Spring, Northern"
group3_alt <- process_samples(path,group)


group1_alt$group <- "Fall, Northern"
group2_alt$group <- "Spring, Summer"
group3_alt$group <- "Fall, Southern"

mean_effects_alt_base<- rbind(group1_alt,group2_alt,group3_alt)


# ggplot(mean_effects_alt, aes(x = value, fill = variable))+
#   geom_density(alpha = 0.6)+
#   geom_vline(aes(xintercept = 0), linetype = 2)+
#   facet_grid(variable~group)+
#   xlab("Effect size")+
#   ylab("Density")+
#   scale_fill_discrete(name = "Index")+
#   theme_classic()


mean_effects_alt_base$Model <- "base"
mean_effects_alt$Model <- "Year Cohort"
mean_effects_alt_models <- rbind(mean_effects_alt_base,mean_effects_alt)
p <- ggplot(mean_effects_alt_models, aes(x = value, 
                             fill = Model))+
    geom_density(alpha = 0.6)+
    geom_vline(aes(xintercept = 0), linetype = 2)+
    facet_grid(variable~group)+
    scale_fill_brewer(name = "Model", palette = "BrBG" )+
    xlab("Effect size")+
    ylab("Density")+
    theme_classic()

ggsave(p,file = "figures/alternative_stock_groupings.png",
       height = 8,
       width = 10)













## plots flexible growth model 

path <- "model_output/HMC_samples/samples_flexible_YC_1.csv"
group <- "Release age 0, Northern"
group1 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_flexible_YC_2.csv"
group <- "Release age 0, Southern"
group2 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_flexible_YC_3.csv"
group <- "Release age 1, Northern"
group3 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_flexible_YC_4.csv"
group <- "Release age 1, Southern"
group4 <- process_samples(path,group)

group1$group <- "Release age 0, North"
group2$group <- "Release age 0, South"
group3$group <- "Release age 1, North"
group4$group <- "Release age 1, South"

mean_effects <- rbind(group1,group2,group3,group4)



sig_code <- function(p){
  if(p[1] < 0.001){
    return("***")
  }else if(p[1] < 0.01){
    return("**")
  }else if(p[1]<0.05){
    return("*")
  }
  return("")
}
p_vals <- mean_effects %>% 
  dplyr::group_by(variable, group) %>%
  dplyr::summarize(b = mean(value),
                   p1 = sum(value < 0)/n(),
                   p2 = sum(value > 0)/n())%>%
  dplyr::ungroup()%>%
  dplyr::group_by(variable, group) %>%
  dplyr::mutate(p = sig_code(min(p1,p2)))



p1 <- ggplot(mean_effects, aes(x = value, fill = variable))+
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  geom_text(data = p_vals, 
            aes(x = 0.05, y = 20, 
                label = p),
            color= "black")+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()+
  theme(legend.title = element_text(size = 24, family = "Times New Roman"),
        legend.text = element_text(size = 16, family = "Times New Roman"),
        axis.text.y = element_text(size = 16, family = "Times New Roman",
                                   angle = 30),
        axis.text.x = element_text(size = 16, family = "Times New Roman"),
        axis.title = element_text(size = 24, family = "Times New Roman"),
        strip.text = element_text(size = 14, family = "Times New Roman"))

p1







path <- "model_output/HMC_samples/samples_year_cohort_RE_age4_1.csv"
group <- "Ocean type, Northern"
group1_RE_age4 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_year_cohort_RE_age4_2.csv"
group <- "Ocean type, Southern"
group2_RE_age4 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_year_cohort_RE_age4_3.csv"
group <- "Stream type, Northern"
group3_RE_age4 <- process_samples(path,group)

path <- "model_output/HMC_samples/samples_year_cohort_RE_age4_4.csv"
group <- "Stream type, Southern"
group4_RE_age4 <- process_samples(path,group)


mean_effects_RE_age4 <- rbind(group1_RE_age4,group2_RE_age4,group3_RE_age4,group4_RE_age4)

p6 <- ggplot(mean_effects_RE2, aes(x = value, fill = variable))+
  geom_density(alpha = 0.5)+
  geom_vline(aes(xintercept = 0), linetype = 2)+
  facet_grid(variable~group)+
  xlab("Effect size")+
  ylab("Density")+
  scale_fill_discrete(name = "Index")+
  theme_classic()

ggsave(p6,file = "figures/year_cohort_effects_age_4_regression_plot.png",
       height = 8,
       width = 10)
