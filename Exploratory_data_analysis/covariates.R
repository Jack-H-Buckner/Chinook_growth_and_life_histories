library(ggplot2)
library(dplyr)
library(reshape2)
setwd("~/Chinook_growth_repo")

dat <- read.csv("transformed_data/covariates.csv")

dat <- dat %>% dplyr::select(year,Pinks, WA_Pinks, BI,  
                             NPGO, ALPI, MEI_summer,PDO,
                             SSTarc_summer)%>%
  dplyr::mutate(SSTarc = SSTarc_summer,MEI = MEI_summer)%>%
  dplyr::select(-MEI_summer,-SSTarc_summer)

cor_mat <- cor(dat)
melted_cormat <- reshape2::melt(cor_mat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', name = "Cor", 
                       limits= c(-1,1) )+
  theme_classic()+
  theme(axis.text.x =element_text(family = "Times New Roman",size = 20,
                                   angle = 30, hjust = 0.75),
        axis.text.y = element_text(family = "Times New Roman",size = 20),
        axis.title = element_blank(),
        legend.text = element_text(family = "Times New Roman",size = 20),
        legend.title = element_text(family = "Times New Roman",size = 25))

ggsave("figures/covars_cor_mat.png",
      height = 6,
      width = 8.0)



dat_melt <- dat %>% reshape2::melt(id.var = "year")

dat_melt$variable <- factor(dat_melt$variable, 
                           levels=c("MEI", "NPGO", "SSTarc", "BI",
                                    "PDO","WA_Pinks","ALPI", "Pinks"))

ggplot(dat_melt, aes(x = year, y = value))+
  geom_point()+
  geom_path()+
  facet_wrap(~variable, ncol= 2)+
  theme_classic()+
  theme(axis.text = element_text(family = "Times New Roman",size = 16),
        axis.title = element_text(family = "Times New Roman",size = 28),
        legend.text = element_text(family = "Times New Roman",size = 20),
        legend.title = element_text(family = "Times New Roman",size = 25),
        strip.text = element_text(family = "Times New Roman",size = 25))



ggsave("figures/covars_series.png",
       height = 7,
       width = 8.5)


