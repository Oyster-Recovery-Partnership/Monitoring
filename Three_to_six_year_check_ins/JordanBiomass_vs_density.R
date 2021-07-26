# additional analyses Oyster Recovery Partnership, Inc\ORP - Operations\GIS\
#1.0 Restoration and Monitoring\1.0 3_6_yr_monitoring\6.0 2020-2017 cohort\2.0 Data

library(readxl)
library(ggplot2)
library(ggpmisc)

dat <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/GIS/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/6.0 2020-2017 cohort/2.0 Data/additional analyses.xlsx", skip=1)
dat2 = dat[,c(13,14,23,24)]
names(dat2) = c("Density","density_sd","Biomass","biomass_sd")

my.formula <- y ~ x
p = ggplot(data = dat2, aes(x=Density, y=Biomass)) + 
  geom_errorbar(aes(ymin=Biomass-biomass_sd, ymax=Biomass+biomass_sd), col="darkgrey") + 
  geom_errorbar(aes(xmin=Density-density_sd, xmax=Density+density_sd), col="darkgrey") + 
  geom_smooth(method = lm) + 
  geom_point() + 
  theme_bw() +
  theme(text = element_text(size=20)) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()
p
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/GIS/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/6.0 2020-2017 cohort/2.0 Data/JordanBiomass_vs_Density.png",p)
