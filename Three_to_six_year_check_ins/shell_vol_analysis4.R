# this code is to compare between 3 and 6 years to see if there was a sig. change in shell vol. 

# load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)

# load data
dir = "~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/shell_volume_analysis/"
dat = read_excel(paste(dir,"2020-2017 6yr Volume Analysis.xlsx",sep=""))

# TOTAL VOLUME
# for total volume, exclude  H52, H54, H60, H53, H49, H51, H62, H56, H57, H58, H59
tdat = dat %>% dplyr::filter(!ReefID %in% c("H52", "H54", "H60", "H53", "H49", "H51", "H62", "H56", "H57", "H58", "H59"))
# check that distribution is normal 
ggplot(tdat %>% dplyr::filter(Year %in% 2017), aes(x=TotalVolume)) + 
  geom_density(alpha=.2, col="red") +
  facet_wrap(~ReefID, scales="free") + 
  theme_bw() + ggtitle("2017")

ggplot(tdat %>% dplyr::filter(Year %in% 2020), aes(x=TotalVolume)) + 
  geom_density(alpha=.2, col="blue") +
  facet_wrap(~ReefID, scales="free") + 
  theme_bw() + ggtitle("2020")
## distributions are not all normal

# t-test 
means = tdat %>% 
  group_by(ReefID, Year) %>% 
  summarise(avg=mean(TotalVolume)) %>% 
  spread(Year, avg)
names(means) = c("ReefID", "three", "six")

ids = unique(tdat$ReefID)
tests = as.data.frame(matrix(ncol = 3, nrow = length(ids), data = NA))
names(tests) = c("ReefID", "df","p")

for(a in 1:length(ids)) {
  t = t.test(TotalVolume ~ Year, data = dplyr::filter(tdat, ReefID %in% ids[a]), var.equal = TRUE)
  outp = cbind(ids[a], t$parameter, t$p.value)
  names(outp) = c("ReefID", "df","p")
  tests[a,] = outp
}

tests = tests %>% 
  mutate(sig = NA,
         sig = ifelse(p <= 0.05, "*", sig),
         sig = ifelse(p  <= 0.01, "**", sig),
         sig = ifelse(p  <= 0.005, "***", sig)) %>%
  left_join(., means, by="ReefID") %>% 
  mutate(diff = ifelse(!is.na(sig), six-three, NA),
         sign = diff, 
         sign = replace(sign, sign<0,"-"),
         sign = replace(sign, sign>0,"+"))

# plots
ggplot() + geom_boxplot(data=tdat, aes(x=ReefID, y=TotalVolume, fill=Year))

ggplot() + geom_bar(data=tests, aes(ReefID, y=diff), stat="identity", fill="darkred") + 
  labs(y="Difference in Total Volume") + 
  theme_bw()




