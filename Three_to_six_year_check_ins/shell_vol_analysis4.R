# -------------------------- #
# this code is to compare between 3 and 6 years to see if there was a sig. change in shell vol. 
# -------------------------- #

# -------------------------- #
# load libraries
# -------------------------- #
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gridExtra)
# -------------------------- #

# -------------------------- #
# load data
# -------------------------- #
dir = "~/Oyster Recovery Partnership, Inc/ORP - Operations/GIS/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/shell_volume_analysis/"
dat = read_excel(paste(dir,"2020-2017 6yr Volume Analysis.xlsx",sep=""))
# -------------------------- #

# -------------------------- #
# TOTAL VOLUME
# -------------------------- #
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
  mutate(sig = " ",
         sig = ifelse(p <= 0.05, "*", sig),
         sig = ifelse(p  <= 0.01, "**", sig),
         sig = ifelse(p  <= 0.005, "***", sig)) %>%
  left_join(., means, by="ReefID") %>% 
  mutate(diff = ifelse(!is.na(sig), six-three, NA),
         sign = diff, 
         sign = replace(sign, sign<0,"-"),
         sign = replace(sign, sign>0,"+"))

# plots
pdat = tdat %>% dplyr::select(ReefID, Year, TotalVolume) %>%
  mutate(Year=as.character(Year)) %>%
  left_join(.,dplyr::select(tests, sig, ReefID), by="ReefID")
p1 = ggplot() + 
  geom_boxplot(data = pdat, aes(x = ReefID, y = TotalVolume, fill = Year)) + 
  geom_hline(yintercept=0, col="darkgrey") + 
  geom_text(data = pdat, aes(x = ReefID, y = -5, label = sig), col="gold", size=6) + 
  theme_bw() + 
  labs(y = "Total Volume", title = "Total Shell Volume") +
  theme(legend.position = "bottom")
p1
ggsave(paste(dir,"TotalShellVol_2017_2020.png"), p1)

p2 = ggplot() + 
  geom_bar(data = tests, aes(ReefID, y=diff), stat="identity", col="black") + 
  labs(y="Difference in Total Volume") + 
  theme_bw()
p2
#grid.arrange(p1, p2, nrow = 2)
# -------------------------- #

# -------------------------- #
# OYSTER VOLUME
# -------------------------- #
odat = dat 
# check that distribution is normal 
ggplot(odat %>% dplyr::filter(Year %in% 2017), aes(x=OysterVolume)) + 
  geom_density(alpha=.2, col="red") +
  facet_wrap(~ReefID, scales="free") + 
  theme_bw() + ggtitle("2017")

ggplot(odat %>% dplyr::filter(Year %in% 2020), aes(x=OysterVolume)) + 
  geom_density(alpha=.2, col="blue") +
  facet_wrap(~ReefID, scales="free") + 
  theme_bw() + ggtitle("2020")

# t-test 
omeans = odat %>% 
  group_by(ReefID, Year) %>% 
  summarise(avg=mean(OysterVolume)) %>% 
  spread(Year, avg)
names(omeans) = c("ReefID", "three", "six")

oids = unique(odat$ReefID)
otests = as.data.frame(matrix(ncol = 3, nrow = length(ids), data = NA))
names(otests) = c("ReefID", "df","p")

for(a in 1:length(ids)) {
  t = t.test(OysterVolume ~ Year, data = dplyr::filter(odat, ReefID %in% ids[a]), var.equal = TRUE)
  outp = cbind(ids[a], t$parameter, t$p.value)
  names(outp) = c("ReefID", "df","p")
  otests[a,] = outp
}

otests = otests %>% 
  mutate(sig = " ",
         sig = ifelse(p <= 0.05, "*", sig),
         sig = ifelse(p  <= 0.01, "**", sig),
         sig = ifelse(p  <= 0.005, "***", sig)) %>%
  left_join(., omeans, by="ReefID") %>% 
  mutate(diff = ifelse(!is.na(sig), six-three, NA),
         sign = diff, 
         sign = replace(sign, sign<0,"-"),
         sign = replace(sign, sign>0,"+"))

# plots
pdat2 = odat %>% dplyr::select(ReefID, Year, OysterVolume) %>%
  mutate(Year=as.character(Year)) %>%
  left_join(.,dplyr::select(otests, sig, ReefID), by="ReefID")
p3 = ggplot() + 
  geom_boxplot(data = pdat2, aes(x = ReefID, y = OysterVolume, fill = Year)) + 
  geom_hline(yintercept=0, col="darkgrey") + 
  geom_text(data = pdat2, aes(x = ReefID, y = -5, label = sig), col="gold", size=6) + 
  theme_bw() + 
  labs(y = "Oyster Volume", title = "Oyster Shell Volume") +
  theme(legend.position = "bottom")
p3
ggsave(paste(dir,"OysterShellVol_2017_2020.png"), p3)

p4 = ggplot() + 
  geom_bar(data = otests, aes(ReefID, y=diff), stat="identity", col="black") + 
  labs(y="Difference in Oyster Volume") + 
  theme_bw()
p4
#grid.arrange(p4, p4, nrow = 2)
# -------------------------- #

p5 = grid.arrange(p1, p3, nrow = 2)
ggsave(paste(dir,"Total_and_OysterShellVol_2017_2020.png"), p5)



