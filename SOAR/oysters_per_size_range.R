library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(agricolae)

SOARData <- read_excel("Oyster Recovery Partnership, Inc/ORP - Operations/Oyster Restoration/SOAR/2021Round1_Data/SOAR_2021_Round1_Data.xlsx")
names(SOARData) = gsub(" ","", names(SOARData))
names(SOARData) = gsub("[(]","", names(SOARData))
names(SOARData) = gsub("[)]","", names(SOARData))
names(SOARData) = gsub("[,]","", names(SOARData))

#Filter by Bar
MillHill <- filter(SOARData, Bar == "Mill Hill")
EXG04 <- filter(SOARData, Bar == "EXG04")
UpperNewfoundland <- filter(SOARData, Bar == "Upper Newfoundland")
CherryTree <- filter(SOARData, Bar == "Cherry Tree")
summary(MillHill$Lengthmm)
summary(EXG04$Lengthmm)
summary(UpperNewfoundland$Lengthmm)
summary(CherryTree$Lengthmm)

#Plot of Number of oysters per size range
p1 = ggplot(data = SOARData, aes(x=Lengthmm)) +
  geom_histogram(binwidth=10) +
  geom_freqpoly(binwidth=10, col="orange", lwd=2) + 
  theme_bw() +
  theme(text = element_text(size = 15))+
  labs(x="Oyster Size (mm)", y="Number of Oysters", title="Oyster Size by 10 mm bins")
p1
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/Oyster Restoration/SOAR/2021Round1_Data/OysterSizeHistogram10mmBins.png",p1) 

p2 = ggplot(data = SOARData, aes(x=Lengthmm, col=Bar)) +
  geom_freqpoly(binwidth=10, lwd=2) + 
  theme_bw() +
  theme(text = element_text(size = 15))+
  labs(x="Oyster Size (mm)", y="Frequency of Oysters", title="Oyster Size by 10 mm bins by bar") 
p2
ggsave("Oyster Recovery Partnership, Inc/ORP - Operations/Oyster Restoration/SOAR/2021Round1_Data/OysterSizeFreqByBar10mmBins.png",p2) 

