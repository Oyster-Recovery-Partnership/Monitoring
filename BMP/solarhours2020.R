# calculate daylight hours for 2020 to confirm graph


# packages
require(dplyr)
library(readxl)
library(lubridate)

# data 
# data from: https://www.esrl.noaa.gov/gmd/grad/solcalc/
sr <- read_excel("~/Downloads/daylight2020.xlsx", sheet = 1)
ss <- read_excel("~/Downloads/daylight2020.xlsx", sheet = 2)

# reformat
days = as.data.frame(matrix(ncol=3, nrow=366, data=NA))
names(days) = c("jday", "sunrise", "sunset")
days = days %>% mutate(jday = 0:365,
                       sunrise = c(
                         paste("1/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$Jan), " "), tail, 1), sep=""),
                         paste("2/", 1:29, "/2020 ", sapply(strsplit(as.character(sr$Feb[1:29]), " "), tail, 1), sep=""),
                         paste("3/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$Mar), " "), tail, 1), sep=""),
                         paste("4/", 1:30, "/2020 ", sapply(strsplit(as.character(sr$Apr[1:30]), " "), tail, 1), sep=""),
                         paste("5/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$May), " "), tail, 1), sep=""),
                         paste("6/", 1:30, "/2020 ", sapply(strsplit(as.character(sr$Jun[1:30]), " "), tail, 1), sep=""),
                         paste("7/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$Jul), " "), tail, 1), sep=""),
                         paste("8/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$Aug), " "), tail, 1), sep=""),
                         paste("9/", 1:30, "/2020 ", sapply(strsplit(as.character(sr$Sep[1:30]), " "), tail, 1), sep=""),
                         paste("10/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$Oct), " "), tail, 1), sep=""),
                         paste("11/", 1:30, "/2020 ", sapply(strsplit(as.character(sr$Nov[1:30]), " "), tail, 1), sep=""),
                         paste("12/", 1:31, "/2020 ", sapply(strsplit(as.character(sr$Dec), " "), tail, 1), sep="")),
                       sunset = c(
                         paste("1/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$Jan), " "), tail, 1), sep=""),
                         paste("2/", 1:29, "/2020 ", sapply(strsplit(as.character(ss$Feb[1:29]), " "), tail, 1), sep=""),
                         paste("3/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$Mar), " "), tail, 1), sep=""),
                         paste("4/", 1:30, "/2020 ", sapply(strsplit(as.character(ss$Apr[1:30]), " "), tail, 1), sep=""),
                         paste("5/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$May), " "), tail, 1), sep=""),
                         paste("6/", 1:30, "/2020 ", sapply(strsplit(as.character(ss$Jun[1:30]), " "), tail, 1), sep=""),
                         paste("7/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$Jul), " "), tail, 1), sep=""),
                         paste("8/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$Aug), " "), tail, 1), sep=""),
                         paste("9/", 1:30, "/2020 ", sapply(strsplit(as.character(ss$Sep[1:30]), " "), tail, 1), sep=""),
                         paste("10/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$Oct), " "), tail, 1), sep=""),
                         paste("11/", 1:30, "/2020 ", sapply(strsplit(as.character(ss$Nov[1:30]), " "), tail, 1), sep=""),
                         paste("12/", 1:31, "/2020 ", sapply(strsplit(as.character(ss$Dec), " "), tail, 1), sep="")),
                       sunrise = as.POSIXct(sunrise, format = "%m/%d/%Y %H:%M:%OS"),
                       sunset = as.POSIXct(sunset, format = "%m/%d/%Y %H:%M:%OS"),
                       mo = month(sunrise))

# difftime
days = mutate(days, solarhours = difftime(sunset, sunrise, units = "hours"))

#summary
group_by(days, mo) %>% summarize(n = mean(solarhours))
