library(tidyverse)
library("lubridate")
library(dplyr)
library(ggplot2)
library(chron)
str(bikedata12_21)
alltrips21 <- bind_rows(
  bikedata01_21,
  bikedata02_21,
  bikedata03_21,
  bikedata04_21,
  bikedata05_21,
  bikedata06_21,
  bikedata07_21,
  bikedata08_21,
  bikedata09_21,
  bikedata10_21,
  bikedata11_21,
  bikedata12_21)
nrow(alltrips)
str(alltrips2)
as.POSIXct(alltrips2$duration, format = "%H:%M:%S")
hms(alltrips4$duration)
str(alltrips3)
is_hms(alltrips$duration)
as.duration(alltrips3$duration)
as.numeric(hms(alltrips4$duration))
which.min(alltrips5$duration)
biketrips$ride_total <- difftime(biketrips$ended_at, biketrips$started_at)
biketrips$started_at <- mdy_hm(biketrips$started_at)
summary(biketrips)
is.Date(biketrips$end_date)
as.Date(biketrips$end_date, )
str(biketrips)
as.Date(divvytrips$started_at, )
mdy_hms(alltrips$started_at, "%m/%d/%y_%h:%m:%s")
alltrips$startdate <- as.Date(mdy(alltrips$start_date, "%m/%d/%y"))
hms(biketrips$start_time, "%h:%m:%s")
str(biketrips$start_time)
str(biketrips$start_date)
table(alltrips$member_casual)
table(alltrips21$member_casual,alltrips21$start_day, table)
alltrips21 %>% 
  ggplot(aes(x = start_day, y = number_of_rides))
  