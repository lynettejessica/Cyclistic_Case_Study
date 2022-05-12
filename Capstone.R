library(tidyverse) #for datasets
library("lubridate") #need this for dates
library(dplyr) #for data manipulation
library(ggplot2) #for elegant data viz presentation

`202101.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/01_2021divvy-tripdata/202101-divvy-tripdata.csv")
`202102.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/02_2021divvy-tripdata/202102-divvy-tripdata.csv")
`202103.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/03_2021divvy-tripdata/202103-divvy-tripdata.csv")
`202104.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/04_2021divvy-tripdata/202104-divvy-tripdata.csv")
`202105.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/05_2021divvy-tripdata/202105-divvy-tripdata.csv")
`202106.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/06_2021divvy-tripdata/202106-divvy-tripdata.csv")
`202107.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/07_2021divvy-tripdata/202107-divvy-tripdata.csv")
`202108.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/08_2021divvy-tripdata/202108-divvy-tripdata.csv")
`202109.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/09_2021divvy-tripdata/202109-divvy-tripdata.csv")
`202110.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/10_2021divvy-tripdata/202110-divvy-tripdata.csv")
`202111.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/11_2021divvy-tripdata/202111-divvy-tripdata.csv")
`202112.divvy.tripdata` <- read.csv("~/Cyclistic_Case_Study/Cyclistic_Capstone/Used/12_2021divvy-tripdata/202112-divvy-tripdata.csv")

str(`202101.divvy.tripdata`)
str(`202102.divvy.tripdata`)
str(`202103.divvy.tripdata`)
str(`202104.divvy.tripdata`)
str(`202105.divvy.tripdata`)
str(`202106.divvy.tripdata`)
str(`202107.divvy.tripdata`)
str(`202108.divvy.tripdata`)
str(`202109.divvy.tripdata`)
str(`202110.divvy.tripdata`)
str(`202111.divvy.tripdata`)
str(`202112.divvy.tripdata`)

bikes_2021 <- bind_rows(
  `202101.divvy.tripdata`,
  `202102.divvy.tripdata`,
  `202103.divvy.tripdata`,
  `202104.divvy.tripdata`,
  `202105.divvy.tripdata`,
  `202106.divvy.tripdata`,
  `202107.divvy.tripdata`,
  `202108.divvy.tripdata`,
  `202109.divvy.tripdata`,
  `202110.divvy.tripdata`,
  `202111.divvy.tripdata`,
  `202112.divvy.tripdata`,
)

summary(bikes_2021)



bikes_2021_v2 <- bikes_2021[!is.na(bikes_2021$end_lat),]  

remove(`202101.divvy.tripdata`,
       `202102.divvy.tripdata`,
       `202103.divvy.tripdata`,
       `202104.divvy.tripdata`,
       `202105.divvy.tripdata`,
       `202106.divvy.tripdata`,
       `202107.divvy.tripdata`,
       `202108.divvy.tripdata`,
       `202109.divvy.tripdata`,
       `202110.divvy.tripdata`,
       `202111.divvy.tripdata`,
       `202112.divvy.tripdata`,)

summary(bikes_2021)
str(bikes_2021)

bikes_2021$ride_id[duplicated(bikes_2021$ride_id)]

bikes_2021_v2 %>% 
  count(start_station_name)

options(max.print = 10000000) 

bikes_2021_v2 %>% 
  count(start_station_name)

bikes_2021_v2 %>% 
  count(end_station_name)


bikes_2021_v2 %>% 
  count(start_station_id) 

bikes_2021_v2 %>% 
  count(end_station_id) 

bikes_2021_v2 %>% 
  filter(start_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | start_station_name == "Lyft Driver Center Private Rack" | start_station_name == "Throop/Hastings Mobile Station" | end_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | end_station_name == "Lyft Driver Center Private Rack" | end_station_name == "Throop/Hastings Mobile Station")

bikes_2021_v2 %>% 
  filter(start_station_name == "Halsted St & 18th St (Temp)") %>% 
  arrange(started_at)  #Ran these separately in pairs
bikes_2021_v2 %>% 
  filter(start_station_name == "Halsted St & 18th St") %>% 
  arrange(started_at)
bikes_2021_v2 %>% 
  filter(end_station_name == "Halsted St & 18th St (Temp)") %>% 
  arrange(started_at)
bikes_2021_v2 %>% 
  filter(end_station_name == "Halsted St & 18th St") %>% 
  arrange(started_at)

bikes_2021_v2 %>% 
  filter(start_station_name == "Base - 2132 W Hubbard Warehouse") %>% 
  arrange(started_at)
bikes_2021_v2 %>% 
  filter(end_station_name == "Base - 2132 W Hubbard Warehouse") %>% 
  arrange(started_at)

bikes_2021_v2 %>% 
  filter(start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | end_station_id == "Hubbard Bike-checking (LBS-WH-TEST)")
bikes_2021_v2 %>% 
  filter(start_station_id == "DIVVY 001" | end_station_id == "DIVVY 001")

bikes_2021_v2$time_started <- as_datetime(bikes_2021_v2$started_at)

bikes_2021_v2$time_ended <- as_datetime(bikes_2021_v2$ended_at)

str(bikes_2021_v2)

bikes_2021_v2$duration <- difftime(bikes_2021_v2$time_ended, bikes_2021_v2$time_started,) 

str(bikes_2021_v2)

is.factor(bikes_2021_v2$duration)  

bikes_2021_v2$duration <- as.numeric(as.character(bikes_2021_v2$duration))

is.numeric(bikes_2021_v2$duration)

bikes_2021_v2 %>% 
  filter(start_station_id == "DIVVY 001" | end_station_id == "DIVVY 001"  | start_station_name == "WEST CHI-WATSON" | end_station_name == "WEST CHI-WATSON") %>% 
  arrange(duration)

bikes_2021_v2 %>% 
  filter(start_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | start_station_name == "Lyft Driver Center Private Rack" | start_station_name == "Throop/Hastings Mobile Station" | start_station_name == "WEST CHI-WATSON" | start_station_name == "Base - 2132 W Hubbard Warehouse") %>% 
  count(start_station_name, start_station_id) %>% 
  arrange(start_station_id)

bikes_2021_v2 %>% 
  filter(end_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | end_station_name == "Lyft Driver Center Private Rack" | end_station_name == "Throop/Hastings Mobile Station" | end_station_name == "WEST CHI-WATSON" | end_station_name == "Base - 2132 W Hubbard Warehouse") %>% 
  count(end_station_id, end_station_name) %>% 
  arrange(end_station_id)

bikes_2021_v2 %>% 
  filter(start_station_id == "20999" | start_station_id == "DIVVY 001" | start_station_id == "DIVVY CASSETTE REPAIR MOBILE STATION" | start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | start_station_id == "Throop/Hastings Mobile Station") %>% 
  count(start_station_name, start_station_id) %>% 
  arrange(start_station_id)

bikes_2021_v2 %>% 
  filter(end_station_id == "20999" | end_station_id == "DIVVY 001" | end_station_id == "DIVVY CASSETTE REPAIR MOBILE STATION" | end_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | end_station_id == "Throop/Hastings Mobile Station") %>% 
  count(end_station_name, end_station_id) %>% 
  arrange(end_station_id)

bikes_2021_v2 %>% 
  group_by(start_station_id) %>% 
  summarise(no.unique.names = n_distinct(start_station_name), unique_names = str_c(unique(start_station_name), collapse = ",  ")) %>% 
  filter(no.unique.names > 1) 

bikes_2021_v2 %>% 
  group_by(end_station_id) %>% 
  summarise(no.unique.names = n_distinct(end_station_name), unique_names = str_c(unique(end_station_name), collapse = ",  ")) %>% 
  filter(no.unique.names > 1)

bikes_2021_v2 %>% 
  filter(start_station_id == "351") %>% 
  arrange(started_at)

bikes_2021_v2 %>% 
  filter(start_station_id == "TA1306000029") %>% 
  arrange(started_at)

bikes_2021_v2 %>% 
  filter(start_station_id == "TA1305000039") %>% 
  arrange(started_at)

bikes_2021_v2 %>% 
  filter(end_station_id == "351" | end_station_id == "TA1305000039" | end_station_id == "TA1306000029") %>% 
  arrange(end_station_id)

bikes_2021_v3 <- bikes_2021_v2[!(bikes_2021_v2$start_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | bikes_2021_v2$start_station_name == "Lyft Driver Center Private Rack" | bikes_2021_v2$start_station_name == "Throop/Hastings Mobile Station" | bikes_2021_v2$start_station_name == "WEST CHI-WATSON" | bikes_2021_v2$start_station_name == "Base - 2132 W Hubbard Warehouse" | bikes_2021_v2$start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | bikes_2021_v2$start_station_name == "DIVVY 001" ),]

bikes_2021_v3 %>% 
  filter(start_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | start_station_name == "Lyft Driver Center Private Rack" | start_station_name == "Throop/Hastings Mobile Station"| start_station_name == "WEST CHI-WATSON" | start_station_name == "Base - 2132 W Hubbard Warehouse" | start_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | start_station_name == "DIVVY 001" )

bikes_2021_v4 <- bikes_2021_v3[!(bikes_2021_v3$end_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | bikes_2021_v3$end_station_name == "Lyft Driver Center Private Rack" | bikes_2021_v3$end_station_name == "Throop/Hastings Mobile Station" | bikes_2021_v3$end_station_name == "WEST CHI-WATSON" | bikes_2021_v3$end_station_name == "Base - 2132 W Hubbard Warehouse" | bikes_2021_v3$end_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | bikes_2021_v3$end_station_name == "DIVVY 001" ),]

bikes_2021_v4 %>% 
  filter(end_station_name == "DIVVY CASSETTE REPAIR MOBILE STATION" | end_station_name == "Lyft Driver Center Private Rack" | end_station_name == "Throop/Hastings Mobile Station"| end_station_name == "WEST CHI-WATSON" | end_station_name == "Base - 2132 W Hubbard Warehouse" | end_station_id == "Hubbard Bike-checking (LBS-WH-TEST)" | end_station_name == "DIVVY 001" )

bikes_2021_v4 %>% 
  filter(start_station_id == "351") %>% 
  arrange(start_station_name)

bikes_2021_v4$start_station_name[bikes_2021_v4$ride_id == "5E181D51F7C391F4"] = "Mulligan Ave & Wellington Ave"

bikes_2021_v4$start_station_name[bikes_2021_v4$ride_id == "3036610505F382EF"] = "Mulligan Ave & Wellington Ave"

bikes_2021_v4 %>% 
  filter(ride_id == "5E181D51F7C391F4" | ride_id == "3036610505F382EF")

summary(bikes_2021_v4)

bikes_2021_v4 %>% 
  count(start_station_name)

bikes_2021_v4 %>% 
  count(end_station_name)

bikes_2021_v4 %>% 
  filter(start_station_name == "")

bikes_2021_v4 %>% 
  filter(end_station_name == "") %>% 
  select(rideable_type, start_station_name, end_station_name, start_lng, end_lng)

bikes_2021_v4 %>% 
  group_by(rideable_type) %>% 
  summarise(earliest = min(started_at))

remove(bikes_2021, bikes_2021_v2)

summary(bikes_2021_v4)

bikes_2021_v4$start_date <- as.Date(bikes_2021_v4$started_at)
bikes_2021_v4$month <- format(as.Date(bikes_2021_v4$start_date), "%m")
bikes_2021_v4$day <- format(as.Date(bikes_2021_v4$start_date), "%d")
bikes_2021_v4$year <- format(as.Date(bikes_2021_v4$start_date), "%Y")
bikes_2021_v4$day_of_week <- format(as.Date(bikes_2021_v4$start_date), "%A")

summary(bikes_2021_v4)

bikes_2021_v4 %>% 
  filter(month == "11" & day == "07" & duration <= 1) %>% 
  select(ride_id, start_station_name, end_station_name, time_started, time_ended, duration) %>% 
  arrange(time_started)

bikes_2021_v4 %>% 
  filter(month == "03" & day == "14") %>% 
  select(ride_id, time_started, time_ended, duration) %>% 
  arrange(time_started)

attributes(bikes_2021_v4$started_at)  # NULL
attributes(bikes_2021_v4$time_started) # POSIXct and UTC

bikes_2021_v4$time_started_central <- force_tz(bikes_2021_v4$time_started, tzone = "America/Chicago") # assign central timezone
attributes(bikes_2021_v4$time_started_central) # double checking
bikes_2021_v4$time_started_UTC <- format(bikes_2021_v4$time_started_central, tz = "UTC", usetz = FALSE) # format to UTC
bikes_2021_v4$time_started_UTC <- as_datetime(bikes_2021_v4$time_started_UTC, tz = "UTC") # format to datetime
attributes(bikes_2021_v4$time_started_UTC) # double checking
bikes_2021_v4 %>% 
  select(ride_id, started_at, time_started, time_started_central, time_started_UTC) # pulls cited columns 

bikes_2021_v4$time_ended_central <- force_tz(bikes_2021_v4$time_ended, tzone = "America/Chicago") # assign central timezone
attributes(bikes_2021_v4$time_ended_central) # double checking
bikes_2021_v4$time_ended_UTC <- format(bikes_2021_v4$time_ended_central, tz = "UTC", usetz = FALSE) # format to UTC
bikes_2021_v4$time_ended_UTC <- as_datetime(bikes_2021_v4$time_ended_UTC, tz = "UTC")# format to datetime
attributes(bikes_2021_v4$time_ended_UTC) # double checking
bikes_2021_v4 %>% 
  select(ride_id, ended_at, time_ended, time_ended_central, time_ended_UTC) # pulls cited columns 

bikes_2021_v4$duration_UTC <- difftime(bikes_2021_v4$time_ended_UTC, bikes_2021_v4$time_started_UTC,) # create the new duration column based on UTC
str(bikes_2021_v4)
is.factor(bikes_2021_v4$duration_UTC)  # check the factor
bikes_2021_v4$duration_UTC <- as.numeric(as.character(bikes_2021_v4$duration_UTC)) # changes the character to numeric
is.numeric(bikes_2021_v4$duration) # check is numeric
summary(bikes_2021_v4)

bikes_2021_v4 %>% 
  filter(month == "11" & day == "07" & duration_UTC <= 1) %>% 
  select(ride_id, time_started_central, time_started_UTC, time_ended_central, time_ended_UTC, duration, duration_UTC) %>% 
  arrange(time_started_UTC)

bikes_2021_v5 <- subset(bikes_2021_v4, select = -c(duration_UTC, time_ended_UTC, time_ended_central, time_started_UTC, time_started_central))


bikes_2021_v6 <- bikes_2021_v4[bikes_2021_v4$duration >=1,]

summary(bikes_2021_v6)


remove(bikes_2021_v3)

bikes_2021_v6 %>% 
  filter(month == "11" & day == "07" & duration <= 1) %>% 
  select(ride_id, start_station_name, end_station_name, time_started, time_ended, duration) %>% 
  arrange(time_started)


bikes_2021_v6 %>% 
  filter(ride_id == "C57E141CF03704D7"| ride_id == "9BAA36FE11793902" | ride_id == "1742E1E1C868296D" | ride_id =="DE67703D329D3423" | ride_id == "B4EB229DD2D99D2F" | ride_id == "86FDC11FA0132DAD" | ride_id == "05A8F6B085B973A1" | ride_id == "FE06D0DF80ABEC33" | ride_id == "425C2C4E189F31EA" | ride_id == "54D1D728E2045497" | ride_id == "98117173186A00B8" | ride_id == "5D70AF81320EA3A3" | ride_id == "D3968BC0BFE21697" | ride_id == "302638FE369B67F5" | ride_id == "DF8C309DFF5ABCD8" | ride_id == "0F66CA964EFCD28E" | ride_id == "A95B83F324E0BDEB" | ride_id == "80E6B4B0EA90AD32" | ride_id == "08B3CF3DF98B758B" | ride_id == "178FD08391670F22" | ride_id == "9C61F08301B87DD1" | ride_id == "7F58E9438E3A782F" | ride_id == "B66A33BE0D820756" | ride_id == "42EFDD3299DD4C70" | ride_id == "35DDEE9348E05D7D" | ride_id == "CCF4998BC32A6E7D" | ride_id == "C66E5A6301F13D84" | ride_id == "F7A7096FDB8E8A39" | ride_id == "FFF6757C245D1899" | ride_id == "9B9060CF745DD7F1" | ride_id == "081549DEA616CA22" | ride_id == "DD5B8BE0217DF562" | ride_id == "663DF74908A97A64") %>% 
  select(ride_id, time_started, time_ended, duration) %>% 
  arrange(time_started)

bikes_2021_v7 <- bikes_2021_v6[!(bikes_2021_v6$ride_id == "C57E141CF03704D7"| bikes_2021_v6$ride_id == "9BAA36FE11793902" | bikes_2021_v6$ride_id == "1742E1E1C868296D" | bikes_2021_v6$ride_id =="DE67703D329D3423" | bikes_2021_v6$ride_id == "B4EB229DD2D99D2F" | bikes_2021_v6$ride_id == "86FDC11FA0132DAD" | bikes_2021_v6$ride_id == "05A8F6B085B973A1" | bikes_2021_v6$ride_id == "FE06D0DF80ABEC33" | bikes_2021_v6$ride_id == "425C2C4E189F31EA" | bikes_2021_v6$ride_id == "54D1D728E2045497" | bikes_2021_v6$ride_id == "98117173186A00B8" | bikes_2021_v6$ride_id == "5D70AF81320EA3A3" | bikes_2021_v6$ride_id == "D3968BC0BFE21697" | bikes_2021_v6$ride_id == "302638FE369B67F5" | bikes_2021_v6$ride_id == "DF8C309DFF5ABCD8" | bikes_2021_v6$ride_id == "0F66CA964EFCD28E" | bikes_2021_v6$ride_id == "A95B83F324E0BDEB" | bikes_2021_v6$ride_id == "80E6B4B0EA90AD32" | bikes_2021_v6$ride_id == "08B3CF3DF98B758B" | bikes_2021_v6$ride_id == "178FD08391670F22" | bikes_2021_v6$ride_id == "9C61F08301B87DD1" | bikes_2021_v6$ride_id == "7F58E9438E3A782F" | bikes_2021_v6$ride_id == "B66A33BE0D820756" | bikes_2021_v6$ride_id == "42EFDD3299DD4C70" | bikes_2021_v6$ride_id == "35DDEE9348E05D7D" | bikes_2021_v6$ride_id == "CCF4998BC32A6E7D" | bikes_2021_v6$ride_id == "C66E5A6301F13D84" | bikes_2021_v6$ride_id == "F7A7096FDB8E8A39" | bikes_2021_v6$ride_id == "FFF6757C245D1899" | bikes_2021_v6$ride_id == "9B9060CF745DD7F1" | bikes_2021_v6$ride_id == "081549DEA616CA22" | bikes_2021_v6$ride_id == "DD5B8BE0217DF562" | bikes_2021_v6$ride_id == "663DF74908A97A64" ),]

bikes_2021_v7 %>% 
  filter(ride_id == "C57E141CF03704D7"| ride_id == "9BAA36FE11793902" | ride_id == "1742E1E1C868296D" | ride_id =="DE67703D329D3423" | ride_id == "B4EB229DD2D99D2F" | ride_id == "86FDC11FA0132DAD" | ride_id == "05A8F6B085B973A1" | ride_id == "FE06D0DF80ABEC33" | ride_id == "425C2C4E189F31EA" | ride_id == "54D1D728E2045497" | ride_id == "98117173186A00B8" | ride_id == "5D70AF81320EA3A3" | ride_id == "D3968BC0BFE21697" | ride_id == "302638FE369B67F5" | ride_id == "DF8C309DFF5ABCD8" | ride_id == "0F66CA964EFCD28E" | ride_id == "A95B83F324E0BDEB" | ride_id == "80E6B4B0EA90AD32" | ride_id == "08B3CF3DF98B758B" | ride_id == "178FD08391670F22" | ride_id == "9C61F08301B87DD1" | ride_id == "7F58E9438E3A782F" | ride_id == "B66A33BE0D820756" | ride_id == "42EFDD3299DD4C70" | ride_id == "35DDEE9348E05D7D" | ride_id == "CCF4998BC32A6E7D" | ride_id == "C66E5A6301F13D84" | ride_id == "F7A7096FDB8E8A39" | ride_id == "FFF6757C245D1899" | ride_id == "9B9060CF745DD7F1" | ride_id == "081549DEA616CA22" | ride_id == "DD5B8BE0217DF562" | ride_id == "663DF74908A97A64") %>% 
  select(ride_id, time_started, time_ended, duration) %>% 
  arrange(time_started)

count(bikes_2021_v7 %>% 
        filter(duration <= 30))

bikes_2021_v7 %>% 
  filter(duration <= 5 & start_station_name != end_station_name) %>% 
  select(ride_id, start_station_name, end_station_name, time_started, time_ended, duration) %>% 
  arrange(duration)

bikes_2021_v7 %>% 
  filter(duration <= 10 & duration > 5 & start_station_name != end_station_name) %>% 
  select(start_station_name, end_station_name, time_started, time_ended, duration) %>% 
  arrange(duration)


summary(bikes_2021_v7)
str(bikes_2021_v7)
  
bikedata <- subset(bikes_2021_v7, select = -c(start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng, started_at, ended_at, year))

summary(bikedata)
str(bikedata)

remove(bikes_2021_v4, bikes_2021_v6, bikes_2021_v7)

mean(bikedata$duration)
median(bikedata$duration)
max(bikedata$duration) 
min(bikedata$duration)


bikedata[bikedata$duration == "3356649",]


summary(bikedata$duration)

summary()

aggregate(bikedata$duration ~ bikedata$member_casual, FUN = mean)
aggregate(bikedata$duration ~ bikedata$member_casual, FUN = median)
aggregate(bikedata$duration ~ bikedata$member_casual, FUN = max)
aggregate(bikedata$duration ~ bikedata$member_casual, FUN = min)

aggregate(bikedata$duration ~ bikedata$member_casual + bikedata$day_of_week, FUN = mean)

bikedata$day_of_week <- ordered(bikedata$day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(bikedata$month ~ bikedata$rideable_type, FUN = max)
aggregate(bikedata$month ~ bikedata$rideable_type, FUN = min)
aggregate(bikedata$month ~ bikedata$rideable_type + bikedata$day_of_week, FUN = min)
aggregate(bikedata$ride_id ~ bikedata$rideable_type + bikedata$month, FUN = mean)
bikedata %>% 
  group_by(rideable_type) %>% 
  summarise(earliest = min(time_started))

bikedata %>% 
  group_by(ride_id, rideable_type) %>% 
  count(ride_id)

# bike type by member type
bikedata %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, rideable_type)

aggregate(bikedata$month ~ bikedata$start_station_name + bikedata$day_of_week, FUN = max)
aggregate(bikedata$month ~ bikedata$start_station_name + bikedata$day_of_week, FUN = min)



options(warning=-1)

bikedata %>% 
  mutate(weekday = wday(time_started, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%  
  summarise(number_of_rides = n()  
            ,average_duration = mean(duration)) %>% 		
  arrange(member_casual, weekday)

aggregate(bikedata$day_of_week ~ bikedata$member_casual, FUN = mode)

#viz for average duration by member type by day
bikedata %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# polished up viz
bikedata %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Duration of Bike Rides by Day and Membership Type") +
  labs(fill = "Membership Type") +
  ylab("Average Duration by Seconds") + 
  xlab("Day of the Week")

# number of rides by day and membership
bikedata %>% 
  group_by(member_casual, day_of_week, duration) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week, duration)

# duration(no average) by day and member ship  do not like
bikedata %>% 
  group_by(member_casual, day_of_week, duration) %>% 
  summarise(number_of_rides = n(), duration_in_minutes = (duration/60)) %>% 
  arrange(member_casual, day_of_week, duration)  %>% 
  ggplot(aes(x = day_of_week, y = duration_in_minutes, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Duration of Bike Rides by Day and Membership Type") +
  labs(fill = "Membership Type") +
  ylab("Duration by minutes") + 
  xlab("Day of the Week")

bikedata %>% 
  group_by(member_casual, rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, rideable_type)

str(bikedata$day_of_week)

bikedata %>% 
  group_by(member_casual, rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, rideable_type) %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, color = rideable_type)) +
  geom_point(type = "1")


bikedata %>% 
  group_by(member_casual, rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, rideable_type, day_of_week)  %>% 
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides by Bike Type and Rider Type") + 
  labs(fill = "Membership Type") +
  ylab("Number of Rides") + 
  xlab("Bike Type")

bikedata %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week)

#number of rides by rider type
bikedata %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides by Day and Rider Type") + 
  ylab("Number of Rides") + 
  xlab("Day of Week")

bikedata %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration/60))

#average duration
bikedata %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration/60)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Duration by Day and Rider Type") + 
  ylab("Average Duration (minutes)") + 
  xlab("Day of Week")

bikedata %>% 
  group_by(rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(rideable_type, day_of_week)

#number of rides by day and bike type
bikedata %>% 
  group_by(rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(rideable_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Number of Rides by Day and Bike Type") + 
  ylab("Number of Rides") + 
  xlab("Day of Week")

bikedata %>% 
  group_by(rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration/60)) %>% #in minutes
  arrange(rideable_type, day_of_week)

#average duration by bike type
bikedata %>% 
  group_by(rideable_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration/60)) %>% #in minutes
  arrange(rideable_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = rideable_type)) +
  geom_col(position = "dodge") + 
  labs(title = "Average Ride Duration by Day and Bike Type") + 
  ylab("Average Duration (minutes)") + 
  xlab("Day of Week")

bikedata %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,month_name = month.name[month]) %>% 
  arrange(member_casual, month_name) 

#number of rides by month and rider type
bikedata %>% 
  group_by(member_casual, month) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(duration)) %>% 
  arrange(member_casual, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, group = member_casual)) +
  geom_line(aes(color = member_casual)) + 
  geom_point() +
  labs(title = "Table 6: Number of Rides by Month and Rider Type") + 
  ylab("Number of Rides)") + 
  xlab("Month")

#code to create a weekday(jic)
mutate(weekday = wday(started_at, label = TRUE))

#equation for average duration in seconds
average_druation = mean(duration)

#equation for average duration in minutes
average_duration = mean(duration/60)

options(scipen=999)
