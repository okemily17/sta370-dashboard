#import data
library(tidyverse)
library(dplyr)
library(readxl)
# read in national fatal crash data by vehicle type
column_names <- c("year","n","rate_per_100m_VMT","rate_per_100k_vehicles", "n","rate_per_100m_VMT","rate_per_100k_vehicles", "n","rate_per_100m_VMT","rate_per_100k_vehicles", "n","rate_per_100m_VMT","rate_per_100k_vehicles")
crash_data <- read_excel(file.path("data","TSFTable3.xlsx"),range = cell_rows(8:57), col_names = column_names )

# split into tables for each separate vehicle type, then add vehicle_type col
group_1 <- crash_data[,c(1,2,3,4)] %>% mutate(vehicle_type ="Passenger Car")
group_2 <- crash_data[,c(1,5,6,7)] %>% mutate(vehicle_type ="Light Truck")
group_3 <- crash_data[,c(1,8,9,10)] %>% mutate(vehicle_type ="Large Truck")
group_4 <- crash_data[,c(1,11,12,13)] %>% mutate(vehicle_type ="Motorcycles")
# Merge back into 1 and convert to df
national_crash <- bind_rows(group_1 ,group_2 ,group_3 ,group_4)
national_crash <- as.data.frame(national_crash)

#checks 
dim(national_crash)
names(national_crash)
summarise(national_crash)
national_crash %>% count(vehicle_type)

national_crash %>% filter(year == 2024) %>%
  ggplot(aes(x=vehicle_type, y=rate_per_100m_VMT)) +
  geom_col(aes(fill=vehicle_type))+
  theme_classic()

# read in NJ fatal crash data by vehicle type 
crash_data <- read_excel(file.path("data","NJ_crash_data.xlsx"),range = cell_rows(4:34), col_names = column_names )
#split into tables for each separate vehicle type, then add vehicle_type col
# get only year and n from data since all other columns contain only NA values 
group_1 <- crash_data[,c(1,2)] %>% mutate(vehicle_type ="Passenger Car")
group_2 <- crash_data[,c(1,5)] %>% mutate(vehicle_type ="Light Truck")
group_3 <- crash_data[,c(1,8)] %>% mutate(vehicle_type ="Large Truck")
group_4 <- crash_data[,c(1,11)] %>% mutate(vehicle_type ="Motorcycles")
# Merge back into 1 and convert to df
nj_crash <- bind_rows(group_1 ,group_2 ,group_3 ,group_4)
nj_crash <- as.data.frame(nj_crash)

#checks 
dim(nj_crash)
names(nj_crash)
summarise(nj_crash)
national_crash %>% count(vehicle_type)

