#import data
library(tidyverse)
library(dplyr)
library(readxl)
library(stringr)

loadEuroData <- function(filepath,value,sheet_num,column_names,min_row,max_row){
  data <- read_excel(filepath, sheet =sheet_num, skip=min_row, col_names = column_names)
  #filter out extra rows and blank columns 
  data <- data %>%
    filter(row_number() < max_row) %>%
    select(!starts_with("blank"))
  # convert net earnings to numeric column
  # will also force ":" missing values to NA
  data <- data %>% 
    mutate(across(starts_with("year"), ~ as.numeric(.x)))
  # run three checks
  print(dim(data))
  print(names(data))
  print(summary(data))
  # pivot so each country and year observation is a separate row 
  # drop na values for non eu countries 
  data <- data %>% pivot_longer(
    cols = starts_with("year"),
    names_to ="year" ,
    values_to = value, 
  ) %>% 
    drop_na()
  # remove year_ prefix from year column and convert to numeric  
  data <- data %>%
    mutate(year = as.numeric(str_remove(year, "^year_")))
  
  return(data)
}

file_paths = c("data/eu_annual_net_earnings.xlsx",
               "data/eu_annual_net_earning_pps.xlsx",
               "data/eu_rent_percent_disposable_income.xlsx",
               "data/eu_employment_rates.xlsx")

column_names <- c("country","year_2016","blank","year_2017","blank","year_2018","blank","year_2019","blank","year_2020","blank","year_2021","blank","year_2022","blank","year_2023","blank","year_2024","blank","year_2025","blank")
# load data, filepath, value, sheet number, column names, row skip value, number of rows to keep
# average annual net earnings for single adult with no children
net_earnings_euro <- loadEuroData(file_paths[1],"net_earnings_euro",4,column_names,17,32)
net_earnings_pps <- loadEuroData(file_paths[2],"net_earnings_pps",4,column_names,17,32)

# share of rent as percent of disposable income for one adult 
rent_percent <- loadEuroData(file_paths[3],"rent_percent",2,column_names,12,32)
# employment rates for 20-64 age range 
employment_rates <- loadEuroData(file_paths[4],"employment_rates",2,column_names,15,32)

# save data 
write_csv(net_earnings_euro, "data/net_earnings_euro_clean.csv")
write_csv(net_earnings_pps, "data/net_earnings_pps_clean.csv")
write_csv(rent_percent, "data/rent_percent_clean.csv")
write_csv(employment_rates, "data/employment_rates_clean.csv")
