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
               "data/eu_employment_rates.xlsx",
               'data/eu_employment_by_profession.xlsx')

column_names <- c("country","year_2016","blank","year_2017","blank","year_2018","blank","year_2019","blank","year_2020","blank","year_2021","blank","year_2022","blank","year_2023","blank","year_2024","blank","year_2025","blank")
# load data, filepath, value, sheet number, column names, row skip value, number of rows to keep
# average annual net earnings for single adult with no children
net_earnings_euro <- loadEuroData(file_paths[1],"net_earnings_euro",4,column_names,17,32)
net_earnings_pps <- loadEuroData(file_paths[2],"net_earnings_pps",4,column_names,17,32)

# share of rent as percent of disposable income for one adult 
rent_percent <- loadEuroData(file_paths[3],"rent_percent",2,column_names,12,32)
# employment rates for 20-64 age range 
employment_rates <- loadEuroData(file_paths[4],"employment_rates",2,column_names,15,32)

# total employees by country (age 20 to 64) in thousands of people 
total_employment <- loadEuroData(file_paths[5],"total_employees",2,column_names,15,32)
# load sheets specifying total employment by occupation code 
hospitality_employees <- loadEuroData(file_paths[5],"hospitality_retail",3,column_names,15,32)
stem_employees <- loadEuroData(file_paths[5],"science_and_engineering_professionals",4,column_names,15,32)
health_employees <- loadEuroData(file_paths[5],"health_professionals",5,column_names,15,32)
information_employees <- loadEuroData(file_paths[5],"information_tech_professionals",6,column_names,15,32)
legal_social_employees <- loadEuroData(file_paths[5],"legal_social_professionals",7,column_names,15,32)
stem_associate_employees <- loadEuroData(file_paths[5],"science_and_engineering_associate",9,column_names,15,32)
information_tech_employees <- loadEuroData(file_paths[5],"information_tech_associate",11,column_names,15,32)

# merge employment type data, 
#merge here instead of merge file so can save employment_type as one df
# check country and year key 
total_employment %>%
  count(country,year) %>%
  filter(n > 1)
hospitality_employees %>%
  count(country,year) %>%
  filter(n > 1)
stem_employees %>%
  count(country,year) %>%
  filter(n > 1)
health_employees %>%
  count(country,year) %>%
  filter(n > 1)
information_employees %>%
  count(country,year) %>%
  filter(n > 1)
legal_social_employees %>%
  count(country,year) %>%
  filter(n > 1)
stem_associate_employees %>%
  count(country,year) %>%
  filter(n > 1)
information_tech_employees %>%
  count(country,year) %>%
  filter(n > 1)

# left join to total employees 
employment_type <- total_employment %>% 
  left_join(hospitality_employees, by = c("country","year")) %>%
  left_join(stem_employees, by = c("country","year")) %>%
  left_join(health_employees, by = c("country","year")) %>%
  left_join(information_employees, by = c("country","year")) %>%
  left_join(legal_social_employees , by = c("country","year")) %>%
  left_join(stem_associate_employees, by = c("country","year")) %>%
  left_join(information_tech_employees, by = c("country","year"))

# create benchmark rates by grouping by year, summing professionals of all country by column then dividing by total employees
employment_type_benchmark_rates <- employment_type %>%
  group_by(year) %>%
  # find total employees by type each year 
  summarize(across(2:last_col(), \(x) sum(x, na.rm = TRUE))) %>%
  # find rates by type for each year 
  mutate(across(3:last_col(),~.x /total_employees))

# add rate column for each employment type 
employment_type <- employment_type %>% 
  mutate(across(3:last_col(),~.x /total_employees, .names = "{.col}_percent"))

employment_type <- employment_type %>% 
  mutate(across(3:last_col(), ~.x * 100))
  
# save data 
write_csv(net_earnings_euro, "data/net_earnings_euro_clean.csv")
write_csv(net_earnings_pps, "data/net_earnings_pps_clean.csv")
write_csv(rent_percent, "data/rent_percent_clean.csv")
write_csv(employment_rates, "data/employment_rates_clean.csv")
write_csv(employment_type, "data/employment_type_clean.csv")
write_csv(employment_type, "app/data/employment_type_clean.csv")
write_csv(employment_type_benchmark_rates, "data/employment_type_benchmark_clean.csv")
write_csv(employment_type_benchmark_rates, "app/data/employment_type_benchmark_clean.csv")