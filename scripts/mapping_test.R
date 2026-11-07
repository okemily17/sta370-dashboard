#install.packages("sf")
#install.packages("rnaturalearth")
library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(freecurrencyapi)


loadEuroData <- function(filepath,sheet_num,column_names,min_row,max_row){
  data <- read_excel(filepath, sheet =sheet_num, skip=min_row, col_names = column_names)
  #filter out extra rows and blank columns 
  #change missing values to NA
  data <- data %>%
    filter(row_number() < max_row) %>%
    select(!starts_with("blank"))
  # convert net earnings to numeric column
  # will also force ":" missing values to NA
  data <- data %>% 
    mutate(across(starts_with("year"), as.numeric(.x)))
  
  # run three checks
  print(dim(data))
  print(names(data))
  print(summary(data))
  
  return(data)
}
file_path = "data/eu_annual_net_earnings.xlsx"

#import data
eu_country_names = c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
sheet_num <- 4 
skip_n <- 17
column_names <- c("country","year_2016","blank","year_2017","blank","year_2018","blank","year_2019","blank","year_2020","blank","year_2021","blank","year_2022","blank","year_2023","blank","year_2024","blank","year_2025","blank")
net_earnings <- read_excel('data/eu_annual_net_earnings.xlsx', sheet =sheet_num, skip=skip_n, col_names = column_names)
#filter out extra rows and blank columns 
#change missing values to NA
net_earnings <- net_earnings %>%
  filter(row_number() < 32) %>%
  select(!starts_with("blank")) %>%
  mutate(across(starts_with("year"), ~na_if(.x,":")))

# convert net earnings to numeric column
net_earnings <- net_earnings %>% 
  mutate(across(starts_with("year"), as.numeric))

#get 2025 net earnings
net_earnings_2025 <- net_earnings %>% 
  select("country","year_2025") %>%
  drop_na("year_2025")

# use rnaturallearth to get world country mapping data as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")
# filter for european countries 
europe <- world %>% 
  filter(continent == "Europe")

#left join map data to net_earnings by name of country 
europe_net_earnings <- europe %>%
  left_join(net_earnings_2025, by = c("name"= "country"))

# Plot with ggplot2
ggplot(europe_net_earnings) +
  geom_sf(aes(fill = year_2025), color = "white") +
  scale_fill_viridis_c(name ="Net Earnings")+
  coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
  theme_void()+
  labs(title = "Eu Countries by 2025 Annual Net earnings in Euros")

#bar plot of annual net earnings 2025 
ggplot(net_earnings_2025, aes(y=reorder(country,year_2025), x= year_2025)) +
  geom_col(fill = "blue", color = "black") +
  labs(title="2025 Average Annual Net Income in European Union",
       x= "2025 Average Net Income in Euros",
       y= "Country",
       caption ="Source: Eurostat Annual Net Income for single person without children making 100% of average")

current_currency ="EUR"
chosen_currency = "USD"

# get currency exchange rate from freecurrency api 
rates_payload <- get_latest_exchange_rates(base_currency = current_currency)
rates <- rates_payload$data
conversion_factor <- rates[[chosen_currency]]
# create earnings in 2025 column 
net_earnings_2025 <- net_earnings_2025 %>% 
  mutate(year_2025_usd = year_2025 * conversion_factor)

#bar plot of annual net earnings 2025 
ggplot(net_earnings_2025, aes(y=reorder(country,year_2025_usd), x= year_2025)) +
  geom_col(fill = "blue", color = "black") +
  labs(title="2025 Average Annual Net Income in European Union",
       x= "2025 Average Net Income in USD",
       y= "Country",
       caption ="Source: Eurostat Annual Net Income for single person without children making 100% of average")
