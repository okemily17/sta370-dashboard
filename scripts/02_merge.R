# load in clean data 
net_earnings_euro <- read.csv("data/net_earnings_euro_clean.csv")
net_earnings_pps <- read.csv( "data/net_earnings_pps_clean.csv")
rent_percent <- read.csv("data/rent_percent_clean.csv")
employment_rates <- read.csv ("data/employment_rates_clean.csv")

# merge 
# check country and year key 
net_earnings_euro %>%
  count(country,year) %>%
  filter(n > 1)
net_earnings_pps %>%
  count(country,year) %>%
  filter(n > 1)
rent_percent %>%
  count(country,year) %>%
  filter(n > 1)
employment_rates %>%
  count(country,year) %>%
  filter(n > 1)

# left join with employment rates since it contain data for highest number of countries 
# missing values for non-eu countries in 
eu_data <- employment_rates %>% 
  left_join(rent_percent, by = c("country","year")) %>%
  left_join(net_earnings_euro, by = c("country","year")) %>%
  left_join(net_earnings_pps, by = c("country","year"))

write_csv(eu_data,"data/eu_data_clean.csv")
