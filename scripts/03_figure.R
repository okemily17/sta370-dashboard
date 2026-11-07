#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearth")
library(tidyverse)
library(sf)
library(rnaturalearth)
library(freecurrencyapi)

# read in data 
eu_data <- read.csv('data/eu_data_clean.csv')

eu_data_2025 <- eu_data %>%
  filter(year == 2025)

# use rnaturallearth to get world country mapping data as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# filter for european countries 
europe <- world %>% 
  filter(continent == "Europe")

#left join map data to net_earnings by name of country 
eu_map_data <- europe %>%
  left_join(eu_data_2025, by = c("name"= "country"))

# Plot net earnings by country 
fill_variable = 'net_earnings_euro'
plot1 <- ggplot(eu_map_data) +
  geom_sf(aes(fill = !!sym(fill_variable)), color = "white") +
  scale_fill_viridis_c(name ="Net Earnings")+
  coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
  theme_void()+
  theme(
    plot.background = element_rect(fill = 'white'),
    plot.title = element_text(size = 20, face ="bold")
  )+
  labs(title = "Eu Countries by 2025 Annual Net Earnings in Euros",
       caption ="Source: Eurostat Annual Net Income for single person without children making 100% of average")

#plot percent of disposable income spent on rent by country 
plot2 <- ggplot(eu_map_data) +
  geom_sf(aes(fill = rent_percent), color = "white") +
  scale_fill_viridis_c(name ="Net Earnings")+
  coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
  theme_void()+
  theme(
    plot.background = element_rect(fill = 'white'),
    plot.title = element_text(size = 20, face ="bold")
  )+
  labs(title = "Eu Countries by 2025 Average share of dispoable income spent on rent",
       caption ="Source: Eurostat Share of rent related to occupied dwelling in dispoable household income for 1 adult households")

#bar plot of employment rates 2025 
plot3 <- ggplot(eu_data_2025, aes(y=reorder(country,employment_rates), x= employment_rates)) +
  geom_col(fill = "blue", color = "black") +
  labs(title="2025 Employment Rates",
       x= "Employmet Rate",
       y= "Country",
       caption ="Source: Eurostat Employment rates by educational attainment level")
width = 10
height = 8 
ggsave("output/NetEarning_map.png",plot1,width = width,height= height )
ggsave("output/Shareofrent_map.png",plot2, width= width, height = height)
