#install.packages("sf")
#install.packages("rnaturalearth")
#install.packages("rnaturalearth")
#install.packages("ggiraph")
library(tidyverse)
library(sf)
library(rnaturalearth)
library(freecurrencyapi)
library(ggiraph)
library(patchwork)

# read in data 
eu_data <- read.csv('data/eu_data_clean.csv')

eu_data_2025 <- eu_data %>%
  filter(year == 2025)

# use rnaturallearth to get world country mapping data as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# filter for European countries 
europe <- world %>% 
  filter(continent == "Europe")

#left join map data to net_earnings by name of country 
eu_map_data <- europe %>%
  left_join(eu_data_2025, by = c("name"= "country"))

coloring = "magma"
# Plot net earnings by country 
fill_variable = 'net_earnings_euro'
plot1 <- ggplot(eu_map_data) +
  geom_sf(aes(fill = !!sym(fill_variable)), color = "white") +
  scale_fill_viridis_c(name ="Net Earnings", option = coloring, direction = -1)+
  coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
  theme_void()+
  theme(
    plot.background = element_rect(fill = 'white'),
    plot.title = element_text(size = 20, face ="bold")
  )+
  labs(title = "Eu Countries by 2025 Annual Net Earnings in Euros",
       caption ="Source: Eurostat Annual Net Income for single person without children making 100% of average. Grayed out countries are not in the EU.")

#plot percent of disposable income spent on rent by country 
plot2 <- ggplot(eu_map_data) +
  geom_sf(aes(fill = rent_percent), color = "white") +
  scale_fill_viridis_c(name ="Percent spent", option = coloring, direction = -1)+
  coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
  theme_void()+
  theme(
    plot.background = element_rect(fill = 'white'),
    plot.title = element_text(size = 20, face ="bold")
  )+
  labs(title = "Eu Countries by 2025 Average share of dispoable income spent on rent",
       caption ="Source: Eurostat Share of rent related to occupied dwelling in dispoable household income for 1 adult households. Grayed out countries are not in the EU.")

#bar plot of employment rates 2025 
plot3 <- ggplot(eu_data_2025, aes(y=reorder(country,net_earnings_euro), x= net_earnings_euro)) +
  geom_col(fill = "blue", color = "black") +
  labs(title="2025 Employment Rates",
       x= "Employmet Rate",
       y= "Country",
       caption ="Source: Eurostat Employment rates by educational attainment level")

# interactive scatter plot 
plot4 <- ggplot(eu_data_2025, aes(x= net_earnings_euro, y= rent_percent, tooltip = country, data_id = country)) +
  geom_point_interactive(hover_nearest = TRUE, size = 3) +
  labs(title="2025 Average Net Earnings vs Average percent of disposable income spent on rent",
       x= "Net Earnings",
       y= "Rent Percent",
       caption ="Source: Eurostat")

# interactive bar plot
plot5 <- ggplot(eu_data_2025, aes(y=reorder(country,net_earnings_euro), x= net_earnings_euro, tooltip = country, data_id = country)) +
  geom_col_interactive(fill = "blue", color = "black") +
  labs(title="2025 ",
       x= "Annual Net Earnings",
       y= "Country",
       caption ="Source: Eurostat Employment rates by educational attainment level")
interactive_plot_2 <- girafe(ggobj = plot5)

#combined interactive plot
combined_plot <- plot4 + plot5 +
  plot_layout(ncol = 2)

# Create a single interactive plot with both subplots
interactive_plot <- girafe(ggobj = combined_plot)

# Set options for the interactive plot
interactive_plot_combined <- girafe_options(
  interactive_plot,
  opts_hover(css = "fill:cyan;stroke:black;cursor:pointer;"),
  opts_selection(type = "single", css = "fill:red;stroke:black;")
)

width = 10
height = 8 
#ggsave("output/NetEarning_map.png",plot1,width = width,height= height )
#ggsave("output/Shareofrent_map.png",plot2, width= width, height = height)
