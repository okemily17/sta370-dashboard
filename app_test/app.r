library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(freecurrencyapi)

# load in data 
eu_data <- read.csv('data/eu_data_clean.csv')

# get currency exchange rate from free currency api 
# rates_payload <- get_latest_exchange_rates(base_currency = "EUR")
# rates <- rates_payload$data
# conversion_factor <- rates[["USD"]]
# 
# # create earnings in usd column  
# eu_data <- eu_data %>% 
#   mutate(net_earnings_usd = net_earnings_euro * conversion_factor)
# 
eu_data_2025 <- eu_data %>%
   filter(year == 2025)

eu_column_names <- colnames(eu_data_2025)

ui <- fluidPage(
  selectInput("map_col_value","Value", choices = eu_column_names, selected = c("country","employment_rates"), multiple = TRUE),
  selectInput("sortby_col", "Sort by", choices = eu_column_names, selected = "country"),
  tableOutput("selection"),
  tableOutput("fixed")
 )
 
 server <- function(input,output, session){
 # display table with selected column values 
 output$selection <- renderTable({
   eu_data_2025 %>% 
     arrange(desc(across(all_of(input$sortby_col)))) %>%
     select(all_of(input$map_col_value))
 }, rownames = TRUE)
 }
 
 shinyApp(ui,server)