library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(freecurrencyapi)
library(ggiraph)
library(patchwork)

# The Data ---------------------------------------------------------------------------------------
eu_data <- read_csv("data/eu_data_clean.csv")
eu_data_2025 <- eu_data %>%
  filter(year == 2025)

eu_column_names <- colnames(eu_data_2025)

# options for map fill 
fill_options <- list(rent = 'rent_percent', earnings = 'net_earnings_euro', employment_rate ='employment_rates' )
legend_labels <- list(rent = 'Percent spent', earnings = 'Net Earnings Euros', employment_rate ='employment rate' )
# graph titles
graph_title <- list(rent = 'Eu countries by Average Share of Disposable Income Spent on rent', 
                    earnings = 'Eu countries by Annual Net Earnings in euros', 
                    employment_rate ='Eu Countries by Employment rate' )
#sources 
sources <- list(rent ="Source: Eurostat Share of rent related to occupied dwelling in dispoable household income for 1 adult households. Grayed out countries are not in the EU.",
                earnings = 'Source: Eurostat Annual Net Income for single person without children making 100% of average. Grayed out countries are not in the EU.',
                employment_rate = "Source: Eurostat Employment rates by educational attainment level")

# get mapping data 
# use rnaturallearth to get world country mapping data as an sf object
world <- ne_countries(scale = "medium", returnclass = "sf")

# filter for European countries 
europe <- world %>% 
  filter(continent == "Europe")

#left join map data to net_earnings by name of country 
eu_map_data <- europe %>%
  left_join(eu_data_2025, by = c("name"= "country"))

# The UI (what the reader sees) ------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("your title"),
  p("For: who is making the decision"),
  p("The decision: the one decision this dashboard helps with"),
  
  selectInput(inputId = "map_col_value",label = "Value",choices = fill_options, selected = c("employment_rates")),
  
  #h3("Figure 1. title"),
  #plotOutput(outputId = "fig_1"),
  #p("a sentence or two saying what the figure shows."),
  
  h3("Figure 2. title"),
  girafeOutput(outputId = "fig_2"),
  p("a sentence or two saying what the figure shows."),
  
  h3("What this dashboard cannot tell you"),
  p("the limits of your data.")
  
)


# The Server (how the figures are made) ----------------------------------------------------------
coloring = "magma" # sets fill scale on map 

server <- function(input, output) {
  
  #output$fig_1 <- renderPlot({
    # get name of selected column, use to update title, legend, and sources 
    # when new fill option is selected 
   # selected_name <- names(fill_options)[fill_options == input$map_col_value]
    # plot map of Eu countries by user selected value
    #ggplot(eu_map_data) +
     # geom_sf(aes(fill = .data[[input$map_col_value]]), color = "white") +
      #scale_fill_viridis_c(name = legend_labels[[selected_name]], option = coloring, direction = -1)+
      #coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
      #theme_void()+
      #theme(
       # plot.background = element_rect(fill = 'white'),
      #  plot.title = element_text(size = 20, face ="bold")
      #)+
      #labs(title = graph_title[[selected_name]],
       #    caption = sources[[selected_name]])
    
  #})
  
  output$fig_2 <- renderGirafe({
    # get name of selected column, use to update title, legend, and sources 
    # when new fill option is selected 
    selected_name <- names(fill_options)[fill_options == input$map_col_value]
    # make interactive bar chart 
    bar <- ggplot(eu_data_2025, aes(y=reorder(country,.data[[input$map_col_value]]), x= .data[[input$map_col_value]], tooltip = country, data_id = country)) +
        geom_col_interactive(fill = "blue", color = "black") +
        labs(title=graph_title[[selected_name]],
             x= selected_name,
             y= "Country",
             caption =sources[[selected_name]])
    # make interactive map, make sure data_id matches for linked highlighting 
    map <- ggplot(eu_map_data) +
      geom_sf_interactive(aes(fill = .data[[input$map_col_value]], tooltip = name, data_id = name), color = "white") +
      scale_fill_viridis_c(name = legend_labels[[selected_name]], option = coloring, direction = -1)+
      coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
      theme_void()+
      theme(
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 20, face ="bold")
      )+
      labs(title = graph_title[[selected_name]],
           caption = sources[[selected_name]])
    
    #plot combined charts, use patchwork to combine 
    girafe(
      ggobj = map + bar ,
      width_svg = 16, # had to specify width, height, and point size to avoid error from default null value
      height_svg = 8,
      pointsize = 12,
      opts_hover(css = "fill:cyan;stroke:black;cursor:pointer;"),
      opts_selection(type = "single", css = "fill:red;stroke:black;")
    )
  })
  
}


# Run the App ------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)