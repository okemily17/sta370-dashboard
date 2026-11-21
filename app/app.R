library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(freecurrencyapi)
library(ggiraph)
library(patchwork)

# The Data ---------------------------------------------------------------------------------------
eu_data <- read_csv("data/eu_data_clean.csv")
employment_type <- read.csv("data/employment_type_clean.csv")

eu_data_2025 <- eu_data %>%
  filter(year == 2025)

employment_type_2025 <- employment_type %>%
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

# options for bar fill for profession  
bar_fill_options <- list(science_engineering_professionals = "science_and_engineering_professionals_percent",
                         science_engineering_associate = "science_and_engineering_associate_percent",
                         information_tech_professionals = "information_tech_professionals_percent",
                         information_tech_associates = "information_tech_associate_percent",
                         legal_social_cultural_professionals = "legal_social_professionals_percent",
                         health_professionals = "health_professionals_percent",
                         hospitality_retail_managers = "hospitality_retail_percent"
                    )
bar_source = "Source: Eurostat Employed persons by detailed occupation code(ISCO -08 two digit level)
for ages 20 to 64. This data was modified in R to obtain the percent of employees by occupation from total employees. Eurostat is not responsibile for modifications to the data."
bar_title = "Percent of Total Employees employed in Profession"
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
  
  titlePanel("EU Countries Dashboard"),
  p("For: A single Adult considering moving to Europe. Average Income and rent shown are for single adult earner"),
  p("The decision: Where in Europe Could I live? "),
  
  selectInput(inputId = "map_col_value",label = "Map fill Value",choices = fill_options, selected = c("employment_rates")),
  
  selectInput(inputId = "bar_col_value", label = "Profession Type ", choice = bar_fill_options, selected = c("science_and_engineering_professionals")),
  
  p("Interactive Map. Hover over country to highlight on map and bar charts."),
  girafeOutput(outputId = "fig_2"),
  
  h5("What this dashboard cannot tell you"),
  p("the limits of your data.")
  
)


# The Server (how the figures are made) ----------------------------------------------------------
coloring = "magma" # sets fill scale on map 

title_text_size = 24
caption_text_size = 15
axis_text_size = 18
server <- function(input, output) {
  
  output$fig_2 <- renderGirafe({
    # get name of selected column, use to update title, legend, and sources 
    # when new fill option is selected 
    selected_name <- names(fill_options)[fill_options == input$map_col_value]
    # profession bar chart 
    selected_bar_name <- names(bar_fill_options)[bar_fill_options == input$bar_col_value]
    
    # make interactive bar chart 
    bar <- eu_data_2025 %>% filter(!is.na(.data[[input$map_col_value]])) %>%
        ggplot(aes(y=reorder(country,.data[[input$map_col_value]]), x= .data[[input$map_col_value]], tooltip = country, data_id = country)) +
        geom_col_interactive(fill = "blue", color = "black") +
        labs(title=graph_title[[selected_name]],
             x= selected_name,
             y= "Country",
             caption =sources[[selected_name]])+
      theme(
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = title_text_size, face ="bold"),
        plot.caption = element_text(size = caption_text_size),
        axis.title = element_text(size = axis_text_size)
      )
    # make interactive map, make sure data_id matches for linked highlighting 
    map <- ggplot(eu_map_data) +
      geom_sf_interactive(aes(fill = .data[[input$map_col_value]], tooltip = name, data_id = name), color = "white") +
      scale_fill_viridis_c(name = legend_labels[[selected_name]], option = coloring, direction = -1)+
      coord_sf(xlim = c(-25, 50), ylim = c(35, 70), expand = FALSE) +
      theme_void()+
      theme(
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = title_text_size, face ="bold"),
        plot.caption = element_text(size = caption_text_size),
        axis.title = element_text(size = axis_text_size)
      )+
      labs(title = graph_title[[selected_name]],
           caption = sources[[selected_name]])
    
    bar2 <- ggplot(employment_type_2025, aes(y=reorder(country,.data[[input$bar_col_value]]), x= .data[[input$bar_col_value]], tooltip = country, data_id = country)) +
      geom_col_interactive(fill = "red", color = "black") +
      labs(title=paste(bar_title," , ",selected_bar_name),
           x= "Percent",
           y= "Country",
           caption =bar_source) +
      theme(
        plot.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 20, face ="bold"),
        plot.caption = element_text(size = caption_text_size),
        axis.title = element_text(size = axis_text_size)
      )
    #plot combined charts, use patchwork to combine 
    girafe(
      ggobj = map + bar + bar2 ,
      width_svg = 36, # had to specify width, height, and point size to avoid error from default null value
      height_svg = 12,
      pointsize = 12,
      opts_hover(css = "fill:cyan;stroke:black;cursor:pointer;"),
      opts_selection(type = "single", css = "fill:red;stroke:black;")
    )
  })
}


# Run the App ------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)