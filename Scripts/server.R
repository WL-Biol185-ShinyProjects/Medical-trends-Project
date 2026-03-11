# server.R - Server Logic for Life Expectancy & Pesticides Dashboard

library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)

# =============================================================================
# SERVER FUNCTION
# =============================================================================

function(input, output, session) {
  
  # ===========================================================================
  # LOAD EXTERNAL DATA
  # ===========================================================================
  
  # Load data
  Parkinson_Data <- read.csv("Parkinson_mortality_rates_clean.csv", stringsAsFactors = FALSE)

  Pesticide_County_Data <- read.csv("pesticides_by_county.csv", stringsAsFactors = FALSE)
  
  Expectancy_Data <- read.csv("LifeExpectancyStateData_clean.csv", stringsAsFactors = FALSE)
  
  Expectancy_State_Data <- read.csv("LifeExpectancyStateData_clean.csv", stringsAsFactors = FALSE)
  
  Farm_Data <- read.csv("Farm_Data_2024.csv", stringsAsFactors = FALSE)

  # ===========================================================================
  # MAPS TAB OUTPUTS
  # ===========================================================================
  
  output$map <- renderLeaflet({
    pal <- colorNumeric(
      palette = "RdYlGn", 
      domain = states_data$Life_Expectancy,
      reverse = TRUE
    )
    
    leaflet(states_data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Pesticide_Use_kg_per_ha) * 4,
        color = ~pal(Life_Expectancy),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = ~paste(
          "<div style='font-family: Inter, sans-serif;'>",
          "<h4 style='margin: 0 0 10px 0; color: #2d5016;'>", State, "</h4>",
          "<strong>Life Expectancy:</strong> ", round(Life_Expectancy, 1), " years<br>",
          "<strong>Pesticide Use:</strong> ", round(Pesticide_Use_kg_per_ha, 2), " kg/ha<br>",
          "<strong>Population:</strong> ", format(Population, big.mark = ","), "<br>",
          "<strong>Agricultural Area:</strong> ", round(Agricultural_Area_pct, 1), "%",
          "</div>"
        ),
        label = ~State
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Life_Expectancy,
        title = "Life Expectancy<br>(years)",
        opacity = 0.8
      )
  })
  