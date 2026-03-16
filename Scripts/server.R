
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
  
  # Load Parkinson's mortality data (by state)
  Parkinson_Data <- reactive({
    tryCatch({
      data <- read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/Parkinsons_mortality_rates_clean.csv", stringsAsFactors = FALSE)
      # Rename Location to State if needed
      if("Location" %in% names(data)) {
        data$State <- data$Location
      }
      return(data)
    }, error = function(e) {
      showNotification("Error loading Parkinsons_mortality_rates_clean.csv", 
                       type = "error", duration = NULL)
      return(NULL)
    })
  })
  
  # Load Pesticide data by county
  Pesticide_County_Data <- reactive({
    tryCatch({
      read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/pesticides_by_county.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading pesticides_by_county.csv", 
                       type = "error", duration = NULL)
      return(NULL)
    })
  })
  
  # Load Life Expectancy by State
  Expectancy_State_Data <- reactive({
    tryCatch({
      read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/LifeExpectancyStateData_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading LifeExpectancyStateData_clean.csv", 
                       type = "error", duration = NULL)
      return(NULL)
    })
  })
  
  # Load Life Expectancy by County
  Expectancy_Data <- reactive({
    tryCatch({
      read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/ExpectancyData_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading ExpectancyData_clean.csv", 
                       type = "error", duration = NULL)
      return(NULL)
    })
  })
  
  # Load Farm data by state
  Farm_Data <- reactive({
    tryCatch({
      read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/Farm_Data_2024.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading Farm_Data_2024.csv", 
                       type = "error", duration = NULL)
      return(NULL)
    })
  })
  
  # ===========================================================================
  # GET STATE COORDINATES
  # ===========================================================================
  
  state_coords <- data.frame(
    State = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
              "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
              "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"),
    Latitude = c(64.2, 32.3, 34.7, 34.0, 36.8, 39.5, 41.6, 38.9, 27.8, 32.2,
                 21.3, 42.0, 44.0, 40.6, 40.3, 38.5, 37.8, 30.4, 42.4, 39.0,
                 45.2, 44.3, 46.4, 38.6, 32.3, 46.9, 35.8, 47.5, 41.5, 43.2,
                 40.2, 34.5, 38.8, 43.0, 40.4, 35.5, 44.0, 41.2, 41.8, 34.0,
                 44.4, 35.8, 31.0, 39.3, 37.5, 44.0, 47.5, 44.5, 38.6, 43.0),
    Longitude = c(-152.4, -86.9, -92.4, -111.1, -119.4, -105.8, -72.7, -75.5, -81.7, -83.4,
                  -157.8, -93.5, -114.7, -89.4, -86.1, -98.4, -84.3, -92.3, -71.4, -76.6,
                  -69.4, -85.6, -94.7, -92.2, -89.7, -110.0, -78.6, -100.4, -99.9, -71.6,
                  -74.5, -106.0, -117.0, -75.5, -82.9, -97.5, -120.5, -77.2, -71.5, -81.0,
                  -100.4, -86.4, -97.5, -111.7, -78.7, -72.6, -120.5, -89.5, -80.5, -107.5)
  )
  
  # ===========================================================================
  # PROCESS AND MERGE DATA FOR STATE-LEVEL ANALYSIS
  # ===========================================================================
  
  # Aggregate pesticide data to state level
  Pesticide_State_Data <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    
    # Group by state and calculate average pesticide use
    state_pest <- data %>%
      group_by(state_name) %>%
      summarise(
        Avg_Pesticide = mean((LOW_ESTIMATE + HIGH_ESTIMATE) / 2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(State = state_name)
    
    return(state_pest)
  })
  
  # Merge Parkinson's with state coordinates
  Parkinson_With_Coords <- reactive({
    req(Parkinson_Data())
    data <- Parkinson_Data()
    
    # Merge with coordinates
    merged <- data %>%
      left_join(state_coords, by = "State")
    
    return(merged)
  })
  
  # Merge Farm data with coordinates  
  Farm_With_Coords <- reactive({
    req(Farm_Data())
    data <- Farm_Data()
    
    # Merge with coordinates
    merged <- data %>%
      left_join(state_coords, by = "State")
    
    return(merged)
  })
  
  # Merge Life Expectancy with coordinates
  Expectancy_With_Coords <- reactive({
    req(Expectancy_State_Data())
    data <- Expectancy_State_Data()
    
    # Merge with coordinates
    merged <- data %>%
      left_join(state_coords, by = "State")
    
    return(merged)
  })
  
  # Combined: Parkinson's + Pesticides (State Level)
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_With_Coords(), Pesticide_State_Data())
    
    parkinson <- Parkinson_With_Coords()
    pesticide <- Pesticide_State_Data()
    
    merged <- parkinson %>%
      left_join(pesticide, by = "State")
    
    return(merged)
  })
  
  # Combined: Parkinson's + Farms (State Level)
  Parkinson_Farm_State <- reactive({
    req(Parkinson_With_Coords(), Farm_With_Coords())
    
    parkinson <- Parkinson_With_Coords()
    farms <- Farm_With_Coords()
    
    # Select relevant columns from farms to avoid duplication
    farms_select <- farms %>%
      select(State, Number_Of_Farms, Area_operated_Acres, Acres_Operated_Millions)
    
    merged <- parkinson %>%
      left_join(farms_select, by = "State")
    
    return(merged)
  })
  
  # ===========================================================================
  # HOME TAB OUTPUTS
  # ===========================================================================
  
  output$stats_boxes <- renderUI({
    req(Parkinson_Data(), Farm_Data())
    
    avg_death_rate <- round(mean(Parkinson_Data()$Avg_Death_Rate, na.rm = TRUE), 2)
    total_farms <- format(sum(Farm_Data()$Number_Of_Farms, na.rm = TRUE), big.mark = ",")
    avg_life_exp <- round(mean(Expectancy_State_Data()$Avg_Life_Expectancy, na.rm = TRUE), 1)
    states_count <- nrow(Parkinson_Data())
    
    div(class = "stats-row",
        div(class = "stat-box",
            div(class = "stat-number", avg_death_rate),
            div(class = "stat-label", "Avg Parkinson's Death Rate")
        ),
        div(class = "stat-box",
            div(class = "stat-number", total_farms),
            div(class = "stat-label", "Total U.S. Farms")
        ),
        div(class = "stat-box",
            div(class = "stat-number", avg_life_exp),
            div(class = "stat-label", "Avg Life Expectancy (years)")
        ),
        div(class = "stat-box",
            div(class = "stat-number", states_count),
            div(class = "stat-label", "States Analyzed")
        )
    )
  })
  
  # ===========================================================================
  # MAP 1: PARKINSON'S VS PESTICIDES (STATE LEVEL)
  # ===========================================================================
  
  output$map_parkinson_pesticide <- renderLeaflet({
    req(Parkinson_Pesticide_State())
    data <- Parkinson_Pesticide_State()
    
    # Remove rows with missing coordinates
    data <- data %>% filter(!is.na(Latitude), !is.na(Longitude))
    
    if(nrow(data) == 0) return(NULL)
    
    # Color by Parkinson's death rate
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = data$Avg_Death_Rate
    )
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Avg_Pesticide) / 50,
        color = ~pal(Avg_Death_Rate),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = ~paste(
          "<div style='font-family: Inter, sans-serif;'>",
          "<h4 style='margin: 0 0 10px 0; color: #8B4513;'>", State, "</h4>",
          "<strong>Parkinson's Death Rate:</strong> ", round(Avg_Death_Rate, 2), "<br>",
          "<strong>Avg Pesticide Use:</strong> ", round(Avg_Pesticide, 2), " lbs<br>",
          "</div>"
        ),
        label = ~State
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Avg_Death_Rate,
        title = "Parkinson's<br>Death Rate",
        opacity = 0.8
      )
  })
  
  # ===========================================================================
  # MAP 2: PARKINSON'S VS FARMS (STATE LEVEL)
  # ===========================================================================
  
  output$map_parkinson_farms <- renderLeaflet({
    req(Parkinson_Farm_State())
    data <- Parkinson_Farm_State()
    
    # Remove rows with missing coordinates
    data <- data %>% filter(!is.na(Latitude), !is.na(Longitude))
    
    if(nrow(data) == 0) return(NULL)
    
    # Color by Parkinson's death rate
    pal <- colorNumeric(
      palette = "RdPu",
      domain = data$Avg_Death_Rate
    )
    
    leaflet(data) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Number_Of_Farms) / 200,
        color = ~pal(Avg_Death_Rate),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = ~paste(
          "<div style='font-family: Inter, sans-serif;'>",
          "<h4 style='margin: 0 0 10px 0; color: #8B4513;'>", State, "</h4>",
          "<strong>Parkinson's Death Rate:</strong> ", round(Avg_Death_Rate, 2), "<br>",
          "<strong>Number of Farms:</strong> ", format(Number_Of_Farms, big.mark = ","), "<br>",
          "<strong>Acres Operated:</strong> ", round(Acres_Operated_Millions, 2), " million<br>",
          "</div>"
        ),
        label = ~State
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~Avg_Death_Rate,
        title = "Parkinson's<br>Death Rate",
        opacity = 0.8
      )
  })
  
  # ===========================================================================
  # MAP 3: PESTICIDES VS LIFE EXPECTANCY (COUNTY LEVEL)
  # ===========================================================================
  
  output$map_pesticide_life_expectancy <- renderLeaflet({
    req(Pesticide_County_Data(), Expectancy_Data())
    
    # This would require county-level coordinates
    # For now, showing a message
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addMarkers(lng = -98.5795, lat = 39.8283,
                 popup = "County-level map requires county coordinates.<br>Please add county lat/long data.")
  })
  
  # ===========================================================================
  # PLOT 1: PARKINSON'S VS PESTICIDES SCATTER
  # ===========================================================================
  
  output$plot_parkinson_pesticide <- renderPlotly({
    req(Parkinson_Pesticide_State())
    data <- Parkinson_Pesticide_State()
    
    data <- data %>% filter(!is.na(Avg_Death_Rate), !is.na(Avg_Pesticide))
    
    if(nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(
      x = Avg_Pesticide,
      y = Avg_Death_Rate,
      text = paste(
        "State:", State,
        "<br>Death Rate:", round(Avg_Death_Rate, 2),
        "<br>Pesticide Use:", round(Avg_Pesticide, 2)
      )
    )) +
      geom_point(size = 3, alpha = 0.7, color = "#e74c3c") +
      geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linetype = "dashed") +
      labs(
        title = "Parkinson's Death Rate vs. Pesticide Use by State",
        x = "Average Pesticide Use (lbs)",
        y = "Parkinson's Death Rate"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ===========================================================================
  # PLOT 2: PARKINSON'S VS FARMS SCATTER
  # ===========================================================================
  
  output$plot_parkinson_farms <- renderPlotly({
    req(Parkinson_Farm_State())
    data <- Parkinson_Farm_State()
    
    data <- data %>% filter(!is.na(Avg_Death_Rate), !is.na(Number_Of_Farms))
    
    if(nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(
      x = Number_Of_Farms,
      y = Avg_Death_Rate,
      text = paste(
        "State:", State,
        "<br>Death Rate:", round(Avg_Death_Rate, 2),
        "<br>Farms:", format(Number_Of_Farms, big.mark = ",")
      )
    )) +
      geom_point(size = 3, alpha = 0.7, color = "#9b59b6") +
      geom_smooth(method = "lm", se = TRUE, color = "darkblue", linetype = "dashed") +
      labs(
        title = "Parkinson's Death Rate vs. Number of Farms by State",
        x = "Number of Farms",
        y = "Parkinson's Death Rate"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ===========================================================================
  # PLOT 3: PESTICIDES VS LIFE EXPECTANCY SCATTER (STATE LEVEL)
  # ===========================================================================
  
  output$plot_pesticide_life_expectancy <- renderPlotly({
    req(Pesticide_State_Data(), Expectancy_With_Coords())
    
    pesticide <- Pesticide_State_Data()
    life_exp <- Expectancy_With_Coords()
    
    # Merge
    data <- life_exp %>%
      left_join(pesticide, by = "State") %>%
      filter(!is.na(Avg_Life_Expectancy), !is.na(Avg_Pesticide))
    
    if(nrow(data) == 0) return(NULL)
    
    p <- ggplot(data, aes(
      x = Avg_Pesticide,
      y = Avg_Life_Expectancy,
      text = paste(
        "State:", State,
        "<br>Life Expectancy:", round(Avg_Life_Expectancy, 2),
        "<br>Pesticide Use:", round(Avg_Pesticide, 2)
      )
    )) +
      geom_point(size = 3, alpha = 0.7, color = "#27ae60") +
      geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
      labs(
        title = "Life Expectancy vs. Pesticide Use by State",
        x = "Average Pesticide Use (lbs)",
        y = "Average Life Expectancy (years)"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p, tooltip = "text")
  })
  
  # ===========================================================================
  # ADDITIONAL VISUALIZATIONS
  # ===========================================================================
  
  # Bar chart: Top states by Parkinson's death rate
  output$plot_top_parkinsons <- renderPlotly({
    req(Parkinson_Data())
    data <- Parkinson_Data() %>%
      arrange(desc(Avg_Death_Rate)) %>%
      head(15)
    
    p <- ggplot(data, aes(x = reorder(State, Avg_Death_Rate), y = Avg_Death_Rate)) +
      geom_bar(stat = "identity", fill = "#e74c3c", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Top 15 States by Parkinson's Death Rate",
        x = "State",
        y = "Death Rate"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # Bar chart: Top states by number of farms
  output$plot_top_farms <- renderPlotly({
    req(Farm_Data())
    data <- Farm_Data() %>%
      arrange(desc(Number_Of_Farms)) %>%
      head(15)
    
    p <- ggplot(data, aes(x = reorder(State, Number_Of_Farms), y = Number_Of_Farms)) +
      geom_bar(stat = "identity", fill = "#27ae60", alpha = 0.8) +
      coord_flip() +
      labs(
        title = "Top 15 States by Number of Farms",
        x = "State",
        y = "Number of Farms"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # ===========================================================================
  # DATA TABLES
  # ===========================================================================
  
  output$data_table_parkinson <- renderDT({
    req(Parkinson_Data())
    
    datatable(
      Parkinson_Data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(columns = c("Avg_Death_Rate", "Avg_Deaths"), digits = 2)
  })
  
  output$data_table_farms <- renderDT({
    req(Farm_Data())
    
    datatable(
      Farm_Data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(columns = c("Area_operated_Acres", "Acres_Operated_Millions"), digits = 2)
  })
  
  # ===========================================================================
  # DOWNLOAD HANDLERS
  # ===========================================================================
  
  output$download_combined <- downloadHandler(
    filename = function() {
      paste("medical_trends_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(Parkinson_Pesticide_State())
      write.csv(Parkinson_Pesticide_State(), file, row.names = FALSE)
    }
  )
}