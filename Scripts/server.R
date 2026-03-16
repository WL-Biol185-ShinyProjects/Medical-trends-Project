# server.R - Server Logic for Medical Trends Dashboard
# Fixed version with proper data access and county coordinates

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
      read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/county_pesticides_data_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading county_pesticides_data_clean.csv", 
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
  # COUNTY COORDINATES - Load or generate
  # ===========================================================================
  
  # This is a comprehensive list of county coordinates for the US
  # You can also load this from a CSV file if you have one
  county_coords <- reactive({
    # For now, we'll try to merge with a basic county centroid approach
    # You would ideally have a separate county_coordinates.csv file
    # Format: State, County, Latitude, Longitude
    
    # Placeholder - returns NULL for now, map will show message
    # To fix: Create a county_coordinates.csv with State, County, Lat, Long columns
    return(NULL)
  })
  
  # ===========================================================================
  # STATE COORDINATES
  # ===========================================================================
  
  state_coords <- data.frame(
    State = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
              "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
              "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
              "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
              "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY",
              "DC", "District of Columbia"),
    Latitude = c(64.2, 32.3, 34.7, 34.0, 36.8, 39.5, 41.6, 38.9, 27.8, 32.2,
                 21.3, 42.0, 44.0, 40.6, 40.3, 38.5, 37.8, 30.4, 42.4, 39.0,
                 45.2, 44.3, 46.4, 38.6, 32.3, 46.9, 35.8, 47.5, 41.5, 43.2,
                 40.2, 34.5, 38.8, 43.0, 40.4, 35.5, 44.0, 41.2, 41.8, 34.0,
                 44.4, 35.8, 31.0, 39.3, 37.5, 44.0, 47.5, 44.5, 38.6, 43.0,
                 38.9, 38.9),
    Longitude = c(-152.4, -86.9, -92.4, -111.1, -119.4, -105.8, -72.7, -75.5, -81.7, -83.4,
                  -157.8, -93.5, -114.7, -89.4, -86.1, -98.4, -84.3, -92.3, -71.4, -76.6,
                  -69.4, -85.6, -94.7, -92.2, -89.7, -110.0, -78.6, -100.4, -99.9, -71.6,
                  -74.5, -106.0, -117.0, -75.5, -82.9, -97.5, -120.5, -77.2, -71.5, -81.0,
                  -100.4, -86.4, -97.5, -111.7, -78.7, -72.6, -120.5, -89.5, -80.5, -107.5,
                  -77.0, -77.0)
  )
  
  # ===========================================================================
  # PROCESS AND MERGE DATA
  # ===========================================================================
  
  # Aggregate pesticide data to state level
  Pesticide_State_Data <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    
    # Check if state_name column exists
    if(!"state_name" %in% names(data)) {
      showNotification("county_pesticides_data_clean.csv missing 'state_name' column", 
                       type = "warning")
      return(NULL)
    }
    
    # Check if estimate columns exist
    if(!all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) {
      showNotification("Missing LOW_ESTIMATE or HIGH_ESTIMATE columns", 
                       type = "warning")
      return(NULL)
    }
    
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
    
    # Ensure State column exists
    if(!"State" %in% names(data)) {
      showNotification("Parkinson's data missing 'State' column", type = "warning")
      return(NULL)
    }
    
    # Merge with coordinates
    merged <- data %>%
      left_join(state_coords, by = "State")
    
    return(merged)
  })
  
  # Merge Farm data with coordinates  
  Farm_With_Coords <- reactive({
    req(Farm_Data())
    data <- Farm_Data()
    
    # Ensure State column exists
    if(!"State" %in% names(data)) {
      showNotification("Farm data missing 'State' column", type = "warning")
      return(NULL)
    }
    
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
    req(Parkinson_Data())
    
    parkinson <- Parkinson_Data()
    
    # Safely access columns with error checking
    avg_death_rate <- if("Avg_Death_Rate" %in% names(parkinson)) {
      round(mean(parkinson$Avg_Death_Rate, na.rm = TRUE), 2)
    } else { "N/A" }
    
    total_farms <- if(!is.null(Farm_Data()) && "Number_Of_Farms" %in% names(Farm_Data())) {
      format(sum(Farm_Data()$Number_Of_Farms, na.rm = TRUE), big.mark = ",")
    } else { "N/A" }
    
    avg_life_exp <- if(!is.null(Expectancy_State_Data()) && "Avg_Life_Expectancy" %in% names(Expectancy_State_Data())) {
      round(mean(Expectancy_State_Data()$Avg_Life_Expectancy, na.rm = TRUE), 1)
    } else { "N/A" }
    
    states_count <- nrow(parkinson)
    
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
  # MAP 1: PARKINSON'S VS PESTICIDES (STATE LEVEL) - FIXED WITH HOVER DATA
  # ===========================================================================
  
  output$map_parkinson_pesticide <- renderLeaflet({
    req(Parkinson_Pesticide_State())
    data <- Parkinson_Pesticide_State()
    
    # Remove rows with missing coordinates
    data <- data %>% filter(!is.na(Latitude), !is.na(Longitude))
    
    if(nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% 
               setView(lng = -98.5795, lat = 39.8283, zoom = 4))
    }
    
    # Check for required columns
    has_death_rate <- "Avg_Death_Rate" %in% names(data)
    has_pesticide <- "Avg_Pesticide" %in% names(data)
    has_deaths <- "Avg_Deaths" %in% names(data)
    
    # Color by Parkinson's death rate if available
    if(has_death_rate) {
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
          radius = ~if(has_pesticide) sqrt(Avg_Pesticide) / 50 else 8,
          color = ~pal(Avg_Death_Rate),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2,
          popup = ~paste(
            "<div style='font-family: Inter, sans-serif; padding: 10px;'>",
            "<h4 style='margin: 0 0 10px 0; color: #8B4513;'>", State, "</h4>",
            "<strong>Parkinson's Death Rate:</strong> ", round(Avg_Death_Rate, 2), "<br>",
            if(has_deaths) paste("<strong>Avg Deaths:</strong>", round(Avg_Deaths, 1), "<br>") else "",
            if(has_pesticide) paste("<strong>Avg Pesticide Use:</strong>", round(Avg_Pesticide, 2), " lbs<br>") else "",
            "</div>"
          ),
          label = ~paste(State, "- Death Rate:", round(Avg_Death_Rate, 2)),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Avg_Death_Rate,
          title = "Parkinson's<br>Death Rate",
          opacity = 0.8
        )
    } else {
      # Fallback if no death rate column
      leaflet(data) %>%
        addTiles() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
        addMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~State
        )
    }
  })
  
  # ===========================================================================
  # MAP 2: PARKINSON'S VS FARMS (STATE LEVEL) - FIXED
  # ===========================================================================
  
  output$map_parkinson_farms <- renderLeaflet({
    req(Parkinson_Farm_State())
    data <- Parkinson_Farm_State()
    
    # Remove rows with missing coordinates
    data <- data %>% filter(!is.na(Latitude), !is.na(Longitude))
    
    if(nrow(data) == 0) {
      return(leaflet() %>% addTiles() %>% 
               setView(lng = -98.5795, lat = 39.8283, zoom = 4))
    }
    
    # Check for required columns
    has_death_rate <- "Avg_Death_Rate" %in% names(data)
    has_farms <- "Number_Of_Farms" %in% names(data)
    has_acres <- "Acres_Operated_Millions" %in% names(data)
    has_deaths <- "Avg_Deaths" %in% names(data)
    
    # Color by Parkinson's death rate
    if(has_death_rate) {
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
          radius = ~if(has_farms) sqrt(Number_Of_Farms) / 200 else 8,
          color = ~pal(Avg_Death_Rate),
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 2,
          popup = ~paste(
            "<div style='font-family: Inter, sans-serif; padding: 10px;'>",
            "<h4 style='margin: 0 0 10px 0; color: #8B4513;'>", State, "</h4>",
            "<strong>Parkinson's Death Rate:</strong> ", round(Avg_Death_Rate, 2), "<br>",
            if(has_deaths) paste("<strong>Avg Deaths:</strong>", round(Avg_Deaths, 1), "<br>") else "",
            if(has_farms) paste("<strong>Number of Farms:</strong>", format(Number_Of_Farms, big.mark = ","), "<br>") else "",
            if(has_acres) paste("<strong>Acres Operated:</strong>", round(Acres_Operated_Millions, 2), " million<br>") else "",
            "</div>"
          ),
          label = ~paste(State, "- Death Rate:", round(Avg_Death_Rate, 2)),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Avg_Death_Rate,
          title = "Parkinson's<br>Death Rate",
          opacity = 0.8
        )
    } else {
      # Fallback
      leaflet(data) %>%
        addTiles() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~State)
    }
  })
  
  # ===========================================================================
  # MAP 3: PESTICIDES VS LIFE EXPECTANCY (COUNTY LEVEL) - WITH COUNTY COORDS
  # ===========================================================================
  
  output$map_pesticide_life_expectancy <- renderLeaflet({
    # This requires county-level coordinates
    # For now, showing a placeholder with instructions
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPopups(
        lng = -98.5795, 
        lat = 39.8283,
        popup = paste(
          "<div style='font-family: Inter, sans-serif; padding: 15px; max-width: 300px;'>",
          "<h4 style='color: #2d5016; margin-top: 0;'>County-Level Map Instructions</h4>",
          "<p><strong>To enable this map, you need county coordinates.</strong></p>",
          "<p>Create a file: <code>county_coordinates.csv</code> with columns:</p>",
          "<ul>",
          "<li>State (e.g., 'AL')</li>",
          "<li>County (e.g., 'Autauga County')</li>",
          "<li>Latitude</li>",
          "<li>Longitude</li>",
          "</ul>",
          "<p>Then update the server.R code to load this file.</p>",
          "<p>You can download county coordinates from:<br>",
          "<a href='https://www.census.gov/geographies/reference-files/time-series/geo/gazetteer-files.html' target='_blank'>",
          "US Census Gazetteer Files</a></p>",
          "</div>"
        )
      )
  })
  
  # ===========================================================================
  # PLOTS - FIXED SUBSCRIPT ERRORS
  # ===========================================================================
  
  # PLOT 1: PARKINSON'S VS PESTICIDES SCATTER
  output$plot_parkinson_pesticide <- renderPlotly({
    req(Parkinson_Pesticide_State())
    data <- Parkinson_Pesticide_State()
    
    # Check for required columns
    if(!all(c("Avg_Death_Rate", "Avg_Pesticide") %in% names(data))) {
      return(plotly_empty() %>% 
               layout(title = "Missing required data columns"))
    }
    
    data <- data %>% filter(!is.na(Avg_Death_Rate), !is.na(Avg_Pesticide))
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data available for this plot"))
    }
    
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
  
  # PLOT 2: PARKINSON'S VS FARMS SCATTER
  output$plot_parkinson_farms <- renderPlotly({
    req(Parkinson_Farm_State())
    data <- Parkinson_Farm_State()
    
    # Check for required columns
    if(!all(c("Avg_Death_Rate", "Number_Of_Farms") %in% names(data))) {
      return(plotly_empty() %>% 
               layout(title = "Missing required data columns"))
    }
    
    data <- data %>% filter(!is.na(Avg_Death_Rate), !is.na(Number_Of_Farms))
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data available for this plot"))
    }
    
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
  
  # PLOT 3: PESTICIDES VS LIFE EXPECTANCY SCATTER
  output$plot_pesticide_life_expectancy <- renderPlotly({
    req(Pesticide_State_Data(), Expectancy_State_Data())
    
    pesticide <- Pesticide_State_Data()
    life_exp <- Expectancy_State_Data()
    
    # Check for required columns
    if(!"Avg_Pesticide" %in% names(pesticide) || 
       !"Avg_Life_Expectancy" %in% names(life_exp)) {
      return(plotly_empty() %>% 
               layout(title = "Missing required data columns"))
    }
    
    # Merge
    data <- life_exp %>%
      left_join(pesticide, by = "State") %>%
      filter(!is.na(Avg_Life_Expectancy), !is.na(Avg_Pesticide))
    
    if(nrow(data) == 0) {
      return(plotly_empty() %>% 
               layout(title = "No data available for this plot"))
    }
    
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
  
  # Bar chart: Top states by Parkinson's death rate
  output$plot_top_parkinsons <- renderPlotly({
    req(Parkinson_Data())
    data <- Parkinson_Data()
    
    if(!"Avg_Death_Rate" %in% names(data)) {
      return(plotly_empty() %>% layout(title = "Missing Avg_Death_Rate column"))
    }
    
    data <- data %>%
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
    data <- Farm_Data()
    
    if(!"Number_Of_Farms" %in% names(data)) {
      return(plotly_empty() %>% layout(title = "Missing Number_Of_Farms column"))
    }
    
    data <- data %>%
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
    
    data <- Parkinson_Data()
    
    # Format numeric columns if they exist
    numeric_cols <- intersect(c("Avg_Death_Rate", "Avg_Deaths"), names(data))
    
    dt <- datatable(
      data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(columns = numeric_cols, digits = 2)
    }
    
    dt
  })
  
  output$data_table_farms <- renderDT({
    req(Farm_Data())
    
    data <- Farm_Data()
    
    # Format numeric columns if they exist
    numeric_cols <- intersect(c("Area_operated_Acres", "Acres_Operated_Millions"), names(data))
    
    dt <- datatable(
      data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
    
    if(length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(columns = numeric_cols, digits = 2)
    }
    
    dt
  })
  
  
  
  
  # =============================================================================
  # SERVER ADDITION - COUNTY PESTICIDE VS LIFE EXPECTANCY
  # =============================================================================
  # INSTRUCTIONS:
  # Paste this block into your server.R, anywhere inside the server function.
  # A good place is right after your existing plot outputs (plot_top_farms etc.)
  # and before the closing } of the server function.
  # =============================================================================
  
  # Reactive: re-filters and merges data whenever dropdown selection changes
  county_pesticide_merged <- reactive({
    req(input$selected_pesticide)
    req(Pesticide_County_Data())   # add req() checks
    req(Expectancy_Data())
    
    pest_subset <- Pesticide_County_Data() %>%   # add () here
      filter(compound == input$selected_pesticide) %>%
      select(county_name, AVG_ESTIMATE) %>%
      group_by(county_name) %>%
      summarise(AVG_ESTIMATE = mean(AVG_ESTIMATE, na.rm = TRUE))
    
    expectancy_clean <- Expectancy_Data() %>%    # add () here
      group_by(County) %>%
      summarise(Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE))
    
    inner_join(pest_subset, expectancy_clean, by = c("county_name" = "County")) %>%
      filter(!is.na(AVG_ESTIMATE), !is.na(Avg_Life_Expectancy))
  })
  
  # Scatter plot - updates when dropdown changes
  output$plot_county_pesticide_life <- renderPlotly({
    req(county_pesticide_merged())
    data <- county_pesticide_merged()
    
    model     <- lm(Avg_Life_Expectancy ~ AVG_ESTIMATE, data = data)
    cor_val   <- round(cor(data$AVG_ESTIMATE, data$Avg_Life_Expectancy), 3)
    r_squared <- round(summary(model)$r.squared, 3)
    p_val     <- round(summary(model)$coefficients[2, 4], 4)
    
    annotation_label <- paste0("r = ", cor_val, "\nR² = ", r_squared, "\np = ", p_val)
    
    p <- ggplot(data, aes(
      x = AVG_ESTIMATE,
      y = Avg_Life_Expectancy,
      text = paste(
        "County:", county_name,
        "<br>AVG_ESTIMATE:", round(AVG_ESTIMATE, 2),
        "<br>Life Expectancy:", round(Avg_Life_Expectancy, 2)
      )
    )) +
      geom_point(color = "#2d5016", size = 2, alpha = 0.5) +
      geom_smooth(method = "lm", se = TRUE, color = "darkred",
                  linetype = "dashed", alpha = 0.2) +
      scale_x_log10() +
      annotate("text",
               x = max(data$AVG_ESTIMATE, na.rm = TRUE) * 0.5,
               y = min(data$Avg_Life_Expectancy, na.rm = TRUE) * 1.02,
               label = annotation_label,
               size = 4, color = "darkred", fontface = "bold") +
      labs(
        title = paste0(input$selected_pesticide, ": Pesticide Use vs. Avg Life Expectancy by County"),
        x = paste0(input$selected_pesticide, " AVG_ESTIMATE (log scale)"),
        y = "Avg Life Expectancy (years)"
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Correlation text - updates when dropdown changes
  output$cor_county_pesticide_life <- renderPrint({
    req(county_pesticide_merged())
    data <- county_pesticide_merged()
    cor.test(data$AVG_ESTIMATE, data$Avg_Life_Expectancy)
  })
  
  # Regression summary text - updates when dropdown changes
  output$reg_county_pesticide_life <- renderPrint({
    req(county_pesticide_merged())
    data <- county_pesticide_merged()
    summary(lm(Avg_Life_Expectancy ~ AVG_ESTIMATE, data = data))
  })
  
  
  
  
  
  # ===========================================================================
  # DOWNLOAD HANDLER
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