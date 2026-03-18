
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
  
  Parkinson_Data <- reactive({
    tryCatch({
      data <- read.csv("Parkinsons_mortality_rates_clean.csv", stringsAsFactors = FALSE)
      if("Location" %in% names(data)) {
        data$State <- data$Location
      }
      return(data)
    }, error = function(e) {
      showNotification("Error loading Parkinsons_mortality_rates_clean.csv", type = "error")
      return(NULL)
    })
  })
  
  Pesticide_County_Data <- reactive({
    tryCatch({
      read.csv("county_pesticides_data_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading pesticides_by_county.csv", type = "error")
      return(NULL)
    })
  })
  
  Expectancy_State_Data <- reactive({
    tryCatch({
      read.csv("LifeExpectancyStateData_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading LifeExpectancyStateData_clean.csv", type = "error")
      return(NULL)
    })
  })
  
  Expectancy_Data <- reactive({
    tryCatch({
      read.csv("ExpectancyData_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading ExpectancyData_clean.csv", type = "error")
      return(NULL)
    })
  })
  
  Farm_Data <- reactive({
    tryCatch({
      read.csv("Farm_Data_2024.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading Farm_Data_2024.csv", type = "error")
      return(NULL)
    })
  })
  
  County_Coords <- reactive({
    tryCatch({
      read.csv("cfips_location.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading cfips_location.csv", type = "error")
      return(NULL)
    })
  })
  
  # ===========================================================================
  # STATE FIPS TO STATE CODE MAPPING
  # ===========================================================================
  
  state_fips_mapping <- data.frame(
    state_fips = c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
                   "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
                   "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
                   "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
                   "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"),
    State = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
              "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
              "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
              "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
              "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
    stringsAsFactors = FALSE
  )
  
  # ===========================================================================
  # STATE COORDINATES - Fixed state centers
  # ===========================================================================
  
  State_Coords <- reactive({
    data.frame(
      State = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA",
                "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD",
                "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH",
                "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
                "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY", "DC"),
      Latitude = c(64.2, 32.3, 34.7, 34.0, 36.8, 39.5, 41.6, 38.9, 27.8, 32.2,
                   21.3, 42.0, 44.0, 40.6, 40.3, 38.5, 37.8, 30.4, 42.4, 39.0,
                   45.2, 44.3, 46.4, 38.6, 32.3, 46.9, 35.8, 47.5, 41.5, 43.2,
                   40.2, 34.5, 38.8, 43.0, 40.4, 35.5, 44.0, 41.2, 41.8, 34.0,
                   44.4, 35.8, 31.0, 39.3, 37.5, 44.0, 47.5, 44.5, 38.6, 43.0, 38.9),
      Longitude = c(-152.4, -86.9, -92.4, -111.1, -119.4, -105.8, -72.7, -75.5, -81.7, -83.4,
                    -157.8, -93.5, -114.7, -89.4, -86.1, -98.4, -84.3, -92.3, -71.4, -76.6,
                    -69.4, -85.6, -94.7, -92.2, -89.7, -110.0, -78.6, -100.4, -99.9, -71.6,
                    -74.5, -106.0, -117.0, -75.5, -82.9, -97.5, -120.5, -77.2, -71.5, -81.0,
                    -100.4, -86.4, -97.5, -111.7, -78.7, -72.6, -120.5, -89.5, -80.5, -107.5, -77.0),
      stringsAsFactors = FALSE
    )
  })
  
  # ===========================================================================
  # PROCESS AND MERGE DATA
  # ===========================================================================
  
  # Aggregate pesticide data to state level
  Pesticide_State_Data <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    
    state_col <- if("state_name" %in% names(data)) {
      "state_name"
    } else if("state_code" %in% names(data)) {
      "state_code"
    } else {
      NULL
    }
    
    if(is.null(state_col)) {
      return(NULL)
    }
    
    if(!all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) {
      return(NULL)
    }
    
    state_pest <- data %>%
      group_by(!!sym(state_col)) %>%
      summarise(
        Avg_Pesticide = mean((LOW_ESTIMATE + HIGH_ESTIMATE) / 2, na.rm = TRUE),
        .groups = "drop"
      )
    
    names(state_pest)[1] <- "State"
    return(state_pest)
  })
  
  # Merge Parkinson's with state coordinates
  Parkinson_With_Coords <- reactive({
    req(Parkinson_Data(), State_Coords())
    
    data <- Parkinson_Data()
    coords <- State_Coords()
    
    merged <- data %>%
      left_join(coords, by = "State")
    
    return(merged)
  })
  
  # Merge Farm data with coordinates  
  Farm_With_Coords <- reactive({
    req(Farm_Data(), State_Coords())
    
    data <- Farm_Data()
    coords <- State_Coords()
    
    merged <- data %>%
      left_join(coords, by = "State")
    
    return(merged)
  })
  
  # Combined: Parkinson's + Pesticides
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_With_Coords(), Pesticide_State_Data())
    
    parkinson <- Parkinson_With_Coords()
    pesticide <- Pesticide_State_Data()
    
    merged <- parkinson %>%
      left_join(pesticide, by = "State")
    
    return(merged)
  })
  
  # Combined: Parkinson's + Farms
  Parkinson_Farm_State <- reactive({
    req(Parkinson_With_Coords(), Farm_With_Coords())
    
    parkinson <- Parkinson_With_Coords()
    farms <- Farm_With_Coords()
    
    park_cols <- names(parkinson)
    farm_cols <- setdiff(names(farms), c("Latitude", "Longitude"))
    
    farms_select <- farms %>%
      select(all_of(farm_cols))
    
    merged <- parkinson %>%
      left_join(farms_select, by = "State")
    
    return(merged)
  })
  
  # County-level data for Map 3
  County_Pesticide_Life <- reactive({
    req(Pesticide_County_Data(), Expectancy_Data(), County_Coords())
    
    pesticide <- Pesticide_County_Data()
    life_exp <- Expectancy_Data()
    coords <- County_Coords()
    
    # Prepare coords with state info
    coords <- coords %>%
      mutate(
        state_fips = substr(sprintf("%05s", cfips), 1, 2),
        county_fips = sprintf("%05s", cfips),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", name)))
      )
    
    # Add state codes to coords
    coords <- coords %>%
      left_join(state_fips_mapping, by = "state_fips")
    
    # Clean county names in pesticide data
    pesticide <- pesticide %>%
      mutate(
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", county_name))),
        Avg_Pesticide = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
      )
    
    # Clean county names in life expectancy data
    life_exp <- life_exp %>%
      mutate(
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", County)))
      )
    
    # Merge pesticide with life expectancy
    merged <- pesticide %>%
      left_join(
        life_exp,
        by = c("state_name" = "State", "county_name_clean" = "county_name_clean")
      )
    
    # Merge with coordinates
    merged <- merged %>%
      left_join(
        coords %>% select(State, county_name_clean, county_fips, lng, lat),
        by = c("state_name" = "State", "county_name_clean" = "county_name_clean")
      )
    
    # Keep only rows with coordinates
    merged <- merged %>%
      filter(!is.na(lat), !is.na(lng))
    
    print(paste("Map 3 - Total counties with coordinates:", nrow(merged)))
    print(paste("Map 3 - Counties with life expectancy:", sum(!is.na(merged$Avg_Life_Expectancy))))
    
    return(merged)
  })
  
  # ===========================================================================
  # HOME TAB OUTPUTS
  # ===========================================================================
  
  output$stats_boxes <- renderUI({
    req(Parkinson_Data())
    
    parkinson <- Parkinson_Data()
    
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
  # MAP SELECTOR OUTPUTS
  # ===========================================================================
  
  output$map_title <- renderText({
    switch(input$selected_map,
           "map1" = "Parkinson's Death Rate vs. Pesticide Use (State Level)",
           "map2" = "Parkinson's Death Rate vs. Number of Farms (State Level)",
           "map3" = "Pesticides vs. Life Expectancy (County Level)",
           "Select a map")
  })
  
  output$map_description <- renderUI({
    switch(input$selected_map,
           "map1" = div(
             p(style = "font-size: 0.9em; line-height: 1.6;",
               "This map shows the relationship between Parkinson's disease death rates and pesticide use across states.",
               br(), br(),
               strong("Circle size:"), " State location", br(),
               strong("Color:"), " Death rate (yellow to red)")
           ),
           "map2" = div(
             p(style = "font-size: 0.9em; line-height: 1.6;",
               "This map shows the relationship between Parkinson's disease death rates and agricultural intensity.",
               br(), br(),
               strong("Circle size:"), " State location", br(),
               strong("Color:"), " Death rate (red to purple)")
           ),
           "map3" = div(
             p(style = "font-size: 0.9em; line-height: 1.6;",
               "This county-level map shows pesticide exposure and life expectancy.",
               br(), br(),
               strong("County points:"), " Small dots", br(),
               strong("Color:"), " Life expectancy (yellow to red)")
           ),
           p("Select a map to view details.")
    )
  })
  
  # ===========================================================================
  # MAIN MAP OUTPUT (switches based on selection)
  # ===========================================================================
  
  output$main_map <- renderLeaflet({
    selected <- input$selected_map
    
    if(selected == "map1") {
      # MAP 1: Parkinson's vs Pesticides (FIXED)
      req(Parkinson_Pesticide_State())
      data <- Parkinson_Pesticide_State()
      
      data <- data %>% filter(!is.na(Latitude), !is.na(Longitude), !is.na(Avg_Death_Rate))
      
      print(paste("Map 1 - Rows with valid data:", nrow(data)))
      
      if(nrow(data) == 0) {
        return(leaflet() %>% 
                 addProviderTiles(providers$CartoDB.Positron) %>% 
                 setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                 addPopups(-98.5, 39.8283, "No data with valid coordinates found"))
      }
      
      pal <- colorNumeric(
        palette = "YlOrRd",
        domain = data$Avg_Death_Rate
      )
      
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 8,
          fillColor = ~pal(Avg_Death_Rate),
          color = "white",
          weight = 1.5,
          opacity = 1,
          fillOpacity = 0.8,
          popup = ~paste(
            "<div style='font-family: Inter, sans-serif; padding: 10px;'>",
            "<h4 style='margin: 0 0 10px 0; color: #8B4513;'>", State, "</h4>",
            "<strong>Death Rate:</strong> ", round(Avg_Death_Rate, 2), "<br>",
            if("Avg_Deaths" %in% names(data)) paste("<strong>Avg Deaths:</strong>", round(Avg_Deaths, 1), "<br>") else "",
            if("Avg_Pesticide" %in% names(data)) paste("<strong>Pesticide Use:</strong>", round(Avg_Pesticide, 2), " lbs<br>") else "",
            "</div>"
          ),
          label = ~paste(State, "-", round(Avg_Death_Rate, 2)),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Avg_Death_Rate,
          title = "Death Rate",
          opacity = 0.8
        )
      
    } else if(selected == "map2") {
      # MAP 2: Parkinson's vs Farms (WORKING)
      req(Parkinson_Farm_State())
      data <- Parkinson_Farm_State()
      
      data <- data %>% 
        filter(!is.na(Latitude), !is.na(Longitude), !is.na(Avg_Death_Rate))
      
      print(paste("Map 2 - Rows with valid data:", nrow(data)))
      
      if(nrow(data) == 0) {
        return(leaflet() %>% 
                 addProviderTiles(providers$CartoDB.Positron) %>% 
                 setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                 addPopups(-98.5, 39.8283, "No valid data for Map 2"))
      }
      
      pal <- colorNumeric(
        palette = "RdPu",
        domain = data$Avg_Death_Rate
      )
      
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addCircleMarkers(
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 8,
          fillColor = ~pal(Avg_Death_Rate),
          color = "white",
          weight = 1.5,
          opacity = 1,
          fillOpacity = 0.8,
          popup = ~paste(
            "<div style='font-family: Inter, sans-serif; padding: 10px;'>",
            "<h4 style='margin: 0 0 10px 0; color: #8B4513;'>", State, "</h4>",
            "<strong>Death Rate:</strong> ", round(Avg_Death_Rate, 2), "<br>",
            if("Avg_Deaths" %in% names(data)) paste("<strong>Avg Deaths:</strong>", round(Avg_Deaths, 1), "<br>") else "",
            if("Number_Of_Farms" %in% names(data)) paste("<strong>Farms:</strong>", format(Number_Of_Farms, big.mark = ","), "<br>") else "",
            if("Acres_Operated_Millions" %in% names(data)) paste("<strong>Acres:</strong>", round(Acres_Operated_Millions, 2), " M<br>") else "",
            "</div>"
          ),
          label = ~paste(State, "-", round(Avg_Death_Rate, 2)),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Avg_Death_Rate,
          title = "Death Rate",
          opacity = 0.8
        )
      
    } else if(selected == "map3") {
      # MAP 3: County Level (FIXED)
      data <- County_Pesticide_Life()
      
      if(is.null(data) || nrow(data) == 0) {
        return(leaflet() %>% 
                 addProviderTiles(providers$CartoDB.Positron) %>% 
                 setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                 addPopups(-98.5, 39.8283, "No county data available after merging"))
      }
      
      print(paste("Map 3 - Total rows with coordinates:", nrow(data)))
      print(paste("Map 3 - Rows with life expectancy:", sum(!is.na(data$Avg_Life_Expectancy))))
      
      # If we have life expectancy data, color by it
      if("Avg_Life_Expectancy" %in% names(data) && sum(!is.na(data$Avg_Life_Expectancy)) > 0) {
        
        # Filter to only rows with valid life expectancy
        data_with_life <- data %>% filter(!is.na(Avg_Life_Expectancy))
        
        print(paste("Map 3 - Using", nrow(data_with_life), "counties with life expectancy"))
        
        pal <- colorNumeric(
          palette = "YlOrRd",
          domain = data_with_life$Avg_Life_Expectancy,
          reverse = TRUE
        )
        
        leaflet(data_with_life) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
          addCircleMarkers(
            lng = ~lng,
            lat = ~lat,
            radius = 3,
            fillColor = ~pal(Avg_Life_Expectancy),
            color = "white",
            weight = 0.5,
            opacity = 0.8,
            fillOpacity = 0.7,
            popup = ~paste(
              "<div style='padding: 5px;'>",
              "<strong>", county_name, "</strong><br>",
              "State: ", state_name, "<br>",
              "Life Exp: ", round(Avg_Life_Expectancy, 1), " yrs<br>",
              if(!is.na(Avg_Pesticide)) paste("Pesticide:", round(Avg_Pesticide, 2), " lbs<br>") else "",
              "</div>"
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = pal,
            values = ~Avg_Life_Expectancy,
            title = "Life Expectancy",
            opacity = 0.8
          )
      } else {
        # Just show all counties without color coding
        print("Map 3 - No life expectancy data, showing counties only")
        
        leaflet(data) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
          addCircleMarkers(
            lng = ~lng,
            lat = ~lat,
            radius = 3,
            color = "blue",
            fillOpacity = 0.5,
            stroke = FALSE,
            popup = ~paste("<strong>", county_name, "</strong><br>State:", state_name)
          )
      }
    } else {
      # Default blank map
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4)
    }
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
      formatRound(
        columns = intersect(c("Avg_Death_Rate", "Avg_Deaths"), names(Parkinson_Data())),
        digits = 2
      )
  })
  
  output$data_table_farms <- renderDT({
    req(Farm_Data())
    
    datatable(
      Farm_Data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(
        columns = intersect(c("Area_operated_Acres", "Acres_Operated_Millions"), names(Farm_Data())),
        digits = 2
      )
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
      filter(!is.na(AVG_ESTIMATE), !is.na(Avg_Life_Expectancy), AVG_ESTIMATE > 0)
  })
  
  # Scatter plot - updates when dropdown changes
  output$plot_county_pesticide_life <- renderPlotly({
    req(county_pesticide_merged())
    data <- as.data.frame(county_pesticide_merged())
    
    # Calculate stats
    model     <- lm(Avg_Life_Expectancy ~ log10(AVG_ESTIMATE), data = data)
    cor_val   <- round(cor(log10(data$AVG_ESTIMATE), data$Avg_Life_Expectancy), 3)
    r_squared <- round(summary(model)$r.squared, 3)
    coef_table <- summary(model)$coefficients
    p_val     <- if(nrow(coef_table) >= 2) round(coef_table[2, 4], 4) else NA
    
    # Build regression line
    x_seq    <- seq(min(log10(data$AVG_ESTIMATE)), max(log10(data$AVG_ESTIMATE)), length.out = 100)
    y_fitted <- coef(model)[1] + coef(model)[2] * x_seq
    
    plot_ly() %>%
      # Scatter points
      add_trace(
        data = data,
        x = ~log10(AVG_ESTIMATE),
        y = ~Avg_Life_Expectancy,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#2d5016", size = 5, opacity = 0.5),
        text = ~paste("County:", county_name,
                      "<br>AVG_ESTIMATE:", round(AVG_ESTIMATE, 2),
                      "<br>Life Expectancy:", round(Avg_Life_Expectancy, 2)),
        hoverinfo = "text",
        name = "Counties"
      ) %>%
      # Regression line
      add_trace(
        x = x_seq,
        y = y_fitted,
        type = "scatter",
        mode = "lines",
        line = list(color = "darkred", dash = "dash", width = 2),
        hoverinfo = "skip",
        name = "Regression Line"
      ) %>%
      # Layout
      layout(
        title = list(
          text = paste0(input$selected_pesticide, ": Pesticide Use vs. Avg Life Expectancy by County"),
          font = list(size = 15)
        ),
        xaxis = list(title = paste0(input$selected_pesticide, " AVG_ESTIMATE (log scale)")),
        yaxis = list(title = "Avg Life Expectancy (years)"),
        hovermode = "closest",
        annotations = list(
          list(
            x = max(log10(data$AVG_ESTIMATE)) * 0.85,
            y = min(data$Avg_Life_Expectancy) + 1,
            text = paste0("r = ", cor_val, "<br>R² = ", r_squared, "<br>p = ", p_val),
            showarrow = FALSE,
            font = list(color = "darkred", size = 13),
            bgcolor = "white",
            bordercolor = "darkred",
            borderwidth = 1
          )
        )
      )
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
  
  # Reactive: merge farm and parkinson's data by state
  farm_parkinson_merged <- reactive({
    req(Farm_Data(), Parkinson_Data())
    
    farm    <- as.data.frame(Farm_Data())
    park    <- as.data.frame(Parkinson_Data())
    
    data.frame(
      State         = farm$State,
      NumberOfFarms = farm$Number_Of_Farms,
      DeathRate     = park$Avg_Death_Rate
    ) %>%
      filter(!is.na(NumberOfFarms), !is.na(DeathRate))
  })
  
  # Scatter plot
  output$plot_farm_parkinson_detailed <- renderPlotly({
    req(farm_parkinson_merged())
    data <- farm_parkinson_merged()
    
    model     <- lm(DeathRate ~ NumberOfFarms, data = data)
    cor_val   <- round(cor(data$NumberOfFarms, data$DeathRate, use = "complete.obs"), 3)
    r_squared <- round(summary(model)$r.squared, 3)
    coef_table <- summary(model)$coefficients
    p_val     <- if(nrow(coef_table) >= 2) round(coef_table[2, 4], 4) else NA
    
    # Regression line
    x_seq    <- seq(min(data$NumberOfFarms), max(data$NumberOfFarms), length.out = 100)
    y_fitted <- coef(model)[1] + coef(model)[2] * x_seq
    
    plot_ly() %>%
      add_trace(
        data = data,
        x = ~NumberOfFarms,
        y = ~DeathRate,
        type = "scatter",
        mode = "markers+text",
        marker = list(color = "#2d5016", size = 7, opacity = 0.7),
        text = ~State,
        textposition = "top center",
        textfont = list(size = 9, color = "gray30"),
        hovertext = ~paste("State:", State,
                           "<br>Number of Farms:", format(NumberOfFarms, big.mark = ","),
                           "<br>Death Rate:", round(DeathRate, 2)),
        hoverinfo = "text",
        name = "States"
      ) %>%
      add_trace(
        x = x_seq,
        y = y_fitted,
        type = "scatter",
        mode = "lines",
        line = list(color = "darkred", dash = "dash", width = 2),
        hoverinfo = "skip",
        name = "Regression Line"
      ) %>%
      layout(
        title = list(
          text = "Number of Farms vs. Parkinson's Avg Death Rate by State",
          font = list(size = 15)
        ),
        xaxis = list(title = "Number of Farms"),
        yaxis = list(title = "Avg Death Rate (Parkinson's)"),
        hovermode = "closest",
        annotations = list(
          list(
            x = max(data$NumberOfFarms) * 0.85,
            y = min(data$DeathRate) + 0.5,
            text = paste0("r = ", cor_val, "<br>R² = ", r_squared, "<br>p = ", p_val),
            showarrow = FALSE,
            font = list(color = "darkred", size = 13),
            bgcolor = "white",
            bordercolor = "darkred",
            borderwidth = 1
          )
        )
      )
  })
  
  # Correlation text output
  output$cor_farm_parkinson_detailed <- renderPrint({
    req(farm_parkinson_merged())
    data <- farm_parkinson_merged()
    cor.test(data$NumberOfFarms, data$DeathRate)
  })
  
  # Regression summary text output
  output$reg_farm_parkinson_detailed <- renderPrint({
    req(farm_parkinson_merged())
    data <- farm_parkinson_merged()
    summary(lm(DeathRate ~ NumberOfFarms, data = data))
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