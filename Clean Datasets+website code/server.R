
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
           "map1" = div(class = "map-info",
                        p("This map shows the relationship between Parkinson's disease death rates and pesticide use across states."),
                        p(strong("Color:"), " Death rate (yellow to red)")
           ),
           "map2" = div(class = "map-info",
                        p("This map shows the relationship between Parkinson's disease death rates and agricultural intensity."),
                        p(strong("Color:"), " Death rate (red to purple)")
           ),
           "map3" = div(class = "map-info",
                        p("This county-level map shows pesticide exposure and life expectancy."),
                        p(strong("Color:"), " Life expectancy (yellow to red)")
           ),
           p("Select a map to view details.")
    )
  })
  
  # ===========================================================================
  # MAIN MAP OUTPUT (CHOROPLETH - switches based on selection)
  # ===========================================================================
  
  output$main_map <- renderLeaflet({
    selected <- input$selected_map
    
    if(selected == "map1") {
      # MAP 1: PARKINSON'S VS PESTICIDES (STATE LEVEL CHOROPLETH)
      req(Parkinson_Pesticide_State())
      data <- Parkinson_Pesticide_State()
      
      # Load US states GeoJSON
      states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
      
      # Map state names to abbreviations
      state_name_to_abbr <- setNames(data$State, c(
        "Alabama"="AL","Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO",
        "Connecticut"="CT","Delaware"="DE","Florida"="FL","Georgia"="GA","Hawaii"="HI","Idaho"="ID",
        "Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA",
        "Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS",
        "Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ",
        "New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK",
        "Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD",
        "Tennessee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA",
        "West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"
      )[states$name])
      
      # Merge data
      states$death_rate <- data$Avg_Death_Rate[match(state_name_to_abbr, data$State)]
      states$pesticide <- data$Avg_Pesticide[match(state_name_to_abbr, data$State)]
      
      pal <- colorNumeric("YlOrRd", domain = states$death_rate, na.color = "#cccccc")
      
      labels <- sprintf("<strong>%s</strong><br/>Death Rate: %s<br/>Pesticide: %s lbs",
                        states$name,
                        ifelse(is.na(states$death_rate), "N/A", round(states$death_rate, 2)),
                        ifelse(is.na(states$pesticide), "N/A", round(states$pesticide, 0))) %>% 
        lapply(htmltools::HTML)
      
      leaflet(states) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor = ~pal(death_rate),
          weight = 1.5,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
        ) %>%
        addLegend(pal = pal, values = ~death_rate, opacity = 1, 
                  title = "Death Rate", position = "bottomright")
      
    } else if(selected == "map2") {
      # MAP 2: PARKINSON'S VS FARMS (STATE LEVEL CHOROPLETH)
      req(Parkinson_Farm_State())
      data <- Parkinson_Farm_State()
      
      # Load US states GeoJSON
      states <- geojson_read("https://raw.githubusercontent.com/PublicaMundi/MappingAPI/master/data/geojson/us-states.json", what = "sp")
      
      # Map state names to abbreviations
      state_name_to_abbr <- setNames(data$State, c(
        "Alabama"="AL","Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO",
        "Connecticut"="CT","Delaware"="DE","Florida"="FL","Georgia"="GA","Hawaii"="HI","Idaho"="ID",
        "Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA",
        "Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS",
        "Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ",
        "New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK",
        "Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD",
        "Tennessee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA",
        "West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"
      )[states$name])
      
      # Merge data
      states$death_rate <- data$Avg_Death_Rate[match(state_name_to_abbr, data$State)]
      states$farms <- data$Number_Of_Farms[match(state_name_to_abbr, data$State)]
      
      pal <- colorNumeric("RdPu", domain = states$death_rate, na.color = "#cccccc")
      
      labels <- sprintf("<strong>%s</strong><br/>Death Rate: %s<br/>Farms: %s",
                        states$name,
                        ifelse(is.na(states$death_rate), "N/A", round(states$death_rate, 2)),
                        ifelse(is.na(states$farms), "N/A", format(round(states$farms), big.mark = ","))) %>% 
        lapply(htmltools::HTML)
      
      leaflet(states) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor = ~pal(death_rate),
          weight = 1.5,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
        ) %>%
        addLegend(pal = pal, values = ~death_rate, opacity = 1, 
                  title = "Death Rate", position = "bottomright")
      
    } else if(selected == "map3") {
      # MAP 3: PESTICIDES VS LIFE EXPECTANCY (COUNTY LEVEL CHOROPLETH)
      req(County_Pesticide_Life())
      data <- County_Pesticide_Life()
      
      # Filter by selected state
      if(!is.null(input$state_selector_map3) && input$state_selector_map3 != "all") {
        data <- data %>% filter(state_name == input$state_selector_map3)
      }
      
      data_with_life <- data %>% filter(!is.na(Avg_Life_Expectancy), !is.na(county_fips))
      
      if(nrow(data_with_life) == 0) {
        return(leaflet() %>% 
                 addProviderTiles(providers$CartoDB.Positron) %>% 
                 setView(lng = -98.5, lat = 39.5, zoom = 4))
      }
      
      # Load US counties GeoJSON
      counties <- geojson_read("https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json", what = "sp")
      
      # Match data to counties
      counties$life_exp <- data_with_life$Avg_Life_Expectancy[match(counties$id, data_with_life$county_fips)]
      counties$pesticide <- data_with_life$Avg_Pesticide[match(counties$id, data_with_life$county_fips)]
      counties$county_name <- data_with_life$county_name[match(counties$id, data_with_life$county_fips)]
      counties$state_name <- data_with_life$state_name[match(counties$id, data_with_life$county_fips)]
      
      # Filter to only counties with data
      counties_with_data <- counties[!is.na(counties$life_exp), ]
      
      pal <- colorNumeric("YlOrRd", domain = counties_with_data$life_exp, reverse = TRUE, na.color = "#cccccc")
      
      labels <- sprintf("<strong>%s, %s</strong><br/>Life Expectancy: %s years<br/>Pesticide: %s lbs",
                        counties_with_data$county_name,
                        counties_with_data$state_name,
                        round(counties_with_data$life_exp, 1),
                        round(counties_with_data$pesticide, 1)) %>% 
        lapply(htmltools::HTML)
      
      zoom_level <- if(input$state_selector_map3 == "all") 4 else 6
      center_lng <- if(input$state_selector_map3 == "all") -98.5 else mean(counties_with_data@bbox[1,])
      center_lat <- if(input$state_selector_map3 == "all") 39.5 else mean(counties_with_data@bbox[2,])
      
      leaflet(counties_with_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
        addPolygons(
          fillColor = ~pal(life_exp),
          weight = 1,
          opacity = 1,
          color = "white",
          fillOpacity = 0.7,
          highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
        ) %>%
        addLegend(pal = pal, values = ~life_exp, opacity = 1, 
                  title = "Life Expectancy<br/>(years)", position = "bottomright")
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