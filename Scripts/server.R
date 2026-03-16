# server.R - Server Logic for Medical Trends Dashboard
# Fixed: Map 3 merging and Map 1/2 centering on state centers

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
  
  # Load County FIPS location data (coordinates)
  # Columns: cfips, name, lng, lat
  County_Coords <- reactive({
    tryCatch({
      read.csv("/home/rbernot@ad.wlu.edu/BIOL185/Medical-trends-project/Clean Datasets/cfips_location.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading cfips_location.csv", 
                       type = "error", duration = NULL)
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
  # STATE COORDINATES - Use geometric center, not aggregated county coords
  # ===========================================================================
  
  State_Coords <- reactive({
    # Use fixed state centers for accurate positioning
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
      showNotification("Pesticide data missing state identifier", type = "warning")
      return(NULL)
    }
    
    if(!all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) {
      showNotification("Missing pesticide estimate columns", type = "warning")
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
    
    print("Parkinson_With_Coords sample:")
    print(head(merged))
    print(paste("Rows with coordinates:", sum(!is.na(merged$Latitude))))
    
    return(merged)
  })
  
  # Merge Farm data with coordinates  
  Farm_With_Coords <- reactive({
    req(Farm_Data(), State_Coords())
    
    data <- Farm_Data()
    coords <- State_Coords()
    
    merged <- data %>%
      left_join(coords, by = "State")
    
    print("Farm_With_Coords sample:")
    print(head(merged))
    print(paste("Rows with coordinates:", sum(!is.na(merged$Latitude))))
    
    return(merged)
  })
  
  # Combined: Parkinson's + Pesticides
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_With_Coords(), Pesticide_State_Data())
    
    parkinson <- Parkinson_With_Coords()
    pesticide <- Pesticide_State_Data()
    
    merged <- parkinson %>%
      left_join(pesticide, by = "State")
    
    print("Parkinson_Pesticide_State sample:")
    print(head(merged))
    
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
    
    print("Parkinson_Farm_State sample:")
    print(head(merged))
    print(paste("Rows with coordinates:", sum(!is.na(merged$Latitude))))
    
    return(merged)
  })
  
  # County-level merge for Map 3 (FIXED MERGING)
  County_Pesticide_Life <- reactive({
    req(Pesticide_County_Data(), Expectancy_Data(), County_Coords())
    
    pesticide <- Pesticide_County_Data()
    life_exp <- Expectancy_Data()
    coords <- County_Coords()
    
    print("=== MAP 3 MERGE DEBUG ===")
    print("Pesticide columns:")
    print(names(pesticide))
    print("Life expectancy columns:")
    print(names(life_exp))
    print("Coords columns:")
    print(names(coords))
    
    # Prepare coords with state info
    coords <- coords %>%
      mutate(
        state_fips = substr(sprintf("%05s", cfips), 1, 2),
        county_name_clean = trimws(tolower(name))
      )
    
    # Add state codes to coords
    coords <- coords %>%
      left_join(state_fips_mapping, by = "state_fips")
    
    print("Coords after adding state:")
    print(head(coords %>% select(cfips, name, State, state_fips)))
    
    # Clean county names in pesticide data
    pesticide <- pesticide %>%
      mutate(
        county_name_clean = trimws(tolower(gsub(" County$", "", county_name))),
        Avg_Pesticide = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
      )
    
    # Clean county names in life expectancy data
    life_exp <- life_exp %>%
      mutate(
        county_name_clean = trimws(tolower(gsub(" County$", "", County)))
      )
    
    print("Sample cleaned names:")
    print("Pesticide:")
    print(head(pesticide %>% select(state_name, county_name, county_name_clean)))
    print("Life exp:")
    print(head(life_exp %>% select(State, County, county_name_clean)))
    
    # Merge pesticide with life expectancy
    merged <- pesticide %>%
      left_join(
        life_exp,
        by = c("state_name" = "State", "county_name_clean" = "county_name_clean")
      )
    
    print("After merging pesticide + life exp:")
    print(paste("Rows:", nrow(merged)))
    print(paste("With Avg_Life_Expectancy:", sum(!is.na(merged$Avg_Life_Expectancy))))
    
    # Now merge with coordinates
    merged <- merged %>%
      left_join(
        coords %>% select(State, county_name_clean, lng, lat),
        by = c("state_name" = "State", "county_name_clean" = "county_name_clean")
      )
    
    print("After merging with coords:")
    print(paste("Rows:", nrow(merged)))
    print(paste("With coordinates:", sum(!is.na(merged$lat))))
    print(paste("With life expectancy:", sum(!is.na(merged$Avg_Life_Expectancy))))
    print(paste("With BOTH:", sum(!is.na(merged$lat) & !is.na(merged$Avg_Life_Expectancy))))
    
    # Filter to only rows with both coordinates and data
    merged <- merged %>%
      filter(!is.na(lat), !is.na(lng))
    
    print("Final merged dataset:")
    print(paste("Total rows:", nrow(merged)))
    print("Sample:")
    print(head(merged %>% select(state_name, county_name, Avg_Pesticide, Avg_Life_Expectancy, lat, lng)))
    
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
  # MAP 1: PARKINSON'S VS PESTICIDES (STATE LEVEL) - CENTERED ON STATES
  # ===========================================================================
  
  output$map_parkinson_pesticide <- renderLeaflet({
    req(Parkinson_Pesticide_State())
    data <- Parkinson_Pesticide_State()
    
    data <- data %>% filter(!is.na(Latitude), !is.na(Longitude), !is.na(Avg_Death_Rate))
    
    print(paste("Map 1 - Rows with valid data:", nrow(data)))
    
    if(nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
               addPopups(-98.5795, 39.8283, "No data with valid coordinates found"))
    }
    
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
        radius = ~if("Avg_Pesticide" %in% names(data)) sqrt(pmax(Avg_Pesticide, 0)) / 50 + 5 else 8,
        color = ~pal(Avg_Death_Rate),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
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
  })
  
  # ===========================================================================
  # MAP 2: PARKINSON'S VS FARMS (STATE LEVEL) - CENTERED ON STATES
  # ===========================================================================
  
  output$map_parkinson_farms <- renderLeaflet({
    req(Parkinson_Farm_State())
    data <- Parkinson_Farm_State()
    
    print("=== MAP 2 DEBUG ===")
    print(paste("Total rows:", nrow(data)))
    
    data <- data %>% 
      filter(!is.na(Latitude), !is.na(Longitude), !is.na(Avg_Death_Rate))
    
    print(paste("Rows with valid coordinates and death rate:", nrow(data)))
    
    if(nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
               addPopups(-98.5795, 39.8283, 
                         paste("No valid data for Map 2.<br>",
                               "Check that Parkinson and Farm data have matching State codes.")))
    }
    
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
        radius = ~if("Number_Of_Farms" %in% names(data)) sqrt(pmax(Number_Of_Farms, 0)) / 200 + 5 else 8,
        color = ~pal(Avg_Death_Rate),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
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
  })
  
  # ===========================================================================
  # MAP 3: PESTICIDES VS LIFE EXPECTANCY (COUNTY LEVEL) - FIXED DOMAIN
  # ===========================================================================
  
  output$map_pesticide_life_expectancy <- renderLeaflet({
    data <- County_Pesticide_Life()
    
    if(is.null(data) || nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
               addPopups(-98.5795, 39.8283, "No county data available after merging"))
    }
    
    print(paste("Map 3 - Total rows with coordinates:", nrow(data)))
    print(paste("Map 3 - Rows with life expectancy:", sum(!is.na(data$Avg_Life_Expectancy))))
    
    # If we have life expectancy data, color by it
    if("Avg_Life_Expectancy" %in% names(data) && sum(!is.na(data$Avg_Life_Expectancy)) > 0) {
      
      # Filter to only rows with valid life expectancy
      data_with_life <- data %>% filter(!is.na(Avg_Life_Expectancy))
      
      print(paste("Map 3 - Using", nrow(data_with_life), "counties with life expectancy"))
      
      pal <- colorNumeric(
        palette = "RdYlGn",
        domain = data_with_life$Avg_Life_Expectancy,
        reverse = FALSE
      )
      
      leaflet(data_with_life) %>%
        addTiles() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
        addCircleMarkers(
          lng = ~lng,
          lat = ~lat,
          radius = 4,
          color = ~pal(Avg_Life_Expectancy),
          fillOpacity = 0.6,
          stroke = FALSE,
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
        addTiles() %>%
        setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
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