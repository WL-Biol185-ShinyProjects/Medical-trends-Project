library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)
library(jsonlite)

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
  
  # State name to abbreviation mapping
  state_name_abbrev <- c(
    "Alabama"="AL","Alaska"="AK","Arizona"="AZ","Arkansas"="AR","California"="CA","Colorado"="CO",
    "Connecticut"="CT","Delaware"="DE","Florida"="FL","Georgia"="GA","Hawaii"="HI","Idaho"="ID",
    "Illinois"="IL","Indiana"="IN","Iowa"="IA","Kansas"="KS","Kentucky"="KY","Louisiana"="LA",
    "Maine"="ME","Maryland"="MD","Massachusetts"="MA","Michigan"="MI","Minnesota"="MN","Mississippi"="MS",
    "Missouri"="MO","Montana"="MT","Nebraska"="NE","Nevada"="NV","New Hampshire"="NH","New Jersey"="NJ",
    "New Mexico"="NM","New York"="NY","North Carolina"="NC","North Dakota"="ND","Ohio"="OH","Oklahoma"="OK",
    "Oregon"="OR","Pennsylvania"="PA","Rhode Island"="RI","South Carolina"="SC","South Dakota"="SD",
    "Tennessee"="TN","Texas"="TX","Utah"="UT","Vermont"="VT","Virginia"="VA","Washington"="WA",
    "West Virginia"="WV","Wisconsin"="WI","Wyoming"="WY"
  )
  
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
  
  # Merged datasets for maps
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_Data())
    data <- Parkinson_Data()
    if(!is.null(Pesticide_State_Data())) {
      data <- data %>% left_join(Pesticide_State_Data(), by = "State")
    }
    data %>% filter(!is.na(Avg_Death_Rate))
  })
  
  Parkinson_Farm_State <- reactive({
    req(Parkinson_Data(), Farm_Data())
    Parkinson_Data() %>%
      left_join(Farm_Data() %>% select(State, Number_Of_Farms, Acres_Operated_Millions), by = "State") %>%
      filter(!is.na(Avg_Death_Rate))
  })
  
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
      filter(!is.na(lat), !is.na(lng), !is.na(county_fips))
    
    return(merged)
  })
  
  # Update state selector for Map 3
  observe({
    req(County_Pesticide_Life())
    states <- County_Pesticide_Life() %>%
      filter(!is.na(state_name)) %>%
      distinct(state_name) %>%
      arrange(state_name) %>%
      pull(state_name)
    
    updateSelectInput(session, "state_selector_map3",
                      choices = c("All States" = "all", setNames(states, states)),
                      selected = "all")
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