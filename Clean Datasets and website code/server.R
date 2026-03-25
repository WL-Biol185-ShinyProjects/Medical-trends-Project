library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)
library(sf)

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
  
  State_Pesticide_Data <- reactive({
    tryCatch({
      read.csv("state_pesticide_data_clean.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      showNotification("Error loading state_pesticide_data_clean.csv", type = "error")
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
  
  # Normalize state values to 2-letter abbreviations for reliable joins
  normalize_state_to_abbr <- function(x) {
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    upper <- toupper(x)
    is_abbr <- grepl("^[A-Z]{2}$", upper)
    
    name_lookup <- setNames(state.abb, tolower(state.name))
    name_lookup["district of columbia"] <- "DC"
    
    out <- upper
    idx <- which(!is_abbr & !is.na(x))
    if (length(idx) > 0) {
      out[idx] <- toupper(name_lookup[tolower(x[idx])])
    }
    out
  }
  
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
      mutate(State_Abbr = normalize_state_to_abbr(.data[[state_col]])) %>%
      filter(!is.na(State_Abbr)) %>%
      group_by(State_Abbr) %>%
      summarise(
        Avg_Pesticide = mean((LOW_ESTIMATE + HIGH_ESTIMATE) / 2, na.rm = TRUE),
        .groups = "drop"
      )
    state_pest
  })
  
  # Per-state summary for key compounds used in Map 1 hover labels
  Pesticide_State_Compounds <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    
    state_col <- if ("state_name" %in% names(data)) {
      "state_name"
    } else if ("state_code" %in% names(data)) {
      "state_code"
    } else {
      NULL
    }
    
    if (is.null(state_col) || !"compound" %in% names(data)) return(NULL)
    
    if ("AVG_ESTIMATE" %in% names(data)) {
      data <- data %>% mutate(pest_value = AVG_ESTIMATE)
    } else if (all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) {
      data <- data %>% mutate(pest_value = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2)
    } else {
      return(NULL)
    }
    
    targets <- c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos")
    
    data %>%
      mutate(
        State_Abbr = normalize_state_to_abbr(.data[[state_col]]),
        compound = trimws(as.character(compound))
      ) %>%
      filter(!is.na(State_Abbr), compound %in% targets) %>%
      group_by(State_Abbr, compound) %>%
      summarise(value = mean(pest_value, na.rm = TRUE), .groups = "drop") %>%
      group_by(State_Abbr) %>%
      summarise(
        `2,4-D` = ifelse(any(compound == "2,4-D"), value[compound == "2,4-D"][1], NA_real_),
        Glyphosate = ifelse(any(compound == "Glyphosate"), value[compound == "Glyphosate"][1], NA_real_),
        Paraquat = ifelse(any(compound == "Paraquat"), value[compound == "Paraquat"][1], NA_real_),
        Chlorpyrifos = ifelse(any(compound == "Chlorpyrifos"), value[compound == "Chlorpyrifos"][1], NA_real_),
        .groups = "drop"
      )
  })
  
  # Merged datasets for maps
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_Data())
    data <- Parkinson_Data() %>%
      mutate(State_Abbr = normalize_state_to_abbr(State))
    if(!is.null(Pesticide_State_Data())) {
      data <- data %>% left_join(Pesticide_State_Data(), by = "State_Abbr")
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
        county_fips = sprintf("%05d", as.integer(gsub("[^0-9]", "", as.character(cfips)))),
        state_fips = substr(county_fips, 1, 2),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", name)))
      )
    
    # Add state codes to coords
    coords <- coords %>%
      left_join(state_fips_mapping, by = "state_fips")
    
    # Clean county names in pesticide data
    pesticide <- pesticide %>%
      mutate(
        state_abbr = normalize_state_to_abbr(if ("state_name" %in% names(pesticide)) state_name else state_code),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", county_name))),
        Avg_Pesticide = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
      )
    
    # Clean county names in life expectancy data
    life_exp <- life_exp %>%
      mutate(
        state_abbr = normalize_state_to_abbr(State),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", County)))
      )
    
    pesticide_state_county <- pesticide %>%
      filter(!is.na(state_abbr), !is.na(county_name_clean)) %>%
      group_by(state_abbr, county_name_clean) %>%
      summarise(
        Avg_Pesticide = mean(Avg_Pesticide, na.rm = TRUE),
        state_name = first(state_abbr),
        county_name = first(county_name),
        .groups = "drop"
      )
    
    life_state_county <- life_exp %>%
      filter(!is.na(state_abbr), !is.na(county_name_clean)) %>%
      group_by(state_abbr, county_name_clean) %>%
      summarise(
        Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE),
        .groups = "drop"
      )
    
    merged <- pesticide_state_county %>%
      left_join(life_state_county, by = c("state_abbr", "county_name_clean")) %>%
      left_join(
        coords %>%
          transmute(
            state_abbr = State,
            county_name_clean,
            county_fips = as.character(county_fips),
            lng,
            lat
          ),
        by = c("state_abbr", "county_name_clean")
      )
    
    # Keep only rows with coordinates
    merged <- merged %>%
      filter(!is.na(lat), !is.na(lng), !is.na(county_fips))
    
    return(merged)
  })
  
  # Build state centroids from county coordinates (no GeoJSON required)
  State_Centroids <- reactive({
    req(County_Coords())
    coords <- County_Coords() %>%
      mutate(
        county_fips = sprintf("%05d", as.integer(gsub("[^0-9]", "", as.character(cfips)))),
        state_fips = substr(county_fips, 1, 2)
      ) %>%
      left_join(state_fips_mapping, by = "state_fips") %>%
      filter(!is.na(State), !is.na(lat), !is.na(lng))
    
    coords %>%
      group_by(State) %>%
      summarise(
        lat = mean(lat, na.rm = TRUE),
        lng = mean(lng, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Polygon layers for choropleth rendering
  state_sf <- reactive({
    states_map <- maps::map("state", fill = TRUE, plot = FALSE)
    sf_obj <- sf::st_as_sf(states_map)
    sf_obj$state_lower <- tolower(sf_obj$ID)
    sf_obj
  })
  
  county_sf <- reactive({
    counties_map <- maps::map("county", fill = TRUE, plot = FALSE)
    sf_obj <- sf::st_as_sf(counties_map)
    id_parts <- strsplit(sf_obj$ID, ",")
    sf_obj$state_lower <- tolower(vapply(id_parts, `[`, character(1), 1))
    sf_obj$county_lower <- trimws(tolower(vapply(id_parts, `[`, character(1), 2)))
    sf_obj
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
  # MAP SELECTOR OUTPUTS (for selected_map / main_map UI)
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
           "map1" = div(p(style = "font-size: 0.9em; line-height: 1.6;",
                          "This choropleth map shows state-level Parkinson's death rates and key pesticide values.",
                          br(), br(),
                          strong("Color:"), " Parkinson's death rate")),
           "map2" = div(p(style = "font-size: 0.9em; line-height: 1.6;",
                          "This choropleth map shows state-level Parkinson's death rates overlaid with farm counts.",
                          br(), br(),
                          strong("Color:"), " Parkinson's death rate")),
           "map3" = div(p(style = "font-size: 0.9em; line-height: 1.6;",
                          "This county-level choropleth map shows life expectancy by county.",
                          br(), br(),
                          strong("Color:"), " Life expectancy")),
           p("Select a map to view details.")
    )
  })
  
  # ===========================================================================
  # MAIN MAP OUTPUT (choropleths rendered from maps/sf polygons)
  # ===========================================================================
  
  output$main_map <- renderLeaflet({
    selected <- input$selected_map
    
    if (selected == "map1") {
      req(Parkinson_Pesticide_State(), state_sf())
      data <- Parkinson_Pesticide_State()
      compounds <- Pesticide_State_Compounds()
      
      state_lookup <- data.frame(
        state_lower = tolower(state.name),
        State_Abbr = state.abb,
        stringsAsFactors = FALSE
      )
      
      states <- state_sf() %>%
        left_join(state_lookup, by = "state_lower") %>%
        left_join(
          data %>% select(State_Abbr, Avg_Death_Rate, Avg_Pesticide),
          by = "State_Abbr"
        )
      
      if (!is.null(compounds)) {
        states <- states %>%
          left_join(compounds, by = "State_Abbr") %>%
          rename(
            pest_24d = `2,4-D`,
            pest_glyphosate = Glyphosate,
            pest_paraquat = Paraquat,
            pest_chlorpyrifos = Chlorpyrifos
          )
      } else {
        states$pest_24d <- NA_real_
        states$pest_glyphosate <- NA_real_
        states$pest_paraquat <- NA_real_
        states$pest_chlorpyrifos <- NA_real_
      }
      
      pal <- colorNumeric("YlOrRd", domain = states$Avg_Death_Rate, na.color = "#d0d0d0")
      labels <- sprintf(
        "<strong>%s</strong><br/>Death Rate: %s<br/>2,4-D: %s lbs<br/>Glyphosate: %s lbs<br/>Paraquat: %s lbs<br/>Chlorpyrifos: %s lbs",
        tools::toTitleCase(states$state_lower),
        ifelse(is.na(states$Avg_Death_Rate), "N/A", round(states$Avg_Death_Rate, 2)),
        ifelse(is.na(states$pest_24d), "N/A", round(states$pest_24d, 1)),
        ifelse(is.na(states$pest_glyphosate), "N/A", round(states$pest_glyphosate, 1)),
        ifelse(is.na(states$pest_paraquat), "N/A", round(states$pest_paraquat, 1)),
        ifelse(is.na(states$pest_chlorpyrifos), "N/A", round(states$pest_chlorpyrifos, 1))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(states) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor = ~pal(Avg_Death_Rate),
          fillOpacity = 0.75,
          color = "white",
          weight = 1.5,
          opacity = 1,
          highlightOptions = highlightOptions(weight = 3, color = "#2d5016", fillOpacity = 0.9, bringToFront = TRUE),
          label = labels
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~Avg_Death_Rate,
                  title = "Death Rate", opacity = 0.8, na.label = "No data")
      
    } else if (selected == "map2") {
      req(Parkinson_Farm_State(), state_sf())
      data <- Parkinson_Farm_State()
      state_lookup <- data.frame(
        state_lower = tolower(state.name),
        State_Abbr = state.abb,
        stringsAsFactors = FALSE
      )
      states <- state_sf() %>%
        left_join(state_lookup, by = "state_lower") %>%
        left_join(data %>% mutate(State_Abbr = normalize_state_to_abbr(State)),
                  by = "State_Abbr")
      
      pal <- colorNumeric("RdPu", domain = states$Avg_Death_Rate, na.color = "#d0d0d0")
      labels <- sprintf(
        "<strong>%s</strong><br/>Death Rate: %s<br/>Farms: %s",
        tools::toTitleCase(states$state_lower),
        ifelse(is.na(states$Avg_Death_Rate), "N/A", round(states$Avg_Death_Rate, 2)),
        ifelse(is.na(states$Number_Of_Farms), "N/A", format(states$Number_Of_Farms, big.mark = ","))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(states) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor = ~pal(Avg_Death_Rate),
          fillOpacity = 0.75,
          color = "white",
          weight = 1.5,
          opacity = 1,
          highlightOptions = highlightOptions(weight = 3, color = "#2d5016", fillOpacity = 0.9, bringToFront = TRUE),
          label = labels
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~Avg_Death_Rate,
                  title = "Death Rate", opacity = 0.8, na.label = "No data")
      
    } else if (selected == "map3") {
      data <- County_Pesticide_Life()
      if (is.null(data) || nrow(data) == 0 ||
          !"Avg_Life_Expectancy" %in% names(data) ||
          sum(!is.na(data$Avg_Life_Expectancy)) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
            addPopups(-98.5, 39.83, "No county life-expectancy data available")
        )
      }
      
      data_with_life <- data %>% filter(!is.na(Avg_Life_Expectancy), !is.na(lat), !is.na(lng))
      abbr_to_full <- setNames(tolower(state.name), state.abb)
      county_summary <- data_with_life %>%
        mutate(
          state_lower = ifelse(
            nchar(trimws(state_name)) == 2,
            unname(abbr_to_full[toupper(trimws(state_name))]),
            tolower(state_name)
          ),
          county_lower = trimws(tolower(gsub(" County$| Parish$", "", county_name)))
        ) %>%
        filter(!is.na(state_lower), !is.na(county_lower)) %>%
        group_by(state_lower, county_lower) %>%
        summarise(
          Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE),
          Avg_Pesticide = mean(Avg_Pesticide, na.rm = TRUE),
          .groups = "drop"
        )
      
      cp <- county_sf() %>%
        left_join(county_summary, by = c("state_lower", "county_lower"))
      
      if (sum(!is.na(cp$Avg_Life_Expectancy)) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
            addPopups(-98.5, 39.83, "No county life-expectancy data available after county match")
        )
      }
      
      pal <- colorNumeric("YlGn", domain = cp$Avg_Life_Expectancy, na.color = "#d0d0d0")
      labels <- sprintf(
        "<strong>%s, %s</strong><br/>Life Expectancy: %s yrs<br/>Pesticide: %s lbs",
        tools::toTitleCase(cp$county_lower),
        tools::toTitleCase(cp$state_lower),
        ifelse(is.na(cp$Avg_Life_Expectancy), "N/A", round(cp$Avg_Life_Expectancy, 1)),
        ifelse(is.na(cp$Avg_Pesticide), "N/A", round(cp$Avg_Pesticide, 2))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(cp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor = ~pal(Avg_Life_Expectancy),
          fillOpacity = 0.75,
          color = "white",
          weight = 0.4,
          opacity = 0.8,
          highlightOptions = highlightOptions(weight = 2, color = "#2d5016", fillOpacity = 0.9, bringToFront = TRUE),
          label = labels
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~Avg_Life_Expectancy,
                  title = "Life Expectancy (yrs)", opacity = 0.8, na.label = "No data")
      
    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4)
    }
  })
  
  # ===========================================================================
  # MAP 1: PARKINSON'S VS PESTICIDES (STATE LEVEL CHOROPLETH)
  # ===========================================================================
  
  output$map1 <- renderLeaflet({
    req(Parkinson_Pesticide_State(), State_Centroids())
    data <- Parkinson_Pesticide_State()
    compounds <- Pesticide_State_Compounds()
    
    states <- State_Centroids() %>%
      left_join(
        data %>% select(State_Abbr, Avg_Death_Rate, Avg_Pesticide),
        by = c("State" = "State_Abbr")
      )
    
    if (!is.null(compounds)) {
      states <- states %>%
        left_join(compounds, by = c("State" = "State_Abbr")) %>%
        rename(
          pest_24d = `2,4-D`,
          pest_glyphosate = Glyphosate,
          pest_paraquat = Paraquat,
          pest_chlorpyrifos = Chlorpyrifos
        )
    } else {
      states$pest_24d <- NA_real_
      states$pest_glyphosate <- NA_real_
      states$pest_paraquat <- NA_real_
      states$pest_chlorpyrifos <- NA_real_
    }
    
    pal <- colorNumeric("YlOrRd", domain = states$Avg_Death_Rate, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s</strong><br/>Death Rate: %s<br/>2,4-D: %s lbs<br/>Glyphosate: %s lbs<br/>Paraquat: %s lbs<br/>Chlorpyrifos: %s lbs",
                      states$State,
                      ifelse(is.na(states$Avg_Death_Rate), "N/A", round(states$Avg_Death_Rate, 2)),
                      ifelse(is.na(states$pest_24d), "N/A", round(states$pest_24d, 1)),
                      ifelse(is.na(states$pest_glyphosate), "N/A", round(states$pest_glyphosate, 1)),
                      ifelse(is.na(states$pest_paraquat), "N/A", round(states$pest_paraquat, 1)),
                      ifelse(is.na(states$pest_chlorpyrifos), "N/A", round(states$pest_chlorpyrifos, 1))) %>%
      lapply(htmltools::HTML)
    
    leaflet(states) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        stroke = TRUE,
        weight = 1,
        color = "white",
        fillColor = ~pal(Avg_Death_Rate),
        fillOpacity = 0.85,
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = ~Avg_Death_Rate, opacity = 1,
                title = "Parkinson's<br/>Death Rate", position = "bottomright")
  })
  
  # ===========================================================================
  # MAP 2: PARKINSON'S VS FARMS (STATE LEVEL CHOROPLETH)
  # ===========================================================================
  
  output$map2 <- renderLeaflet({
    req(Parkinson_Farm_State(), State_Centroids())
    data <- Parkinson_Farm_State()
    states <- State_Centroids() %>%
      left_join(data %>% mutate(State_Abbr = normalize_state_to_abbr(State)),
                by = c("State" = "State_Abbr"))
    
    pal <- colorNumeric("RdPu", domain = states$Avg_Death_Rate, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s</strong><br/>Death Rate: %s<br/>Farms: %s",
                      states$State,
                      ifelse(is.na(states$Avg_Death_Rate), "N/A", round(states$Avg_Death_Rate, 2)),
                      ifelse(is.na(states$Number_Of_Farms), "N/A", format(round(states$Number_Of_Farms), big.mark = ","))) %>%
      lapply(htmltools::HTML)
    
    leaflet(states) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        stroke = TRUE,
        weight = 1,
        color = "white",
        fillColor = ~pal(Avg_Death_Rate),
        fillOpacity = 0.85,
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = ~Avg_Death_Rate, opacity = 1,
                title = "Parkinson's<br/>Death Rate", position = "bottomright")
  })
  
  # ===========================================================================
  # MAP 3: PESTICIDES VS LIFE EXPECTANCY (COUNTY LEVEL CHOROPLETH)
  # ===========================================================================
  
  output$map3 <- renderLeaflet({
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
    
    pal <- colorNumeric("YlOrRd", domain = data_with_life$Avg_Life_Expectancy, reverse = TRUE, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s, %s</strong><br/>Life Expectancy: %s years<br/>Pesticide: %s lbs",
                      data_with_life$county_name,
                      data_with_life$state_name,
                      round(data_with_life$Avg_Life_Expectancy, 1),
                      round(data_with_life$Avg_Pesticide, 1)) %>%
      lapply(htmltools::HTML)
    
    zoom_level <- if(input$state_selector_map3 == "all") 4 else 6
    center_lng <- if(input$state_selector_map3 == "all") -98.5 else mean(data_with_life$lng, na.rm = TRUE)
    center_lat <- if(input$state_selector_map3 == "all") 39.5 else mean(data_with_life$lat, na.rm = TRUE)
    
    leaflet(data_with_life) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        stroke = FALSE,
        fillColor = ~pal(Avg_Life_Expectancy),
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = ~Avg_Life_Expectancy, opacity = 1,
                title = "Life Expectancy<br/>(years)", position = "bottomright")
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
  

  output$data_table_life_expectancy <- renderDT({
    req(Expectancy_State_Data())       # fixed: was Life_Expectancy_Data
    datatable(
      Expectancy_State_Data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(
        columns = intersect(c("Avg_Life_Expectancy", "Avg_Range_Min"), names(Expectancy_State_Data())),
        digits = 2
      )
  })
  
  output$data_table_county_pesticides <- renderDT({
    req(Pesticide_County_Data())       # fixed: was County_Pesticide_Data
    datatable(
      Pesticide_County_Data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  output$data_table_state_pesticides <- renderDT({
    req(State_Pesticide_Data())
    datatable(
      State_Pesticide_Data(),
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  # =============================================================================
  ## STATE PESTICIDE VS PARKINSON'S MORTALITY RATE
  # =============================================================================
  
  state_pesticide_parkinson_merged <- reactive({
    req(input$selected_state_pesticide, State_Pesticide_Data(), Parkinson_Data())
    
    pest_subset <- State_Pesticide_Data() %>%
      filter(compound == input$selected_state_pesticide) %>%
      select(state_name, AVG_ESTIMATE) %>%
      group_by(state_name) %>%
      summarise(AVG_ESTIMATE = mean(AVG_ESTIMATE, na.rm = TRUE))
    
    park_clean <- Parkinson_Data() %>%
      select(Location, Avg_Death_Rate) %>%
      group_by(Location) %>%
      summarise(Avg_Death_Rate = mean(Avg_Death_Rate, na.rm = TRUE))
    
    inner_join(pest_subset, park_clean, by = c("state_name" = "Location")) %>%
      filter(!is.na(AVG_ESTIMATE), !is.na(Avg_Death_Rate), AVG_ESTIMATE > 0)
  })
  
  output$plot_state_pesticide_parkinson <- renderPlotly({
    req(state_pesticide_parkinson_merged())
    data <- as.data.frame(state_pesticide_parkinson_merged())
    
    model      <- lm(Avg_Death_Rate ~ AVG_ESTIMATE, data = data)
    cor_val    <- round(cor(data$AVG_ESTIMATE, data$Avg_Death_Rate, use = "complete.obs"), 3)
    r_squared  <- round(summary(model)$r.squared, 3)
    coef_table <- summary(model)$coefficients
    p_val      <- if (nrow(coef_table) >= 2) round(coef_table[2, 4], 4) else NA
    
    x_seq    <- seq(min(data$AVG_ESTIMATE), max(data$AVG_ESTIMATE), length.out = 100)
    y_fitted <- coef(model)[1] + coef(model)[2] * x_seq
    
    plot_ly() %>%
      add_trace(
        data         = data,
        x            = ~AVG_ESTIMATE,
        y            = ~Avg_Death_Rate,
        type         = "scatter",
        mode         = "markers+text",
        marker       = list(color = "#2d5016", size = 7, opacity = 0.7),
        text         = ~state_name,
        textposition = "top center",
        textfont     = list(size = 9, color = "gray30"),
        hovertext    = ~paste("State:", state_name,
                              "<br>AVG_ESTIMATE:", round(AVG_ESTIMATE, 4),
                              "<br>Parkinson's Death Rate:", round(Avg_Death_Rate, 2)),
        hoverinfo    = "text",
        name         = "States"
      ) %>%
      add_trace(
        x         = x_seq,
        y         = y_fitted,
        type      = "scatter",
        mode      = "lines",
        line      = list(color = "darkred", dash = "dash", width = 2),
        hoverinfo = "skip",
        name      = "Regression Line"
      ) %>%
      layout(
        title = list(
          text = paste0(input$selected_state_pesticide, ": Pesticide Use vs. Parkinson's Death Rate by State"),
          font = list(size = 15)
        ),
        xaxis     = list(title = paste0(input$selected_state_pesticide, " AVG_ESTIMATE")),
        yaxis     = list(title = "Avg Parkinson's Death Rate"),
        hovermode = "closest",
        annotations = list(list(
          x           = max(data$AVG_ESTIMATE) * 0.85,
          y           = min(data$Avg_Death_Rate) + 0.3,
          text        = paste0("r = ", cor_val, "<br>R² = ", r_squared, "<br>p = ", p_val),
          showarrow   = FALSE,
          font        = list(color = "darkred", size = 13),
          bgcolor     = "white",
          bordercolor = "darkred",
          borderwidth = 1
        ))
      )
  })
  
  output$cor_state_pesticide_parkinson <- renderPrint({
    req(state_pesticide_parkinson_merged())
    cor.test(state_pesticide_parkinson_merged()$AVG_ESTIMATE,
             state_pesticide_parkinson_merged()$Avg_Death_Rate)
  })
  
  output$reg_state_pesticide_parkinson <- renderPrint({
    req(state_pesticide_parkinson_merged())
    summary(lm(Avg_Death_Rate ~ AVG_ESTIMATE, data = state_pesticide_parkinson_merged()))
  })
  
  # =============================================================================
  ## COUNTY PESTICIDE VS LIFE EXPECTANCY
  # =============================================================================
  
  county_pesticide_merged <- reactive({
    req(input$selected_pesticide, Pesticide_County_Data(), Expectancy_Data())
    
    pest_subset <- Pesticide_County_Data() %>%
      filter(compound == input$selected_pesticide) %>%
      select(county_name, AVG_ESTIMATE) %>%
      group_by(county_name) %>%
      summarise(AVG_ESTIMATE = mean(AVG_ESTIMATE, na.rm = TRUE))
    
    expectancy_clean <- Expectancy_Data() %>%
      group_by(County) %>%
      summarise(Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE))
    
    inner_join(pest_subset, expectancy_clean, by = c("county_name" = "County")) %>%
      filter(!is.na(AVG_ESTIMATE), !is.na(Avg_Life_Expectancy), AVG_ESTIMATE > 0)
  })
  
  output$plot_county_pesticide_life <- renderPlotly({
    req(county_pesticide_merged())
    data <- as.data.frame(county_pesticide_merged())
    
    model      <- lm(Avg_Life_Expectancy ~ log10(AVG_ESTIMATE), data = data)
    cor_val    <- round(cor(log10(data$AVG_ESTIMATE), data$Avg_Life_Expectancy), 3)
    r_squared  <- round(summary(model)$r.squared, 3)
    coef_table <- summary(model)$coefficients
    p_val      <- if (nrow(coef_table) >= 2) round(coef_table[2, 4], 4) else NA
    
    x_seq    <- seq(min(log10(data$AVG_ESTIMATE)), max(log10(data$AVG_ESTIMATE)), length.out = 100)
    y_fitted <- coef(model)[1] + coef(model)[2] * x_seq
    
    plot_ly() %>%
      add_trace(
        data      = data,
        x         = ~log10(AVG_ESTIMATE),
        y         = ~Avg_Life_Expectancy,
        type      = "scatter",
        mode      = "markers",
        marker    = list(color = "#2d5016", size = 5, opacity = 0.5),
        text      = ~paste("County:", county_name,
                           "<br>AVG_ESTIMATE:", round(AVG_ESTIMATE, 2),
                           "<br>Life Expectancy:", round(Avg_Life_Expectancy, 2)),
        hoverinfo = "text",
        name      = "Counties"
      ) %>%
      add_trace(
        x         = x_seq,
        y         = y_fitted,
        type      = "scatter",
        mode      = "lines",
        line      = list(color = "darkred", dash = "dash", width = 2),
        hoverinfo = "skip",
        name      = "Regression Line"
      ) %>%
      layout(
        title = list(
          text = paste0(input$selected_pesticide, ": Pesticide Use vs. Avg Life Expectancy by County"),
          font = list(size = 15)
        ),
        xaxis     = list(title = paste0(input$selected_pesticide, " AVG_ESTIMATE (log scale)")),
        yaxis     = list(title = "Avg Life Expectancy (years)"),
        hovermode = "closest",
        annotations = list(list(
          x           = max(log10(data$AVG_ESTIMATE)) * 0.85,
          y           = min(data$Avg_Life_Expectancy) + 1,
          text        = paste0("r = ", cor_val, "<br>R² = ", r_squared, "<br>p = ", p_val),
          showarrow   = FALSE,
          font        = list(color = "darkred", size = 13),
          bgcolor     = "white",
          bordercolor = "darkred",
          borderwidth = 1
        ))
      )
  })
  
  output$cor_county_pesticide_life <- renderPrint({
    req(county_pesticide_merged())
    cor.test(county_pesticide_merged()$AVG_ESTIMATE,
             county_pesticide_merged()$Avg_Life_Expectancy)
  })
  
  output$reg_county_pesticide_life <- renderPrint({
    req(county_pesticide_merged())
    summary(lm(Avg_Life_Expectancy ~ AVG_ESTIMATE, data = county_pesticide_merged()))
  })
  
  # Farm vs Parkinson's
  farm_parkinson_merged <- reactive({
    req(Farm_Data(), Parkinson_Data())
    farm <- as.data.frame(Farm_Data())
    park <- as.data.frame(Parkinson_Data())
    data.frame(
      State         = farm$State,
      NumberOfFarms = farm$Number_Of_Farms,
      DeathRate     = park$Avg_Death_Rate
    ) %>% filter(!is.na(NumberOfFarms), !is.na(DeathRate))
  })
  
  output$plot_farm_parkinson_detailed <- renderPlotly({
    req(farm_parkinson_merged())
    data <- farm_parkinson_merged()
    
    model      <- lm(DeathRate ~ NumberOfFarms, data = data)
    cor_val    <- round(cor(data$NumberOfFarms, data$DeathRate, use = "complete.obs"), 3)
    r_squared  <- round(summary(model)$r.squared, 3)
    coef_table <- summary(model)$coefficients
    p_val      <- if (nrow(coef_table) >= 2) round(coef_table[2, 4], 4) else NA
    
    x_seq    <- seq(min(data$NumberOfFarms), max(data$NumberOfFarms), length.out = 100)
    y_fitted <- coef(model)[1] + coef(model)[2] * x_seq
    
    plot_ly() %>%
      add_trace(
        data         = data,
        x            = ~NumberOfFarms,
        y            = ~DeathRate,
        type         = "scatter",
        mode         = "markers+text",
        marker       = list(color = "#2d5016", size = 7, opacity = 0.7),
        text         = ~State,
        textposition = "top center",
        textfont     = list(size = 9, color = "gray30"),
        hovertext    = ~paste("State:", State,
                              "<br>Number of Farms:", format(NumberOfFarms, big.mark = ","),
                              "<br>Death Rate:", round(DeathRate, 2)),
        hoverinfo    = "text",
        name         = "States"
      ) %>%
      add_trace(
        x         = x_seq,
        y         = y_fitted,
        type      = "scatter",
        mode      = "lines",
        line      = list(color = "darkred", dash = "dash", width = 2),
        hoverinfo = "skip",
        name      = "Regression Line"
      ) %>%
      layout(
        title     = list(text = "Number of Farms vs. Parkinson's Avg Death Rate by State",
                         font = list(size = 15)),
        xaxis     = list(title = "Number of Farms"),
        yaxis     = list(title = "Avg Death Rate (Parkinson's)"),
        hovermode = "closest",
        annotations = list(list(
          x           = max(data$NumberOfFarms) * 0.85,
          y           = min(data$DeathRate) + 0.5,
          text        = paste0("r = ", cor_val, "<br>R² = ", r_squared, "<br>p = ", p_val),
          showarrow   = FALSE,
          font        = list(color = "darkred", size = 13),
          bgcolor     = "white",
          bordercolor = "darkred",
          borderwidth = 1
        ))
      )
  })
  
  output$cor_farm_parkinson_detailed <- renderPrint({
    req(farm_parkinson_merged())
    cor.test(farm_parkinson_merged()$NumberOfFarms, farm_parkinson_merged()$DeathRate)
  })
  
  output$reg_farm_parkinson_detailed <- renderPrint({
    req(farm_parkinson_merged())
    summary(lm(DeathRate ~ NumberOfFarms, data = farm_parkinson_merged()))
  })
  
  # =============================================================================
  ## ANOVA: COUNTY PESTICIDE EXPOSURE LEVEL VS LIFE EXPECTANCY
  # =============================================================================
  
  assign_tertiles <- function(x) {
    breaks <- quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    if (length(unique(breaks)) < 4) {
      return(factor(dplyr::ntile(x, 3), levels = 1:3, labels = c("Low", "Medium", "High")))
    }
    cut(x, breaks = breaks, labels = c("Low", "Medium", "High"), include.lowest = TRUE)
  }
  
  anova_exposure_life_data <- reactive({
    req(input$anova_county_compound, Pesticide_County_Data(), Expectancy_Data())
    
    pest <- Pesticide_County_Data() %>%
      filter(compound == input$anova_county_compound) %>%
      select(county_name, AVG_ESTIMATE) %>%
      group_by(county_name) %>%
      summarise(AVG_ESTIMATE = mean(AVG_ESTIMATE, na.rm = TRUE)) %>%
      filter(!is.na(AVG_ESTIMATE), AVG_ESTIMATE > 0) %>%
      mutate(exposure_group = assign_tertiles(AVG_ESTIMATE))
    
    exp_clean <- Expectancy_Data() %>%
      group_by(County) %>%
      summarise(Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE))
    
    inner_join(pest, exp_clean, by = c("county_name" = "County")) %>%
      filter(!is.na(Avg_Life_Expectancy), !is.na(exposure_group))
  })
  
  output$plot_anova_exposure_life <- renderPlotly({
    req(anova_exposure_life_data())
    data  <- anova_exposure_life_data()
    p_val <- round(summary(aov(Avg_Life_Expectancy ~ exposure_group, data = data))[[1]][["Pr(>F)"]][1], 4)
    
    plot_ly(
      data      = data,
      x         = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
      y         = ~Avg_Life_Expectancy,
      type      = "box",
      color     = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
      colors    = c("#a7c957", "#f4a261", "#bc4749"),
      boxpoints = "outliers",
      hoverinfo = "y+name"
    ) %>%
      layout(
        title = list(
          text = paste0(input$anova_county_compound,
                        ": Exposure Level vs. Life Expectancy (ANOVA p = ", p_val, ")"),
          font = list(size = 14)
        ),
        xaxis      = list(title = "Pesticide Exposure Group"),
        yaxis      = list(title = "Avg Life Expectancy (years)"),
        showlegend = FALSE
      )
  })
  
  output$anova_exposure_life <- renderPrint({
    req(anova_exposure_life_data())
    summary(aov(Avg_Life_Expectancy ~ exposure_group, data = anova_exposure_life_data()))
  })
  
  output$tukey_exposure_life_print <- renderPrint({
    req(anova_exposure_life_data())
    TukeyHSD(aov(Avg_Life_Expectancy ~ exposure_group, data = anova_exposure_life_data()))
  })
  
  output$tukey_exposure_life_table <- renderDataTable({
    req(anova_exposure_life_data())
    tukey_df <- as.data.frame(
      TukeyHSD(aov(Avg_Life_Expectancy ~ exposure_group, data = anova_exposure_life_data()))$exposure_group
    )
    tukey_df <- tibble::rownames_to_column(tukey_df, var = "Comparison")
    colnames(tukey_df) <- c("Comparison", "Difference", "Lower CI", "Upper CI", "Adjusted p-value")
    tukey_df <- tukey_df %>% mutate(across(where(is.numeric), ~ round(., 4)))
    DT::datatable(tukey_df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      DT::formatStyle("Adjusted p-value",
                      backgroundColor = DT::styleInterval(0.05, c("#d4edda", "white")))
  })
  
  # =============================================================================
  ## ANOVA: STATE PESTICIDE EXPOSURE LEVEL VS PARKINSON'S DEATH RATE
  # =============================================================================
  
  anova_exposure_parkinson_data <- reactive({
    req(input$anova_state_compound, State_Pesticide_Data(), Parkinson_Data())
    
    pest <- State_Pesticide_Data() %>%
      filter(compound == input$anova_state_compound) %>%
      select(state_name, AVG_ESTIMATE) %>%
      group_by(state_name) %>%
      summarise(AVG_ESTIMATE = mean(AVG_ESTIMATE, na.rm = TRUE)) %>%
      filter(!is.na(AVG_ESTIMATE), AVG_ESTIMATE > 0) %>%
      mutate(exposure_group = assign_tertiles(AVG_ESTIMATE))
    
    park_clean <- Parkinson_Data() %>%
      group_by(Location) %>%
      summarise(Avg_Death_Rate = mean(Avg_Death_Rate, na.rm = TRUE))
    
    inner_join(pest, park_clean, by = c("state_name" = "Location")) %>%
      filter(!is.na(Avg_Death_Rate), !is.na(exposure_group))
  })
  
  output$plot_anova_exposure_parkinson <- renderPlotly({
    req(anova_exposure_parkinson_data())
    data  <- anova_exposure_parkinson_data()
    p_val <- round(summary(aov(Avg_Death_Rate ~ exposure_group, data = data))[[1]][["Pr(>F)"]][1], 4)
    
    plot_ly(
      data      = data,
      x         = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
      y         = ~Avg_Death_Rate,
      type      = "box",
      color     = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
      colors    = c("#a7c957", "#f4a261", "#bc4749"),
      boxpoints = "outliers",
      hoverinfo = "y+name"
    ) %>%
      layout(
        title = list(
          text = paste0(input$anova_state_compound,
                        ": Exposure Level vs. Parkinson's Death Rate (ANOVA p = ", p_val, ")"),
          font = list(size = 14)
        ),
        xaxis      = list(title = "Pesticide Exposure Group"),
        yaxis      = list(title = "Avg Parkinson's Death Rate"),
        showlegend = FALSE
      )
  })
  
  output$anova_exposure_parkinson <- renderPrint({
    req(anova_exposure_parkinson_data())
    summary(aov(Avg_Death_Rate ~ exposure_group, data = anova_exposure_parkinson_data()))
  })
  
  output$tukey_exposure_parkinson_print <- renderPrint({
    req(anova_exposure_parkinson_data())
    TukeyHSD(aov(Avg_Death_Rate ~ exposure_group, data = anova_exposure_parkinson_data()))
  })
  
  output$tukey_exposure_parkinson_table <- renderDataTable({
    req(anova_exposure_parkinson_data())
    tukey_df <- as.data.frame(
      TukeyHSD(aov(Avg_Death_Rate ~ exposure_group, data = anova_exposure_parkinson_data()))$exposure_group
    )
    tukey_df <- tibble::rownames_to_column(tukey_df, var = "Comparison")
    colnames(tukey_df) <- c("Comparison", "Difference", "Lower CI", "Upper CI", "Adjusted p-value")
    tukey_df <- tukey_df %>% mutate(across(where(is.numeric), ~ round(., 4)))
    DT::datatable(tukey_df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      DT::formatStyle("Adjusted p-value",
                      backgroundColor = DT::styleInterval(0.05, c("#d4edda", "white")))
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
