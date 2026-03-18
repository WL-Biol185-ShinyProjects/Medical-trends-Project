

library(shiny)
library(DT)
library(leaflet)

# =============================================================================
# UI DEFINITION
# =============================================================================

fluidPage(
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      
      body {
        font-family: 'Inter', sans-serif;
        background-color: #f5f5f5;
      }
      
      .top-brand {
        background: #1a3d0a;
        color: white;
        padding: 8px 40px;
        font-size: 13px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .top-brand .project-title {
        font-weight: 600;
      }
      
      .nav-tabs {
        border-bottom: none;
        background: #2d5016;
        padding-left: 40px;
      }
      
      .nav-tabs > li > a {
        color: rgba(255,255,255,0.85);
        background: transparent;
        border: none;
        padding: 16px 28px;
        font-weight: 500;
        font-size: 15px;
      }
      
      .nav-tabs > li > a:hover {
        background: rgba(255,255,255,0.1);
        color: white;
      }
      
      .nav-tabs > li.active > a {
        color: white;
        background: #4a7c2a;
        border-bottom: 3px solid #ffc857;
      }
      
      .main-container {
        max-width: 1400px;
        margin: 0 auto;
        padding: 40px;
      }
      
      .hero-section {
        background: linear-gradient(135deg, #2d5016 0%, #4a7c2a 100%);
        color: white;
        padding: 60px;
        border-radius: 12px;
        margin-bottom: 40px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.1);
      }
      
      .hero-section h1 {
        font-size: 2.8em;
        margin-bottom: 20px;
        font-weight: 700;
      }
      
      .hero-section p {
        font-size: 1.15em;
        line-height: 1.7;
      }
      
      .stats-row {
        display: flex;
        gap: 20px;
        margin-bottom: 40px;
        flex-wrap: wrap;
      }
      
      .stats-boxes {
        display: flex;
        flex-direction: row;
        flex-wrap: nowrap;
        gap: 16px;
        margin: 20px 0;
        justify-content: center;
        width: 100%;
      }

      .stat-box {
        flex: 1;
        min-width: 0;
        aspect-ratio: 1/1;
        background: white;
        padding: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border-top: 4px solid #2c7bb6;
        text-align: center;
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        box-sizing: border-box;
}
      
      .stat-number {
        font-size: 2.5em;
        font-weight: 700;
        color: #2d5016;
        margin-bottom: 8px;
      }
      
      .stat-label {
        font-size: 0.95em;
        color: #666;
        font-weight: 500;
      }
      
      .content-box {
        background: white;
        padding: 35px;
        border-radius: 10px;
        margin-bottom: 30px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      
      .content-box h2 {
        color: #2d5016;
        font-size: 1.8em;
        margin-bottom: 20px;
        font-weight: 700;
        border-bottom: 3px solid #4a7c2a;
        padding-bottom: 12px;
      }
      
      /* Map viewer layout */
      .map-viewer-container {
        display: flex;
        gap: 0;
        height: 700px;
        background: white;
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      
      .map-sidebar {
        width: 300px;
        background: #f8f9fa;
        padding: 20px;
        border-right: 1px solid #e0e0e0;
        overflow-y: auto;
      }
      
      .map-sidebar h3 {
        font-size: 1.1em;
        color: #333;
        margin-bottom: 15px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .map-selector {
        margin-bottom: 25px;
      }
      
      .map-selector label {
        display: block;
        padding: 12px 15px;
        margin-bottom: 8px;
        background: white;
        border: 2px solid #e0e0e0;
        border-radius: 6px;
        cursor: pointer;
        transition: all 0.2s;
        font-weight: 500;
      }
      
      .map-selector input[type='radio'] {
        margin-right: 10px;
      }
      
      .map-selector label:hover {
        border-color: #4a7c2a;
        background: #f0f7f0;
      }
      
      .map-selector input[type='radio']:checked + label,
      .map-selector label:has(input:checked) {
        background: #4a7c2a;
        color: white;
        border-color: #4a7c2a;
      }
      
      .map-content {
        flex: 1;
        position: relative;
      }
      
      .map-title-bar {
        background: #2d5016;
        color: white;
        padding: 15px 20px;
        font-size: 1.1em;
        font-weight: 600;
      }
      
      .page-header {
        color: #2d5016;
        font-size: 2.5em;
        margin-bottom: 30px;
        font-weight: 700;
      }
      
      .empty-state {
        text-align: center;
        padding: 80px 40px;
        background: white;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      
      .empty-state-icon {
        font-size: 4em;
        color: #ccc;
        margin-bottom: 20px;
      }
      
      .empty-state-text {
        font-size: 1.2em;
        color: #666;
      }
    "))
  ),
  
  # Top branding bar
  div(class = "top-brand",
      div(class = "project-title", "BIOL-185 Project - Medical Trends Analysis"),
      div("Parkinson's Disease, Pesticides, & Life Expectancy")
  ),
  
  # Main navigation
  navbarPage(
    title = "",
    id = "main_nav",
    windowTitle = "Medical Trends Dashboard",
    
    # =========================================================================
    # HOME TAB
    # =========================================================================
    tabPanel("Home",
             div(class = "main-container",
                 div(class = "hero-section",
                     h1("Medical Trends Analysis Dashboard"),
                     
                     p("Welcome to our BIOL-185 project analyzing Parkinson's data across the United States. This interactive dashboard explores the relationships between Parkinson's disease, pesticide exposure, agricultural practices, and life expectancy across the United States. This interactive dashboard integrates multiple datasets to visualize geographic patterns and correlations."),
                     
                     p("Exploring relationships between Parkinson's disease, pesticide exposure, agricultural practices, and life expectancy across the United States.")
                     
                 ),
                 
                 
                 uiOutput("stats_boxes" ),
                 
                 div(class = "stats-boxes",
                     div(class = "stat-box",
                         h4("States Covered"),
                         p(textOutput("n_states")),
                         span("across the U.S.")
                     ),
                     div(class = "stat-box",
                         h4("Counties Analyzed"),
                         p(textOutput("n_counties")),
                         span("county-level records")
                     ),
                     div(class = "stat-box",
                         h4("Avg Annual Rate"),
                         p(textOutput("avg_rate")),
                         span("per 100,000 population")
                     ),
                     div(class = "stat-box",
                         h4("Years Covered"),
                         p("2017 - 2021"),
                         span("incidence data")
                     )
                 ),
                 div(class = "content-box",
                     h2("Research Questions"),
                     tags$ul(
                       tags$li("Is there a relationship between pesticide use and Parkinson's disease rates?"),
                       tags$li("How does agricultural intensity (number of farms) correlate with health outcomes?"),
                       tags$li("What are the geographic patterns of Parkinson's disease across states?"),
                       tags$li("How does pesticide exposure relate to life expectancy at the county level?")
                     )
                 ),
                 
                 div(class = "content-box",
                     h2("Datasets Used"),
                     tags$ul(
                       tags$li(tags$strong("Parkinsons_mortality_rates_clean.csv"), " - Parkinson's death rates by state"),
                       tags$li(tags$strong("pesticides_by_county.csv"), " - Pesticide usage by county"),
                       tags$li(tags$strong("LifeExpectancyStateData_clean.csv"), " - Life expectancy by state"),
                       tags$li(tags$strong("ExpectancyData_clean.csv"), " - Life expectancy by county"),
                       tags$li(tags$strong("Farm_Data_2024.csv"), " - Number and size of farms by state"),
                       tags$li(tags$strong("cfips_location.csv"), " - County coordinates (cfips, name, lng, lat)")
                     )
                 )
             )
    ),
    
    # =========================================================================
    # MAPS TAB - With Sidebar Selector
    # =========================================================================
    tabPanel("Maps",
             div(class = "main-container",
                 h1(class = "page-header", "Geographic Analysis"),
                 
                 div(class = "map-viewer-container",
                     # Sidebar with map selector
                     div(class = "map-sidebar",
                         h3("Select Map"),
                         div(class = "map-selector",
                             radioButtons(
                               "selected_map",
                               label = NULL,
                               choices = list(
                                 "Parkinson's vs Pesticides" = "map1",
                                 "Parkinson's vs Farms" = "map2",
                                 "Pesticides vs Life Expectancy" = "map3"
                               ),
                               selected = "map1"
                             )
                         ),
                         
                         hr(),
                         
                         h3("Map Info"),
                         uiOutput("map_description")
                     ),
                     
                     # Main map area
                     div(class = "map-content",
                         div(class = "map-title-bar",
                             textOutput("map_title")
                         ),
                         leafletOutput("main_map", height = "640px")
                     )
                 )
             )
    ),
    
    # =========================================================================
    # DATA VISUALIZATION TAB (EMPTY)
    # =========================================================================
    tabPanel("Data Visualization",
             div(class = "main-container",
                 h1(class = "page-header", "Data Visualizations"),
                 
                 div(class = "empty-state",
                     div(class = "empty-state-icon", "📊"),
                     div(class = "empty-state-text", 
                         "Visualization space reserved for future charts and graphs.",
                         br(), br(),
                         "Check the 'Maps' tab to explore geographic patterns.")
                 )
             )
    ),
    
    # =========================================================================
    # DATA TABLES TAB
    # =========================================================================
    tabPanel("Data Tables",
             div(class = "main-container",
                 h1(class = "page-header", "Data Tables"),
                 
                 div(class = "content-box",
                     h2("Parkinson's Disease Data"),
                     p("State-level Parkinson's mortality data."),
                     DTOutput("data_table_parkinson"),
                     br(),
                     downloadButton("download_combined", "Download Combined Dataset", 
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "content-box",
                     h2("Farm Data"),
                     p("State-level agricultural data."),
                     DTOutput("data_table_farms")
                 )
             )
    ),
    
    # =========================================================================
    # ABOUT TAB
    # =========================================================================
    tabPanel("About",
             div(class = "main-container",
                 div(class = "content-box",
                     h2("About This Project"),
                     
                     tags$h3("Purpose"),
                     p("This dashboard explores relationships between environmental factors and health outcomes across the United States."),
                     
                     tags$h3("Data Sources"),
                     tags$ul(
                       tags$li("CDC - Parkinson's disease mortality"),
                       tags$li("USDA - Pesticide usage and farm statistics"),
                       tags$li("CDC WONDER - Life expectancy data"),
                       tags$li("Census - County coordinate data (cfips_location.csv)")
                     ),
                     
                     tags$h3("Maps"),
                     p("Three interactive maps visualize:"),
                     tags$ul(
                       tags$li(tags$strong("Map 1:"), " State-level Parkinson's death rates vs. pesticide use"),
                       tags$li(tags$strong("Map 2:"), " State-level Parkinson's death rates vs. farm density"),
                       tags$li(tags$strong("Map 3:"), " County-level pesticide exposure vs. life expectancy")
                     ),
                     
                     tags$h3("Important Limitations"),
                     tags$ul(
                       tags$li("Correlation does not imply causation"),
                       tags$li("Aggregated data may mask local variations"),
                       tags$li("Multiple confounding variables exist")
                     ),
                     
                     br(),
                     p(tags$em("BIOL-185 Course Project - March 2026"), 
                       style = "color: #888; text-align: right;")
                 )
             )
    )
  )
)










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