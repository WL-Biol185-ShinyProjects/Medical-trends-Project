# ui.R - User Interface for Medical Trends Dashboard

library(shiny)
library(plotly)
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
        background-color: #f8f9fa;
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
      
      .stat-box {
        flex: 1;
        min-width: 200px;
        background: white;
        padding: 30px;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border-left: 4px solid #4a7c2a;
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
      
      .map-container {
        background: white;
        padding: 25px;
        border-radius: 10px;
        margin-bottom: 30px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      
      .map-title {
        font-size: 1.3em;
        color: #2d5016;
        font-weight: 600;
        margin-bottom: 15px;
        padding-bottom: 8px;
        border-bottom: 2px solid #e0e0e0;
      }
      
      .plot-container {
        background: white;
        padding: 30px;
        border-radius: 10px;
        margin-bottom: 30px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      
      .page-header {
        color: #2d5016;
        font-size: 2.5em;
        margin-bottom: 30px;
        font-weight: 700;
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
                     p("Exploring relationships between Parkinson's disease, pesticide exposure, agricultural practices, and life expectancy across the United States. This interactive dashboard integrates multiple datasets to visualize geographic patterns and correlations.")
                 ),
                 
                 uiOutput("stats_boxes"),
                 
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
                       tags$li(tags$strong("Clean Datasets/Parkinsons_mortality_rates_clean.csv"), " - Parkinson's death rates by state"),
                       tags$li(tags$strong("Clean Datasets/pesticides_by_county.csv"), " - Pesticide usage by county"),
                       tags$li(tags$strong("Clean Datasets/LifeExpectancyStateData_clean.csv"), " - Life expectancy by state"),
                       tags$li(tags$strong("Clean Datasets/ExpectancyData_clean.csv"), " - Life expectancy by county"),
                       tags$li(tags$strong("Clean Datasets/Farm_Data_2024.csv"), " - Number and size of farms by state"),
                       
                       tags$li(tags$strong("Centers for Disease Control and Prevention:"), " Air quality data"),
                       tags$li(tags$strong("U.S. Environmental Protection Agency:"), " Pesticide use estimates"),
                       tags$li(tags$strong("USDA National Agriculture Statistics Service"), " Environmental exposure and agricultural land data"),
                       tags$li(tags$strong("National Center for Health Statistics"), " Parkinson's mortality data")
                     ),
                     
                     div(class = "data-sources",
                         div(class = "source-logo", "CDC"),
                         div(class = "source-logo", "USDA"),
                         div(class = "source-logo", "EPA"),
                         div(class = "source-logo", "Census")
                     )
                 )
             )
    ),
    
    # =========================================================================
    # MAPS TAB
    # =========================================================================
    tabPanel("Maps",
             div(class = "main-container",
                 h1(class = "page-header", "Geographic Maps"),
                 
                 div(class = "map-container",
                     div(class = "map-title", "Map 1: Parkinson's Disease vs. Pesticide Use (State Level)"),
                     p("Circle size represents pesticide use; color represents Parkinson's death rate (darker red = higher rate)."),
                     leafletOutput("map_parkinson_pesticide", height = 600)
                 ),
                 
                 div(class = "map-container",
                     div(class = "map-title", "Map 2: Parkinson's Disease vs. Number of Farms (State Level)"),
                     p("Circle size represents number of farms; color represents Parkinson's death rate."),
                     leafletOutput("map_parkinson_farms", height = 600)
                 ),
                 
                 div(class = "map-container",
                     div(class = "map-title", "Map 3: Pesticides vs. Life Expectancy (County Level)"),
                     p("County-level analysis of pesticide exposure and life expectancy."),
                     leafletOutput("map_pesticide_life_expectancy", height = 600)
                 )
             )
    ),
    
    # =========================================================================
    # DATA VISUALIZATION TAB
    # =========================================================================
    tabPanel("Data Visualization",
             div(class = "main-container",
                 h1(class = "page-header", "Data Visualizations"),
                 
                 # Scatter plots
                 div(class = "plot-container",
                     plotlyOutput("plot_parkinson_pesticide", height = 500)
                 ),
                 
                 div(class = "plot-container",
                     plotlyOutput("plot_parkinson_farms", height = 500)
                 ),
                 
                 div(class = "plot-container",
                     plotlyOutput("plot_pesticide_life_expectancy", height = 500)
                 ),
                 
                 # Bar charts
                 fluidRow(
                   column(6,
                          div(class = "plot-container",
                              plotlyOutput("plot_top_parkinsons", height = 500)
                          )
                   ),
                   column(6,
                          div(class = "plot-container",
                              plotlyOutput("plot_top_farms", height = 500)
                          )
                   )
                 )
             )
    ),
    
    # =========================================================================
    # STATISTICAL ANALYSIS TAB
    # =========================================================================
    tabPanel("Statistical Analysis",
             div(class = "main-container",
                 h1(class = "page-header", "Statistical Analysis"),
                 
                 div(class = "content-box",
                     h2("Parkinson's Disease Data"),
                     p("State-level Parkinson's mortality data including death rates and total deaths."),
                     DTOutput("data_table_parkinson"),
                     br(),
                     downloadButton("download_combined", "Download Combined Dataset", 
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "content-box",
                     h2("Farm Data"),
                     p("State-level agricultural data including number of farms and acreage."),
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
                     p("This dashboard was created to explore potential relationships between environmental factors (pesticide use, agricultural intensity) and health outcomes (Parkinson's disease rates, life expectancy) across the United States."),
                     
                     tags$h3("Data Sources"),
                     p("All data comes from publicly available government datasets:"),
                     tags$ul(
                       tags$li("Centers for Disease Control and Prevention - Air quality data"),
                       tags$li("U.S. Environmental Protection Agency - Pesticide use estimates"),
                       tags$li("USDA National Agriculture Statistics Service - Environmental exposure and agricultural land data"),
                       tags$li("National Center for Health Statistics - Parkinson's mortality data")
                     ),
                     
                     tags$h3("Methodology"),
                     p("This analysis uses:"),
                     tags$ul(
                       tags$li("State-level aggregation for Parkinson's and farm data"),
                       tags$li("County-level pesticide and life expectancy data"),
                       tags$li("Geographic visualization with Leaflet maps"),
                       tags$li("Statistical correlation analysis")
                     ),
                     
                     tags$h3("Important Limitations"),
                     tags$ul(
                       tags$li("Correlation does not imply causation"),
                       tags$li("State and county-level data may mask local variations"),
                       tags$li("Multiple confounding variables affect health outcomes"),
                       tags$li("Temporal lags between exposure and disease manifestation")
                     ),
                     
                     tags$h3("Contact"),
                     p(tags$strong("Course:"), " BIOL-185"),
                     p(tags$strong("Project:"), " Medical Trends Analysis"),
                     
                     br(),
                     p(tags$em("Last Updated: March 2026"), 
                       style = "color: #888; text-align: right;")
                 )
             )
    )
  )
)

SERVER:
  
  
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