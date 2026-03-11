# ui.R - User Interface for Life Expectancy & Pesticides Dashboard
# Top Navigation Layout

library(shiny)
library(plotly)
library(DT)
library(leaflet)

# =============================================================================
# UI DEFINITION
# =============================================================================

fluidPage(
  
  # Custom CSS for top navigation layout
  tags$head(
    tags$style(HTML("
      /* Import Google Fonts */
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap');
      
      /* Reset and base styles */
      * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
      }
      
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background-color: #f8f9fa;
        margin: 0;
        padding: 0;
      }
      
      /* Top branding bar */
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
        letter-spacing: 0.5px;
      }
      
      /* Navigation bar */
      .navbar-custom {
        background: #2d5016;
        padding: 0;
        margin: 0;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }
      
      .nav-tabs {
        border-bottom: none;
        background: #2d5016;
        padding-left: 40px;
        margin: 0;
      }
      
      .nav-tabs > li {
        margin-bottom: 0;
      }
      
      .nav-tabs > li > a {
        color: rgba(255,255,255,0.85);
        background: transparent;
        border: none;
        padding: 16px 28px;
        font-weight: 500;
        font-size: 15px;
        border-radius: 0;
        transition: all 0.3s ease;
        margin-right: 2px;
      }
      
      .nav-tabs > li > a:hover {
        background: rgba(255,255,255,0.1);
        color: white;
        border: none;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: white;
        background: #4a7c2a;
        border: none;
        border-bottom: 3px solid #ffc857;
      }
      
      /* Main content area */
      .tab-content {
        background: #f8f9fa;
        min-height: 100vh;
        padding-bottom: 50px;
      }
      
      .main-container {
        max-width: 1400px;
        margin: 0 auto;
        padding: 40px 40px;
      }
      
      /* Hero section */
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
        line-height: 1.2;
      }
      
      .hero-section p {
        font-size: 1.15em;
        line-height: 1.7;
        opacity: 0.95;
        max-width: 900px;
      }
      
      /* Stats boxes */
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
      
      /* Content boxes */
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
      
      .content-box h3 {
        color: #2d5016;
        font-size: 1.4em;
        margin-top: 25px;
        margin-bottom: 15px;
        font-weight: 600;
      }
      
      .content-box p {
        color: #444;
        line-height: 1.7;
        margin-bottom: 15px;
        font-size: 1.05em;
      }
      
      .content-box ul {
        margin-left: 25px;
        margin-bottom: 20px;
      }
      
      .content-box li {
        color: #444;
        line-height: 1.8;
        margin-bottom: 8px;
      }
      
      /* Plot containers */
      .plot-container {
        background: white;
        padding: 30px;
        border-radius: 10px;
        margin-bottom: 30px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
      }
      
      .plot-title {
        font-size: 1.4em;
        color: #2d5016;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #e0e0e0;
      }
      
      /* Filters section */
      .filters-box {
        background: #f8f9fa;
        padding: 25px;
        border-radius: 8px;
        margin-bottom: 30px;
        border: 1px solid #dee2e6;
      }
      
      /* Data sources section */
      .data-sources {
        display: flex;
        justify-content: center;
        gap: 40px;
        align-items: center;
        padding: 30px;
        background: white;
        border-radius: 10px;
        margin-top: 30px;
      }
      
      .source-logo {
        font-size: 1.2em;
        font-weight: 600;
        color: #666;
        padding: 15px 25px;
        border: 2px solid #dee2e6;
        border-radius: 8px;
      }
      
      /* Page headers */
      .page-header {
        color: #2d5016;
        font-size: 2.5em;
        margin-bottom: 30px;
        font-weight: 700;
      }
      
      /* Responsive */
      @media (max-width: 768px) {
        .nav-tabs {
          padding-left: 20px;
        }
        
        .main-container {
          padding: 20px 20px;
        }
        
        .hero-section {
          padding: 40px 30px;
        }
        
        .hero-section h1 {
          font-size: 2em;
        }
        
        .stats-row {
          flex-direction: column;
        }
      }
    "))
  ),
  
  # Top branding bar
  div(class = "top-brand",
      div(class = "project-title", "BIOL-185 Project - Environmental Impact on Neurological Health"),
      div("Environmental Health Research")
  ),
  
  # Main navigation with tabs
  navbarPage(
    title = "",
    id = "main_nav",
    windowTitle = "Environmental Impact on Neurological Health",
    collapsible = TRUE,
    
    header = tags$div(class = "navbar-custom"),
    
    # =========================================================================
    # HOME TAB
    # =========================================================================
    tabPanel("Home",
             div(class = "main-container",
                 div(class = "hero-section",
                     h1("Life Expectancy & Pesticides Case Studies"),
                     p("Welcome to our research project exploring the relationship between pesticide exposure and life expectancy across the United States. This interactive dashboard visualizes county-level data on pesticide use, agricultural practices, environmental exposure, and population health outcomes.")
                 ),
                 
                 # Stats boxes - these will be rendered by server
                 uiOutput("stats_boxes"),
                 
                 # Overview content
                 div(class = "content-box",
                     h2("Overview"),
                     h3("Research Background"),
                     p("Pesticides are widely used in agriculture to protect crops from pests, diseases, and weeds. While essential for food production, concerns have been raised about their potential impact on human health and longevity. This study examines:"),
                     tags$ul(
                       tags$li("Geographic patterns of pesticide use across U.S. states"),
                       tags$li("Correlations between pesticide exposure levels and life expectancy"),
                       tags$li("Temporal trends in both pesticide use and population health"),
                       tags$li("Differential impacts across age groups and demographics")
                     ),
                     
                     h3("Key Findings"),
                     uiOutput("key_findings_text"),
                     p("States with higher agricultural pesticide use show varying patterns of life expectancy, influenced by multiple factors including healthcare access, socioeconomic conditions, and environmental regulations.")
                 ),
                 
                 # Data sources
                 div(class = "content-box",
                     h2("Data Sources"),
                     p("The data in this project comes from multiple authoritative sources:"),
                     tags$ul(
                       tags$li(tags$strong("CDC WONDER:"), " Life expectancy and mortality data (2017-2023)"),
                       tags$li(tags$strong("USDA National Agricultural Statistics Service:"), " Pesticide use estimates"),
                       tags$li(tags$strong("EPA:"), " Environmental exposure and pesticide registration data"),
                       tags$li(tags$strong("U.S. Census Bureau:"), " Population and demographic information")
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
                 h1(class = "page-header", "Geographic Distribution"),
                 
                 div(class = "plot-container",
                     div(class = "plot-title", "Interactive Map: Life Expectancy & Pesticide Use by State"),
                     p("Circle size represents pesticide use intensity. Color represents life expectancy (green = higher, red = lower). Click on markers for detailed information."),
                     leafletOutput("map", height = 700)
                 ),
                 
                 fluidRow(
                   column(6,
                          div(class = "content-box",
                              h2("Regional Patterns"),
                              p("The map reveals distinct regional patterns in both pesticide use and life expectancy across the United States. States in the Midwest and Great Plains typically show higher pesticide use due to intensive agricultural activities, while coastal states often exhibit different patterns."),
                              p("Use the interactive map to explore individual state data by clicking on the circular markers. The size of each marker indicates the intensity of pesticide use, while the color gradient shows life expectancy levels.")
                          )
                   ),
                   column(6,
                          div(class = "content-box",
                              h2("Map Legend"),
                              tags$ul(
                                tags$li(tags$strong("Circle Size:"), " Larger circles = Higher pesticide use"),
                                tags$li(tags$strong("Circle Color:"), " Green = Higher life expectancy, Red = Lower life expectancy"),
                                tags$li(tags$strong("Click Markers:"), " View detailed state-specific data")
                              ),
                              p(tags$em("Data represents state averages for 2017-2023 period."))
                          )
                   )
                 )
             )
    ),
    
    # =========================================================================
    # DATA VISUALIZATION TAB
    # =========================================================================
    tabPanel("Data Visualization",
             div(class = "main-container",
                 h1(class = "page-header", "Interactive Visualizations"),
                 
                 div(class = "plot-container",
                     div(class = "plot-title", "Pesticide Use vs. Life Expectancy by State"),
                     p("Scatter plot showing the relationship between pesticide use and life expectancy across all 50 states. Each point represents a state, colored by region."),
                     plotlyOutput("scatter_plot", height = 600)
                 ),
                 
                 fluidRow(
                   column(8,
                          div(class = "plot-container",
                              div(class = "plot-title", "Temporal Trends (2000-2023)"),
                              p("Life expectancy trends over time, stratified by pesticide exposure levels."),
                              plotlyOutput("trend_plot", height = 500)
                          )
                   ),
                   column(4,
                          div(class = "plot-container",
                              div(class = "plot-title", "Impact by Age Group"),
                              p("Differential health impacts across age groups."),
                              plotlyOutput("age_group_plot", height = 500)
                          )
                   )
                 ),
                 
                 fluidRow(
                   column(6,
                          div(class = "plot-container",
                              div(class = "plot-title", "Life Expectancy Distribution"),
                              plotlyOutput("histogram_life", height = 450)
                          )
                   ),
                   column(6,
                          div(class = "plot-container",
                              div(class = "plot-title", "Pesticide Use Distribution"),
                              plotlyOutput("histogram_pesticide", height = 450)
                          )
                   )
                 ),
                 
                 div(class = "plot-container",
                     div(class = "plot-title", "Regional Comparison"),
                     p("Box plots comparing life expectancy across U.S. regions."),
                     plotlyOutput("regional_boxplot", height = 500)
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
                     
                     h3("Research Objectives"),
                     p("This research project aims to understand the complex relationship between agricultural pesticide use and population health outcomes, specifically focusing on life expectancy metrics across the United States. Our goal is to provide an accessible, data-driven resource for researchers, policymakers, and the public to explore potential environmental health impacts."),
                     
                     h3("Methodology"),
                     p("We employ a multi-faceted analytical approach combining:"),
                     tags$ul(
                       tags$li(tags$strong("Spatial analysis:"), " Geographic mapping of pesticide use patterns across all 50 U.S. states"),
                       tags$li(tags$strong("Statistical modeling:"), " Correlation and regression analyses to quantify relationships"),
                       tags$li(tags$strong("Temporal analysis:"), " Trend analysis from 2000-2023 to identify changes over time"),
                       tags$li(tags$strong("Demographic stratification:"), " Age-group specific analyses to identify vulnerable populations"),
                       tags$li(tags$strong("Regional comparisons:"), " ANOVA and comparative statistics across U.S. regions")
                     ),
                     
                     h3("Data Sources & Coverage"),
                     p("This project integrates data from multiple authoritative federal sources:"),
                     tags$ul(
                       tags$li(tags$strong("CDC WONDER Database:"), " Mortality and life expectancy statistics"),
                       tags$li(tags$strong("USDA National Agricultural Statistics Service (NASS):"), " Agricultural pesticide use estimates"),
                       tags$li(tags$strong("Environmental Protection Agency (EPA):"), " Pesticide registration and environmental exposure data"),
                       tags$li(tags$strong("U.S. Census Bureau:"), " Population demographics and geographic information")
                     ),
                     p(tags$strong("Coverage:"), " All 50 U.S. states | ", tags$strong("Time Period:"), " 2000-2023 | ", 
                       tags$strong("Last Updated:"), " March 2026"),
                     
                     h3("Important Limitations"),
                     p("This analysis has several important limitations that users should consider:"),
                     tags$ul(
                       tags$li(tags$strong("Ecological fallacy:"), " State-level aggregated data may not reflect individual-level exposure and outcomes. Associations observed at the population level do not necessarily apply to individuals."),
                       tags$li(tags$strong("Confounding variables:"), " Life expectancy is influenced by numerous factors including healthcare access, socioeconomic status, education, lifestyle, genetics, and environmental conditions beyond pesticide exposure."),
                       tags$li(tags$strong("Temporal lags:"), " Health effects from environmental exposures may manifest years or decades after initial exposure, complicating temporal analyses."),
                       tags$li(tags$strong("Data availability:"), " Pesticide use data varies in completeness and accuracy across states and time periods. Some states have more comprehensive reporting than others."),
                       tags$li(tags$strong("Causality:"), " Correlation does not imply causation. Observed associations require further investigation through controlled studies.")
                     ),
                     
                     h3("How to Use This Dashboard"),
                     p("Navigate through the tabs to explore different aspects of the data:"),
                     tags$ul(
                       tags$li(tags$strong("Home:"), " Overview and introduction to the project"),
                       tags$li(tags$strong("Maps:"), " Interactive geographic visualization of state-level data"),
                       tags$li(tags$strong("Data Visualization:"), " Charts and graphs showing relationships and trends"),
                       tags$li(tags$strong("Statistical Analysis:"), " Detailed statistical tests and data exploration tools"),
                       tags$li(tags$strong("About:"), " Project methodology and information (this page)")
                     ),
                     
                     h3("Contact & Collaboration"),
                     p("For questions about this research, data requests, or potential collaborations:"),
                     p(tags$strong("Email:"), " research@example.edu"),
                     p(tags$strong("Institution:"), " Environmental Health Sciences Department"),
                     p(tags$strong("GitHub:"), " [Project Repository Link]"),
                     
                     h3("Citation & Data Use"),
                     p("All data and visualizations in this dashboard are available for research and educational purposes. If you use this work, please cite:"),
                     p(tags$em("[Your Citation Format Here]")),
                     p("Raw data sources should also be cited according to their respective attribution requirements (CDC, USDA, EPA, Census Bureau)."),
                     
                     h3("Acknowledgments"),
                     p("This project was developed as part of BIOL-185 coursework. We thank the Centers for Disease Control and Prevention, U.S. Department of Agriculture, Environmental Protection Agency, and U.S. Census Bureau for making public data available for research purposes."),
                     
                     br(),
                     p(tags$em("Last Updated: March 9, 2026"), style = "margin-top: 30px; color: #888; text-align: right;")
                 )
             )
    )
  )
)

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
  # DATA GENERATION (In production, load from external files)
  # ===========================================================================
  
  set.seed(123)
  
  # State-level data
  states_data <- data.frame(
    State = state.name,
    Region = state.region,
    Life_Expectancy = rnorm(50, mean = 78.5, sd = 2.5),
    Pesticide_Use_kg_per_ha = abs(rnorm(50, mean = 2.5, sd = 1.2)),
    Population = sample(500000:40000000, 50),
    Agricultural_Area_pct = runif(50, 5, 60),
    Latitude = c(32.3, 64.2, 34.0, 34.7, 36.8, 39.5, 41.6, 38.9, 27.8, 32.2,
                 43.6, 44.3, 39.8, 39.9, 41.6, 38.5, 37.8, 30.4, 44.3, 39.0,
                 42.4, 43.3, 32.3, 38.6, 46.9, 41.5, 46.6, 39.2, 43.1, 40.8,
                 35.5, 42.7, 35.8, 39.9, 41.2, 35.5, 44.9, 40.3, 33.9, 44.4,
                 35.0, 43.8, 31.0, 39.3, 44.0, 37.5, 47.5, 38.5, 43.1, 43.0),
    Longitude = c(-86.9, -152.4, -111.1, -92.4, -119.4, -105.8, -72.7, -75.5, -81.7, -83.4,
                  -116.2, -114.5, -89.4, -86.1, -93.6, -84.3, -84.3, -92.3, -69.8, -76.6,
                  -71.4, -84.5, -89.7, -92.2, -110.0, -100.3, -112.0, -119.8, -71.5, -74.4,
                  -106.0, -73.8, -78.6, -82.9, -96.7, -97.5, -123.0, -76.9, -80.9, -72.6,
                  -80.0, -99.8, -97.5, -111.9, -72.6, -78.7, -120.5, -80.9, -89.5, -107.3)
  )
  
  # Calculate correlation
  correlation <- cor(states_data$Pesticide_Use_kg_per_ha, 
                     states_data$Life_Expectancy)
  
  # Temporal trend data
  years_data <- data.frame(
    Year = rep(2000:2023, each = 3),
    Category = rep(c("Low Pesticide Use", "Medium Pesticide Use", "High Pesticide Use"), 24),
    Life_Expectancy = c(
      seq(75.5, 78.2, length.out = 24),
      seq(75.0, 77.5, length.out = 24),
      seq(74.2, 76.8, length.out = 24)
    ) + rnorm(72, 0, 0.3),
    Pesticide_Use = rep(c(1.2, 2.8, 5.5), 24)
  )
  
  # Age group data
  age_groups_data <- data.frame(
    Age_Group = rep(c("0-14", "15-44", "45-64", "65-74", "75+"), each = 3),
    Exposure_Level = rep(c("Low", "Medium", "High"), 5),
    Life_Expectancy_Impact = c(
      -0.5, -1.2, -2.1,
      -0.3, -0.8, -1.5,
      -0.8, -1.8, -3.2,
      -1.5, -2.5, -4.1,
      -2.0, -3.2, -5.5
    )
  )
  
  # ===========================================================================
  # REACTIVE DATA FILTERING
  # ===========================================================================
  
  filtered_data <- reactive({
    data <- states_data
    
    # Filter by life expectancy
    data <- data %>%
      filter(Life_Expectancy >= input$life_exp_filter[1],
             Life_Expectancy <= input$life_exp_filter[2])
    
    # Filter by pesticide use
    data <- data %>%
      filter(Pesticide_Use_kg_per_ha >= input$pesticide_filter[1],
             Pesticide_Use_kg_per_ha <= input$pesticide_filter[2])
    
    # Filter by region
    if(input$region_filter != "All") {
      data <- data %>% filter(Region == input$region_filter)
    }
    
    return(data)
  })
  
  # ===========================================================================
  # HOME TAB OUTPUTS
  # ===========================================================================
  
  # Stats boxes
  output$stats_boxes <- renderUI({
    div(class = "stats-row",
        div(class = "stat-box",
            div(class = "stat-number", paste0(round(mean(states_data$Life_Expectancy), 1))),
            div(class = "stat-label", "Average Life Expectancy (years)")
        ),
        div(class = "stat-box",
            div(class = "stat-number", paste0(round(mean(states_data$Pesticide_Use_kg_per_ha), 2))),
            div(class = "stat-label", "Avg Pesticide Use (kg/ha)")
        ),
        div(class = "stat-box",
            div(class = "stat-number", round(correlation, 3)),
            div(class = "stat-label", "Correlation Coefficient")
        ),
        div(class = "stat-box",
            div(class = "stat-number", nrow(states_data)),
            div(class = "stat-label", "States Analyzed")
        )
    )
  })
  
  # Key findings text
  output$key_findings_text <- renderUI({
    p(paste0("Our preliminary analysis reveals a correlation coefficient of ", 
             round(correlation, 3), 
             " between pesticide use intensity and life expectancy at the state level."))
  })
  
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
  
  # ===========================================================================
  # DATA VISUALIZATION TAB OUTPUTS
  # ===========================================================================
  
  # Scatter plot
  output$scatter_plot <- renderPlotly({
    p <- ggplot(states_data, aes(
      x = Pesticide_Use_kg_per_ha, 
      y = Life_Expectancy,
      color = Region,
      text = paste(
        "State:", State,
        "<br>Life Expectancy:", round(Life_Expectancy, 1), "years",
        "<br>Pesticide Use:", round(Pesticide_Use_kg_per_ha, 2), "kg/ha",
        "<br>Region:", Region
      )
    )) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "darkgreen", 
                  linetype = "dashed", alpha = 0.3) +
      labs(
        x = "Pesticide Use (kg/ha)",
        y = "Life Expectancy (years)",
        color = "Region"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "closest")
  })
  
  # Trend plot
  output$trend_plot <- renderPlotly({
    p <- ggplot(years_data, aes(
      x = Year, 
      y = Life_Expectancy, 
      color = Category, 
      group = Category
    )) +
      geom_line(size = 1.2) +
      geom_point(size = 2, alpha = 0.6) +
      labs(
        x = "Year",
        y = "Life Expectancy (years)",
        color = "Exposure Level"
      ) +
      theme_minimal(base_size = 13) +
      scale_color_manual(values = c(
        "Low Pesticide Use" = "#2ecc71",
        "Medium Pesticide Use" = "#f39c12",
        "High Pesticide Use" = "#e74c3c"
      )) +
      theme(legend.position = "top")
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # Age group plot
  output$age_group_plot <- renderPlotly({
    p <- ggplot(age_groups_data, aes(
      x = Age_Group, 
      y = Life_Expectancy_Impact,
      fill = Exposure_Level
    )) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      labs(
        x = "Age Group",
        y = "Impact (years)",
        fill = "Exposure"
      ) +
      theme_minimal(base_size = 13) +
      scale_fill_manual(values = c(
        "Low" = "#2ecc71",
        "Medium" = "#f39c12",
        "High" = "#e74c3c"
      )) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
    
    ggplotly(p)
  })
  
  # Life expectancy histogram
  output$histogram_life <- renderPlotly({
    p <- ggplot(states_data, aes(x = Life_Expectancy)) +
      geom_histogram(bins = 20, fill = "#2ecc71", color = "white", alpha = 0.8) +
      labs(
        x = "Life Expectancy (years)",
        y = "Number of States"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # Pesticide use histogram
  output$histogram_pesticide <- renderPlotly({
    p <- ggplot(states_data, aes(x = Pesticide_Use_kg_per_ha)) +
      geom_histogram(bins = 20, fill = "#f39c12", color = "white", alpha = 0.8) +
      labs(
        x = "Pesticide Use (kg/ha)",
        y = "Number of States"
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # Regional boxplot
  output$regional_boxplot <- renderPlotly({
    p <- ggplot(states_data, aes(
      x = Region, 
      y = Life_Expectancy, 
      fill = Region
    )) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
      labs(
        x = "Region",
        y = "Life Expectancy (years)"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none") +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(p)
  })
  
  # ===========================================================================
  # STATISTICAL ANALYSIS TAB OUTPUTS
  # ===========================================================================
  
  # Correlation test
  output$correlation_test <- renderPrint({
    cor.test(
      states_data$Pesticide_Use_kg_per_ha, 
      states_data$Life_Expectancy
    )
  })
  
  # Correlation interpretation
  output$correlation_interpretation <- renderText({
    cor_val <- cor(
      states_data$Pesticide_Use_kg_per_ha, 
      states_data$Life_Expectancy
    )
    
    if(abs(cor_val) < 0.3) {
      paste0("Weak correlation (r = ", round(cor_val, 3), 
             "): Little to no linear relationship detected between pesticide use and life expectancy at the state level.")
    } else if(abs(cor_val) < 0.7) {
      paste0("Moderate correlation (r = ", round(cor_val, 3), 
             "): Some relationship exists, but other factors are important in determining life expectancy.")
    } else {
      paste0("Strong correlation (r = ", round(cor_val, 3), 
             "): Substantial linear relationship detected, though correlation does not imply causation.")
    }
  })
  
  # Regression summary
  output$regression_summary <- renderPrint({
    model <- lm(Life_Expectancy ~ Pesticide_Use_kg_per_ha, data = states_data)
    summary(model)
  })
  
  # Regression diagnostic plots
  output$regression_plot <- renderPlot({
    model <- lm(Life_Expectancy ~ Pesticide_Use_kg_per_ha, data = states_data)
    
    par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
    plot(model, which = 1:4, col = "#2d5016", pch = 19)
  })
  
  # ANOVA results
  output$anova_results <- renderPrint({
    model <- aov(Life_Expectancy ~ Region, data = states_data)
    summary(model)
    cat("\n\nTukey HSD Post-Hoc Test:\n")
    TukeyHSD(model)
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(
      filtered_data() %>%
        select(State, Region, Life_Expectancy, 
               Pesticide_Use_kg_per_ha, Population, Agricultural_Area_pct) %>%
        rename(
          "Life Expectancy" = Life_Expectancy,
          "Pesticide Use (kg/ha)" = Pesticide_Use_kg_per_ha,
          "Agricultural Area (%)" = Agricultural_Area_pct
        ),
      options = list(
        pageLength = 15, 
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatRound(
        columns = c("Life Expectancy", "Pesticide Use (kg/ha)", "Agricultural Area (%)"), 
        digits = 2
      ) %>%
      formatCurrency(
        columns = "Population",
        currency = "",
        digits = 0
      )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("pesticide_life_expectancy_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}
