# ui.R - User Interface for Medical Trends Dashboard
# Maps + empty Data Visualization tab

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
                     p("Exploring relationships between Parkinson's disease, pesticide exposure, agricultural practices, and life expectancy across the United States.")
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
    # MAPS TAB
    # =========================================================================
    tabPanel("Maps",
             div(class = "main-container",
                 h1(class = "page-header", "Geographic Maps"),
                 
                 div(class = "map-container",
                     div(class = "map-title", "Map 1: Parkinson's Death Rate vs. Pesticide Use (State Level)"),
                     p("Hover over states to see death rate. Circle size represents pesticide use; color represents Parkinson's death rate (yellow to red)."),
                     leafletOutput("map_parkinson_pesticide", height = 600)
                 ),
                 
                 div(class = "map-container",
                     div(class = "map-title", "Map 2: Parkinson's Death Rate vs. Number of Farms (State Level)"),
                     p("Hover over states to see death rate. Circle size represents number of farms; color represents Parkinson's death rate (red to purple)."),
                     leafletOutput("map_parkinson_farms", height = 600)
                 ),
                 
                 div(class = "map-container",
                     div(class = "map-title", "Map 3: Pesticides vs. Life Expectancy (County Level)"),
                     p("County-level analysis using cfips_location.csv (cfips, name, lng, lat). Color represents life expectancy."),
                     leafletOutput("map_pesticide_life_expectancy", height = 600)
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
                       tags$li(tags$strong("Map 3:"), " County-level pesticide exposure vs. life expectancy (using cfips coordinates)")
                     ),
                     
                     tags$h3("County Coordinate System"),
                     p("Map 3 uses the cfips_location.csv file which contains:"),
                     tags$ul(
                       tags$li(tags$strong("cfips:"), " 5-digit FIPS code (first 2 digits = state FIPS)"),
                       tags$li(tags$strong("name:"), " County name"),
                       tags$li(tags$strong("lng:"), " Longitude coordinate"),
                       tags$li(tags$strong("lat:"), " Latitude coordinate")
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