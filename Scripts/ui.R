# ui.R - User Interface for Medical Trends Dashboard
# Parkinson's Disease, Pesticides, Life Expectancy, and Farm Data

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
                       tags$li(tags$strong("Parkinsons_mortality_rates_clean.csv"), " - Parkinson's death rates by state"),
                       tags$li(tags$strong("pesticides_by_county.csv"), " - Pesticide usage by county"),
                       tags$li(tags$strong("LifeExpectancyStateData_clean.csv"), " - Life expectancy by state"),
                       tags$li(tags$strong("ExpectancyData_clean.csv"), " - Life expectancy by county"),
                       tags$li(tags$strong("Farm_Data_2024.csv"), " - Number and size of farms by state")
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
                       tags$li("CDC - Parkinson's disease mortality statistics"),
                       tags$li("USDA - Pesticide usage and farm statistics"),
                       tags$li("CDC WONDER - Life expectancy data")
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