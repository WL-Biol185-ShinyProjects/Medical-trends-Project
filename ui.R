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