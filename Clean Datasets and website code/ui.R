library(shiny)
library(DT)
library(leaflet)
library (plotly)

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
      
    
    /* ---- HOME PAGE TEMPLATE STYLES ---- */
      
      .home-hero {
        text-align: center;
        padding: 5rem 2rem 3.5rem;
        background: #f5f5f5;
      }
    
    .home-hero h1 {
      font-family: 'Lora', serif;
      font-size: clamp(2rem, 5vw, 3rem);
      font-weight: 700;
      color: #1a3d0a;
        margin-bottom: 0.75rem;
      letter-spacing: -0.01em;
    }
    
    .home-hero .hero-subtitle {
      font-size: 1.05rem;
      color: #666;
        font-weight: 300;
      margin: 0;
    }
    
    .home-section {
      max-width: 900px;
      margin: 0 auto;
      padding: 0 2rem 2rem;
    }
    
    .about-card {
      background: white;
      border-radius: 12px;
      box-shadow: 0 4px 24px rgba(0,0,0,0.08);
      padding: 2.5rem 3rem;
      margin-bottom: 2rem;
      display: flex;
      gap: 2.5rem;
      align-items: center;
    }
    
    .about-card-text { flex: 1; }
    
    .about-card h2 {
      font-family: 'Lora', serif;
      font-size: 1.5rem;
      font-weight: 700;
      color: #1a3d0a;
        margin-bottom: 1rem;
      border-bottom: none !important;
      padding-bottom: 0 !important;
    }
    
    .about-card p {
      font-size: 0.95rem;
      color: #4a4a4a;
        line-height: 1.75;
      margin-bottom: 0.75rem;
    }
    
    .about-card p:last-child { margin-bottom: 0; }
    
    .about-illustration { width: 200px; flex-shrink: 0; }
    
    .features-heading {
      font-family: 'Lora', serif;
      font-size: 1.5rem;
      font-weight: 600;
      color: #1a3d0a;
        text-align: center;
      margin: 0.5rem 0 1.25rem;
    }
    
    .features-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 1.25rem;
      margin-bottom: 2rem;
    }
    
    .feature-card {
      background: white;
      border-radius: 10px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.07);
      padding: 1.75rem 1.5rem;
      border-top: 4px solid #4a7c2a;
    }
    
    .feature-card h3 {
      font-family: 'Lora', serif;
      font-size: 1rem;
      font-weight: 700;
      color: #1a3d0a;
        margin-bottom: 0.5rem;
    }
    
    .feature-card p {
      font-size: 0.875rem;
      color: #6b7280;
        line-height: 1.6;
      margin: 0;
    }
    
    .how-to-card {
      background: #1a3d0a;
        border-radius: 12px;
      padding: 3rem 2.5rem;
      margin-bottom: 2rem;
      text-align: center;
    }
    
    .how-to-card h2 {
      font-family: 'Lora', serif;
      font-size: 1.6rem;
      font-weight: 700;
      color: white;
      margin-bottom: 2.5rem;
    }
    
    .steps-grid {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 2rem;
    }
    
    .step { text-align: center; }
    
    .step-number {
      width: 48px;
      height: 48px;
      border-radius: 50%;
      background: rgba(255,255,255,0.15);
      color: white;
      font-family: 'Lora', serif;
      font-size: 1.2rem;
      font-weight: 700;
      display: flex;
      align-items: center;
      justify-content: center;
      margin: 0 auto 1rem;
    }
    
    .step h3 {
      font-size: 1rem;
      font-weight: 600;
      color: white;
      margin-bottom: 0.5rem;
    }
    
    .step p {
      font-size: 0.875rem;
      color: rgba(255,255,255,0.65);
      line-height: 1.6;
      margin: 0;
    }
    
    .home-plain-card {
      background: white;
      border-radius: 12px;
      box-shadow: 0 4px 24px rgba(0,0,0,0.08);
      padding: 2.5rem 3rem;
      margin-bottom: 2rem;
    }
    
    .home-plain-card h2 {
      font-family: 'Lora', serif;
      font-size: 1.4rem;
      font-weight: 700;
      color: #1a3d0a;
        margin-bottom: 1.25rem;
      border-bottom: 3px solid #4a7c2a;
      padding-bottom: 0.6rem;
    }
    
    .home-plain-card ul { padding-left: 1.25rem; }
    
    .home-plain-card li {
      font-size: 0.95rem;
      color: #4a4a4a;
        line-height: 1.75;
      margin-bottom: 0.3rem;
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
             
             div(class = "home-hero",
                 h1("Medical Trends Analysis Dashboard"),
                 p(class = "hero-subtitle",
                   "Exploring Parkinson's Disease, Pesticide Exposure & Life Expectancy across the United States")
             ),
             
             div(class = "home-section",
                 
                 div(class = "about-card",
                     div(class = "about-card-text",
                         h2("About This Project"),
                         p("Parkinson's disease affects millions of Americans, yet the environmental
           drivers behind its geographic distribution remain poorly understood. This
           project investigates whether pesticide use and agricultural intensity are
           associated with higher Parkinson's mortality rates across U.S. states and counties."),
                         p("Our interactive dashboard integrates CDC mortality data, USDA pesticide
           and farm records, and CDC WONDER life-expectancy figures to visualize
           geographic patterns and surface potential correlations.")
                     ),
                     
                     tags$svg(
                       class = "about-illustration",
                       viewBox = "0 0 220 240",
                       xmlns = "http://www.w3.org/2000/svg",
                       tags$circle(cx="110", cy="120", r="100", fill="#e8f5e2"),
                       tags$ellipse(cx="90",  cy="160", rx="18", ry="50", fill="#4a7c2a",
                                    transform="rotate(-20 90 160)"),
                       tags$ellipse(cx="110", cy="155", rx="18", ry="55", fill="#2d5016",
                                    transform="rotate(0 110 155)"),
                       tags$ellipse(cx="130", cy="160", rx="18", ry="50", fill="#4a7c2a",
                                    transform="rotate(20 130 160)"),
                       tags$ellipse(cx="75",  cy="165", rx="14", ry="42", fill="#6aaa3a",
                                    transform="rotate(-35 75 165)"),
                       tags$ellipse(cx="145", cy="165", rx="14", ry="42", fill="#6aaa3a",
                                    transform="rotate(35 145 165)"),
                       tags$circle(cx="85",  cy="80", r="7", fill="#2d5016"),
                       tags$circle(cx="110", cy="68", r="7", fill="#4a7c2a"),
                       tags$circle(cx="135", cy="80", r="7", fill="#2d5016"),
                       tags$line(x1="85", y1="80", x2="110", y2="68",
                                 stroke="#6aaa3a", `stroke-width`="2"),
                       tags$line(x1="110", y1="68", x2="135", y2="80",
                                 stroke="#6aaa3a", `stroke-width`="2"),
                       tags$circle(cx="88",  cy="98", r="6", fill="#4a7c2a"),
                       tags$circle(cx="110", cy="90", r="6", fill="#2d5016"),
                       tags$circle(cx="132", cy="98", r="6", fill="#4a7c2a"),
                       tags$line(x1="88", y1="98", x2="110", y2="90",
                                 stroke="#6aaa3a", `stroke-width`="2"),
                       tags$line(x1="110", y1="90", x2="132", y2="98",
                                 stroke="#6aaa3a", `stroke-width`="2")
                     )
                 ),
                 
                 h2(class = "features-heading", "Key Features"),
                 div(class = "features-grid",
                     div(class = "feature-card",
                         tags$h3("Interactive Maps"),
                         tags$p("Explore Parkinson's mortality, pesticide use, and life expectancy
                geographically across all U.S. states and counties.")
                     ),
                     div(class = "feature-card",
                         tags$h3("Correlation Analysis"),
                         tags$p("Compare pesticide exposure and farm density against disease rates
                to surface potential environmental risk patterns.")
                     ),
                     div(class = "feature-card",
                         tags$h3("Evidence-Based Data"),
                         tags$p("Built on CDC, USDA, and CDC WONDER datasets with transparent
                methodology and acknowledged limitations.")
                     )
                 ),
                 
                 div(class = "how-to-card",
                     tags$h2("How to Use This Dashboard"),
                     div(class = "steps-grid",
                         div(class = "step",
                             div(class = "step-number", "1"),
                             tags$h3("Select a Topic"),
                             tags$p("Navigate using the tabs to explore maps, visualizations, or raw data tables.")
                         ),
                         div(class = "step",
                             div(class = "step-number", "2"),
                             tags$h3("Apply Filters"),
                             tags$p("Use the sidebar controls on the Maps tab to switch between datasets.")
                         ),
                         div(class = "step",
                             div(class = "step-number", "3"),
                             tags$h3("Explore Insights"),
                             tags$p("Interact with the maps and charts to discover geographic patterns and correlations.")
                         )
                     )
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Research Questions"),
                     tags$ul(
                       tags$li("Is there a relationship between pesticide use and Parkinson's disease rates?"),
                       tags$li("How does agricultural intensity (number of farms) correlate with health outcomes?"),
                       tags$li("What are the geographic patterns of Parkinson's disease across states?"),
                       tags$li("How does pesticide exposure relate to life expectancy at the county level?")
                     )
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Datasets Used"),
                     tags$ul(
                       tags$li(tags$strong("Parkinsons_mortality_rates_clean.csv"), " — Parkinson's death rates by state"),
                       tags$li(tags$strong("pesticides_by_county.csv"), " — Pesticide usage by county"),
                       tags$li(tags$strong("LifeExpectancyStateData_clean.csv"), " — Life expectancy by state"),
                       tags$li(tags$strong("ExpectancyData_clean.csv"), " — Life expectancy by county"),
                       tags$li(tags$strong("Farm_Data_2024.csv"), " — Number and size of farms by state"),
                       tags$li(tags$strong("cfips_location.csv"), " — County coordinates (cfips, name, lng, lat)")
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
                 ),
                # State level Farm Count VS Parkinsons data
                  div(class = "plot-container",
                     h2("Number of Farms vs. Parkinson's Death Rate by State"),
                     p("State-level scatter plot showing the relationship between number of farms and Parkinson's average death rate."),
                     
                     fluidRow(
                       column(8,
                              plotlyOutput("plot_farm_parkinson_detailed", height = "500px")
                       ),
                       column(4,
                              h4("Correlation Result"),
                              verbatimTextOutput("cor_farm_parkinson_detailed"),
                              br(),
                              h4("Regression Summary"),
                              verbatimTextOutput("reg_farm_parkinson_detailed")
                       )
                     )
                 ),
                 
                 # County-level pesticide vs life expectancy
                 div(class = "plot-container",
                     h2("Pesticide Use vs. Life Expectancy by County"),
                     p("Select a pesticide to view its relationship with average life expectancy at the county level."),
                     
                     selectInput(
                       inputId  = "selected_pesticide",
                       label    = "Select Pesticide:",
                       choices  = c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos"),
                       selected = "Glyphosate"
                     ),
                     
                     fluidRow(
                       column(8,
                              plotlyOutput("plot_county_pesticide_life", height = "500px")
                       ),
                       column(4,
                              h4("Correlation Result"),
                              verbatimTextOutput("cor_county_pesticide_life"),
                              br(),
                              h4("Regression Summary"),
                              verbatimTextOutput("reg_county_pesticide_life")
                       )
                     )

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









