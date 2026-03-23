# INTERMEDIATE COPY OF UI FOR SAVING 


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
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&family=Lora:wght@400;600;700&display=swap');
      
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
        border-bottom: none !important;
        background: #2d5016 !important;
        padding-left: 40px;
      }
      
      .nav-tabs > li > a {
        color: rgba(255,255,255,0.85) !important;
        background: transparent !important;
        border: none !important;
        padding: 16px 28px;
        font-weight: 500;
        font-size: 15px;
      }
      
      .nav-tabs > li > a:hover {
        background: rgba(255,255,255,0.1) !important;
        color: white !important;
        border: none !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: white !important;
        background: #4a7c2a !important;
        border: none !important;
        border-bottom: 3px solid #ffc857 !important;
      }
      
      .navbar {
        background: #2d5016 !important;
        border: none !important;
        margin-bottom: 0 !important;
        margin-top: 0 !important;
      }
      
      .navbar-default {
        background-color: #2d5016 !important;
        border-color: transparent !important;
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
      font-size: 1.5rem !important;
      color: #666;
        font-weight: 300;
      margin: 0;
    }
    
    .home-section {
      max-width: 1200px;
      margin: 0 auto;
      padding: 0 3rem 2rem;
    }
    
    .home-section > * {
      max-width: 100%;
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
    
    .about-card-text { 
      flex: 0 0 60%; }
    
    .about-card h2 {
      font-family: 'Lora', serif !important;
      font-size: 1.9rem !important;
      font-weight: 700 !important;
      color: #1a3d0a !important;
      margin-bottom: 1rem !important;
      border-bottom: none !important;
      padding-bottom: 0 !important;
    }
    .about-illustration {
      flex: 0 0 35%;         /* Image takes up 35% */
      max-width: 400px;      /* Prevents it from getting absurdly huge */
      width: 100%;           /* Ensures it fills its 35% container */
      height: auto;
      border-radius: 10px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1); /* Optional: makes the image pop */
    }
    
    .home-hero h1 {
      font-family: 'Lora', serif !important;
      font-size: clamp(2rem, 5vw, 3rem) !important;
      font-weight: 700 !important;
      color: #1a3d0a !important;
      margin-bottom: 0.75rem !important;
      letter-spacing: -0.01em !important;
    }
    
    .how-to-card h2 {
      font-family: 'Lora', serif !important;
      font-size: 2.2rem !important;
      font-weight: 700 !important;
      color: white !important;
      margin-bottom: 2.5rem !important;
    }
    
    .home-plain-card h2 {
      font-family: 'Lora', serif !important;
      font-size: 1.9rem !important;
      font-weight: 700 !important;
      color: #1a3d0a !important;
      margin-bottom: 1.25rem !important;
      border-bottom: 3px solid #4a7c2a !important;
      padding-bottom: 0.6rem !important;
    }
    
    .about-card p {
      font-size: 1.5rem !important;
      color: #4a4a4a;
      line-height: 1.8;
      margin-bottom: 0.75rem;
    }
    
    .about-card p:last-child { margin-bottom: 0; }
    
    .about-illustration { width: 200px; flex-shrink: 0; }
    
    .features-heading {
      font-family: 'Lora', serif !important;
      font-size: 2rem !important;
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
      background: white !important;
      border-radius: 10px !important;
      box-shadow: 0 2px 12px rgba(0,0,0,0.07) !important;
      padding: 1.75rem 1.5rem !important;
      border-top: 4px solid #4a7c2a !important;
    }
    
    .feature-card h3 {
      font-family: 'Lora', serif !important;
      font-size: 1.5rem !important;
      font-weight: 700 !important;
      color: #1a3d0a !important;
      margin-bottom: 0.6rem !important;
    }
    
    .feature-card p {
      font-size: 1.5rem !important;
      color: #6b7280 !important;
      line-height: 1.7 !important;
      margin: 0 !important;
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
      width: 64px !important;
      height: 64px !important;
      border-radius: 50%;
      background: rgba(255,255,255,0.15);
      color: white;
      font-family: 'Lora', serif;
      font-size: 1.6rem !important;
      font-weight: 700;
      display: flex;
      align-items: center;
      justify-content: center;
      margin: 0 auto 1rem;
    }
    
    .step p {
      font-size: 1.15rem !important;
      color: rgba(255,255,255,0.8) !important;
      line-height: 1.7 !important;
      margin: 0;
    }
      
    .step h3 {
      font-size: 1.2rem !important;
      font-weight: 600;
      color: white !important;
      margin-bottom: 0.5rem;
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
      font-size: 1.5rem !important;
      color: #4a4a4a;
      line-height: 1.75;
      margin-bottom: 0.3rem;
    }
    
    .plot-container {
      background: white;
      padding: 20px;
      border-radius: 10px;
      margin-bottom: 25px;
      box-shadow: 0 2px 10px rgba(0,0,0,0.05);
    }
    .home-plain-card {
      display: flex;
      flex-direction: column;
      background: white;
      padding: 35px;
      border-radius: 12px;
      margin-bottom: 25px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.05);
    }
    
    /* Centering the image wrapper */
    .card-image-wrapper {
      display: flex;
      justify-content: center; /* Centers image horizontally */
      margin-top: 25px;       /* Pushes image away from the list */
      width: 100%;
    }
    
    /* Consistent styling for both bottom images */
    .bottom-stack-img {
      width: 100%;
      max-width: 700px;      /* Matches the width of your farm photo */
      height: auto;
      border-radius: 8px;
      border: 1px solid #eee; /* Subtle border for a clean look */
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
                 h1("Parkinson's Disease and Environmental Factors Dashboard"),
                 p(class = "hero-subtitle",
                   "Exploring Parkinson's Disease, Agricultural Data, Pesticide Exposure & Life Expectancy across the United States")
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
                     
                     tags$img(
                       src = "about_image.jpg",
                       class = "about-illustration",
                       alt = "Medical research illustration"
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
                     ),
                     div(class = "card-image-wrapper",
                         tags$img(src = "questions_image.webp", class = "bottom-stack-img")
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
                       tags$li(tags$strong("cfips_location.csv"), " — County coordinates (County Federal Processing Standard codes, county names, longitude, latitude)")
                     ),
                     div(class = "card-image-wrapper",
                         tags$img(src = "datasets_image.jpg", class = "bottom-stack-img")
                     )
                 )
             ) # End home-section
    ), # End Home tabPanel
    
    
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
                 
                 
                 # State-level pesticide vs Parkinson's mortality
                 div(class = "plot-container",
                     h2("Pesticide Use vs. Parkinson's Mortality Rate by State"),
                     p("Select a pesticide to view its relationship with average Parkinson's death rate at the state level. States with missing pesticide data are excluded."),
                     
                     selectInput(
                       inputId  = "selected_state_pesticide",
                       label    = "Select Pesticide:",
                       choices  = c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos"),
                       selected = "Glyphosate"
                     ),
                     
                     fluidRow(
                       column(8,
                              plotlyOutput("plot_state_pesticide_parkinson", height = "500px")
                       ),
                       column(4,
                              h4("Correlation Result"),
                              verbatimTextOutput("cor_state_pesticide_parkinson"),
                              br(),
                              h4("Regression Summary"),
                              verbatimTextOutput("reg_state_pesticide_parkinson")
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
                 ),
                 
                 # =============================================================================
                 # ANOVA SECTION: EXPOSURE LEVEL VS HEALTH OUTCOMES
                 # =============================================================================
                 
                 # --- ANOVA 1: County Pesticide Exposure vs Life Expectancy ---
                 div(class = "plot-container",
                     h2("ANOVA: Pesticide Exposure Level vs. Life Expectancy (County-Level)"),
                     p("Counties are grouped into Low, Medium, and High exposure tertiles for each 
       pesticide. One-way ANOVA tests whether life expectancy differs significantly 
       across exposure levels. Select a pesticide to view its results."),
                     
                     selectInput(
                       inputId  = "anova_county_compound",
                       label    = "Select Pesticide:",
                       choices  = c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos"),
                       selected = "Glyphosate"
                     ),
                     
                     fluidRow(
                       column(8,
                              plotlyOutput("plot_anova_exposure_life", height = "500px")
                       ),
                       column(4,
                              h4("ANOVA Summary"),
                              verbatimTextOutput("anova_exposure_life"),
                              br(),
                              h4("Tukey Post-Hoc Summary"),
                              verbatimTextOutput("tukey_exposure_life_print")
                       )
                     ),
                     br(),
                     h4("Tukey Post-Hoc Table"),
                     DTOutput("tukey_exposure_life_table")
                 ),
                 
                 # --- ANOVA 2: State Pesticide Exposure vs Parkinson's Mortality ---
                 div(class = "plot-container",
                     h2("ANOVA: Pesticide Exposure Level vs. Parkinson's Death Rate (State-Level)"),
                     p("States are grouped into Low, Medium, and High exposure tertiles for each 
       pesticide. One-way ANOVA tests whether Parkinson's death rate differs 
       significantly across exposure levels. Select a pesticide to view its results."),
                     
                     selectInput(
                       inputId  = "anova_state_compound",
                       label    = "Select Pesticide:",
                       choices  = c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos"),
                       selected = "Glyphosate"
                     ),
                     
                     fluidRow(
                       column(8,
                              plotlyOutput("plot_anova_exposure_parkinson", height = "500px")
                       ),
                       column(4,
                              h4("ANOVA Summary"),
                              verbatimTextOutput("anova_exposure_parkinson"),
                              br(),
                              h4("Tukey Post-Hoc Summary"),
                              verbatimTextOutput("tukey_exposure_parkinson_print")
                       )
                     ),
                     br(),
                     h4("Tukey Post-Hoc Table"),
                     DTOutput("tukey_exposure_parkinson_table")
                 ),
                 
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
                     downloadButton("download_data", "Download Dataset", 
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "content-box",
                     h2("Farm Data"),
                     p("State-level agricultural data."),
                     DTOutput("data_table_farms"),
                     br(),
                     downloadButton("download_data", "Download Dataset", 
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "content-box",
                     h2("Life Expectancy Data"),
                     p("State-level life expectancy data."),
                     DTOutput("data_table_life_expectancy"),
                     br(),
                     downloadButton("download_data", "Download Dataset", 
                                    class = "btn btn-success btn-lg")
                 ),
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


