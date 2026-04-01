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
      
      .content-box h2 {
        font-family: 'Lora', serif !important;
        font-size: 2.5rem !important;
        font-weight: 700 !important;
        color: #1a3d0a !important;
        margin-bottom: 1.25rem !important;
        border-bottom: 3px solid #4a7c2a !important;
        padding-bottom: 0.6rem !important;
      }
      
      .content-box p {
        font-size: 1.5rem !important;
        color: #4a4a4a;
        line-height: 1.75;
        margin-bottom: 1rem;
      }
      
      .page-header {
        font-family: 'Lora', serif !important;
        color: #1a3d0a !important;
        font-size: 2.5rem !important;
        margin-bottom: 1.5rem !important;
        font-weight: 700 !important;
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
      font-size: clamp(2.5rem, 6vw, 4rem);
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
      font-size: 2.5rem !important;
      font-weight: 700 !important;
      color: #1a3d0a !important;
      margin-bottom: 1rem !important;
      border-bottom: none !important;
      padding-bottom: 0 !important;
    }
    .about-illustration {
      flex: 0 0 35%;
      max-width: 400px;
      width: 100%;
      height: auto;
      border-radius: 10px;
      box-shadow: 0 4px 15px rgba(0,0,0,0.1);
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
      font-size: 2.5rem !important;
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
      font-size: 2.5rem !important;
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
      font-size: 1.9rem !important;
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
      font-size: 1.35rem !important;
      color: rgba(255,255,255,0.8) !important;
      line-height: 1.7 !important;
      margin: 0;
    }
      
    .step h3 {
      font-size: 1.5rem !important;
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
  padding: 2.5rem 3rem;
  border-radius: 12px;
  margin-bottom: 2rem;
  box-shadow: 0 4px 24px rgba(0,0,0,0.08);
}

.plot-container h2 {
  font-family: 'Lora', serif !important;
  font-size: 1.9rem !important;
  font-weight: 700 !important;
  color: #1a3d0a !important;
  margin-bottom: 1.25rem !important;
  border-bottom: 3px solid #4a7c2a !important;
  padding-bottom: 0.6rem !important;
}

.plot-container > p {
  font-size: 1.5rem !important;
  color: #4a4a4a !important;
  line-height: 1.8 !important;
  margin-bottom: 1.25rem !important;
}

.plot-container h4 {
  font-family: 'Lora', serif !important;
  font-size: 1.35rem !important;
  font-weight: 700 !important;
  color: #2d5016 !important;
  margin-bottom: 0.5rem !important;
  border-bottom: 1px solid #e0e0e0 !important;
  padding-bottom: 0.3rem !important;
}

.plot-container .selectize-input,
.plot-container label {
  font-family: 'Inter', sans-serif !important;
  font-size: 1.3rem !important;
  color: #333 !important;
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
    
    .card-image-wrapper {
      display: flex;
      justify-content: center;
      margin-top: 25px;
      width: 100%;
    }
    
    .bottom-stack-img {
      width: 100%;
      max-width: 600px;
      height: auto;
      border-radius: 8px;
      border: 1px solid #eee;
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
                         p("Parkinson's disease (PD) affects millions of Americans, but the environmental
           risk factors associated with developing this disease remain poorly understood. Studies have suggested that certain pesticides can cause neurological harm resulting in increased risk of developing PD. This
           project investigates whether the use of certain pesticides (Paraquat, Glyphosate, Chlorpyrifos, 2,4-D) and agricultural intensity are
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
                geographically across U.S. states and counties.")
                     ),
                     div(class = "feature-card",
                         tags$h3("Data Analysis"),
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
                       tags$li(tags$strong("Parkinsons_mortality_rates_clean.csv"), " -- Parkinson's death rates by state"),
                       tags$li(tags$strong("pesticides_by_county.csv"), " -- Pesticide usage by county"),
                       tags$li(tags$strong("LifeExpectancyStateData_clean.csv"), " -- Life expectancy by state"),
                       tags$li(tags$strong("ExpectancyData_clean.csv"), " -- Life expectancy by county"),
                       tags$li(tags$strong("Farm_Data_2024.csv"), " -- Number and size of farms by state"),
                       tags$li(tags$strong("cfips_location.csv"), " -- County coordinates (County Federal Processing Standard codes, county names, longitude, latitude)")
                     ),
                     div(class = "card-image-wrapper",
                         tags$img(src = "datasets_image.jpg", class = "bottom-stack-img")
                     )
                 )
             ) # End home-section
    ), # End Home tabPanel
    
    
    # =========================================================================
    # MAPS TAB
    # =========================================================================
    tabPanel("Maps",
             div(class = "main-container",
                 h1(class = "page-header", "Geographic Analysis"),
                 
                 div(class = "map-viewer-container",
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
                     div(class = "map-content",
                         div(class = "map-title-bar",
                             textOutput("map_title")
                         ),
                         leafletOutput("main_map", height = "640px"),
                         
                         conditionalPanel(
                           condition = "input.selected_map == 'map3'",
                           div(
                             style = "padding: 12px 20px; background: #f8f9fa; border-top: 1px solid #e0e0e0;",
                             selectInput(
                               inputId = "map3_state_selector",
                               label   = "Zoom to State:",
                               choices = c("All States" = "all"),
                               selected = "all",
                               width   = "280px"
                             )
                           )
                         ) 
                  
                     )
                 )
             )
    ),
    
    # =========================================================================
    # DATA VISUALIZATION TAB
    # =========================================================================
    tabPanel("Data Visualization",
             
             div(class = "home-hero",
                 h1("Data Visualizations"),
                 p(class = "hero-subtitle",
                   "Explore statistical relationships between pesticide exposure and neurological health outcomes across U.S. states and counties")
             ),
             
             div(class = "home-section",
                 
                 # ==========================================================
                 # SUMMARY BANNER
                 # ==========================================================
                 HTML('
<div style="
  background: linear-gradient(135deg, #1a3009 0%, #2d5016 60%, #3d6b1f 100%);
  border-radius: 12px;
  padding: 36px 40px 32px 40px;
  margin-bottom: 36px;
  box-shadow: 0 4px 24px rgba(45,80,22,0.18);
  color: white;
  font-family: Georgia, serif;
">
  <div style="text-align: center; margin-bottom: 32px;">
    <div style="font-size: 11px; font-family: monospace; letter-spacing: 4px; text-transform: uppercase; color: #a7c957; margin-bottom: 8px;">Analysis Overview</div>
    <h2 style="margin: 0 0 10px 0; font-size: 26px; font-weight: normal; letter-spacing: 1px; color: white;">Understanding the Visualizations</h2>
    <p style="margin: 0 auto; max-width: 680px; font-size: 14px; color: rgba(255,255,255,0.75); line-height: 1.7; font-family: Arial, sans-serif;">
      This dashboard investigates relationships between agricultural pesticide exposure and health outcomes, including Parkinson\'s Death Rate, using county- and state-level data across the United States. Explore the dropdown menus within each section to explore statistical results and data visualizations by pesticide compound. Review information about the statistical values reported in the section using the glossary at the bottom of the page.
    </p>
  </div>
  <div style="border-top: 1px solid rgba(167,201,87,0.3); margin-bottom: 32px;"></div>
  <div style="display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 24px;">
 
    <div style="background: rgba(255,255,255,0.07); border-radius: 10px; padding: 24px 20px; border-top: 3px solid #a7c957;">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="8" cy="32" r="3" fill="#a7c957"/><circle cx="14" cy="24" r="3" fill="#a7c957"/>
          <circle cx="20" cy="26" r="3" fill="#a7c957"/><circle cx="26" cy="16" r="3" fill="#a7c957"/>
          <circle cx="32" cy="12" r="3" fill="#a7c957"/><circle cx="19" cy="20" r="3" fill="#a7c957" opacity="0.5"/>
          <line x1="5" y1="35" x2="35" y2="35" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
          <line x1="5" y1="35" x2="5" y2="5" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="font-size: 10px; letter-spacing: 3px; text-transform: uppercase; color: #a7c957; font-family: monospace; text-align: center; margin-bottom: 10px;">Scatter Plot</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        The scatter plots below visualize the raw relationship between pesticide exposure and health outcomes, with each point representing a county or state.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #a7c957; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Pesticide Use vs. Parkinson\'s Death Rate | Number of Farms vs. Parkinson\'s Death Rate | Pesticide Use vs. Life Expectancy
        </p>
        <div style="font-size: 11px; color: #a7c957; font-family: monospace; margin-bottom: 6px;">HOW TO INTERACT</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          Use the <strong style="color:white;">Select Pesticide</strong> dropdown to switch between compounds. Hover over points to see county or state details.
        </p>
      </div>
    </div>
 
    <div style="background: rgba(255,255,255,0.07); border-radius: 10px; padding: 24px 20px; border-top: 3px solid #f4a261;">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="8" cy="30" r="2.5" fill="rgba(255,255,255,0.5)"/><circle cx="14" cy="26" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="20" cy="22" r="2.5" fill="rgba(255,255,255,0.5)"/><circle cx="26" cy="17" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="32" cy="13" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <line x1="5" y1="32" x2="35" y2="10" stroke="#f4a261" stroke-width="2" stroke-dasharray="4 2"/>
          <line x1="5" y1="35" x2="35" y2="35" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
          <line x1="5" y1="35" x2="5" y2="5" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="font-size: 10px; letter-spacing: 3px; text-transform: uppercase; color: #f4a261; font-family: monospace; text-align: center; margin-bottom: 10px;">Regression</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        Linear regression models the direction and strength of the relationship between two variables, shown as a dashed trend line overlaid on the scatter plot.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #f4a261; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Same pairings as scatter plots. Full regression summary is shown in the side panel.
        </p>
        <div style="font-size: 11px; color: #f4a261; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          <strong style="color:white;">R²</strong> measures how well the trend line fits the data -- an R² of 0.40 means 40% of the variation in health outcomes is explained by pesticide exposure. <strong style="color:white;">p &lt; 0.05</strong> on the slope confirms the trend is unlikely to be due to chance.
        </p>
      </div>
    </div>
 
    <div style="background: rgba(255,255,255,0.07); border-radius: 10px; padding: 24px 20px; border-top: 3px solid #7ec8e3;">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="20" cy="20" r="13" stroke="#7ec8e3" stroke-width="1.5" fill="none"/>
          <circle cx="20" cy="20" r="7" stroke="#7ec8e3" stroke-width="1.5" fill="none" opacity="0.5"/>
          <line x1="20" y1="7" x2="20" y2="33" stroke="#7ec8e3" stroke-width="1" opacity="0.4"/>
          <line x1="7" y1="20" x2="33" y2="20" stroke="#7ec8e3" stroke-width="1" opacity="0.4"/>
          <text x="14" y="24" font-size="10" fill="#7ec8e3" font-family="monospace">r</text>
        </svg>
      </div>
      <div style="font-size: 10px; letter-spacing: 3px; text-transform: uppercase; color: #7ec8e3; font-family: monospace; text-align: center; margin-bottom: 10px;">Correlation</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        Pearson\'s correlation coefficient (r) quantifies how strongly two variables move together, from -1 (perfect negative) to +1 (perfect positive).
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #7ec8e3; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Same as scatter plot.
        </p>
        <div style="font-size: 11px; color: #7ec8e3; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          The <strong style="color:white;">r value</strong> is shown on each scatter plot. The side panel gives the full test output including 95% confidence interval and p-value.
        </p>
      </div>
    </div>
 
    <div style="background: rgba(255,255,255,0.07); border-radius: 10px; padding: 24px 20px; border-top: 3px solid #e76f51;">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <rect x="4" y="18" width="8" height="10" rx="1" stroke="#e76f51" stroke-width="1.5" fill="rgba(231,111,81,0.2)"/>
          <line x1="8" y1="18" x2="8" y2="13" stroke="#e76f51" stroke-width="1.5"/>
          <line x1="8" y1="28" x2="8" y2="33" stroke="#e76f51" stroke-width="1.5"/>
          <line x1="5" y1="22" x2="11" y2="22" stroke="#e76f51" stroke-width="1.5"/>
          <rect x="16" y="14" width="8" height="12" rx="1" stroke="#e76f51" stroke-width="1.5" fill="rgba(231,111,81,0.2)"/>
          <line x1="20" y1="14" x2="20" y2="9" stroke="#e76f51" stroke-width="1.5"/>
          <line x1="20" y1="26" x2="20" y2="31" stroke="#e76f51" stroke-width="1.5"/>
          <line x1="17" y1="19" x2="23" y2="19" stroke="#e76f51" stroke-width="1.5"/>
          <rect x="28" y="10" width="8" height="14" rx="1" stroke="#e76f51" stroke-width="1.5" fill="rgba(231,111,81,0.2)"/>
          <line x1="32" y1="10" x2="32" y2="5" stroke="#e76f51" stroke-width="1.5"/>
          <line x1="32" y1="24" x2="32" y2="29" stroke="#e76f51" stroke-width="1.5"/>
          <line x1="29" y1="16" x2="35" y2="16" stroke="#e76f51" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="font-size: 10px; letter-spacing: 3px; text-transform: uppercase; color: #e76f51; font-family: monospace; text-align: center; margin-bottom: 10px;">ANOVA & Tukey</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        ANOVA tests whether health outcomes differ significantly across pesticide exposure levels (Low to High). Tukey\'s post-hoc identifies which specific pairs of groups drive that difference.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #e76f51; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          County Pesticide Exposure Level vs. Life Expectancy | State Pesticide Exposure Level vs. Parkinson\'s Death Rate
        </p>
        <div style="font-size: 11px; color: #e76f51; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          ANOVA p &lt; 0.05 means at least one group differs. Check the <strong style="color:white;">Tukey table</strong> -- pairs highlighted in green have adjusted p &lt; 0.05 and are significantly different.
        </p>
      </div>
    </div>
 
  </div>
</div>
'),
                 
                 # -----------------------------------------------------------------------
                 # State-level pesticide vs Parkinson's mortality
                 # -----------------------------------------------------------------------
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
                              uiOutput("stats_state_pesticide_parkinson")
                       )
                     )
                 ),
                 
                 # -----------------------------------------------------------------------
                 # State level Farm Count VS Parkinsons data
                 # -----------------------------------------------------------------------
                 div(class = "plot-container",
                     h2("Number of Farms vs. Parkinson's Death Rate by State"),
                     p("State-level scatter plot showing the relationship between number of farms and Parkinson's average death rate."),
                     fluidRow(
                       column(8,
                              plotlyOutput("plot_farm_parkinson_detailed", height = "500px")
                       ),
                       column(4,
                              uiOutput("stats_farm_parkinson")
                       )
                     )
                 ),
                 
                 # -----------------------------------------------------------------------
                 # County-level pesticide vs life expectancy
                 # -----------------------------------------------------------------------
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
                              uiOutput("stats_county_pesticide_life")
                       )
                     )
                 ),
                 
                 # =============================================================================
                 # ANOVA SECTION
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
                              uiOutput("stats_anova_exposure_life")
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
                              uiOutput("stats_anova_exposure_parkinson")
                       )
                     ),
                     br(),
                     h4("Tukey Post-Hoc Table"),
                     DTOutput("tukey_exposure_parkinson_table")
                 ),

                 # =============================================================================
                 # STATISTICS GLOSSARY ACCORDION
                 # =============================================================================
                 div(class = "plot-container",
                     h2(style = "color: #2d5016; font-size: 1.6em; font-weight: 700;
                border-bottom: 3px solid #4a7c2a; padding-bottom: 12px; margin-bottom: 20px;",
                        "Statistics Glossary"),
                     p(style = "color: #666; font-size: 14px; margin-bottom: 20px;",
                       "Click any term to expand its definition."),
                     
                     tags$style(HTML("
      .glossary-item {
        border-bottom: 1px solid #e8e8e8;
      }
      .glossary-btn {
        width: 100%;
        background: none;
        border: none;
        text-align: left;
        padding: 14px 8px;
        font-size: 14px;
        font-family: monospace;
        font-weight: 600;
        color: #2d5016;
        cursor: pointer;
        display: flex;
        justify-content: space-between;
        align-items: center;
        letter-spacing: 0.5px;
      }
      .glossary-btn:hover {
        background: #f5f9f0;
      }
      .glossary-btn .arrow {
        font-size: 11px;
        color: #a7c957;
        transition: transform 0.2s;
      }
      .glossary-btn.open .arrow {
        transform: rotate(180deg);
      }
      .glossary-body {
        display: none;
        padding: 4px 12px 16px 12px;
        font-size: 13.5px;
        color: #444;
        line-height: 1.7;
        background: #fafdf7;
        border-left: 3px solid #a7c957;
        margin: 0 4px 8px 4px;
        border-radius: 0 0 4px 4px;
      }
    ")),
                     
                     tags$script(HTML("
      function toggleGlossary(btn) {
        var body = btn.nextElementSibling;
        var isOpen = body.style.display === 'block';
        body.style.display = isOpen ? 'none' : 'block';
        btn.classList.toggle('open', !isOpen);
      }
    ")),
                     
                     # Glossary terms in alphabetical order
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "95% Confidence Interval (CI)", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "The range of values within which the true population parameter (e.g. the true correlation) 
             is estimated to fall with 95% probability. A narrow CI indicates a more precise estimate. 
             If the CI does not include zero, the result is statistically significant at p < 0.05.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "Adjusted p-value (Tukey)", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "A p-value that has been corrected for the fact that multiple pairwise comparisons are 
             being made simultaneously (in Tukey's post-hoc test). Without this correction, the 
             probability of a false positive would increase with each additional comparison. 
             Adjusted p < 0.05 indicates a significant difference between a specific pair of groups.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "Adjusted R\u00b2", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "A version of R\u00b2 that is penalized for the number of predictors in the model. 
             Unlike standard R\u00b2, it will decrease if a predictor is added that does not 
             genuinely improve the model. Useful for comparing models with different numbers 
             of variables.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "F-statistic", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "The ratio of variance explained by the model to the unexplained (residual) variance. 
             Used in both linear regression and ANOVA. A larger F-statistic indicates a stronger 
             overall model fit. The associated p-value tells you whether this F is larger than 
             you would expect by chance.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "p-value", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "The probability of observing a result at least as extreme as the one found, 
             assuming the null hypothesis (no relationship) is true. A p-value below 0.05 
             is the conventional threshold for statistical significance, meaning there is 
             less than a 5% chance the result is due to random variation alone.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "r (Pearson Correlation Coefficient)", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "Measures the strength and direction of the linear relationship between two variables. 
             Ranges from -1 (perfect negative relationship) to +1 (perfect positive relationship). 
             A value near 0 indicates little to no linear relationship. Note that correlation 
             does not imply causation.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "R\u00b2 (Coefficient of Determination)", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "The proportion of variance in the outcome variable (e.g. life expectancy) that is 
             explained by the predictor (e.g. pesticide use). An R\u00b2 of 0.25 means 25% of 
             the variation in the outcome is accounted for by the model. R\u00b2 ranges from 
             0 (no fit) to 1 (perfect fit).")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "Slope", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "The regression coefficient for the predictor variable. It represents how much the 
             outcome variable is expected to change for each one-unit increase in the predictor. 
             For example, a positive slope indicates that higher pesticide exposure is associated with a higher 
             outcome value; a negative slope indicates the reverse.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "Slope SE (Standard Error)", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "The standard error of the slope estimate, measuring the precision of the regression 
             coefficient. A smaller SE relative to the slope indicates a more reliable estimate. 
             It is used to calculate the t-statistic and confidence interval for the slope.")
                     ),
                     div(class = "glossary-item",
                         tags$button(class = "glossary-btn", onclick = "toggleGlossary(this)",
                                     "t-statistic", tags$span(class = "arrow", "▼")),
                         div(class = "glossary-body",
                             "Used in the Pearson correlation test to determine whether the observed correlation 
             is significantly different from zero. It is calculated as the correlation coefficient 
             divided by its standard error. A larger absolute t-value corresponds to a smaller 
             p-value and stronger evidence against the null hypothesis.")
                     )
                 )    
             ) # end main-container
    ), # end Data Visualization tabPanel
    
   ### =========================================================================
      # Discussion TAB
      # =========================================================================
    tabPanel("Discussion",
             div(class = "main-container",
                 h1(class = "page-header", "Discussion"),
                 
                 
                 # =============================================================================
                 # MAIN TAKEAWAYS
                 # =============================================================================
                 div(class = "plot-container",
                     h2(style = "color: #2d5016; font-size: 1.6em; font-weight: 700;
                border-bottom: 3px solid #4a7c2a; padding-bottom: 12px; margin-bottom: 24px;",
                        "Main Takeaways"),
                     
                     # Intro
                     p(style = "font-size: 14px; color: #444; line-height: 1.8; margin-bottom: 28px;",
                       "The analyses below examined associations between agricultural pesticide exposure,
       farm density, and neurological health outcomes across U.S. counties and states.
       Results were mixed across compounds and levels of analysis, and all findings
       should be interpreted as observational associations rather than causal relationships."),
                     
                     # --- Section 1: Pesticide vs Parkinson's (State) ---
                     div(style = "margin-bottom: 28px;",
                         div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
                             div(style = "width: 4px; height: 28px; background: #a7c957; border-radius: 2px; flex-shrink: 0;"),
                             h3(style = "margin: 0; font-size: 1.1em; font-weight: 700; color: #1a3009;",
                                "Pesticide Use vs. Parkinson's Death Rate (State-Level Scatter Plot & Regression)")
                         ),
                         p(style = "font-size: 14px; color: #444; line-height: 1.8; margin: 0 0 10px 16px;",
                           "Of the four compounds tested, only ", tags$strong("2,4-D"), " showed a statistically
           significant positive correlation with state-level Parkinson's mortality
           (r = 0.34, p = 0.019), with higher 2,4-D use associated with modestly higher
           death rates. Glyphosate trended in the same direction (r = 0.26) but did not
           reach significance (p = 0.075). Paraquat (r = 0.18, p = 0.223) and Chlorpyrifos
           (r = 0.15, p = 0.317) showed weak, non-significant associations. Across all
           compounds, R\u00b2 values were low (range: 0.02-0.12), indicating that pesticide
           use alone explains only a small fraction of the variation in state-level
           Parkinson's mortality."
                         )
                     ),
                     
                     # --- Section 2: Farm Count vs Parkinson's ---
                     div(style = "margin-bottom: 28px;",
                         div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
                             div(style = "width: 4px; height: 28px; background: #f4a261; border-radius: 2px; flex-shrink: 0;"),
                             h3(style = "margin: 0; font-size: 1.1em; font-weight: 700; color: #1a3009;",
                                "Farm Count vs. Parkinson's Death Rate (State-Level)")
                         ),
                         p(style = "font-size: 14px; color: #444; line-height: 1.8; margin: 0 0 10px 16px;",
                           "The number of farms per state showed a statistically significant positive
           association with Parkinson's mortality (r = 0.33, p = 0.021, R\u00b2 = 0.11).
           States with more farms tended to have modestly higher Parkinson's death rates.
           This finding is consistent with the hypothesis that agricultural intensity
           is associated with increased neurological disease burden, though the effect
           size remains small and confounding factors such as age distribution,
           rural healthcare access, and overall population size cannot be ruled out."
                         )
                     ),
                     
                     # --- Section 3: Pesticide vs Life Expectancy (County) ---
                     div(style = "margin-bottom: 28px;",
                         div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
                             div(style = "width: 4px; height: 28px; background: #7ec8e3; border-radius: 2px; flex-shrink: 0;"),
                             h3(style = "margin: 0; font-size: 1.1em; font-weight: 700; color: #1a3009;",
                                "Pesticide Use vs. Life Expectancy (County-Level Scatter Plot & Regression)")
                         ),
                         p(style = "font-size: 14px; color: #444; line-height: 1.8; margin: 0 0 10px 16px;",
                           "Results at the county level were statistically significant for all four
           compounds but directionally inconsistent. ", tags$strong("2,4-D"),
                           " (r = -0.13, p < 0.001) and ", tags$strong("Paraquat"), " (r = -0.09,
           p < 0.001) showed negative associations with life expectancy, suggesting
           that counties with higher use of these compounds tended to have lower
           life expectancy. In contrast, ", tags$strong("Glyphosate"), " (r = 0.10)
           and ", tags$strong("Chlorpyrifos"), " (r = 0.19) showed positive associations,
           which is counterintuitive and likely reflects geographic confounding: these
           compounds are heavily used in large, agriculturally productive states that
           also tend to have higher median incomes and better healthcare infrastructure.
           Across all compounds, R\u00b2 values were very low (0.008-0.036), indicating
           minimal explanatory power at the county level."
                         )
                     ),
                     
                     # --- Section 4: ANOVA County ---
                     div(style = "margin-bottom: 28px;",
                         div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
                             div(style = "width: 4px; height: 28px; background: #e76f51; border-radius: 2px; flex-shrink: 0;"),
                             h3(style = "margin: 0; font-size: 1.1em; font-weight: 700; color: #1a3009;",
                                "Exposure Level vs. Life Expectancy (County ANOVA & Tukey)")
                         ),
                         p(style = "font-size: 14px; color: #444; line-height: 1.8; margin: 0 0 10px 16px;",
                           "One-way ANOVA revealed significant differences in life expectancy across
           Low, Medium, and High pesticide exposure groups for all four compounds
           (all p < 0.001). However, the direction of these differences was inconsistent
           across compounds. For ", tags$strong("2,4-D"), " and ", tags$strong("Paraquat"),
                           ", Tukey post-hoc tests confirmed that high-exposure counties had significantly
           lower life expectancy than low-exposure counties, consistent with a negative
           health association. For ", tags$strong("Glyphosate"), " and ",
                           tags$strong("Chlorpyrifos"), ", the pattern reversed: high-exposure counties
           had significantly higher life expectancy, likely driven by the same geographic
           confounding noted above. Effect sizes were small across all compounds
           (eta-squared range: 0.014-0.034), meaning exposure group explains only
           1-3% of the variance in county-level life expectancy."
                         )
                     ),
                     
                     # --- Section 5: ANOVA State ---
                     div(style = "margin-bottom: 28px;",
                         div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
                             div(style = "width: 4px; height: 28px; background: #bc4749; border-radius: 2px; flex-shrink: 0;"),
                             h3(style = "margin: 0; font-size: 1.1em; font-weight: 700; color: #1a3009;",
                                "Exposure Level vs. Parkinson's Death Rate (State ANOVA & Tukey)")
                         ),
                         p(style = "font-size: 14px; color: #444; line-height: 1.8; margin: 0 0 10px 16px;",
                           "At the state level, no ANOVA reached statistical significance for any of
           the four compounds (p range: 0.065-0.636). Glyphosate came closest
           (F = 2.90, p = 0.066), with High-exposure states showing a trend toward
           higher Parkinson's mortality, but this did not survive the 0.05 threshold.
           No Tukey pairwise comparisons were significant for any compound. The
           limited statistical power of this analysis (approximately 15-17 states
           per exposure group) is a likely contributor to these null results, and
           these findings should not be interpreted as evidence that no relationship exists."
                         )
                     ),
                     
                     # --- Overall Conclusion ---
                     div(style = "background: linear-gradient(135deg, #1a3009 0%, #2d5016 100%);
                 border-radius: 10px; padding: 24px 28px; margin-top: 8px;",
                         div(style = "font-size: 10px; letter-spacing: 3px; text-transform: uppercase;
                     color: #a7c957; font-family: monospace; margin-bottom: 14px;",
                             "Overall Conclusion"),
                         p(style = "font-size: 14px; color: rgba(255,255,255,0.9); line-height: 1.85; margin: 0;",
                           "Taken together, the data provide weak to moderate evidence for a positive
           association between agricultural pesticide exposure and Parkinson's disease
           mortality at the state level, most consistently for 2,4-D and, to a lesser
           extent, glyphosate. Farm density also showed a modest but significant positive
           association with Parkinson's death rates. County-level analyses suggest that
           2,4-D and paraquat may be negatively associated with life expectancy, though
           these findings are complicated by geographic confounding from other compounds.
           Critically, all effect sizes across every analysis were small, and none of
           these findings establish causation. ", tags$strong("Our overall 
           interpretation is that agricultural intensity — including pesticide use —
           is a weak positive predictor of Parkinson's mortality at the state level,
           consistent with the broader epidemiological literature, but that the strength
           and direction of this relationship varies by compound and is likely moderated
           by unmeasured geographic, demographic, and socioeconomic factors."), "We believe access to county-level data for all analyses would provide a more robust characterization of the relationship between pesticide use and Parkinson's disease prevalence, and would be a good direction for future projects to pursue.")
                     )
                 ),
             )
    ),
    
    
    # =========================================================================
    # DATA TABLES TAB
    # =========================================================================
    tabPanel("Data Tables",
             div(class = "home-section",
                 h1(class = "page-header", 
                    style = "padding-top: 2rem;",
                    "Data Tables"),
                 
                 div(class = "home-plain-card",
                     tags$h2("Parkinson's Disease Data"),
                     p("State-level Parkinson's mortality data."),
                     DTOutput("data_table_parkinson"),
                     br(),
                     downloadButton("download_parkinsons_data", "Download Dataset",
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Farm Data"),
                     p("State-level agricultural data."),
                     DTOutput("data_table_farms"),
                     br(),
                     downloadButton("download_farm_data", "Download Dataset",
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Life Expectancy Data"),
                     p("State-level life expectancy data."),
                     DTOutput("data_table_life_expectancy"),
                     br(),
                     downloadButton("download_life_expectancy_data", "Download Dataset",
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("State-Level Pesticide Data"),
                     p("State-level pesticide data."),
                     DTOutput("data_table_state_pesticide"),
                     br(),
                     downloadButton("download_state_pesticide_data", "Download Dataset",
                                    class = "btn btn-success btn-lg")
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("County-Level Pesticide Data"),
                     p("County-level pesticide data."),
                     DTOutput("data_table_county_pesticide"),
                     br(),
                     downloadButton("download_county_pesticide_data", "Download Dataset",
                                    class = "btn btn-success btn-lg")
                 ),
             )
    ),
    
    # =========================================================================
    # ABOUT TAB
    # =========================================================================
    tabPanel("About",
             
             div(class = "home-hero",
                 h1("About This Project"),
                 p(class = "hero-subtitle", "BIOL-185 Course Project - Winter 2026")
             ),
             
             div(class = "home-section",
                 
                 div(class = "about-card",
                     div(class = "about-card-text",
                         tags$h2("Purpose"),
                         tags$p("Parkinson's disease affects millions of Americans, yet the environmental
                     drivers behind its geographic distribution remain poorly understood. This
                     dashboard explores whether pesticide use and agricultural intensity are
                     associated with higher Parkinson's mortality rates and lower life expectancy
                     across U.S. states and counties."),
                         tags$p("Using statistical methods including Pearson correlation, linear regression,
                     ANOVA, and Tukey post-hoc tests, we surface potential environmental risk
                     patterns from publicly available federal datasets.")
                     ),
                     tags$img(
                       src = "green_neuron.jpg",
                       class = "about-illustration",
                       alt = "Medical research illustration"
                     )
                 ),
                 
                 # Two-column row for Data Sources + Limitations
                 div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1.5rem; margin-bottom: 2rem;",
                     
                     div(class = "home-plain-card", style = "margin-bottom: 0;",
                         tags$h2("Data Sources"),
                         tags$ul(
                           tags$li(tags$strong("CDC"), " — Parkinson's disease mortality rates by state"),
                           tags$li(tags$strong("USDA"), " — Pesticide usage and farm statistics by county and state"),
                           tags$li(tags$strong("CDC WONDER"), " — Life expectancy data by state and county"),
                           tags$li(tags$strong("U.S. Census"), " — County coordinate data (FIPS codes, lat/lon)")
                         )
                     ),
                     
                     div(class = "home-plain-card", style = "margin-bottom: 0;",
                         tags$h2("Important Limitations"),
                         tags$ul(
                           tags$li("Correlation does not imply causation"),
                           tags$li("Aggregated data may mask local variations"),
                           tags$li("Multiple confounding variables exist (age, income, healthcare access)"),
                           tags$li("Data represents averages across multi-year periods"),
                           tags$li("County-level life expectancy and state pesticide data have different temporal coverage")
                         )
                     )
                 ),
                 
                 # Maps card
                 div(class = "home-plain-card",
                     tags$h2("Maps"),
                     tags$p("Three interactive maps allow geographic exploration of the data:"),
                     tags$ul(
                       tags$li(tags$strong("Map 1:"), " State-level Parkinson's death rates vs. pesticide use"),
                       tags$li(tags$strong("Map 2:"), " State-level Parkinson's death rates vs. farm density"),
                       tags$li(tags$strong("Map 3:"), " County-level pesticide exposure vs. life expectancy")
                     )
                 ),
                 
                 # Methodology card
                 div(class = "home-plain-card",
                     tags$h2("Methodology"),
                     tags$p("Datasets were joined at the state and county level using FIPS codes and state
                 names. Pesticide exposure was averaged across available survey years. For ANOVA
                 analyses, counties and states were grouped into Low, Medium, and High exposure
                 tertiles based on pesticide application estimates. All statistical tests were
                 performed in R using base stats functions."),
                     tags$p("Scatter plots display raw relationships with overlaid linear regression lines.
                 Pearson correlation coefficients and full regression summaries are shown
                 alongside each plot for transparency.")
                 ),
                 
                 # Team card
                 div(class = "home-plain-card",
                     tags$h2("Team & Credits"),
                     tags$p(tags$strong("Course:"), " BIOL-185 — Medical Trends Analysis, Winter 2026"),
                     tags$p(tags$strong("Institution:"), " Washington & Lee University"),
                     tags$p(tags$strong("Contributors:"), " Ashley Ellis ('26), Robert Bernot ('26), Georgia Busbee ('26)"),
                     tags$p("Dashboard built in R using Shiny, Leaflet, Plotly, and DT.")
                 )
                 
             ) # End home-section
             
    ) # End About tabPanel


  ) # End navbarPage
  
) # End fluidPage

