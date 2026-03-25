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
      max-width: 600px;      /* Matches the width of your farm photo */
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
                 
                 # =============================================================================
                 # DATA VISUALIZATION SUMMARY BANNER
                 # =============================================================================
                 
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

  <!-- Banner header -->
  <div style="text-align: center; margin-bottom: 32px;">
    <div style="
      font-size: 11px;
      font-family: monospace;
      letter-spacing: 4px;
      text-transform: uppercase;
      color: #a7c957;
      margin-bottom: 8px;
    ">Analysis Overview</div>
    <h2 style="
      margin: 0 0 10px 0;
      font-size: 26px;
      font-weight: normal;
      letter-spacing: 1px;
      color: white;
    ">Understanding the Visualizations</h2>
    <p style="
      margin: 0 auto;
      max-width: 680px;
      font-size: 14px;
      color: rgba(255,255,255,0.75);
      line-height: 1.7;
      font-family: Arial, sans-serif;
    ">
      This dashboard investigates relationships between agricultural pesticide exposure 
      and neurological health outcomes using county- and state-level data across the 
      United States. Use the dropdown menus within each section to explore statistical results and data visualizations by 
      pesticide compound.
    </p>
  </div>

  <!-- Divider -->
  <div style="border-top: 1px solid rgba(167,201,87,0.3); margin-bottom: 32px;"></div>

  <!-- Four columns -->
  <div style="display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 24px;">

    <!-- Column 1: Scatter Plot -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #a7c957;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="8" cy="32" r="3" fill="#a7c957"/>
          <circle cx="14" cy="24" r="3" fill="#a7c957"/>
          <circle cx="20" cy="26" r="3" fill="#a7c957"/>
          <circle cx="26" cy="16" r="3" fill="#a7c957"/>
          <circle cx="32" cy="12" r="3" fill="#a7c957"/>
          <circle cx="19" cy="20" r="3" fill="#a7c957" opacity="0.5"/>
          <line x1="5" y1="35" x2="35" y2="35" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
          <line x1="5" y1="35" x2="5" y2="5" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #a7c957;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">Scatter Plot</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        The scatter plots below visualize the raw relationship between pesticide exposure and health outcomes, 
        with each point representing a county or state.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #a7c957; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Scroll to view scatter plots comparing the following data: Pesticide Use vs. Parkinsons Death Rate, Number of Farms vs. Parkinsons Death rate, Pesticide Use vs. Life Expectancy
        </p>
        <div style="font-size: 11px; color: #a7c957; font-family: monospace; margin-bottom: 6px;">HOW TO INTERACT</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          Use the <strong style="color:white;">Select Pesticide</strong> dropdown to switch between compounds. Hover over points to see county or state details.
        </p>
      </div>
    </div>

    <!-- Column 2: Regression -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #f4a261;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="8" cy="30" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="14" cy="26" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="20" cy="22" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="26" cy="17" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="32" cy="13" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <line x1="5" y1="32" x2="35" y2="10" stroke="#f4a261" stroke-width="2" stroke-dasharray="4 2"/>
          <line x1="5" y1="35" x2="35" y2="35" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
          <line x1="5" y1="35" x2="5" y2="5" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #f4a261;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">Regression</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        Linear regression models the direction and strength of the relationship between two variables, 
        shown as a dashed trend line overlaid on the scatter plot.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #f4a261; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Same pairings as scatter plots. Full regression summary (coefficients, standard error, p-value) are shown in the side panel.
        </p>
        <div style="font-size: 11px; color: #f4a261; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          The <strong style="color:white;">R²</strong> value reveals how well the trend line fits the data, with R² = 1 indicating a perfect fit. A value <strong style="color:white;">p &lt; 0.05</strong> on the slope coefficient indicates a statistically significant trend.
        </p>
      </div>
    </div>

    <!-- Column 3: Correlation -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #7ec8e3;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="20" cy="20" r="13" stroke="#7ec8e3" stroke-width="1.5" fill="none"/>
          <circle cx="20" cy="20" r="7" stroke="#7ec8e3" stroke-width="1.5" fill="none" opacity="0.5"/>
          <line x1="20" y1="7" x2="20" y2="33" stroke="#7ec8e3" stroke-width="1" opacity="0.4"/>
          <line x1="7" y1="20" x2="33" y2="20" stroke="#7ec8e3" stroke-width="1" opacity="0.4"/>
          <text x="14" y="24" font-size="10" fill="#7ec8e3" font-family="monospace">r</text>
        </svg>
      </div>
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #7ec8e3;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">Correlation</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        Pearson\'s correlation coefficient (r) quantifies how strongly two variables 
        move together, from &minus;1 (perfect negative) to +1 (perfect positive).
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #7ec8e3; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Same as scatter plot.
        </p>
        <div style="font-size: 11px; color: #7ec8e3; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          The <strong style="color:white;">r value</strong> is shown on each scatter plot. The side panel gives the full test output, including 95% confidence interval and p-value.
        </p>
      </div>
    </div>

    <!-- Column 4: ANOVA / Tukey -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #e76f51;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <!-- Three box plots schematic -->
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
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #e76f51;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">ANOVA & Tukey</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        We ran ANOVA tests to detect whether health outcomes differ significantly across pesticide 
        exposure levels, classified asLow &rarr; High. Tukey\'s post-hoc identifies which 
        specific pairs of groups drive that difference.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #e76f51; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          County exposure quartiles vs. life expectancy &bull; State exposure quartiles vs. Parkinson\'s death rate
        </p>
        <div style="font-size: 11px; color: #e76f51; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          ANOVA p &lt; 0.05 means at least one group differs. Check the <strong style="color:white;">Tukey table</strong> — pairs highlighted in green have adjusted p &lt; 0.05 and are significantly different from each other.
        </p>
      </div>
    </div>

  </div><!-- end grid -->
</div><!-- end banner -->
'),
                 
                 
                 
                 
                 
                 
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
             
             div(class = "home-hero",
                 h1("About This Project"),
                 p(class = "hero-subtitle", "BIOL-185 Course Project - Winter 2026")
             ),
             
             div(class = "home-section",
                 
                 div(class = "home-plain-card",
                     tags$h2("Purpose"),
                     tags$p("This dashboard explores relationships between environmental factors
                  and health outcomes across the United States.")
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Data Sources"),
                     tags$ul(
                       tags$li("CDC - Parkinson's disease mortality"),
                       tags$li("USDA - Pesticide usage and farm statistics"),
                       tags$li("CDC WONDER - Life expectancy data"),
                       tags$li("Census - County coordinate data (cfips_location.csv)")
                     )
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Maps"),
                     tags$p("Three interactive maps visualize:"),
                     tags$ul(
                       tags$li(tags$strong("Map 1:"), " State-level Parkinson's death rates vs. pesticide use"),
                       tags$li(tags$strong("Map 2:"), " State-level Parkinson's death rates vs. farm density"),
                       tags$li(tags$strong("Map 3:"), " County-level pesticide exposure vs. life expectancy")
                     )
                 ),
                 
                 div(class = "home-plain-card",
                     tags$h2("Important Limitations"),
                     tags$ul(
                       tags$li("Correlation does not imply causation"),
                       tags$li("Aggregated data may mask local variations"),
                       tags$li("Multiple confounding variables exist")
                     )
                 )
                 
             ) # End home-section
    ) # End About tabPanel
  )
)





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
  
  # Full state name -> abbreviation lookup (matches maps package lowercase naming)
  state_name_to_abbr <- data.frame(
    full_name = c(
      "alabama","alaska","arizona","arkansas","california","colorado","connecticut",
      "delaware","florida","georgia","hawaii","idaho","illinois","indiana","iowa",
      "kansas","kentucky","louisiana","maine","maryland","massachusetts","michigan",
      "minnesota","mississippi","missouri","montana","nebraska","nevada",
      "new hampshire","new jersey","new mexico","new york","north carolina",
      "north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island",
      "south carolina","south dakota","tennessee","texas","utah","vermont",
      "virginia","washington","west virginia","wisconsin","wyoming"
    ),
    abbr = c(
      "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
      "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
      "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
      "VA","WA","WV","WI","WY"
    ),
    stringsAsFactors = FALSE
  )
  
  # ===========================================================================
  # BUILD STATE & COUNTY SF OBJECTS using sf::st_as_sf(maps::map(...))
  # This replaces the retired maptools::map2SpatialPolygons approach
  # ===========================================================================
  
  state_sf <- reactive({
    states_map <- maps::map("state", fill = TRUE, plot = FALSE)
    sf_obj     <- sf::st_as_sf(states_map, coords = c("x", "y"), crs = 4326)
    sf_obj$full_name <- tolower(sf_obj$ID)
    sf_obj
  })
  
  county_sf <- reactive({
    counties_map <- maps::map("county", fill = TRUE, plot = FALSE)
    sf_obj       <- sf::# INTERMEDIATE COPY OF UI FOR SAVING 
      
      
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
      max-width: 600px;      /* Matches the width of your farm photo */
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
                     
                     # =============================================================================
                     # DATA VISUALIZATION SUMMARY BANNER
                     # =============================================================================
                     
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

  <!-- Banner header -->
  <div style="text-align: center; margin-bottom: 32px;">
    <div style="
      font-size: 11px;
      font-family: monospace;
      letter-spacing: 4px;
      text-transform: uppercase;
      color: #a7c957;
      margin-bottom: 8px;
    ">Analysis Overview</div>
    <h2 style="
      margin: 0 0 10px 0;
      font-size: 26px;
      font-weight: normal;
      letter-spacing: 1px;
      color: white;
    ">Understanding the Visualizations</h2>
    <p style="
      margin: 0 auto;
      max-width: 680px;
      font-size: 14px;
      color: rgba(255,255,255,0.75);
      line-height: 1.7;
      font-family: Arial, sans-serif;
    ">
      This dashboard investigates relationships between agricultural pesticide exposure 
      and neurological health outcomes using county- and state-level data across the 
      United States. Use the dropdown menus within each section to explore statistical results and data visualizations by 
      pesticide compound.
    </p>
  </div>

  <!-- Divider -->
  <div style="border-top: 1px solid rgba(167,201,87,0.3); margin-bottom: 32px;"></div>

  <!-- Four columns -->
  <div style="display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 24px;">

    <!-- Column 1: Scatter Plot -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #a7c957;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="8" cy="32" r="3" fill="#a7c957"/>
          <circle cx="14" cy="24" r="3" fill="#a7c957"/>
          <circle cx="20" cy="26" r="3" fill="#a7c957"/>
          <circle cx="26" cy="16" r="3" fill="#a7c957"/>
          <circle cx="32" cy="12" r="3" fill="#a7c957"/>
          <circle cx="19" cy="20" r="3" fill="#a7c957" opacity="0.5"/>
          <line x1="5" y1="35" x2="35" y2="35" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
          <line x1="5" y1="35" x2="5" y2="5" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #a7c957;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">Scatter Plot</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        The scatter plots below visualize the raw relationship between pesticide exposure and health outcomes, 
        with each point representing a county or state.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #a7c957; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Scroll to view scatter plots comparing the following data: Pesticide Use vs. Parkinsons Death Rate, Number of Farms vs. Parkinsons Death rate, Pesticide Use vs. Life Expectancy
        </p>
        <div style="font-size: 11px; color: #a7c957; font-family: monospace; margin-bottom: 6px;">HOW TO INTERACT</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          Use the <strong style="color:white;">Select Pesticide</strong> dropdown to switch between compounds. Hover over points to see county or state details.
        </p>
      </div>
    </div>

    <!-- Column 2: Regression -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #f4a261;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="8" cy="30" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="14" cy="26" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="20" cy="22" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="26" cy="17" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <circle cx="32" cy="13" r="2.5" fill="rgba(255,255,255,0.5)"/>
          <line x1="5" y1="32" x2="35" y2="10" stroke="#f4a261" stroke-width="2" stroke-dasharray="4 2"/>
          <line x1="5" y1="35" x2="35" y2="35" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
          <line x1="5" y1="35" x2="5" y2="5" stroke="rgba(255,255,255,0.3)" stroke-width="1.5"/>
        </svg>
      </div>
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #f4a261;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">Regression</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        Linear regression models the direction and strength of the relationship between two variables, 
        shown as a dashed trend line overlaid on the scatter plot.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #f4a261; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Same pairings as scatter plots. Full regression summary (coefficients, standard error, p-value) are shown in the side panel.
        </p>
        <div style="font-size: 11px; color: #f4a261; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          The <strong style="color:white;">R²</strong> value reveals how well the trend line fits the data, with R² = 1 indicating a perfect fit. A value <strong style="color:white;">p &lt; 0.05</strong> on the slope coefficient indicates a statistically significant trend.
        </p>
      </div>
    </div>

    <!-- Column 3: Correlation -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #7ec8e3;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <circle cx="20" cy="20" r="13" stroke="#7ec8e3" stroke-width="1.5" fill="none"/>
          <circle cx="20" cy="20" r="7" stroke="#7ec8e3" stroke-width="1.5" fill="none" opacity="0.5"/>
          <line x1="20" y1="7" x2="20" y2="33" stroke="#7ec8e3" stroke-width="1" opacity="0.4"/>
          <line x1="7" y1="20" x2="33" y2="20" stroke="#7ec8e3" stroke-width="1" opacity="0.4"/>
          <text x="14" y="24" font-size="10" fill="#7ec8e3" font-family="monospace">r</text>
        </svg>
      </div>
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #7ec8e3;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">Correlation</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        Pearson\'s correlation coefficient (r) quantifies how strongly two variables 
        move together, from &minus;1 (perfect negative) to +1 (perfect positive).
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #7ec8e3; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          Same as scatter plot.
        </p>
        <div style="font-size: 11px; color: #7ec8e3; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          The <strong style="color:white;">r value</strong> is shown on each scatter plot. The side panel gives the full test output, including 95% confidence interval and p-value.
        </p>
      </div>
    </div>

    <!-- Column 4: ANOVA / Tukey -->
    <div style="
      background: rgba(255,255,255,0.07);
      border-radius: 10px;
      padding: 24px 20px;
      border-top: 3px solid #e76f51;
    ">
      <div style="text-align: center; margin-bottom: 16px;">
        <svg width="40" height="40" viewBox="0 0 40 40" fill="none" xmlns="http://www.w3.org/2000/svg">
          <!-- Three box plots schematic -->
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
      <div style="
        font-size: 10px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #e76f51;
        font-family: monospace;
        text-align: center;
        margin-bottom: 10px;
      ">ANOVA & Tukey</div>
      <p style="font-size: 13px; color: rgba(255,255,255,0.85); line-height: 1.65; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
        We ran ANOVA tests to detect whether health outcomes differ significantly across pesticide 
        exposure levels, classified asLow &rarr; High. Tukey\'s post-hoc identifies which 
        specific pairs of groups drive that difference.
      </p>
      <div style="border-top: 1px solid rgba(255,255,255,0.1); padding-top: 12px; margin-top: 4px;">
        <div style="font-size: 11px; color: #e76f51; font-family: monospace; margin-bottom: 6px;">DATA USED</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0 0 12px 0;">
          County exposure quartiles vs. life expectancy &bull; State exposure quartiles vs. Parkinson\'s death rate
        </p>
        <div style="font-size: 11px; color: #e76f51; font-family: monospace; margin-bottom: 6px;">HOW TO INTERPRET</div>
        <p style="font-size: 12px; color: rgba(255,255,255,0.7); line-height: 1.6; font-family: Arial, sans-serif; margin: 0;">
          ANOVA p &lt; 0.05 means at least one group differs. Check the <strong style="color:white;">Tukey table</strong> — pairs highlighted in green have adjusted p &lt; 0.05 and are significantly different from each other.
        </p>
      </div>
    </div>

  </div><!-- end grid -->
</div><!-- end banner -->
'),
                     
                     
                     
                     
                     
                     
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
                 
                 div(class = "home-hero",
                     h1("About This Project"),
                     p(class = "hero-subtitle", "BIOL-185 Course Project - Winter 2026")
                 ),
                 
                 div(class = "home-section",
                     
                     div(class = "home-plain-card",
                         tags$h2("Purpose"),
                         tags$p("This dashboard explores relationships between environmental factors
                  and health outcomes across the United States.")
                     ),
                     
                     div(class = "home-plain-card",
                         tags$h2("Data Sources"),
                         tags$ul(
                           tags$li("CDC - Parkinson's disease mortality"),
                           tags$li("USDA - Pesticide usage and farm statistics"),
                           tags$li("CDC WONDER - Life expectancy data"),
                           tags$li("Census - County coordinate data (cfips_location.csv)")
                         )
                     ),
                     
                     div(class = "home-plain-card",
                         tags$h2("Maps"),
                         tags$p("Three interactive maps visualize:"),
                         tags$ul(
                           tags$li(tags$strong("Map 1:"), " State-level Parkinson's death rates vs. pesticide use"),
                           tags$li(tags$strong("Map 2:"), " State-level Parkinson's death rates vs. farm density"),
                           tags$li(tags$strong("Map 3:"), " County-level pesticide exposure vs. life expectancy")
                         )
                     ),
                     
                     div(class = "home-plain-card",
                         tags$h2("Important Limitations"),
                         tags$ul(
                           tags$li("Correlation does not imply causation"),
                           tags$li("Aggregated data may mask local variations"),
                           tags$li("Multiple confounding variables exist")
                         )
                     )
                     
                 ) # End home-section
        ) # End About tabPanel
      )
    )
    
    
    
    st_as_sf(counties_map, coords = c("x", "y"), crs = 4326)
    # ID format is "state,county"
    sf_obj$state_lower  <- tolower(sapply(strsplit(sf_obj$ID, ","), `[`, 1))
    sf_obj$county_lower <- tolower(sapply(strsplit(sf_obj$ID, ","), `[`, 2))
    sf_obj
  })
  
  # ===========================================================================
  # PROCESS AND MERGE DATA
  # ===========================================================================
  
  Pesticide_State_Data_Agg <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    
    state_col <- if ("state_name" %in% names(data)) "state_name"
    else if ("state_code" %in% names(data)) "state_code"
    else return(NULL)
    
    if (!all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) return(NULL)
    
    state_pest <- data %>%
      group_by(!!sym(state_col)) %>%
      summarise(Avg_Pesticide = mean((LOW_ESTIMATE + HIGH_ESTIMATE) / 2, na.rm = TRUE),
                .groups = "drop")
    names(state_pest)[1] <- "State"
    state_pest
  })
  
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_Data(), Pesticide_State_Data_Agg())
    left_join(Parkinson_Data(), Pesticide_State_Data_Agg(), by = "State")
  })
  
  Parkinson_Farm_State <- reactive({
    req(Parkinson_Data(), Farm_Data())
    left_join(Parkinson_Data(), Farm_Data(), by = "State")
  })
  
  County_Pesticide_Life <- reactive({
    req(Pesticide_County_Data(), Expectancy_Data(), County_Coords())
    
    pesticide <- Pesticide_County_Data()
    life_exp  <- Expectancy_Data()
    coords    <- County_Coords()
    
    coords <- coords %>%
      mutate(
        state_fips        = substr(sprintf("%05s", cfips), 1, 2),
        county_fips       = sprintf("%05s", cfips),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", name)))
      ) %>%
      left_join(state_fips_mapping, by = "state_fips")
    
    pesticide <- pesticide %>%
      mutate(
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", county_name))),
        Avg_Pesticide     = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
      )
    
    life_exp <- life_exp %>%
      mutate(county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", County))))
    
    merged <- pesticide %>%
      left_join(life_exp,
                by = c("state_name" = "State", "county_name_clean" = "county_name_clean")) %>%
      left_join(coords %>% select(State, county_name_clean, county_fips, lng, lat),
                by = c("state_name" = "State", "county_name_clean" = "county_name_clean")) %>%
      filter(!is.na(lat), !is.na(lng))
    
    merged
  })
  
  # ===========================================================================
  # HOME TAB OUTPUTS
  # ===========================================================================
  
  output$stats_boxes <- renderUI({
    req(Parkinson_Data())
    parkinson <- Parkinson_Data()
    
    avg_death_rate <- if ("Avg_Death_Rate" %in% names(parkinson))
      round(mean(parkinson$Avg_Death_Rate, na.rm = TRUE), 2) else "N/A"
    
    total_farms <- if (!is.null(Farm_Data()) && "Number_Of_Farms" %in% names(Farm_Data()))
      format(sum(Farm_Data()$Number_Of_Farms, na.rm = TRUE), big.mark = ",") else "N/A"
    
    avg_life_exp <- if (!is.null(Expectancy_State_Data()) && "Avg_Life_Expectancy" %in% names(Expectancy_State_Data()))
      round(mean(Expectancy_State_Data()$Avg_Life_Expectancy, na.rm = TRUE), 1) else "N/A"
    
    states_count <- nrow(parkinson)
    
    div(class = "stats-row",
        div(class = "stat-box",
            div(class = "stat-number", avg_death_rate),
            div(class = "stat-label", "Avg Parkinson's Death Rate")),
        div(class = "stat-box",
            div(class = "stat-number", total_farms),
            div(class = "stat-label", "Total U.S. Farms")),
        div(class = "stat-box",
            div(class = "stat-number", avg_life_exp),
            div(class = "stat-label", "Avg Life Expectancy (years)")),
        div(class = "stat-box",
            div(class = "stat-number", states_count),
            div(class = "stat-label", "States Analyzed"))
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
           "map1" = div(p(style = "font-size: 0.9em; line-height: 1.6;",
                          "This choropleth map shows Parkinson's disease death rates by state.",
                          br(), br(),
                          strong("Fill color:"), " Death rate (yellow to red)")),
           "map2" = div(p(style = "font-size: 0.9em; line-height: 1.6;",
                          "This choropleth map shows Parkinson's death rates overlaid with farm counts.",
                          br(), br(),
                          strong("Fill color:"), " Death rate (red to purple)")),
           "map3" = div(p(style = "font-size: 0.9em; line-height: 1.6;",
                          "This county-level choropleth shows life expectancy by county.",
                          br(), br(),
                          strong("Fill color:"), " Life expectancy (yellow to green)")),
           p("Select a map to view details.")
    )
  })
  
  # ===========================================================================
  # MAIN MAP OUTPUT (choropleth using sf)
  # ===========================================================================
  
  output$main_map <- renderLeaflet({
    selected <- input$selected_map
    
    # ---- MAP 1: Parkinson's Death Rate choropleth (state) -------------------
    if (selected == "map1") {
      req(Parkinson_Pesticide_State(), state_sf())
      
      data <- Parkinson_Pesticide_State()
      sp   <- state_sf() %>%
        left_join(state_name_to_abbr, by = "full_name") %>%
        left_join(data, by = c("abbr" = "State"))
      
      pal <- colorNumeric("YlOrRd", domain = sp$Avg_Death_Rate, na.color = "#d0d0d0")
      
      labels <- sprintf(
        "<strong>%s</strong><br/>Death Rate: %s<br/>Pesticide Use: %s lbs",
        tools::toTitleCase(sp$full_name),
        ifelse(is.na(sp$Avg_Death_Rate), "N/A", round(sp$Avg_Death_Rate, 2)),
        ifelse(is.na(sp$Avg_Pesticide),  "N/A", round(sp$Avg_Pesticide,  2))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(sp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor        = ~pal(Avg_Death_Rate),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 1.5,
          opacity          = 1,
          highlightOptions = highlightOptions(
            weight       = 3,
            color        = "#2d5016",
            fillOpacity  = 0.9,
            bringToFront = TRUE
          ),
          label        = labels,
          labelOptions = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ~Avg_Death_Rate,
          title    = "Death Rate",
          opacity  = 0.8,
          na.label = "No data"
        )
      
      # ---- MAP 2: Parkinson's Death Rate choropleth vs Farms (state) ----------
    } else if (selected == "map2") {
      req(Parkinson_Farm_State(), state_sf())
      
      data <- Parkinson_Farm_State()
      sp   <- state_sf() %>%
        left_join(state_name_to_abbr, by = "full_name") %>%
        left_join(data, by = c("abbr" = "State"))
      
      pal <- colorNumeric("RdPu", domain = sp$Avg_Death_Rate, na.color = "#d0d0d0")
      
      labels <- sprintf(
        "<strong>%s</strong><br/>Death Rate: %s<br/>Farms: %s",
        tools::toTitleCase(sp$full_name),
        ifelse(is.na(sp$Avg_Death_Rate),  "N/A", round(sp$Avg_Death_Rate, 2)),
        ifelse(is.na(sp$Number_Of_Farms), "N/A", format(sp$Number_Of_Farms, big.mark = ","))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(sp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor        = ~pal(Avg_Death_Rate),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 1.5,
          opacity          = 1,
          highlightOptions = highlightOptions(
            weight       = 3,
            color        = "#2d5016",
            fillOpacity  = 0.9,
            bringToFront = TRUE
          ),
          label        = labels,
          labelOptions = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ~Avg_Death_Rate,
          title    = "Death Rate",
          opacity  = 0.8,
          na.label = "No data"
        )
      
      # ---- MAP 3: Life Expectancy choropleth (county) -------------------------
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
      
      # Summarise to one row per state+county
      data_summary <- data %>%
        filter(!is.na(Avg_Life_Expectancy)) %>%
        mutate(
          state_lower  = tolower(state_name),
          county_lower = trimws(tolower(gsub(" County$| Parish$", "", county_name)))
        ) %>%
        group_by(state_lower, county_lower) %>%
        summarise(
          Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE),
          Avg_Pesticide       = mean(Avg_Pesticide,       na.rm = TRUE),
          .groups = "drop"
        )
      
      cp <- county_sf() %>%
        left_join(data_summary,
                  by = c("state_lower" = "state_lower",
                         "county_lower" = "county_lower"))
      
      pal <- colorNumeric("YlGn", domain = cp$Avg_Life_Expectancy,
                          na.color = "#d0d0d0", reverse = FALSE)
      
      labels <- sprintf(
        "<strong>%s, %s</strong><br/>Life Expectancy: %s yrs<br/>Pesticide: %s lbs",
        tools::toTitleCase(cp$county_lower),
        tools::toTitleCase(cp$state_lower),
        ifelse(is.na(cp$Avg_Life_Expectancy), "N/A", round(cp$Avg_Life_Expectancy, 1)),
        ifelse(is.na(cp$Avg_Pesticide),       "N/A", round(cp$Avg_Pesticide, 2))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(cp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor        = ~pal(Avg_Life_Expectancy),
          fillOpacity      = 0.75,
          color            = "white",
          weight           = 0.4,
          opacity          = 0.8,
          highlightOptions = highlightOptions(
            weight       = 2,
            color        = "#2d5016",
            fillOpacity  = 0.9,
            bringToFront = TRUE
          ),
          label        = labels,
          labelOptions = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "12px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal      = pal,
          values   = ~Avg_Life_Expectancy,
          title    = "Life Expectancy (yrs)",
          opacity  = 0.8,
          na.label = "No data"
        )
      
    } else {
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
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class    = 'cell-border stripe hover'
    ) %>%
      formatRound(
        columns = intersect(c("Avg_Death_Rate", "Avg_Deaths"), names(Parkinson_Data())),
        digits  = 2
      )
  })
  
  output$data_table_farms <- renderDT({
    req(Farm_Data())
    datatable(
      Farm_Data(),
      options  = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class    = 'cell-border stripe hover'
    ) %>%
      formatRound(
        columns = intersect(c("Area_operated_Acres", "Acres_Operated_Millions"), names(Farm_Data())),
        digits  = 2
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
    filename = function() paste0("medical_trends_data_", Sys.Date(), ".csv"),
    content  = function(file) {
      req(Parkinson_Pesticide_State())
      write.csv(Parkinson_Pesticide_State(), file, row.names = FALSE)
    }
  )
}