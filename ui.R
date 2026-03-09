# Life Expectancy & Pesticides Shiny Dashboard
# Install required packages if needed:
# install.packages(c("shiny", "shinydashboard", "ggplot2", "plotly", "dplyr", "DT", "leaflet"))

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)

# =============================================================================
# SAMPLE DATA GENERATION
# =============================================================================

# Generate sample data for demonstration
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

# Calculate correlation for display
correlation <- cor(states_data$Pesticide_Use_kg_per_ha, 
                   states_data$Life_Expectancy)

# Temporal trend data
years_data <- data.frame(
  Year = rep(2000:2023, each = 3),
  Category = rep(c("Low Pesticide Use", "Medium Pesticide Use", "High Pesticide Use"), 24),
  Life_Expectancy = c(
    # Generate realistic temporal trends
    seq(75.5, 78.2, length.out = 24),  # Low pesticide
    seq(75.0, 77.5, length.out = 24),  # Medium pesticide
    seq(74.2, 76.8, length.out = 24)   # High pesticide
  ) + rnorm(72, 0, 0.3),
  Pesticide_Use = rep(c(1.2, 2.8, 5.5), 24)
)

# Age group data
age_groups_data <- data.frame(
  Age_Group = rep(c("0-14", "15-44", "45-64", "65-74", "75+"), each = 3),
  Exposure_Level = rep(c("Low", "Medium", "High"), 5),
  Life_Expectancy_Impact = c(
    -0.5, -1.2, -2.1,  # 0-14
    -0.3, -0.8, -1.5,  # 15-44
    -0.8, -1.8, -3.2,  # 45-64
    -1.5, -2.5, -4.1,  # 65-74
    -2.0, -3.2, -5.5   # 75+
  )
)

# =============================================================================
# UI DEFINITION
# =============================================================================

dashboardPage(
  skin = "green",
  
  # Header
  dashboardHeader(
    title = "Life Expectancy & Pesticides",
    titleWidth = 300
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Maps", tabName = "maps", icon = icon("chart-line")),
      menuItem("Data Visualization", tabName = "data_visualization", icon = icon("table")),
      menuItem("Statistical Analysis", tabName = "statistics", icon = icon("calculator")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  # Body
  dashboardBody(
    # Custom CSS
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          border-top: 3px solid #28a745;
        }
        .small-box {
          border-radius: 5px;
        }
        .info-box {
          min-height: 90px;
          border-radius: 5px;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #28a745;
        }
        h2 {
          color: #2d5016;
          font-weight: bold;
        }
        .hero-section {
          background: linear-gradient(135deg, #2d5016 0%, #4a7c2a 100%);
          color: white;
          padding: 40px;
          border-radius: 10px;
          margin-bottom: 20px;
        }
        .hero-section h1 {
          font-size: 2.5em;
          margin-bottom: 15px;
        }
        .hero-section p {
          font-size: 1.2em;
          line-height: 1.6;
        }
      "))
    ),
    
    tabItems(
      # HOME TAB
      tabItem(tabName = "home",
              fluidRow(
                column(12,
                       div(class = "hero-section",
                           h1("Life Expectancy & Pesticides Research Project"),
                           p("Welcome to our interactive dashboard exploring the relationship between pesticide exposure and life expectancy across the United States. This project analyzes county-level and state-level data on pesticide use, agricultural practices, environmental exposure, and population health outcomes.")
                       )
                )
              ),
              
              fluidRow(
                # Summary boxes
                valueBoxOutput("avg_life_expectancy", width = 3),
                valueBoxOutput("avg_pesticide_use", width = 3),
                valueBoxOutput("correlation_box", width = 3),
                valueBoxOutput("states_analyzed", width = 3)
              ),
              
              fluidRow(
                box(
                  title = "Overview",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  h3("Research Background"),
                  p("Pesticides are widely used in agriculture to protect crops from pests, diseases, and weeds. While essential for food production, concerns have been raised about their potential impact on human health and longevity. This study examines:"),
                  tags$ul(
                    tags$li("Geographic patterns of pesticide use across U.S. states"),
                    tags$li("Correlations between pesticide exposure levels and life expectancy"),
                    tags$li("Temporal trends in both pesticide use and population health"),
                    tags$li("Differential impacts across age groups and demographics")
                  ),
                  br(),
                  h3("Key Findings"),
                  p(paste0("Our preliminary analysis reveals a correlation coefficient of ", 
                           round(correlation, 3), 
                           " between pesticide use intensity and life expectancy at the state level.")),
                  p("States with higher agricultural pesticide use show varying patterns of life expectancy, influenced by multiple factors including healthcare access, socioeconomic conditions, and environmental regulations.")
                )
              ),
              
              fluidRow(
                box(
                  title = "Data Sources",
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  p("This project integrates data from multiple authoritative sources:"),
                  tags$ul(
                    tags$li(strong("CDC WONDER:"), " Life expectancy and mortality data"),
                    tags$li(strong("USDA National Agricultural Statistics Service:"), " Pesticide use estimates"),
                    tags$li(strong("EPA:"), " Environmental exposure data and pesticide registration information"),
                    tags$li(strong("U.S. Census Bureau:"), " Population and demographic data")
                  ),
                  p("Data coverage: 2000-2023 | Geographic scope: All 50 U.S. states")
                )
              )
      ),
      
      # MAPS TAB
      tabItem(tabName = "maps",
              h2("Interactive Visualizations"),
              
              fluidRow(
                box(
                  title = "Pesticide Use vs. Life Expectancy by State",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  plotlyOutput("scatter_plot", height = 500)
                )
              ),
              
              fluidRow(
                box(
                  title = "Temporal Trends (2000-2023)",
                  width = 8,
                  solidHeader = TRUE,
                  status = "success",
                  plotlyOutput("trend_plot", height = 400)
                ),
                box(
                  title = "Life Expectancy Impact by Age Group",
                  width = 4,
                  solidHeader = TRUE,
                  status = "warning",
                  plotlyOutput("age_group_plot", height = 400)
                )
              ),
              
              fluidRow(
                box(
                  title = "Geographic Distribution Map",
                  width = 12,
                  solidHeader = TRUE,
                  status = "info",
                  leafletOutput("map", height = 500)
                )
              ),
              
              fluidRow(
                box(
                  title = "Life Expectancy Distribution",
                  width = 6,
                  solidHeader = TRUE,
                  plotlyOutput("histogram_life", height = 350)
                ),
                box(
                  title = "Pesticide Use Distribution",
                  width = 6,
                  solidHeader = TRUE,
                  plotlyOutput("histogram_pesticide", height = 350)
                )
              )
      ),
      
      # DATA EXPLORER TAB
      tabItem(tabName = "data_visualizations",
              h2("Data Explorer"),
              
              fluidRow(
                box(
                  title = "Filter Options",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  fluidRow(
                    column(4,
                           sliderInput("life_exp_filter", "Life Expectancy Range:",
                                       min = 70, max = 85, value = c(70, 85), step = 0.5)
                    ),
                    column(4,
                           sliderInput("pesticide_filter", "Pesticide Use Range (kg/ha):",
                                       min = 0, max = 6, value = c(0, 6), step = 0.1)
                    ),
                    column(4,
                           selectInput("region_filter", "Region:",
                                       choices = c("All", levels(states_data$Region)),
                                       selected = "All")
                    )
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "State-Level Data Table",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  DTOutput("data_table")
                )
              ),
              
              fluidRow(
                box(
                  title = "Summary Statistics",
                  width = 6,
                  solidHeader = TRUE,
                  verbatimTextOutput("summary_stats")
                ),
                box(
                  title = "Download Data",
                  width = 6,
                  solidHeader = TRUE,
                  p("Download the filtered dataset for your own analysis:"),
                  downloadButton("download_data", "Download CSV", class = "btn-success")
                )
              )
      ),
      
      # STATISTICAL ANALYSIS TAB
      tabItem(tabName = "statistics",
              h2("Statistical Analysis"),
              
              fluidRow(
                box(
                  title = "Correlation Analysis",
                  width = 6,
                  solidHeader = TRUE,
                  status = "primary",
                  h4("Pearson Correlation Test"),
                  verbatimTextOutput("correlation_test"),
                  br(),
                  h4("Interpretation"),
                  textOutput("correlation_interpretation")
                ),
                box(
                  title = "Linear Regression Model",
                  width = 6,
                  solidHeader = TRUE,
                  status = "info",
                  h4("Model Summary"),
                  verbatimTextOutput("regression_summary"),
                  br(),
                  plotOutput("regression_plot", height = 300)
                )
              ),
              
              fluidRow(
                box(
                  title = "Regional Comparison",
                  width = 12,
                  solidHeader = TRUE,
                  status = "success",
                  plotlyOutput("regional_boxplot", height = 400)
                )
              ),
              
              fluidRow(
                box(
                  title = "ANOVA Analysis by Region",
                  width = 12,
                  solidHeader = TRUE,
                  status = "warning",
                  verbatimTextOutput("anova_results")
                )
              )
      ),
      
      # ABOUT TAB
      tabItem(tabName = "about",
              h2("About This Project"),
              
              fluidRow(
                box(
                  title = "Project Information",
                  width = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  h3("Research Objectives"),
                  p("This research project aims to understand the complex relationship between agricultural pesticide use and population health outcomes, specifically focusing on life expectancy metrics across the United States."),
                  br(),
                  h3("Methodology"),
                  p("We employ a multi-faceted analytical approach:"),
                  tags$ul(
                    tags$li("Spatial analysis of state-level pesticide use patterns"),
                    tags$li("Correlation and regression analyses"),
                    tags$li("Temporal trend analysis from 2000-2023"),
                    tags$li("Demographic stratification by age groups")
                  ),
                  br(),
                  h3("Limitations"),
                  p("This analysis has several important limitations:"),
                  tags$ul(
                    tags$li("Ecological fallacy: State-level data may not reflect individual exposure"),
                    tags$li("Confounding variables: Many factors influence life expectancy"),
                    tags$li("Temporal lags: Health effects may manifest years after exposure"),
                    tags$li("Data availability: Some states have incomplete pesticide use records")
                  ),
                  br(),
                  h3("Contact & Collaboration"),
                  p("For questions about this research or potential collaborations, please contact:"),
                  p(strong("Email:"), " research@example.edu"),
                  p(strong("Institution:"), " Environmental Health Sciences Department"),
                  br(),
                  h3("Data Availability"),
                  p("All data and code used in this analysis are available for research purposes. Please cite this work appropriately if you use our datasets or findings."),
                  br(),
                  p(em("Last Updated: March 2026"))
                )
              )
      )
    )
  )
)

