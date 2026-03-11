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