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
  # LOAD EXTERNAL DATA
  # ===========================================================================
  
  # Load Parkinson's disease data
  Parkinson_Data <- read.csv("Parkinson_mortality_rates_clean.csv")

  # Load Pesticide use data  
  Pesticide_County_Data <- read.csv("pesticides_by_county.csv")
  
  # Load Pesticide use data  
  Expectancy_Data <- read.csv("LifeExpectancyStateData_clean.csv")
  
  # Load Pesticide use data  
  Farm_Data <- read.csv("Farm_Data_2024.csv")
  
  # ===========================================================================
  # COMBINED DATA FOR ANALYSIS
  # ===========================================================================
  
  # Merge the two datasets by State
  combined_data <- reactive({
    req(Parkinson_Data(), Pesticide_Data())
    
    # Merge datasets - keep all columns, adding suffixes for duplicates
    merged <- Parkinson_Data() %>%
      full_join(Pesticide_Data(), 
                by = c("State"), 
                suffix = c("_park", "_pest"))
    
    # Consolidate Region, Latitude, Longitude columns
    if("Region_park" %in% names(merged)) {
      merged$Region <- coalesce(merged$Region_park, merged$Region_pest)
    }
    if("Latitude_park" %in% names(merged)) {
      merged$Latitude <- coalesce(merged$Latitude_park, merged$Latitude_pest)
    }
    if("Longitude_park" %in% names(merged)) {
      merged$Longitude <- coalesce(merged$Longitude_park, merged$Longitude_pest)
    }
    
    return(merged)
  })
  
  # Calculate correlation
  correlation <- reactive({
    req(combined_data())
    data <- combined_data()
    
    # Check if required columns exist
    if("Parkinson_Rate" %in% names(data) && "Pesticide_Use" %in% names(data)) {
      cor(data$Pesticide_Use, data$Parkinson_Rate, use = "complete.obs")
    } else {
      NA
    }
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