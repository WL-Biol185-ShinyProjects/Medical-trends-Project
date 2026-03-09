library(shiny)

function(input, output, session) {
  
  # Reactive data filtering
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
  
  # Value Boxes
  output$avg_life_expectancy <- renderValueBox({
    valueBox(
      paste0(round(mean(states_data$Life_Expectancy), 1), " years"),
      "Average Life Expectancy",
      icon = icon("heartbeat"),
      color = "green"
    )
  })
  
  output$avg_pesticide_use <- renderValueBox({
    valueBox(
      paste0(round(mean(states_data$Pesticide_Use_kg_per_ha), 2), " kg/ha"),
      "Average Pesticide Use",
      icon = icon("spray-can"),
      color = "yellow"
    )
  })
  
  output$correlation_box <- renderValueBox({
    valueBox(
      round(correlation, 3),
      "Correlation Coefficient",
      icon = icon("chart-line"),
      color = if(abs(correlation) > 0.5) "red" else "blue"
    )
  })
  
  output$states_analyzed <- renderValueBox({
    valueBox(
      nrow(states_data),
      "States Analyzed",
      icon = icon("map"),
      color = "purple"
    )
  })
  
  # Scatter Plot
  output$scatter_plot <- renderPlotly({
    p <- ggplot(states_data, aes(x = Pesticide_Use_kg_per_ha, 
                                 y = Life_Expectancy,
                                 color = Region,
                                 text = paste("State:", State,
                                              "<br>Life Expectancy:", round(Life_Expectancy, 1),
                                              "<br>Pesticide Use:", round(Pesticide_Use_kg_per_ha, 2)))) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "darkgreen", linetype = "dashed") +
      labs(title = "Relationship Between Pesticide Use and Life Expectancy",
           x = "Pesticide Use (kg/ha)",
           y = "Life Expectancy (years)") +
      theme_minimal() +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Trend Plot
  output$trend_plot <- renderPlotly({
    p <- ggplot(years_data, aes(x = Year, y = Life_Expectancy, 
                                color = Category, group = Category)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(title = "Life Expectancy Trends by Pesticide Exposure Level",
           x = "Year",
           y = "Life Expectancy (years)",
           color = "Exposure Level") +
      theme_minimal() +
      scale_color_manual(values = c("Low Pesticide Use" = "#2ecc71",
                                    "Medium Pesticide Use" = "#f39c12",
                                    "High Pesticide Use" = "#e74c3c"))
    
    ggplotly(p)
  })
  
  # Age Group Plot
  output$age_group_plot <- renderPlotly({
    p <- ggplot(age_groups_data, aes(x = Age_Group, y = Life_Expectancy_Impact,
                                     fill = Exposure_Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Life Expectancy Impact",
           x = "Age Group",
           y = "Impact (years)",
           fill = "Exposure") +
      theme_minimal() +
      scale_fill_manual(values = c("Low" = "#2ecc71",
                                   "Medium" = "#f39c12",
                                   "High" = "#e74c3c")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Map
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "RdYlGn", domain = states_data$Life_Expectancy,
                        reverse = TRUE)
    
    leaflet(states_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = ~sqrt(Pesticide_Use_kg_per_ha) * 3,
        color = ~pal(Life_Expectancy),
        fillOpacity = 0.7,
        popup = ~paste("<strong>", State, "</strong><br>",
                       "Life Expectancy:", round(Life_Expectancy, 1), "years<br>",
                       "Pesticide Use:", round(Pesticide_Use_kg_per_ha, 2), "kg/ha")
      ) %>%
      addLegend(position = "bottomright",
                pal = pal,
                values = ~Life_Expectancy,
                title = "Life Expectancy")
  })
  
  # Histograms
  output$histogram_life <- renderPlotly({
    p <- ggplot(states_data, aes(x = Life_Expectancy)) +
      geom_histogram(bins = 20, fill = "#2ecc71", color = "white", alpha = 0.8) +
      labs(title = "Distribution of Life Expectancy",
           x = "Life Expectancy (years)",
           y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$histogram_pesticide <- renderPlotly({
    p <- ggplot(states_data, aes(x = Pesticide_Use_kg_per_ha)) +
      geom_histogram(bins = 20, fill = "#f39c12", color = "white", alpha = 0.8) +
      labs(title = "Distribution of Pesticide Use",
           x = "Pesticide Use (kg/ha)",
           y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data Table
  output$data_table <- renderDT({
    datatable(filtered_data() %>%
                select(State, Region, Life_Expectancy, 
                       Pesticide_Use_kg_per_ha, Agricultural_Area_pct) %>%
                rename("Life Expectancy" = Life_Expectancy,
                       "Pesticide Use (kg/ha)" = Pesticide_Use_kg_per_ha,
                       "Agricultural Area (%)" = Agricultural_Area_pct),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatRound(columns = c("Life Expectancy", "Pesticide Use (kg/ha)", 
                              "Agricultural Area (%)"), digits = 2)
  })
  
  # Summary Statistics
  output$summary_stats <- renderPrint({
    data <- filtered_data()
    cat("Life Expectancy Summary:\n")
    print(summary(data$Life_Expectancy))
    cat("\n\nPesticide Use Summary:\n")
    print(summary(data$Pesticide_Use_kg_per_ha))
  })
  
  # Download Handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("pesticide_life_expectancy_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Correlation Test
  output$correlation_test <- renderPrint({
    cor.test(states_data$Pesticide_Use_kg_per_ha, 
             states_data$Life_Expectancy)
  })
  
  output$correlation_interpretation <- renderText({
    cor_val <- cor(states_data$Pesticide_Use_kg_per_ha, 
                   states_data$Life_Expectancy)
    
    if(abs(cor_val) < 0.3) {
      "Weak correlation: Little to no linear relationship detected."
    } else if(abs(cor_val) < 0.7) {
      "Moderate correlation: Some relationship exists but other factors are important."
    } else {
      "Strong correlation: Substantial linear relationship detected."
    }
  })
  
  # Regression
  output$regression_summary <- renderPrint({
    model <- lm(Life_Expectancy ~ Pesticide_Use_kg_per_ha, data = states_data)
    summary(model)
  })
  
  output$regression_plot <- renderPlot({
    model <- lm(Life_Expectancy ~ Pesticide_Use_kg_per_ha, data = states_data)
    
    par(mfrow = c(2, 2))
    plot(model)
  })
  
  # Regional Boxplot
  output$regional_boxplot <- renderPlotly({
    p <- ggplot(states_data, aes(x = Region, y = Life_Expectancy, fill = Region)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      labs(title = "Life Expectancy by Region",
           x = "Region",
           y = "Life Expectancy (years)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ANOVA
  output$anova_results <- renderPrint({
    model <- aov(Life_Expectancy ~ Region, data = states_data)
    summary(model)
  })
}
