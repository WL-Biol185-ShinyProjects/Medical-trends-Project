library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)
library(leaflet)
library(sf)

# =============================================================================
# BIVARIATE COLOR HELPERS  (outside server function — built once at startup)
# =============================================================================

# Classic Scherer 3×3 bivariate palette
# Index = (var2_tertile - 1) * 3 + var1_tertile   (both 1/2/3)
.BIV_PALETTE <- c(
  "#e8e8e8", "#ace4e4", "#5ac8c8",   # var2 = Low  , var1 = Lo/Mid/Hi
  "#dfb0d6", "#a5add3", "#5698b9",   # var2 = Mid
  "#be64ac", "#8c62aa", "#3b4994"    # var2 = High
)

# Return a hex-colour vector the same length as var1/var2
bivariate_colors <- function(var1, var2, palette = .BIV_PALETTE) {
  rank3 <- function(x) {
    cuts <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
    out  <- rep(NA_integer_, length(x))
    out[!is.na(x) & x <= cuts[1]]                <- 1L
    out[!is.na(x) & x >  cuts[1] & x <= cuts[2]] <- 2L
    out[!is.na(x) & x >  cuts[2]]                <- 3L
    out
  }
  r1  <- rank3(var1)
  r2  <- rank3(var2)
  idx <- (r2 - 1L) * 3L + r1          # 1..9; NA when either is NA
  out <- palette[idx]
  out[is.na(out)] <- "#d0d0d0"
  out
}

# Tiny HTML 3×3 legend injected via addControl()
bivariate_legend_html <- function(label_x, label_y, palette = .BIV_PALETTE) {
  cell <- function(r, c)
    sprintf('<td style="width:18px;height:18px;background:%s;"></td>',
            palette[(r - 1L) * 3L + c])
  rows_html <- paste(
    sapply(3:1, function(r)
      sprintf("<tr>%s%s%s</tr>", cell(r,1), cell(r,2), cell(r,3))),
    collapse = ""
  )
  sprintf('
<div style="
  background:rgba(255,255,255,0.93);
  padding:10px 12px 8px 12px;
  border-radius:6px;
  font-family:Inter,sans-serif;
  font-size:11px;
  line-height:1.3;
  box-shadow:0 1px 5px rgba(0,0,0,0.25);">
  <div style="margin-bottom:5px;font-weight:600;color:#333;">Legend</div>
  <div style="display:flex;align-items:flex-end;gap:4px;">
    <div style="writing-mode:vertical-rl;transform:rotate(180deg);
                font-size:10px;color:#555;margin-bottom:4px;">%s ▲</div>
    <table style="border-collapse:collapse;border-spacing:0;">%s</table>
  </div>
  <div style="text-align:center;margin-top:3px;font-size:10px;color:#555;">%s ▶</div>
  <div style="margin-top:6px;font-size:10px;color:#888;line-height:1.4;">
    Low / Mid / High<br>based on tertiles
  </div>
</div>', label_y, rows_html, label_x)
}

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
  
  normalize_state_to_abbr <- function(x) {
    x <- trimws(as.character(x))
    x[x == ""] <- NA_character_
    upper <- toupper(x)
    is_abbr <- grepl("^[A-Z]{2}$", upper)
    name_lookup <- setNames(state.abb, tolower(state.name))
    name_lookup["district of columbia"] <- "DC"
    out <- upper
    idx <- which(!is_abbr & !is.na(x))
    if (length(idx) > 0) {
      out[idx] <- toupper(name_lookup[tolower(x[idx])])
    }
    out
  }
  
  # ===========================================================================
  # STAT CARD HELPER
  # ===========================================================================
  
  make_stat_card <- function(test_name, metrics) {
    rows <- paste0(
      sapply(names(metrics), function(label) {
        sprintf('
        <div style="display:flex;justify-content:space-between;align-items:baseline;
                    padding:8px 0;border-bottom:1px solid rgba(255,255,255,0.08);">
          <span style="font-size:10px;letter-spacing:2px;text-transform:uppercase;
                       color:#a7c957;font-family:monospace;">%s</span>
          <span style="font-size:15px;font-weight:bold;color:white;font-family:monospace;">%s</span>
        </div>', label, metrics[[label]])
      }),
      collapse = ""
    )
    HTML(sprintf('
    <div style="background:linear-gradient(160deg,#1a3009 0%%,#2d5016 100%%);
                border-radius:10px;padding:20px 22px;margin-top:8px;
                box-shadow:0 2px 12px rgba(45,80,22,0.25);">
      <div style="font-size:11px;letter-spacing:3px;text-transform:uppercase;
                  color:#a7c957;font-family:monospace;margin-bottom:14px;
                  padding-bottom:10px;border-bottom:1px solid rgba(167,201,87,0.3);">%s</div>
      %s
    </div>', test_name, rows))
  }
  
  # ===========================================================================
  # PROCESS AND MERGE DATA
  # ===========================================================================
  
  Pesticide_State_Data_Agg <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    state_col <- if("state_name" %in% names(data)) "state_name"
    else if("state_code" %in% names(data)) "state_code"
    else return(NULL)
    if(!all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) return(NULL)
    data %>%
      mutate(State_Abbr = normalize_state_to_abbr(.data[[state_col]])) %>%
      filter(!is.na(State_Abbr)) %>%
      group_by(State_Abbr) %>%
      summarise(Avg_Pesticide = mean((LOW_ESTIMATE + HIGH_ESTIMATE) / 2, na.rm = TRUE),
                .groups = "drop")
  })
  
  Pesticide_State_Compounds <- reactive({
    req(Pesticide_County_Data())
    data <- Pesticide_County_Data()
    state_col <- if ("state_name" %in% names(data)) "state_name"
    else if ("state_code" %in% names(data)) "state_code"
    else return(NULL)
    if (is.null(state_col) || !"compound" %in% names(data)) return(NULL)
    if ("AVG_ESTIMATE" %in% names(data)) {
      data <- data %>% mutate(pest_value = AVG_ESTIMATE)
    } else if (all(c("LOW_ESTIMATE", "HIGH_ESTIMATE") %in% names(data))) {
      data <- data %>% mutate(pest_value = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2)
    } else return(NULL)
    targets <- c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos")
    data %>%
      mutate(State_Abbr = normalize_state_to_abbr(.data[[state_col]]),
             compound   = trimws(as.character(compound))) %>%
      filter(!is.na(State_Abbr), compound %in% targets) %>%
      group_by(State_Abbr, compound) %>%
      summarise(value = mean(pest_value, na.rm = TRUE), .groups = "drop") %>%
      group_by(State_Abbr) %>%
      summarise(
        `2,4-D`      = ifelse(any(compound == "2,4-D"),      value[compound == "2,4-D"][1],      NA_real_),
        Glyphosate   = ifelse(any(compound == "Glyphosate"), value[compound == "Glyphosate"][1], NA_real_),
        Paraquat     = ifelse(any(compound == "Paraquat"),   value[compound == "Paraquat"][1],   NA_real_),
        Chlorpyrifos = ifelse(any(compound == "Chlorpyrifos"), value[compound == "Chlorpyrifos"][1], NA_real_),
        .groups = "drop"
      )
  })
  
  Parkinson_Pesticide_State <- reactive({
    req(Parkinson_Data())
    data <- Parkinson_Data() %>% mutate(State_Abbr = normalize_state_to_abbr(State))
    if (!is.null(Pesticide_State_Data_Agg())) {
      data <- data %>% left_join(Pesticide_State_Data_Agg(), by = "State_Abbr")
    }
    data %>% filter(!is.na(Avg_Death_Rate))
  })
  
  Parkinson_Farm_State <- reactive({
    req(Parkinson_Data(), Farm_Data())
    Parkinson_Data() %>%
      left_join(Farm_Data() %>% select(State, Number_Of_Farms, Acres_Operated_Millions),
                by = "State") %>%
      filter(!is.na(Avg_Death_Rate))
  })
  
  County_Pesticide_Life <- reactive({
    req(Pesticide_County_Data(), Expectancy_Data(), County_Coords())
    pesticide <- Pesticide_County_Data()
    life_exp  <- Expectancy_Data()
    coords    <- County_Coords()
    
    coords <- coords %>%
      mutate(
        county_fips       = sprintf("%05d", as.integer(gsub("[^0-9]", "", as.character(cfips)))),
        state_fips        = substr(county_fips, 1, 2),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", name)))
      ) %>%
      left_join(state_fips_mapping, by = "state_fips")
    
    pesticide <- pesticide %>%
      mutate(
        state_abbr        = normalize_state_to_abbr(
          if ("state_name" %in% names(pesticide)) state_name else state_code),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", county_name))),
        Avg_Pesticide     = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
      )
    
    life_exp <- life_exp %>%
      mutate(
        state_abbr        = normalize_state_to_abbr(State),
        county_name_clean = trimws(tolower(gsub(" County$| Parish$", "", County)))
      )
    
    pesticide_state_county <- pesticide %>%
      filter(!is.na(state_abbr), !is.na(county_name_clean)) %>%
      group_by(state_abbr, county_name_clean) %>%
      summarise(Avg_Pesticide = mean(Avg_Pesticide, na.rm = TRUE),
                state_name    = first(state_abbr),
                county_name   = first(county_name),
                .groups = "drop")
    
    life_state_county <- life_exp %>%
      filter(!is.na(state_abbr), !is.na(county_name_clean)) %>%
      group_by(state_abbr, county_name_clean) %>%
      summarise(Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE),
                .groups = "drop")
    
    merged <- pesticide_state_county %>%
      left_join(life_state_county, by = c("state_abbr", "county_name_clean")) %>%
      left_join(
        coords %>% transmute(state_abbr = State, county_name_clean,
                             county_fips = as.character(county_fips), lng, lat),
        by = c("state_abbr", "county_name_clean")
      ) %>%
      filter(!is.na(lat), !is.na(lng), !is.na(county_fips))
    
    return(merged)
  })
  
  State_Centroids <- reactive({
    req(County_Coords())
    County_Coords() %>%
      mutate(
        county_fips = sprintf("%05d", as.integer(gsub("[^0-9]", "", as.character(cfips)))),
        state_fips  = substr(county_fips, 1, 2)
      ) %>%
      left_join(state_fips_mapping, by = "state_fips") %>%
      filter(!is.na(State), !is.na(lat), !is.na(lng)) %>%
      group_by(State) %>%
      summarise(lat = mean(lat, na.rm = TRUE), lng = mean(lng, na.rm = TRUE),
                .groups = "drop")
  })
  
  # SF polygon layers
  state_sf <- reactive({
    sm     <- maps::map("state", fill = TRUE, plot = FALSE)
    sf_obj <- sf::st_as_sf(sm)
    sf_obj$state_lower <- tolower(sf_obj$ID)
    # Add abbreviation for joining
    abbr_lookup <- setNames(state.abb, tolower(state.name))
    sf_obj$State_Abbr <- unname(abbr_lookup[sf_obj$state_lower])
    sf_obj
  })
  
  county_sf <- reactive({
    cm     <- maps::map("county", fill = TRUE, plot = FALSE)
    sf_obj <- sf::st_as_sf(cm)
    parts  <- strsplit(sf_obj$ID, ",")
    sf_obj$state_lower  <- tolower(vapply(parts, `[`, character(1), 1))
    sf_obj$county_lower <- trimws(tolower(vapply(parts, `[`, character(1), 2)))
    sf_obj
  })
  
  # ===========================================================================
  # POPULATE MAP3 STATE SELECTOR
  # ===========================================================================
  
  observe({
    req(County_Pesticide_Life())
    abbr_to_full <- setNames(state.name, state.abb)
    abbrs <- County_Pesticide_Life() %>%
      filter(!is.na(state_abbr)) %>%
      distinct(state_abbr) %>%
      arrange(state_abbr) %>%
      pull(state_abbr)
    labels <- ifelse(!is.na(abbr_to_full[abbrs]), abbr_to_full[abbrs], abbrs)
    choices <- c("All States" = "all", setNames(abbrs, labels))
    updateSelectInput(session, "map3_state_selector",
                      choices = choices, selected = "all")
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
        div(class = "stat-box", div(class = "stat-number", avg_death_rate),
            div(class = "stat-label", "Avg Parkinson's Death Rate")),
        div(class = "stat-box", div(class = "stat-number", total_farms),
            div(class = "stat-label", "Total U.S. Farms")),
        div(class = "stat-box", div(class = "stat-number", avg_life_exp),
            div(class = "stat-label", "Avg Life Expectancy (years)")),
        div(class = "stat-box", div(class = "stat-number", states_count),
            div(class = "stat-label", "States Analyzed"))
    )
  })
  
  # ===========================================================================
  # MAP SELECTOR OUTPUTS
  # ===========================================================================
  
  output$map_title <- renderText({
    switch(input$selected_map,
           "map1" = "Parkinson's Death Rate \u00d7 Pesticide Use — Bivariate Choropleth (State Level)",
           "map2" = "Parkinson's Death Rate \u00d7 Farm Count — Bivariate Choropleth (State Level)",
           "map3" = "Pesticide Use \u00d7 Life Expectancy — Bivariate Choropleth (County Level)",
           "Select a map")
  })
  
  output$map_description <- renderUI({
    switch(input$selected_map,
           "map1" = div(p(style = "font-size:0.9em;line-height:1.6;",
                          "Bivariate choropleth — each state is colored by the combination of its",
                          strong(" Parkinson's death rate"), " (X axis) and",
                          strong(" avg pesticide use"), " (Y axis).",
                          br(), br(),
                          "Hover a state for individual pesticide breakdowns.")),
           "map2" = div(p(style = "font-size:0.9em;line-height:1.6;",
                          "Bivariate choropleth — each state is colored by the combination of its",
                          strong(" Parkinson's death rate"), " (X axis) and",
                          strong(" number of farms"), " (Y axis).",
                          br(), br(),
                          "Hover a state for farm acreage details.")),
           "map3" = div(p(style = "font-size:0.9em;line-height:1.6;",
                          "Bivariate county choropleth — each county is colored by the combination of",
                          strong(" pesticide use"), " (X axis) and",
                          strong(" life expectancy"), " (Y axis).",
                          br(), br(),
                          "Use the selector below the map to zoom to a single state.")),
           p("Select a map to view details.")
    )
  })
  
  # ===========================================================================
  # MAIN MAP — bivariate choropleths
  # ===========================================================================
  
  output$main_map <- renderLeaflet({
    selected <- input$selected_map
    
    # ── MAP 1: Death Rate × Pesticide ────────────────────────────────────────
    if (selected == "map1") {
      req(Parkinson_Pesticide_State(), state_sf())
      data      <- Parkinson_Pesticide_State()
      compounds <- Pesticide_State_Compounds()
      
      sp <- state_sf() %>%
        left_join(data %>% select(State_Abbr, Avg_Death_Rate, Avg_Pesticide),
                  by = "State_Abbr")
      
      if (!is.null(compounds)) {
        sp <- sp %>%
          left_join(compounds, by = "State_Abbr") %>%
          rename(pest_24d = `2,4-D`, pest_glyphosate = Glyphosate,
                 pest_paraquat = Paraquat, pest_chlorpyrifos = Chlorpyrifos)
      } else {
        sp$pest_24d <- sp$pest_glyphosate <- sp$pest_paraquat <- sp$pest_chlorpyrifos <- NA_real_
      }
      
      sp$biv_color <- bivariate_colors(sp$Avg_Death_Rate, sp$Avg_Pesticide)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>
         Death Rate: %s<br/>
         Avg Pesticide: %s lbs<br/>
         2,4-D: %s | Glyphosate: %s<br/>
         Paraquat: %s | Chlorpyrifos: %s",
        tools::toTitleCase(sp$state_lower),
        ifelse(is.na(sp$Avg_Death_Rate),      "N/A", round(sp$Avg_Death_Rate, 2)),
        ifelse(is.na(sp$Avg_Pesticide),        "N/A", round(sp$Avg_Pesticide, 1)),
        ifelse(is.na(sp$pest_24d),             "N/A", round(sp$pest_24d, 1)),
        ifelse(is.na(sp$pest_glyphosate),      "N/A", round(sp$pest_glyphosate, 1)),
        ifelse(is.na(sp$pest_paraquat),        "N/A", round(sp$pest_paraquat, 1)),
        ifelse(is.na(sp$pest_chlorpyrifos),    "N/A", round(sp$pest_chlorpyrifos, 1))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(sp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor        = ~biv_color,
          fillOpacity      = 0.80,
          color            = "white",
          weight           = 1.5,
          opacity          = 1,
          highlightOptions = highlightOptions(weight = 3, color = "#333",
                                              fillOpacity = 0.95, bringToFront = TRUE),
          label            = labels,
          labelOptions     = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "12px", direction = "auto")
        ) %>%
        addControl(html = bivariate_legend_html("Death Rate", "Pesticide Use"),
                   position = "bottomright")
      
      # ── MAP 2: Death Rate × Farm Count ───────────────────────────────────────
    } else if (selected == "map2") {
      req(Parkinson_Farm_State(), state_sf())
      data <- Parkinson_Farm_State() %>%
        mutate(State_Abbr = normalize_state_to_abbr(State))
      
      sp <- state_sf() %>%
        left_join(data %>% select(State_Abbr, Avg_Death_Rate,
                                  Number_Of_Farms, Acres_Operated_Millions),
                  by = "State_Abbr")
      
      sp$biv_color <- bivariate_colors(sp$Avg_Death_Rate, sp$Number_Of_Farms)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>Death Rate: %s<br/>Farms: %s<br/>Acres (M): %s",
        tools::toTitleCase(sp$state_lower),
        ifelse(is.na(sp$Avg_Death_Rate),         "N/A", round(sp$Avg_Death_Rate, 2)),
        ifelse(is.na(sp$Number_Of_Farms),         "N/A", format(sp$Number_Of_Farms, big.mark = ",")),
        ifelse(is.na(sp$Acres_Operated_Millions), "N/A", round(sp$Acres_Operated_Millions, 1))
      ) %>% lapply(htmltools::HTML)
      
      leaflet(sp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
        addPolygons(
          fillColor        = ~biv_color,
          fillOpacity      = 0.80,
          color            = "white",
          weight           = 1.5,
          opacity          = 1,
          highlightOptions = highlightOptions(weight = 3, color = "#333",
                                              fillOpacity = 0.95, bringToFront = TRUE),
          label            = labels,
          labelOptions     = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "12px", direction = "auto")
        ) %>%
        addControl(html = bivariate_legend_html("Death Rate", "Farm Count"),
                   position = "bottomright")
      
      # ── MAP 3: Pesticide × Life Expectancy (county, with state zoom) ─────────
    } else if (selected == "map3") {
      data <- County_Pesticide_Life()
      
      if (is.null(data) || nrow(data) == 0 ||
          !"Avg_Life_Expectancy" %in% names(data) ||
          sum(!is.na(data$Avg_Life_Expectancy)) == 0) {
        return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
                 setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                 addPopups(-98.5, 39.83, "No county life-expectancy data available"))
      }
      
      # Apply state filter
      sel_state <- if (!is.null(input$map3_state_selector)) input$map3_state_selector else "all"
      if (!is.null(sel_state) && sel_state != "all") {
        data <- data %>% filter(state_abbr == sel_state)
      }
      
      abbr_to_full <- setNames(tolower(state.name), state.abb)
      
      county_summary <- data %>%
        filter(!is.na(Avg_Life_Expectancy)) %>%
        mutate(
          state_lower  = ifelse(nchar(trimws(state_abbr)) == 2,
                                unname(abbr_to_full[toupper(trimws(state_abbr))]),
                                tolower(state_abbr)),
          county_lower = trimws(tolower(gsub(" County$| Parish$", "", county_name)))
        ) %>%
        filter(!is.na(state_lower), !is.na(county_lower)) %>%
        group_by(state_lower, county_lower) %>%
        summarise(Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE),
                  Avg_Pesticide       = mean(Avg_Pesticide,       na.rm = TRUE),
                  .groups = "drop")
      
      cp <- county_sf()
      
      # Subset polygons when zooming to a state
      if (!is.null(sel_state) && sel_state != "all") {
        full_name <- tolower(state.name[state.abb == sel_state])
        if (length(full_name) == 1) cp <- cp %>% filter(state_lower == full_name)
      }
      
      cp <- cp %>%
        left_join(county_summary, by = c("state_lower", "county_lower"))
      
      if (sum(!is.na(cp$Avg_Life_Expectancy)) == 0) {
        return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
                 setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
                 addPopups(-98.5, 39.83, "No matching county data after filtering"))
      }
      
      cp$biv_color <- bivariate_colors(cp$Avg_Pesticide, cp$Avg_Life_Expectancy)
      
      labels <- sprintf(
        "<strong>%s, %s</strong><br/>Life Expectancy: %s yrs<br/>Avg Pesticide: %s lbs",
        tools::toTitleCase(cp$county_lower),
        tools::toTitleCase(cp$state_lower),
        ifelse(is.na(cp$Avg_Life_Expectancy), "N/A", round(cp$Avg_Life_Expectancy, 1)),
        ifelse(is.na(cp$Avg_Pesticide),        "N/A", round(cp$Avg_Pesticide, 2))
      ) %>% lapply(htmltools::HTML)
      
      # Compute view from filtered polygon bbox
      if (!is.null(sel_state) && sel_state != "all") {
        bbox  <- sf::st_bbox(cp)
        c_lng <- mean(c(bbox["xmin"], bbox["xmax"]))
        c_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
        zoom  <- 6
      } else {
        c_lng <- -98.5; c_lat <- 39.5; zoom <- 4
      }
      
      leaflet(cp) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = c_lng, lat = c_lat, zoom = zoom) %>%
        addPolygons(
          fillColor        = ~biv_color,
          fillOpacity      = 0.80,
          color            = "white",
          weight           = 0.5,
          opacity          = 0.9,
          highlightOptions = highlightOptions(weight = 2, color = "#333",
                                              fillOpacity = 0.95, bringToFront = TRUE),
          label            = labels,
          labelOptions     = labelOptions(
            style     = list("font-weight" = "normal", padding = "4px 8px"),
            textsize  = "12px", direction = "auto")
        ) %>%
        addControl(html = bivariate_legend_html("Pesticide Use", "Life Expectancy"),
                   position = "bottomright")
      
    } else {
      leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -98.5, lat = 39.5, zoom = 4)
    }
  })
  
  # ===========================================================================

  # MAP 1: PARKINSON'S VS PESTICIDES (STATE LEVEL CHOROPLETH)
  # ===========================================================================
  
  output$map1 <- renderLeaflet({
    req(Parkinson_Pesticide_State(), State_Centroids())
    data <- Parkinson_Pesticide_State()
    compounds <- Pesticide_State_Compounds()
    
    states <- State_Centroids() %>%
      left_join(
        data %>% select(State_Abbr, Avg_Death_Rate, Avg_Pesticide),
        by = c("State" = "State_Abbr")
      )
    
    if (!is.null(compounds)) {
      states <- states %>%
        left_join(compounds, by = c("State" = "State_Abbr")) %>%
        rename(
          pest_24d = `2,4-D`,
          pest_glyphosate = Glyphosate,
          pest_paraquat = Paraquat,
          pest_chlorpyrifos = Chlorpyrifos
        )
    } else {
      states$pest_24d <- NA_real_
      states$pest_glyphosate <- NA_real_
      states$pest_paraquat <- NA_real_
      states$pest_chlorpyrifos <- NA_real_
    }
    
    pal <- colorNumeric("YlOrRd", domain = states$Avg_Death_Rate, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s</strong><br/>Death Rate: %s<br/>2,4-D: %s lbs<br/>Glyphosate: %s lbs<br/>Paraquat: %s lbs<br/>Chlorpyrifos: %s lbs",
                      states$State,
                      ifelse(is.na(states$Avg_Death_Rate), "N/A", round(states$Avg_Death_Rate, 2)),
                      ifelse(is.na(states$pest_24d), "N/A", round(states$pest_24d, 1)),
                      ifelse(is.na(states$pest_glyphosate), "N/A", round(states$pest_glyphosate, 1)),
                      ifelse(is.na(states$pest_paraquat), "N/A", round(states$pest_paraquat, 1)),
                      ifelse(is.na(states$pest_chlorpyrifos), "N/A", round(states$pest_chlorpyrifos, 1))) %>%
      lapply(htmltools::HTML)
    
    leaflet(states) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        stroke = TRUE,
        weight = 1,
        color = "white",
        fillColor = ~pal(Avg_Death_Rate),
        fillOpacity = 0.85,
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = ~Avg_Death_Rate, opacity = 1,
                title = "Parkinson's<br/>Death Rate", position = "bottomright")
  })
  
  # ===========================================================================
  # MAP 2: PARKINSON'S VS FARMS (STATE LEVEL CHOROPLETH)
  # ===========================================================================
  
  output$map2 <- renderLeaflet({
    req(Parkinson_Farm_State(), State_Centroids())
    data <- Parkinson_Farm_State()
    states <- State_Centroids() %>%
      left_join(data %>% mutate(State_Abbr = normalize_state_to_abbr(State)),
                by = c("State" = "State_Abbr"))
    
    pal <- colorNumeric("RdPu", domain = states$Avg_Death_Rate, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s</strong><br/>Death Rate: %s<br/>Farms: %s",
                      states$State,
                      ifelse(is.na(states$Avg_Death_Rate), "N/A", round(states$Avg_Death_Rate, 2)),
                      ifelse(is.na(states$Number_Of_Farms), "N/A", format(round(states$Number_Of_Farms), big.mark = ","))) %>%
      lapply(htmltools::HTML)
    
    leaflet(states) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.5, lat = 39.5, zoom = 4) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 8,
        stroke = TRUE,
        weight = 1,
        color = "white",
        fillColor = ~pal(Avg_Death_Rate),
        fillOpacity = 0.85,
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = ~Avg_Death_Rate, opacity = 1,
                title = "Parkinson's<br/>Death Rate", position = "bottomright")
  })
  
  # ===========================================================================
  # MAP 3: PESTICIDES VS LIFE EXPECTANCY (COUNTY LEVEL CHOROPLETH)
  # ===========================================================================
  
  output$map3 <- renderLeaflet({
    req(County_Pesticide_Life())
    data <- County_Pesticide_Life()
    
    # Filter by selected state
    if(!is.null(input$state_selector_map3) && input$state_selector_map3 != "all") {
      data <- data %>% filter(state_name == input$state_selector_map3)
    }
    
    data_with_life <- data %>% filter(!is.na(Avg_Life_Expectancy), !is.na(county_fips))
    
    if(nrow(data_with_life) == 0) {
      return(leaflet() %>% 
               addProviderTiles(providers$CartoDB.Positron) %>% 
               setView(lng = -98.5, lat = 39.5, zoom = 4))
    }
    
    pal <- colorNumeric("YlOrRd", domain = data_with_life$Avg_Life_Expectancy, reverse = TRUE, na.color = "#cccccc")
    
    labels <- sprintf("<strong>%s, %s</strong><br/>Life Expectancy: %s years<br/>Pesticide: %s lbs",
                      data_with_life$county_name,
                      data_with_life$state_name,
                      round(data_with_life$Avg_Life_Expectancy, 1),
                      round(data_with_life$Avg_Pesticide, 1)) %>%
      lapply(htmltools::HTML)
    
    zoom_level <- if(input$state_selector_map3 == "all") 4 else 6
    center_lng <- if(input$state_selector_map3 == "all") -98.5 else mean(data_with_life$lng, na.rm = TRUE)
    center_lat <- if(input$state_selector_map3 == "all") 39.5 else mean(data_with_life$lat, na.rm = TRUE)
    
    leaflet(data_with_life) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = center_lng, lat = center_lat, zoom = zoom_level) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        radius = 4,
        stroke = FALSE,
        fillColor = ~pal(Avg_Life_Expectancy),
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"))
      ) %>%
      addLegend(pal = pal, values = ~Avg_Life_Expectancy, opacity = 1,
                title = "Life Expectancy<br/>(years)", position = "bottomright")
  })
  # ===========================================================================
  # DATA TABLES
  # ===========================================================================
  
  output$data_table_parkinson <- renderDT({
    req(Parkinson_Data())
    datatable(Parkinson_Data(), options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = 'cell-border stripe hover') %>%
      formatRound(columns = intersect(c("Avg_Death_Rate", "Avg_Deaths"), names(Parkinson_Data())),
                  digits = 2)
  })
  
  output$data_table_farms <- renderDT({
    req(Farm_Data())
    datatable(Farm_Data(), options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = 'cell-border stripe hover') %>%
      formatRound(columns = intersect(c("Area_operated_Acres", "Acres_Operated_Millions"),
                                      names(Farm_Data())), digits = 2)
  })
  
  output$data_table_life_expectancy <- renderDT({
    req(Expectancy_State_Data())
    datatable(Expectancy_State_Data(), options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE, class = 'cell-border stripe hover') %>%
      formatRound(columns = intersect(c("Avg_Life_Expectancy", "Avg_Range_Min"),
                                      names(Expectancy_State_Data())), digits = 2)
  })
  
  output$data_table_state_pesticide <- renderDT({
    data <- State_Pesticide_Data()
    req(data)
    datatable(
      data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  output$data_table_county_pesticide <- renderDT({
    data <- Pesticide_County_Data()
    req(data)
    datatable(
      data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  output$download_parkinsons_data <- downloadHandler(
    filename = function() paste("parkinsons_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      req(Parkinson_Data())
      write.csv(Parkinson_Data(), file, row.names = FALSE)
    }
  )
  
  output$download_farm_data <- downloadHandler(
    filename = function() paste("farm_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      req(Farm_Data())
      write.csv(Farm_Data(), file, row.names = FALSE)
    }
  )
  
  output$download_life_expectancy_data <- downloadHandler(
    filename = function() paste("life_expectancy_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      req(Expectancy_State_Data())
      write.csv(Expectancy_State_Data(), file, row.names = FALSE)
    }
  )
  
  output$download_state_pesticide_data <- downloadHandler(
    filename = function() paste("state_pesticide_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      req(Pesticide_State_Data())
      write.csv(Pesticide_State_Data(), file, row.names = FALSE)
    }
  )
  
  output$download_county_pesticide_data <- downloadHandler(
    filename = function() paste("county_pesticide_data_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      req(Pesticide_County_Data())
      write.csv(Pesticide_County_Data(), file, row.names = FALSE)
    }
  )
  
  # =============================================================================
  # STATE PESTICIDE VS PARKINSON'S MORTALITY RATE
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
    x_seq      <- seq(min(data$AVG_ESTIMATE), max(data$AVG_ESTIMATE), length.out = 100)
    y_fitted   <- coef(model)[1] + coef(model)[2] * x_seq
    plot_ly() %>%
      add_trace(data = data, x = ~AVG_ESTIMATE, y = ~Avg_Death_Rate,
                type = "scatter", mode = "markers+text",
                marker = list(color = "#2d5016", size = 7, opacity = 0.7),
                text = ~state_name, textposition = "top center",
                textfont = list(size = 9, color = "gray30"),
                hovertext = ~paste("State:", state_name,
                                   "<br>AVG_ESTIMATE:", round(AVG_ESTIMATE, 4),
                                   "<br>Parkinson's Death Rate:", round(Avg_Death_Rate, 2)),
                hoverinfo = "text", name = "States") %>%
      add_trace(inherit = FALSE, x = x_seq, y = y_fitted,
                type = "scatter", mode = "lines",
                line = list(color = "darkred", dash = "dash", width = 2),
                hoverinfo = "skip", name = "Regression Line") %>%
      layout(
        title = list(text = paste0(input$selected_state_pesticide,
                                   ": Pesticide Use vs. Parkinson's Death Rate by State"),
                     font = list(size = 15)),
        xaxis = list(title = paste0(input$selected_state_pesticide, " AVG_ESTIMATE")),
        yaxis = list(title = "Avg Parkinson's Death Rate"),
        hovermode = "closest",
        annotations = list(list(
          x = max(data$AVG_ESTIMATE) * 0.85, y = min(data$Avg_Death_Rate) + 0.3,
          text = paste0("r = ", cor_val, "<br>R\u00b2 = ", r_squared, "<br>p = ", p_val),
          showarrow = FALSE, font = list(color = "darkred", size = 13),
          bgcolor = "white", bordercolor = "darkred", borderwidth = 1
        )))
  })
  
  output$stats_state_pesticide_parkinson <- renderUI({
    req(state_pesticide_parkinson_merged())
    data  <- state_pesticide_parkinson_merged()
    model <- lm(Avg_Death_Rate ~ AVG_ESTIMATE, data = data)
    ct    <- cor.test(data$AVG_ESTIMATE, data$Avg_Death_Rate)
    sm    <- summary(model)
    tagList(
      make_stat_card("Pearson Correlation", list(
        "r"           = round(ct$estimate, 4),
        "t statistic" = round(ct$statistic, 3),
        "p-value"     = signif(ct$p.value, 3),
        "95% CI"      = paste0("[", round(ct$conf.int[1], 3), ", ", round(ct$conf.int[2], 3), "]")
      )),
      br(),
      make_stat_card("Linear Regression", list(
        "R\u00b2"         = round(sm$r.squared, 4),
        "Adj. R\u00b2"    = round(sm$adj.r.squared, 4),
        "Slope"       = signif(coef(sm)[2, 1], 3),
        "Slope SE"    = signif(coef(sm)[2, 2], 3),
        "F-statistic" = round(sm$fstatistic[1], 3),
        "p-value"     = signif(pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3],
                                  lower.tail = FALSE), 3)
      ))
    )
  })
  
  # =============================================================================
  # COUNTY PESTICIDE VS LIFE EXPECTANCY
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
    x_seq      <- seq(min(log10(data$AVG_ESTIMATE)), max(log10(data$AVG_ESTIMATE)), length.out = 100)
    y_fitted   <- coef(model)[1] + coef(model)[2] * x_seq
    plot_ly() %>%
      add_trace(data = data, x = ~log10(AVG_ESTIMATE), y = ~Avg_Life_Expectancy,
                type = "scatter", mode = "markers",
                marker = list(color = "#2d5016", size = 5, opacity = 0.5),
                text = ~paste("County:", county_name,
                              "<br>AVG_ESTIMATE:", round(AVG_ESTIMATE, 2),
                              "<br>Life Expectancy:", round(Avg_Life_Expectancy, 2)),
                hoverinfo = "text", name = "Counties") %>%
      add_trace(inherit = FALSE, x = x_seq, y = y_fitted,
                type = "scatter", mode = "lines",
                line = list(color = "darkred", dash = "dash", width = 2),
                hoverinfo = "skip", name = "Regression Line") %>%
      layout(
        title = list(text = paste0(input$selected_pesticide,
                                   ": Pesticide Use vs. Avg Life Expectancy by County"),
                     font = list(size = 15)),
        xaxis = list(title = paste0(input$selected_pesticide, " AVG_ESTIMATE (log scale)")),
        yaxis = list(title = "Avg Life Expectancy (years)"),
        hovermode = "closest",
        annotations = list(list(
          x = max(log10(data$AVG_ESTIMATE)) * 0.85, y = min(data$Avg_Life_Expectancy) + 1,
          text = paste0("r = ", cor_val, "<br>R\u00b2 = ", r_squared, "<br>p = ", p_val),
          showarrow = FALSE, font = list(color = "darkred", size = 13),
          bgcolor = "white", bordercolor = "darkred", borderwidth = 1
        )))
  })
  
  output$stats_county_pesticide_life <- renderUI({
    req(county_pesticide_merged())
    data  <- county_pesticide_merged()
    model <- lm(Avg_Life_Expectancy ~ log10(AVG_ESTIMATE), data = data)
    ct    <- cor.test(log10(data$AVG_ESTIMATE), data$Avg_Life_Expectancy)
    sm    <- summary(model)
    tagList(
      make_stat_card("Pearson Correlation", list(
        "r"           = round(ct$estimate, 4),
        "t statistic" = round(ct$statistic, 3),
        "p-value"     = signif(ct$p.value, 3),
        "95% CI"      = paste0("[", round(ct$conf.int[1], 3), ", ", round(ct$conf.int[2], 3), "]")
      )),
      br(),
      make_stat_card("Linear Regression (log scale)", list(
        "R\u00b2"         = round(sm$r.squared, 4),
        "Adj. R\u00b2"    = round(sm$adj.r.squared, 4),
        "Slope"       = signif(coef(sm)[2, 1], 3),
        "Slope SE"    = signif(coef(sm)[2, 2], 3),
        "F-statistic" = round(sm$fstatistic[1], 3),
        "p-value"     = signif(pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3],
                                  lower.tail = FALSE), 3)
      ))
    )
  })
  
  # =============================================================================
  # FARM COUNT VS PARKINSON'S
  # =============================================================================
  
  farm_parkinson_merged <- reactive({
    req(Farm_Data(), Parkinson_Data())
    farm <- as.data.frame(Farm_Data())
    park <- as.data.frame(Parkinson_Data())
    data.frame(State = farm$State, NumberOfFarms = farm$Number_Of_Farms,
               DeathRate = park$Avg_Death_Rate) %>%
      filter(!is.na(NumberOfFarms), !is.na(DeathRate))
  })
  
  output$plot_farm_parkinson_detailed <- renderPlotly({
    req(farm_parkinson_merged())
    data <- farm_parkinson_merged()
    model      <- lm(DeathRate ~ NumberOfFarms, data = data)
    cor_val    <- round(cor(data$NumberOfFarms, data$DeathRate, use = "complete.obs"), 3)
    r_squared  <- round(summary(model)$r.squared, 3)
    coef_table <- summary(model)$coefficients
    p_val      <- if (nrow(coef_table) >= 2) round(coef_table[2, 4], 4) else NA
    x_seq      <- seq(min(data$NumberOfFarms), max(data$NumberOfFarms), length.out = 100)
    y_fitted   <- coef(model)[1] + coef(model)[2] * x_seq
    plot_ly() %>%
      add_trace(data = data, x = ~NumberOfFarms, y = ~DeathRate,
                type = "scatter", mode = "markers+text",
                marker = list(color = "#2d5016", size = 7, opacity = 0.7),
                text = ~State, textposition = "top center",
                textfont = list(size = 9, color = "gray30"),
                hovertext = ~paste("State:", State,
                                   "<br>Number of Farms:", format(NumberOfFarms, big.mark = ","),
                                   "<br>Death Rate:", round(DeathRate, 2)),
                hoverinfo = "text", name = "States") %>%
      add_trace(inherit = FALSE, x = x_seq, y = y_fitted,
                type = "scatter", mode = "lines",
                line = list(color = "darkred", dash = "dash", width = 2),
                hoverinfo = "skip", name = "Regression Line") %>%
      layout(
        title = list(text = "Number of Farms vs. Parkinson's Avg Death Rate by State",
                     font = list(size = 15)),
        xaxis = list(title = "Number of Farms"),
        yaxis = list(title = "Avg Death Rate (Parkinson's)"),
        hovermode = "closest",
        annotations = list(list(
          x = max(data$NumberOfFarms) * 0.85, y = min(data$DeathRate) + 0.5,
          text = paste0("r = ", cor_val, "<br>R\u00b2 = ", r_squared, "<br>p = ", p_val),
          showarrow = FALSE, font = list(color = "darkred", size = 13),
          bgcolor = "white", bordercolor = "darkred", borderwidth = 1
        )))
  })
  
  output$stats_farm_parkinson <- renderUI({
    req(farm_parkinson_merged())
    data  <- farm_parkinson_merged()
    model <- lm(DeathRate ~ NumberOfFarms, data = data)
    ct    <- cor.test(data$NumberOfFarms, data$DeathRate)
    sm    <- summary(model)
    tagList(
      make_stat_card("Pearson Correlation", list(
        "r"           = round(ct$estimate, 4),
        "t statistic" = round(ct$statistic, 3),
        "p-value"     = signif(ct$p.value, 3),
        "95% CI"      = paste0("[", round(ct$conf.int[1], 3), ", ", round(ct$conf.int[2], 3), "]")
      )),
      br(),
      make_stat_card("Linear Regression", list(
        "R\u00b2"         = round(sm$r.squared, 4),
        "Adj. R\u00b2"    = round(sm$adj.r.squared, 4),
        "Slope"       = signif(coef(sm)[2, 1], 3),
        "Slope SE"    = signif(coef(sm)[2, 2], 3),
        "F-statistic" = round(sm$fstatistic[1], 3),
        "p-value"     = signif(pf(sm$fstatistic[1], sm$fstatistic[2], sm$fstatistic[3],
                                  lower.tail = FALSE), 3)
      ))
    )
  })
  
  # =============================================================================
  # ANOVA: COUNTY PESTICIDE EXPOSURE VS LIFE EXPECTANCY
  # =============================================================================
  
  assign_tertiles <- function(x) {
    breaks <- quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    if (length(unique(breaks)) < 4)
      return(factor(dplyr::ntile(x, 3), levels = 1:3, labels = c("Low", "Medium", "High")))
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
    plot_ly(data = data,
            x     = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
            y     = ~Avg_Life_Expectancy, type = "box",
            color = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
            colors = c("#a7c957", "#f4a261", "#bc4749"),
            boxpoints = "outliers", hoverinfo = "y+name") %>%
      layout(title = list(text = paste0(input$anova_county_compound,
                                        ": Exposure Level vs. Life Expectancy (ANOVA p = ", p_val, ")"),
                          font = list(size = 14)),
             xaxis = list(title = "Pesticide Exposure Group"),
             yaxis = list(title = "Avg Life Expectancy (years)"),
             showlegend = FALSE)
  })
  
  output$stats_anova_exposure_life <- renderUI({
    req(anova_exposure_life_data())
    data      <- anova_exposure_life_data()
    aov_model <- aov(Avg_Life_Expectancy ~ exposure_group, data = data)
    aov_sum   <- summary(aov_model)[[1]]
    ss        <- aov_sum[["Sum Sq"]]
    eta_sq    <- round(ss[1] / sum(ss), 4)
    make_stat_card("One-Way ANOVA", list(
      "F-statistic" = round(aov_sum[["F value"]][1], 3),
      "p-value"     = signif(aov_sum[["Pr(>F)"]][1], 3),
      "Eta-squared" = eta_sq
    ))
  })
  
  output$tukey_exposure_life_table <- renderDataTable({
    req(anova_exposure_life_data())
    tukey_df <- as.data.frame(
      TukeyHSD(aov(Avg_Life_Expectancy ~ exposure_group,
                   data = anova_exposure_life_data()))$exposure_group)
    tukey_df <- tibble::rownames_to_column(tukey_df, var = "Comparison")
    colnames(tukey_df) <- c("Comparison", "Difference", "Lower CI", "Upper CI", "Adjusted p-value")
    tukey_df <- tukey_df %>% mutate(across(where(is.numeric), ~ round(., 4)))
    DT::datatable(tukey_df, options = list(pageLength = 10, scrollX = TRUE)) %>%
      DT::formatStyle("Adjusted p-value",
                      backgroundColor = DT::styleInterval(0.05, c("#d4edda", "white")))
  })
  
  # =============================================================================
  # ANOVA: STATE PESTICIDE EXPOSURE VS PARKINSON'S DEATH RATE
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
    plot_ly(data = data,
            x     = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
            y     = ~Avg_Death_Rate, type = "box",
            color = ~factor(exposure_group, levels = c("Low", "Medium", "High")),
            colors = c("#a7c957", "#f4a261", "#bc4749"),
            boxpoints = "outliers", hoverinfo = "y+name") %>%
      layout(title = list(text = paste0(input$anova_state_compound,
                                        ": Exposure Level vs. Parkinson's Death Rate (ANOVA p = ", p_val, ")"),
                          font = list(size = 14)),
             xaxis = list(title = "Pesticide Exposure Group"),
             yaxis = list(title = "Avg Parkinson's Death Rate"),
             showlegend = FALSE)
  })
  
  output$stats_anova_exposure_parkinson <- renderUI({
    req(anova_exposure_parkinson_data())
    data      <- anova_exposure_parkinson_data()
    aov_model <- aov(Avg_Death_Rate ~ exposure_group, data = data)
    aov_sum   <- summary(aov_model)[[1]]
    ss        <- aov_sum[["Sum Sq"]]
    eta_sq    <- round(ss[1] / sum(ss), 4)
    make_stat_card("One-Way ANOVA", list(
      "F-statistic" = round(aov_sum[["F value"]][1], 3),
      "p-value"     = signif(aov_sum[["Pr(>F)"]][1], 3),
      "Eta-squared" = eta_sq
    ))
  })
  
  output$tukey_exposure_parkinson_table <- renderDataTable({
    req(anova_exposure_parkinson_data())
    tukey_df <- as.data.frame(
      TukeyHSD(aov(Avg_Death_Rate ~ exposure_group,
                   data = anova_exposure_parkinson_data()))$exposure_group)
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