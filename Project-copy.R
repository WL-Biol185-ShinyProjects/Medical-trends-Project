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
                       src = "about_image.jpg",
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
                     tags$p("Dashboard built in R using Shiny, Leaflet, Plotly, and DT.")
                 )
                 
             ) # End home-section
             
    ) # End About tabPanel
    
    
  ) # End navbarPage
  
) # End fluidPage

