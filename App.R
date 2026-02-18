library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)

# Gaussian Dispersion Model Functions
# Based on VSmoke methodology for prescribed burn smoke prediction

# Stability class parameters for Pasquill-Gifford stability categories
stability_params <- list(
  "A" = list(a = 0.527, b = 0.865, c = 0.28, d = 0.90),   # Very unstable
  "B" = list(a = 0.371, b = 0.866, c = 0.23, d = 0.85),   # Moderately unstable
  "C" = list(a = 0.209, b = 0.897, c = 0.22, d = 0.80),   # Slightly unstable
  "D" = list(a = 0.128, b = 0.905, c = 0.20, d = 0.76),   # Neutral
  "E" = list(a = 0.098, b = 0.902, c = 0.15, d = 0.73),   # Slightly stable
  "F" = list(a = 0.065, b = 0.902, c = 0.12, d = 0.67)    # Moderately stable
)

# Fuel load emission factors (tons particulate matter per ton fuel consumed)
fuel_emission_factors <- list(
  "Grass" = 0.013,
  "Shrub" = 0.016,
  "Hardwood Litter" = 0.018,
  "Conifer Litter" = 0.020,
  "Logging Slash" = 0.022,
  "Heavy Fuels" = 0.025
)

# Photo guide lookup table for custom litter/duff calculator
photo_guide_options <- tribble(
  ~photo_id, ~site_type, ~ecozone, ~litter_factor, ~duff_factor,
  "P01", "Low elevation pine", "Coastal Plain", 1.38, 4.84,
  "P02", "Upland mixed pine", "Coastal Plain", 1.52, 4.20,
  "P03", "Pine-hardwood", "Piedmont", 1.61, 4.02,
  "P04", "Mesic hardwood", "Piedmont", 1.77, 3.38,
  "P05", "Oak-hickory", "Appalachian Foothills", 1.49, 3.82,
  "P06", "Longleaf pine", "Sandhills", 1.30, 5.10
)

# Function to calculate dispersion coefficients
calc_dispersion_coeffs <- function(distance_km, stability_class) {
  params <- stability_params[[stability_class]]
  
  # Horizontal dispersion coefficient (sigma_y)
  sigma_y <- params$a * distance_km^params$b
  
  # Vertical dispersion coefficient (sigma_z)
  sigma_z <- params$c * distance_km^params$d
  
  list(sigma_y = sigma_y, sigma_z = sigma_z)
}

# Gaussian plume concentration calculation
calc_concentration <- function(x, y, z, Q, u, H, stability_class) {
  # x: downwind distance (km)
  # y: crosswind distance (km)
  # z: height above ground (m)
  # Q: emission rate (g/s)
  # u: wind speed (m/s)
  # H: effective stack height (m)
  # stability_class: atmospheric stability class
  
  if (x <= 0) return(0)
  
  coeffs <- calc_dispersion_coeffs(x, stability_class)
  sigma_y <- coeffs$sigma_y * 1000  # Convert to meters
  sigma_z <- coeffs$sigma_z * 1000  # Convert to meters
  
  # Gaussian plume equation
  term1 <- Q / (2 * pi * u * sigma_y * sigma_z)
  term2 <- exp(-0.5 * (y / sigma_y)^2)
  term3 <- exp(-0.5 * ((z - H) / sigma_z)^2) + exp(-0.5 * ((z + H) / sigma_z)^2)
  
  term1 * term2 * term3
}

# Generate smoke plume prediction grid
generate_smoke_plume <- function(lat, lon, acres, duration_hours, fuel_type, tons_per_acre,
                                 wind_speed = 5, wind_dir = 45, stability_class = "D") {
  
  # Calculate total emissions
  total_fuel <- acres * tons_per_acre * 0.907185  # Convert to metric tons
  emission_factor <- fuel_emission_factors[[fuel_type]]
  total_emissions <- total_fuel * emission_factor * 1000000  # Convert to grams
  
  # Emission rate (g/s)
  Q <- total_emissions / (duration_hours * 3600)
  
  # Effective stack height (estimated based on fire intensity)
  H <- 50 + (tons_per_acre * 10)  # Simple approximation
  
  # Create prediction grid (50km x 50km around burn site)
  grid_size <- 50  # km
  resolution <- 1  # km
  
  x_seq <- seq(-grid_size / 2, grid_size / 2, by = resolution)
  y_seq <- seq(-grid_size / 2, grid_size / 2, by = resolution)
  
  expand_grid(x = x_seq, y = y_seq) %>%
    mutate(
      # Rotate coordinates based on wind direction
      x_rot = x * cos(wind_dir * pi / 180) - y * sin(wind_dir * pi / 180),
      y_rot = x * sin(wind_dir * pi / 180) + y * cos(wind_dir * pi / 180)
    ) %>%
    rowwise() %>%
    mutate(
      concentration = if (x_rot > 0) {
        calc_concentration(
          x = x_rot,
          y = y_rot,
          z = 2,
          Q = Q,
          u = wind_speed,
          H = H,
          stability_class = stability_class
        ) * 1e6  # convert to μg/m³
      } else {
        0
      }
    ) %>%
    ungroup() %>%
    mutate(
      distance = sqrt(x_rot^2 + y_rot^2),
      lat_grid = lat + (y / 111.32),
      lon_grid = lon + (x / (111.32 * cos(lat * pi / 180)))
    ) %>%
    mutate(
      aqi_bin = cut(
        concentration,
        breaks = c(0, 12, 35.4, 55.4, 150.4, 250.4, Inf),
        labels = c(
          "0–50 (Good)",
          "51–100 (Moderate)",
          "101–150 (USG)",
          "151–200 (Unhealthy)",
          "201–300 (Very Unhealthy)",
          "301+ (Hazardous)"
        ),
        include.lowest = TRUE
      )
    ) %>%
    filter(!is.na(aqi_bin))
}

make_ellipse <- function(lon, lat, a_km, b_km, angle_deg, n = 60) {
  theta <- seq(0, 2 * pi, length.out = n)
  angle <- angle_deg * pi / 180
  
  x <- a_km * cos(theta)
  y <- b_km * sin(theta)
  
  x_rot <- x * cos(angle) - y * sin(angle)
  y_rot <- x * sin(angle) + y * cos(angle)
  
  tibble(
    lng = lon + x_rot / (111.32 * cos(lat * pi / 180)),
    lat = lat + y_rot / 111.32
  )
}

# UI
ui <- page_sidebar(
  title = "Forest Management: Smoke Plume Prediction Tool",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57",
    base_font = font_google("Inter")
  ),
  
  sidebar = sidebar(
    width = 350,
    
    # Location inputs
    h4("📍 Burn Location", style = "color: #2E8B57;"),
    numericInput("latitude", "Latitude:", value = 40.7128, min = -90, max = 90, step = 0.0001),
    numericInput("longitude", "Longitude:", value = -74.0060, min = -180, max = 180, step = 0.0001),
    
    # Burn parameters
    h4("🔥 Burn Parameters", style = "color: #2E8B57;"),
    numericInput("acres", "Acres to burn:", value = 100, min = 1, max = 10000, step = 1),
    numericInput("duration", "Burn duration (hours):", value = 4, min = 1, max = 24, step = 0.5),
    
    # Fuel characteristics
    h4("🌿 Fuel Characteristics", style = "color: #2E8B57;"),
    selectInput(
      "fuel_type", "Fuel Type:",
      choices = c("Grass", "Shrub", "Hardwood Litter", "Conifer Litter", "Logging Slash", "Heavy Fuels"),
      selected = "Hardwood Litter"
    ),
    numericInput("fuel_load", "Fuel load (tons/acre):", value = 5, min = 0.1, max = 50, step = 0.1),
    
    # Weather parameters
    h4("🌤️ Weather Conditions", style = "color: #2E8B57;"),
    numericInput("wind_speed", "Wind speed (m/s):", value = 5, min = 1, max = 20, step = 0.5),
    numericInput("wind_direction", "Wind direction (degrees):", value = 45, min = 0, max = 360, step = 5),
    selectInput(
      "stability", "Atmospheric stability:",
      choices = c(
        "Very Unstable (A)" = "A", "Moderately Unstable (B)" = "B",
        "Slightly Unstable (C)" = "C", "Neutral (D)" = "D",
        "Slightly Stable (E)" = "E", "Moderately Stable (F)" = "F"
      ),
      selected = "D"
    ),
    
    br(),
    actionButton("predict", "🔍 Generate Smoke Prediction", class = "btn-primary", style = "width: 100%;")
  ),
  
  # Main panel
  navset_card_tab(
    nav_panel(
      "Smoke Plume Map",
      value_box(
        title = "Prediction Status",
        value = textOutput("status_text"),
        showcase = icon("wind"),
        theme = "primary"
      ),
      br(),
      leafletOutput("smoke_map", height = "600px")
    ),
    
    nav_panel(
      "Prediction Data",
      h4("Smoke Concentration Predictions"),
      p("Concentrations shown in μg/m³ at 2m height above ground level."),
      DTOutput("prediction_table")
    ),
    
    nav_panel(
      "Litter and Duff Mass Calculator",
      layout_columns(
        col_widths = c(4, 8),
        card(
          card_header("Photo Guide Inputs"),
          selectInput("photo_option", "Photo guide option", choices = photo_guide_options$photo_id),
          numericInput("litter_depth", "Litter depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1),
          numericInput("duff_depth", "Duff depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1)
        ),
        card(
          card_header("Estimated Fuel Loading"),
          p("This custom tab mirrors the photo-guide workflow by combining selected site factors with measured depths."),
          value_box(title = "Litter mass", value = textOutput("litter_mass_text"), theme = "success"),
          value_box(title = "Duff mass", value = textOutput("duff_mass_text"), theme = "warning"),
          value_box(title = "Total (litter + duff)", value = textOutput("total_mass_text"), theme = "primary"),
          DTOutput("photo_factor_table")
        )
      )
    ),
    
    nav_panel(
      "Model Information",
      h4("Gaussian Dispersion Model Information"),
      div(
        h5("Model Basis:"),
        p("This application implements a Gaussian plume dispersion model based on VSmoke methodology for predicting smoke transport from prescribed burns."),
        
        h5("Key Parameters:"),
        tags$ul(
          tags$li("Pasquill-Gifford atmospheric stability classes"),
          tags$li("Fuel-specific emission factors for particulate matter"),
          tags$li("Effective plume height based on fire intensity"),
          tags$li("Wind speed and direction effects on dispersion")
        ),
        
        h5("Limitations:"),
        tags$ul(
          tags$li("Simplified terrain assumptions (flat ground)"),
          tags$li("Steady-state atmospheric conditions"),
          tags$li("Point source approximation for burn area"),
          tags$li("Ground-level concentration predictions only")
        ),
        
        h5("Usage Notes:"),
        p("Click on the map to see concentration values at specific locations. Red dot indicates the burn location. Contour rings show predicted smoke concentration levels by AQI band.")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    prediction_data = NULL,
    burn_lat = NULL,
    burn_lon = NULL,
    status = "Ready to generate prediction"
  )
  
  # Update status
  output$status_text <- renderText({
    values$status
  })
  
  selected_photo <- reactive({
    photo_guide_options %>%
      filter(photo_id == input$photo_option) %>%
      slice(1)
  })
  
  photo_calc <- reactive({
    option <- selected_photo()
    req(nrow(option) == 1)
    
    litter_mass <- input$litter_depth * option$litter_factor
    duff_mass <- input$duff_depth * option$duff_factor
    
    list(
      litter_mass = litter_mass,
      duff_mass = duff_mass,
      total_mass = litter_mass + duff_mass,
      option = option
    )
  })
  
  output$litter_mass_text <- renderText({
    paste0(round(photo_calc()$litter_mass, 2), " tons/acre")
  })
  
  output$duff_mass_text <- renderText({
    paste0(round(photo_calc()$duff_mass, 2), " tons/acre")
  })
  
  output$total_mass_text <- renderText({
    paste0(round(photo_calc()$total_mass, 2), " tons/acre")
  })
  
  output$photo_factor_table <- renderDT({
    option <- photo_calc()$option
    table_data <- tibble(
      `Photo Option` = option$photo_id,
      `Site Type` = option$site_type,
      Ecozone = option$ecozone,
      `Litter factor (tons/acre/in)` = option$litter_factor,
      `Duff factor (tons/acre/in)` = option$duff_factor
    )
    
    datatable(table_data, options = list(dom = "t"), rownames = FALSE)
  })
  
  # Generate smoke prediction
  observeEvent(input$predict, {
    values$status <- "Generating smoke plume prediction..."
    
    # Validate inputs
    req(
      input$latitude,
      input$longitude,
      input$acres,
      input$duration,
      input$fuel_type,
      input$fuel_load,
      input$wind_speed,
      input$wind_direction
    )
    
    # Store burn location
    values$burn_lat <- input$latitude
    values$burn_lon <- input$longitude
    
    # Generate prediction
    tryCatch({
      prediction_data <- generate_smoke_plume(
        lat = input$latitude,
        lon = input$longitude,
        acres = input$acres,
        duration_hours = input$duration,
        fuel_type = input$fuel_type,
        tons_per_acre = input$fuel_load,
        wind_speed = input$wind_speed,
        wind_dir = input$wind_direction,
        stability_class = input$stability
      )
      
      values$prediction_data <- prediction_data
      values$status <- "Prediction complete"
    }, error = function(e) {
      values$status <- paste("Error generating prediction:", e$message)
      showNotification("Error generating smoke prediction. Please check inputs.", type = "error")
    })
  })
  
  output$smoke_map <- renderLeaflet({
    req(values$prediction_data)
    
    # AQI Color Palette
    pal <- colorFactor(
      palette = c(
        "#00E400", # Good
        "#FFFF00", # Moderate
        "#FF7E00", # USG
        "#FF0000", # Unhealthy
        "#8F3F97", # Very Unhealthy
        "#7E0023"  # Hazardous
      ),
      domain = levels(values$prediction_data$aqi_bin)
    )
    
    # Get max distance for each AQI category
    ellipse_data <- values$prediction_data %>%
      group_by(aqi_bin) %>%
      summarise(max_dist = max(distance, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(max_dist))
    
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap)
    
    # Draw one ellipse per AQI category
    for (i in seq_len(nrow(ellipse_data))) {
      ellipse_coords <- make_ellipse(
        lon = values$burn_lon,
        lat = values$burn_lat,
        a_km = ellipse_data$max_dist[i],
        b_km = ellipse_data$max_dist[i] / 4,
        angle_deg = input$wind_direction
      )
      
      m <- m %>%
        addPolygons(
          lng = ellipse_coords$lng,
          lat = ellipse_coords$lat,
          fillColor = pal(ellipse_data$aqi_bin[i]),
          fillOpacity = 0.5,
          color = "black",
          weight = 1,
          popup = paste("AQI Category:", ellipse_data$aqi_bin[i])
        )
    }
    
    m %>%
      addCircleMarkers(
        lng = values$burn_lon,
        lat = values$burn_lat,
        radius = 8,
        color = "red",
        fillOpacity = 1,
        popup = "Burn location"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = values$prediction_data$aqi_bin,
        title = "Air Quality Index (PM2.5)",
        opacity = 0.9
      )
  })
  
  # Handle map clicks
  observeEvent(input$smoke_map_click, {
    req(values$prediction_data)
    
    click_lon <- input$smoke_map_click$lng
    click_lat <- input$smoke_map_click$lat
    
    distances <- sqrt(
      (values$prediction_data$lon_grid - click_lon)^2 +
        (values$prediction_data$lat_grid - click_lat)^2
    )
    
    nearest_idx <- which.min(distances)
    nearest_point <- values$prediction_data[nearest_idx, ]
    
    showNotification(
      paste(
        "Location:", round(click_lat, 4), ",", round(click_lon, 4),
        "| Predicted concentration:",
        round(nearest_point$concentration, 2), "μg/m³"
      ),
      type = "message",
      duration = 5
    )
  })
  
  # Render prediction data table
  output$prediction_table <- renderDT({
    req(values$prediction_data)
    
    table_data <- values$prediction_data %>%
      select(lat_grid, lon_grid, concentration, distance) %>%
      rename(
        "Latitude" = lat_grid,
        "Longitude" = lon_grid,
        "Concentration (μg/m³)" = concentration,
        "Distance from source (km)" = distance
      ) %>%
      arrange(desc(`Concentration (μg/m³)`)) %>%
      mutate(
        Latitude = round(Latitude, 6),
        Longitude = round(Longitude, 6),
        `Concentration (μg/m³)` = round(`Concentration (μg/m³)`, 2),
        `Distance from source (km)` = round(`Distance from source (km)`, 2)
      )
    
    datatable(
      table_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        columns = "Concentration (μg/m³)",
        backgroundColor = styleInterval(c(10, 50, 100), c("white", "yellow", "orange", "red"))
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)



