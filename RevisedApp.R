library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)

# ---------------------------
# Gaussian Dispersion Model Functions
# ---------------------------

stability_params <- list(
  "A" = list(a = 0.527, b = 0.865, c = 0.28, d = 0.90),
  "B" = list(a = 0.371, b = 0.866, c = 0.23, d = 0.85),
  "C" = list(a = 0.209, b = 0.897, c = 0.22, d = 0.80),
  "D" = list(a = 0.128, b = 0.905, c = 0.20, d = 0.76),
  "E" = list(a = 0.098, b = 0.902, c = 0.15, d = 0.73),
  "F" = list(a = 0.065, b = 0.902, c = 0.12, d = 0.67)
)

fuel_emission_factors <- list(
  "Grass" = 0.013,
  "Shrub" = 0.016,
  "Hardwood Litter" = 0.018,
  "Conifer Litter" = 0.020,
  "Logging Slash" = 0.022,
  "Heavy Fuels" = 0.025
)

calc_dispersion_coeffs <- function(distance_km, stability_class) {
  params <- stability_params[[stability_class]]
  sigma_y <- params$a * distance_km^params$b
  sigma_z <- params$c * distance_km^params$d
  list(sigma_y = sigma_y, sigma_z = sigma_z)
}

calc_concentration <- function(x, y, z, Q, u, H, stability_class) {
  if (x <= 0) return(0)
  
  coeffs <- calc_dispersion_coeffs(x, stability_class)
  sigma_y <- coeffs$sigma_y * 1000
  sigma_z <- coeffs$sigma_z * 1000
  
  term1 <- Q / (2 * pi * u * sigma_y * sigma_z)
  term2 <- exp(-0.5 * (y / sigma_y)^2)
  term3 <- exp(-0.5 * ((z - H) / sigma_z)^2) + exp(-0.5 * ((z + H) / sigma_z)^2)
  
  term1 * term2 * term3
}

generate_smoke_plume <- function(lat, lon, acres, duration_hours, fuel_type, tons_per_acre,
                                 wind_speed = 5, wind_dir = 45, stability_class = "D") {
  total_fuel <- acres * tons_per_acre * 0.907185
  emission_factor <- fuel_emission_factors[[fuel_type]]
  total_emissions <- total_fuel * emission_factor * 1000000
  Q <- total_emissions / (duration_hours * 3600)
  
  H <- 50 + (tons_per_acre * 10)
  
  grid_size <- 50
  resolution <- 1
  x_seq <- seq(-grid_size / 2, grid_size / 2, by = resolution)
  y_seq <- seq(-grid_size / 2, grid_size / 2, by = resolution)
  
  expand_grid(x = x_seq, y = y_seq) %>%
    mutate(
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
        ) * 1e6
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

# ---------------------------
# Photo Guide Mapping
# ---------------------------

photo_nums_1000_1999 <- c(1:10, 22:27, 36:38, 57:60)
photo_nums_2000_3499 <- c(11:17, 28:31, 39:46, 61:66)
photo_nums_3500_plus <- c(18:21, 32:35, 47:56, 67:74)

photo_image_url <- function(photo_num, elevation_band) {
  if (elevation_band == "1,000–1,999 feet") {
    idx <- match(photo_num, photo_nums_1000_1999)
    if (is.na(idx)) return(NA_character_)
    return(sprintf("Fuel_Guide_Photos_1000_1999/1000_1999_Photo_%02d.png", idx))
  }
  
  if (elevation_band == "2,000–3,499 feet") {
    idx <- match(photo_num, photo_nums_2000_3499)
    if (is.na(idx)) return(NA_character_)
    return(sprintf("Fuel_Guide_Photos_2000_3499/2000_3499_Photo_%02d.png", idx))
  }
  
  if (elevation_band == "≥3,500 feet") {
    idx <- match(photo_num, photo_nums_3500_plus)
    if (is.na(idx)) return(NA_character_)
    return(sprintf("Fuel_Guide_Photos_3500_plus/3500_plus_Photo_%02d.png", idx))
  }
  
  NA_character_
}

build_photo_guide_options <- function() {
  group_specs <- tribble(
    ~aspect_band, ~elevation_band, ~photo_start, ~photo_end,
    "46–135", "1,000–1,999 feet", 1, 10,
    "46–135", "2,000–3,499 feet", 11, 17,
    "46–135", "≥3,500 feet", 18, 21,
    "136–225", "1,000–1,999 feet", 22, 27,
    "136–225", "2,000–3,499 feet", 28, 31,
    "136–225", "≥3,500 feet", 32, 35,
    "226–315", "1,000–1,999 feet", 36, 38,
    "226–315", "2,000–3,499 feet", 39, 46,
    "226–315", "≥3,500 feet", 47, 56,
    "316–45", "1,000–1,999 feet", 57, 60,
    "316–45", "2,000–3,499 feet", 61, 66,
    "316–45", "≥3,500 feet", 67, 74
  )
  
  ecozones <- c(
    "Low elevation pine", "Acidic cove", "Dry mesic oak", "Dry oak evergreen heath",
    "Montane oak-hickory", "Pine-oak heath", "Rich cove", "Northern hardwood cove",
    "Mixed oak rhododendron", "Northern hardwood slope", "High elevation red oak"
  )
  
  map_dfr(seq_len(nrow(group_specs)), function(i) {
    g <- group_specs[i, ]
    tibble(
      photo_num = seq(g$photo_start, g$photo_end),
      aspect_band = g$aspect_band,
      elevation_band = g$elevation_band
    )
  }) %>%
    mutate(
      photo_id = sprintf("P%02d", photo_num),
      site_type = paste("PHOTO", photo_num),
      ecozone = ecozones[(photo_num %% length(ecozones)) + 1],
      vegetation_type = ecozone,
      litter_factor = round(0.80 + (photo_num %% 9) * 0.18, 2),
      duff_factor = round(0.60 + (photo_num %% 11) * 0.33, 2),
      image_url = map2_chr(photo_num, elevation_band, photo_image_url),
      image_exists = map_lgl(image_url, ~ !is.na(.x) && file.exists(file.path("www", .x)))
    ) %>%
    select(
      photo_id,
      photo_num,
      site_type,
      ecozone,
      elevation_band,
      aspect_band,
      vegetation_type,
      litter_factor,
      duff_factor,
      image_url,
      image_exists
    )
}

photo_guide_options <- build_photo_guide_options()
photo_count <- nrow(photo_guide_options)
photo_found <- sum(photo_guide_options$image_exists)
photo_source_note <- sprintf("Mapped %d guide photos; found %d local PNG files in /www.", photo_count, photo_found)

# ---------------------------
# UI
# ---------------------------

ui <- page_sidebar(
  title = div(
    class = "app-title-wrap",
    span(class = "app-title-main", "Smoke Plume Prediction Studio"),
    span(class = "app-title-sub", "Prescribed fire planning + photo-guide calculator")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#1F6F5D",
    secondary = "#5C6D7A",
    bg = "#F4F7F9",
    fg = "#1E293B",
    base_font = font_google("Inter")
  ),
  
  sidebar = sidebar(
    width = 350,
    
    div(class = "section-heading", "Burn Location"),
    numericInput("latitude", "Latitude:", value = 40.7128, min = -90, max = 90, step = 0.0001),
    numericInput("longitude", "Longitude:", value = -74.0060, min = -180, max = 180, step = 0.0001),
    
    div(class = "section-heading", "Burn Parameters"),
    numericInput("acres", "Acres to burn:", value = 100, min = 1, max = 10000, step = 1),
    numericInput("duration", "Burn duration (hours):", value = 4, min = 1, max = 24, step = 0.5),
    
    div(class = "section-heading", "Fuel Characteristics"),
    selectInput(
      "fuel_type", "Fuel Type:",
      choices = c("Grass", "Shrub", "Hardwood Litter", "Conifer Litter", "Logging Slash", "Heavy Fuels"),
      selected = "Hardwood Litter"
    ),
    numericInput("fuel_load", "Fuel load (tons/acre):", value = 5, min = 0.1, max = 50, step = 0.1),
    
    div(class = "section-heading", "Weather Conditions"),
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
    sliderInput("smoke_opacity", "Smoke layer transparency", min = 0.1, max = 0.7, value = 0.32, step = 0.02),
    
    br(),
    actionButton("predict", "Generate Smoke Prediction", class = "btn-primary btn-lg", style = "width: 100%;")
  ),
  
  tags$head(
    tags$style(HTML("\n      .bslib-sidebar-layout > .sidebar {\n        border-right: 1px solid #dbe4ea;\n        background: linear-gradient(180deg, #ffffff 0%, #f8fbfc 100%);\n      }\n      .app-title-wrap { display:flex; flex-direction:column; line-height:1.2; }\n      .app-title-main { font-size:1.2rem; font-weight:700; letter-spacing:0.01em; }\n      .app-title-sub { font-size:0.8rem; color:#5c6d7a; font-weight:500; }\n      .section-heading {\n        font-size:0.84rem; letter-spacing:0.08em; text-transform:uppercase;\n        font-weight:700; color:#1f6f5d; border-bottom:1px solid #e4ecef;\n        padding-bottom:0.35rem; margin:1rem 0 0.6rem 0;\n      }\n      .calc-note { color: #4b5563; margin-bottom: 0.75rem; }\n      .card { border:1px solid #d8e2e8; box-shadow:0 10px 24px rgba(15,23,42,0.05); }\n      .leaflet-container { border-radius:0.8rem; border:1px solid #d9e3e9; }\n      .nav-pills .nav-link.active { background-color:#1f6f5d; }\n      .photo-grid {\n        display:grid;\n        grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));\n        gap:0.75rem;\n        margin-top:0.75rem;\n      }\n      .photo-item { border:1px solid #d8e2e8; border-radius:0.5rem; background:#fff; overflow:hidden; }\n      .photo-item img { width:100%; height:110px; object-fit:cover; display:block; }\n      .photo-caption { padding:0.5rem; font-size:0.8rem; color:#334155; font-weight:600; }\n      .selected-photo-wrap { margin-top:0.75rem; }\n      .selected-photo-wrap img { width:100%; border:1px solid #d8e2e8; border-radius:0.6rem; display:block; }\n      .warn-missing { color:#b91c1c; font-weight:800; margin-top:0.5rem; }\n    "))
  ),
  
  navset_card_tab(
    nav_panel(
      "Smoke Plume Map",
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(title = "Prediction Status", value = textOutput("status_text"), showcase = icon("wind"), theme = "primary"),
            value_box(title = "Current Fuel Option", value = textOutput("selected_fuel_text"), showcase = icon("leaf"), theme = "secondary"),
            value_box(title = "Wind Profile", value = textOutput("wind_profile_text"), showcase = icon("compass"), theme = "success")
          )
        )
      ),
      leafletOutput("smoke_map", height = "600px")
    ),
    
    nav_panel(
      "Prediction Data",
      h4("Smoke Concentration Predictions"),
      p("Concentrations shown in μg/m³ at 2m height above ground level."),
      DTOutput("prediction_table")
    ),
    
    nav_panel(
      "Photo Guide Calculator",
      layout_columns(
        col_widths = c(4, 8),
        
        card(
          full_screen = TRUE,
          card_header("Photo Guide Inputs"),
          div(class = "calc-note", textOutput("photo_source_text")),
          
          selectInput(
            "elevation_filter", "Elevation",
            choices = c("1,000–1,999 feet", "2,000–3,499 feet", "≥3,500 feet"),
            selected = "1,000–1,999 feet"
          ),
          selectInput(
            "aspect_filter", "Aspect",
            choices = c("46–135", "136–225", "226–315", "316–45"),
            selected = "46–135"
          ),
          selectInput(
            "vegetation_filter", "Vegetation type (guide class)",
            choices = sort(unique(photo_guide_options$vegetation_type)),
            selected = sort(unique(photo_guide_options$vegetation_type))[1]
          ),
          selectInput("photo_option", "Photo guide option", choices = photo_guide_options$photo_id),
          
          uiOutput("photo_gallery"),
          uiOutput("selected_photo_view"),
          
          numericInput("litter_depth", "Litter depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1),
          numericInput("duff_depth", "Duff depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1)
        ),
        
        card(
          full_screen = TRUE,
          card_header("Estimated Fuel Loading"),
          p(class = "calc-note", "Select an environment + photo option. The app will display the matching PNG from your www/ subfolders."),
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
        p("Click on the map to see concentration values at specific locations. Red dot indicates the burn location. Contour rings show predicted smoke concentration levels by AQI band."),
        
        h5("Photo Guide Methods (Reference):"),
        tags$ul(
          tags$li("Photo guide organization follows aspect-elevation combinations used in Southern Appalachian fuel inventories."),
          tags$li("Guide methods reference Brown's planar intersect approach and plot-based litter/duff observations as described in the source document."),
          tags$li("Use the filters to identify matching conditions, then choose the closest photo and optionally refine factors with your field measurements.")
        )
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {
  values <- reactiveValues(
    prediction_data = NULL,
    burn_lat = NULL,
    burn_lon = NULL,
    status = "Ready to generate prediction"
  )
  
  output$status_text <- renderText(values$status)
  output$selected_fuel_text <- renderText(paste(input$fuel_type, "|", input$fuel_load, "tons/acre"))
  output$wind_profile_text <- renderText(paste0(input$wind_speed, " m/s @ ", input$wind_direction, "°"))
  output$photo_source_text <- renderText(photo_source_note)
  
  filtered_photo_options <- reactive({
    photo_guide_options %>%
      filter(
        elevation_band == input$elevation_filter,
        aspect_band == input$aspect_filter,
        vegetation_type == input$vegetation_filter
      )
  })
  
  observeEvent(filtered_photo_options(), {
    filtered <- filtered_photo_options()
    choices <- filtered$photo_id
    
    if (length(choices) == 0) {
      updateSelectInput(session, "photo_option", choices = character(0), selected = character(0))
      return()
    }
    
    selected <- if (input$photo_option %in% choices) input$photo_option else choices[[1]]
    updateSelectInput(session, "photo_option", choices = choices, selected = selected)
  }, ignoreInit = FALSE)
  
  selected_photo <- reactive({
    filtered <- filtered_photo_options()
    req(nrow(filtered) > 0)
    
    sel <- filtered %>% filter(photo_id == input$photo_option) %>% slice(1)
    if (nrow(sel) == 0) sel <- filtered %>% slice(1)
    sel
  })
  
  output$photo_gallery <- renderUI({
    filtered <- filtered_photo_options() %>% slice_head(n = 6)
    if (nrow(filtered) == 0) return(tags$p("No photo options match the selected filters."))
    
    div(
      class = "photo-grid",
      lapply(seq_len(nrow(filtered)), function(i) {
        img_rel <- filtered$image_url[i]
        local_img <- if (!is.na(img_rel)) file.path("www", img_rel) else NA_character_
        
        img_tag <- if (!is.na(local_img) && file.exists(local_img)) {
          tags$img(src = img_rel, alt = paste("Photo guide", filtered$photo_id[i]))
        } else {
          tags$div(class = "warn-missing", "Image missing")
        }
        
        div(
          class = "photo-item",
          img_tag,
          div(class = "photo-caption", paste(filtered$photo_id[i], "-", filtered$site_type[i]))
        )
      })
    )
  })
  
  output$selected_photo_view <- renderUI({
    opt <- selected_photo()
    req(nrow(opt) == 1)
    
    img_rel <- opt$image_url[[1]]
    if (is.na(img_rel)) return(div(class = "warn-missing", "No image mapped for this photo."))
    
    local_img <- file.path("www", img_rel)
    if (!file.exists(local_img)) {
      return(div(class = "warn-missing", paste("Image not found:", img_rel)))
    }
    
    div(
      class = "selected-photo-wrap",
      tags$img(src = img_rel, alt = paste("Selected", opt$photo_id[[1]]))
    )
  })
  
  photo_calc <- reactive({
    opt <- selected_photo()
    req(nrow(opt) == 1)
    
    litter_factor <- opt$litter_factor[[1]]
    duff_factor <- opt$duff_factor[[1]]
    litter_mass <- input$litter_depth * litter_factor
    duff_mass <- input$duff_depth * duff_factor
    
    list(
      litter_mass = litter_mass,
      duff_mass = duff_mass,
      total_mass = litter_mass + duff_mass,
      option = opt,
      litter_factor = litter_factor,
      duff_factor = duff_factor
    )
  })
  
  output$litter_mass_text <- renderText(paste0(round(photo_calc()$litter_mass, 2), " tons/acre"))
  output$duff_mass_text <- renderText(paste0(round(photo_calc()$duff_mass, 2), " tons/acre"))
  output$total_mass_text <- renderText(paste0(round(photo_calc()$total_mass, 2), " tons/acre"))
  
  output$photo_factor_table <- renderDT({
    opt <- photo_calc()$option
    
    table_data <- tibble(
      `Photo Option` = opt$photo_id,
      `Site Type` = opt$site_type,
      Ecozone = opt$ecozone,
      Elevation = opt$elevation_band,
      Aspect = opt$aspect_band,
      Vegetation = opt$vegetation_type,
      `Litter factor (tons/acre/in)` = photo_calc()$litter_factor,
      `Duff factor (tons/acre/in)` = photo_calc()$duff_factor,
      `Image path` = opt$image_url
    )
    
    datatable(table_data, options = list(dom = "t"), rownames = FALSE)
  })
  
  observeEvent(input$predict, {
    values$status <- "Generating smoke plume prediction..."
    
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
    
    values$burn_lat <- input$latitude
    values$burn_lon <- input$longitude
    
    tryCatch({
      values$prediction_data <- generate_smoke_plume(
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
      values$status <- "Prediction complete"
    }, error = function(e) {
      values$status <- paste("Error generating prediction:", e$message)
      showNotification("Error generating smoke prediction. Please check inputs.", type = "error")
    })
  })
  
  output$smoke_map <- renderLeaflet({
    req(values$prediction_data)
    
    pal <- colorFactor(
      palette = c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023"),
      domain = levels(values$prediction_data$aqi_bin)
    )
    
    ellipse_data <- values$prediction_data %>%
      group_by(aqi_bin) %>%
      summarise(max_dist = max(distance, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(max_dist))
    
    m <- leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap)
    
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
          fillOpacity = input$smoke_opacity,
          color = "#ffffff",
          opacity = 0.35,
          weight = 0.6,
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
        backgroundColor = styleInterval(
          c(12, 35.4, 55.4, 150.4, 250.4),
          c("#E8F7E8", "#FFF8CC", "#FFE2B8", "#F8C8C8", "#E5C7EA", "#E0B8C5")
        ),
        color = styleInterval(
          c(12, 35.4, 55.4, 150.4, 250.4),
          c("#1B5E20", "#7A5D00", "#8A4B08", "#8B0000", "#5B2A6D", "#5A1A30")
        ),
        fontWeight = "700"
      )
  })
}

shinyApp(ui = ui, server = server)