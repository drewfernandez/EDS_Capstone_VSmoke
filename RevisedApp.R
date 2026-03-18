# Smoke Plume Prediction Studio + Photo Guide Calculator (FULL APP)
# VSMOKE-CLOSER VERSION
# ------------------------------------------------------------
# Major changes:
# - duration treated as active flaming duration estimate
# - added fuel consumption fraction (% consumed)
# - added ignition method (Backing/Spot vs Head/Aerial)
# - replaced custom residual controls with VSMOKE-style plume rise fraction
# - negative plume rise fraction curtains smoke from ground to plume-top
# - reduced tendency to keep Hazardous / Very Unhealthy too far downwind
# - uses raw concentration for AQI mapping (no broad severity smoothing)
# - finer grid and small isolated-cell cleanup
#
# install.packages(c("shiny","tidyverse","DT","bslib","leaflet","htmltools"))

library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(htmltools)

# ---------------------------
# Approximate VSMOKE-style dispersion parameters
# ---------------------------

vsmoke_dispersion_params <- list(
  "A" = list(Ay = 0.32, By = 0.040, Cz = 0.20, Dz = 0.90),
  "B" = list(Ay = 0.28, By = 0.036, Cz = 0.16, Dz = 0.88),
  "C" = list(Ay = 0.24, By = 0.032, Cz = 0.12, Dz = 0.86),
  "D" = list(Ay = 0.20, By = 0.028, Cz = 0.08, Dz = 0.82),
  "E" = list(Ay = 0.16, By = 0.024, Cz = 0.06, Dz = 0.78),
  "F" = list(Ay = 0.12, By = 0.020, Cz = 0.04, Dz = 0.74)
)

# Approximate PM2.5 emission factors in tons PM per ton fuel consumed
fuel_emission_factors <- list(
  "Grass" = 0.013,
  "Shrub" = 0.016,
  "Hardwood Litter" = 0.014,
  "Conifer Litter" = 0.016,
  "Logging Slash" = 0.020,
  "Heavy Fuels" = 0.022
)

# ---------------------------
# Wind direction helper
# Direction = where smoke travels TOWARD
# 0 = East, 45 = Northeast, 90 = North, etc.
# ---------------------------

wind_dir_to_degrees <- function(direction_label) {
  switch(
    direction_label,
    "East" = 0,
    "Northeast" = 45,
    "North" = 90,
    "Northwest" = 135,
    "West" = 180,
    "Southwest" = 225,
    "South" = 270,
    "Southeast" = 315,
    45
  )
}

# ---------------------------
# Plume model helpers
# ---------------------------

calc_buoyancy_flux <- function(heat_release_mw) {
  8.8 * max(heat_release_mw, 0)
}

calc_dispersion_coeffs_vsmoke <- function(x_km, stability_class,
                                          x_virtual_y = 0.10,
                                          x_virtual_z = 0.05,
                                          sigma_y_min = 14,
                                          sigma_z_min = 6) {
  p <- vsmoke_dispersion_params[[stability_class]]
  
  Xvy <- max(x_km + x_virtual_y, 0.001)
  Xvz <- max(x_km + x_virtual_z, 0.001)
  
  sigma_y <- 465.116 * Xvy * tan(p$Ay + p$By * log(Xvy + 1))
  sigma_z <- p$Cz * (Xvz ^ p$Dz) * 1000
  
  sigma_y <- max(sigma_y, sigma_y_min)
  sigma_z <- max(sigma_z, sigma_z_min)
  
  list(sigma_y = sigma_y, sigma_z = sigma_z)
}

calc_plume_rise_vsmoke <- function(x_km, F, u, stability_class,
                                   plume_base = 15,
                                   mixing_height = 1200,
                                   ignition_method = "Backing/Spot") {
  if (x_km <= 0) return(plume_base)
  
  x_m <- x_km * 1000
  u <- max(u, 0.5)
  F <- max(F, 0.01)
  
  heat_mult <- if (ignition_method == "Head/Aerial") 1.20 else 0.85
  
  delta_h_raw <- if (stability_class %in% c("E", "F")) {
    1.2 * (F^(1/3)) * (x_m^(2/3)) / u
  } else {
    1.8 * (F^(1/3)) * (x_m^(2/3)) / u
  }
  
  delta_h <- heat_mult * 0.28 * delta_h_raw
  plume_cap <- max(plume_base + 25, 0.65 * mixing_height)
  
  plume_base + min(delta_h, plume_cap - plume_base)
}

calc_concentration_vsmoke <- function(x_km, y_m, z_m, Q, u, H_eff,
                                      stability_class,
                                      x_virtual_y = 0.10,
                                      x_virtual_z = 0.05,
                                      mixing_height = 1200,
                                      background = 0,
                                      sigma_y_min = 14,
                                      sigma_z_min = 6) {
  if (x_km <= 0 || Q <= 0) return(0)
  
  coeffs <- calc_dispersion_coeffs_vsmoke(
    x_km = x_km,
    stability_class = stability_class,
    x_virtual_y = x_virtual_y,
    x_virtual_z = x_virtual_z,
    sigma_y_min = sigma_y_min,
    sigma_z_min = sigma_z_min
  )
  
  sigma_y <- coeffs$sigma_y
  sigma_z <- coeffs$sigma_z
  u <- max(u, 0.5)
  
  term1 <- Q / (2 * pi * u * sigma_y * sigma_z)
  term2 <- exp(-0.5 * (y_m / sigma_y)^2)
  
  if (is.infinite(mixing_height) || mixing_height <= 0) {
    term3 <- exp(-0.5 * ((z_m - H_eff) / sigma_z)^2) +
      exp(-0.5 * ((z_m + H_eff) / sigma_z)^2)
    return(background + term1 * term2 * term3)
  }
  
  M <- 3
  refl_sum <- 0
  for (m in (-M):M) {
    refl_sum <- refl_sum +
      exp(-0.5 * ((z_m - H_eff + 2 * m * mixing_height) / sigma_z)^2) +
      exp(-0.5 * ((z_m + H_eff + 2 * m * mixing_height) / sigma_z)^2)
  }
  
  background + term1 * term2 * refl_sum
}

# VSMOKE-style plume rise fraction:
# + value: lifted portion starts at plume-top
# - value: lifted portion curtains from ground to plume-top
# 0: all from ground
calc_split_plume_conc_vsmoke <- function(x_km, y_m, z_m, Q, u, H_lofted,
                                         stability_class,
                                         plume_rise_fraction = -0.5,
                                         mixing_height = 1200,
                                         curtain_layers = 7,
                                         ground_release_height = 3,
                                         sigma_y_min = 14,
                                         sigma_z_min = 6) {
  if (Q <= 0 || x_km <= 0) return(0)
  
  prf <- max(min(plume_rise_fraction, 1), -1)
  
  if (prf >= 0) {
    loft_frac <- prf
    ground_frac <- 1 - prf
    
    c_ground <- if (ground_frac > 0) {
      calc_concentration_vsmoke(
        x_km, y_m, z_m, Q * ground_frac, u,
        H_eff = ground_release_height,
        stability_class = stability_class,
        mixing_height = mixing_height,
        sigma_y_min = sigma_y_min,
        sigma_z_min = sigma_z_min
      )
    } else 0
    
    c_loft <- if (loft_frac > 0) {
      calc_concentration_vsmoke(
        x_km, y_m, z_m, Q * loft_frac, u,
        H_eff = H_lofted,
        stability_class = stability_class,
        mixing_height = mixing_height,
        sigma_y_min = sigma_y_min,
        sigma_z_min = sigma_z_min
      )
    } else 0
    
    return(c_ground + c_loft)
  }
  
  lift_frac <- abs(prf)
  ground_frac <- 1 - lift_frac
  
  c_ground <- if (ground_frac > 0) {
    calc_concentration_vsmoke(
      x_km, y_m, z_m, Q * ground_frac, u,
      H_eff = ground_release_height,
      stability_class = stability_class,
      mixing_height = mixing_height,
      sigma_y_min = sigma_y_min,
      sigma_z_min = sigma_z_min
    )
  } else 0
  
  if (lift_frac <= 0) return(c_ground)
  
  layer_heights <- seq(ground_release_height, H_lofted, length.out = curtain_layers)
  layer_vals <- sapply(layer_heights, function(h) {
    calc_concentration_vsmoke(
      x_km, y_m, z_m, (Q * lift_frac) / curtain_layers, u,
      H_eff = h,
      stability_class = stability_class,
      mixing_height = mixing_height,
      sigma_y_min = sigma_y_min,
      sigma_z_min = sigma_z_min
    )
  })
  
  c_ground + sum(layer_vals)
}

reclass_aqi <- function(x) {
  cut(
    x,
    breaks = c(0, 12, 35.4, 55.4, 150.4, 250.4, Inf),
    labels = c(
      "0–50 (Good)",
      "51–100 (Moderate)",
      "101–150 (USG)",
      "151–200 (Unhealthy)",
      "201–300 (Very Unhealthy)",
      "301+ (Hazardous)"
    ),
    include.lowest = TRUE,
    right = TRUE
  )
}

count_neighbors <- function(df, threshold_col = "concentration", x_col = "x", y_col = "y", thresh = 12.1) {
  x_vals <- sort(unique(df[[x_col]]))
  y_vals <- sort(unique(df[[y_col]]))
  
  grid <- df %>%
    select(all_of(c(x_col, y_col, threshold_col))) %>%
    mutate(
      x_idx = match(.data[[x_col]], x_vals),
      y_idx = match(.data[[y_col]], y_vals)
    )
  
  mat <- matrix(FALSE, nrow = length(y_vals), ncol = length(x_vals))
  mat[cbind(grid$y_idx, grid$x_idx)] <- grid[[threshold_col]] >= thresh
  
  nbr_mat <- matrix(0L, nrow = nrow(mat), ncol = ncol(mat))
  
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      i1 <- max(1, i - 1)
      i2 <- min(nrow(mat), i + 1)
      j1 <- max(1, j - 1)
      j2 <- min(ncol(mat), j + 1)
      
      window <- mat[i1:i2, j1:j2, drop = FALSE]
      nbr_mat[i, j] <- sum(window) - mat[i, j]
    }
  }
  
  df %>%
    mutate(
      x_idx = match(.data[[x_col]], x_vals),
      y_idx = match(.data[[y_col]], y_vals),
      neighbor_count = nbr_mat[cbind(y_idx, x_idx)]
    ) %>%
    select(-x_idx, -y_idx)
}

# ---------------------------
# Main plume generator
# ---------------------------

generate_smoke_plume <- function(lat, lon, acres, duration_hours, fuel_type, tons_per_acre,
                                 wind_speed = 5, wind_dir = 45, stability_class = "D",
                                 mixing_height = 1200,
                                 plume_base = 15,
                                 consumed_fraction = 0.30,
                                 convective_fraction = 0.35,
                                 heat_content_kj_kg = 18000,
                                 grid_size_km = 50,
                                 resolution_km = 0.20,
                                 ignition_method = "Backing/Spot",
                                 plume_rise_fraction = -0.5,
                                 flaming_fraction = 0.65,
                                 flaming_duration_fraction = 0.30,
                                 background_pm25 = 5) {
  
  validate(
    need(acres > 0, "Acres must be greater than 0."),
    need(duration_hours > 0, "Duration must be greater than 0."),
    need(tons_per_acre > 0, "Fuel load must be greater than 0."),
    need(wind_speed > 0, "Wind speed must be greater than 0."),
    need(!is.null(fuel_emission_factors[[fuel_type]]), "Invalid fuel type.")
  )
  
  consumed_fraction <- min(max(consumed_fraction, 0.05), 1.00)
  flaming_fraction <- min(max(flaming_fraction, 0.10), 0.90)
  flaming_duration_fraction <- min(max(flaming_duration_fraction, 0.10), 0.90)
  
  total_fuel_tonnes_available <- acres * tons_per_acre * 0.907185
  total_fuel_tonnes_consumed <- total_fuel_tonnes_available * consumed_fraction
  
  emission_factor <- fuel_emission_factors[[fuel_type]]
  total_pm_tonnes <- total_fuel_tonnes_consumed * emission_factor
  total_pm_g <- total_pm_tonnes * 1e6
  
  # Periods closer to VSMOKE thinking:
  # duration = active flaming duration estimate
  # acres are effectively burned across that active period
  flaming_hours <- max(duration_hours * flaming_duration_fraction, 0.1)
  smolder_hours <- max(duration_hours - flaming_hours, 0.1)
  
  pm_flaming_g <- total_pm_g * flaming_fraction
  pm_smolder_g <- total_pm_g * (1 - flaming_fraction)
  
  Q_flaming <- pm_flaming_g / (flaming_hours * 3600)
  Q_smolder <- pm_smolder_g / (smolder_hours * 3600)
  
  fuel_flaming_kg_s <- (total_fuel_tonnes_consumed * 1000 * flaming_fraction) / (flaming_hours * 3600)
  
  heat_mult <- if (ignition_method == "Head/Aerial") 1.25 else 0.85
  heat_release_mw <- fuel_flaming_kg_s * heat_content_kj_kg * convective_fraction * heat_mult / 1000
  F <- calc_buoyancy_flux(heat_release_mw)
  
  # Smolder period less lofted than flaming
  plume_rise_fraction_smolder <- if (plume_rise_fraction < 0) {
    plume_rise_fraction * 0.35
  } else {
    plume_rise_fraction * 0.20
  }
  
  x_seq <- seq(-grid_size_km / 2, grid_size_km / 2, by = resolution_km)
  y_seq <- seq(-grid_size_km / 2, grid_size_km / 2, by = resolution_km)
  
  expand_grid(x = x_seq, y = y_seq) %>%
    mutate(
      x_rot = x * cos(wind_dir * pi / 180) + y * sin(wind_dir * pi / 180),
      y_rot = -x * sin(wind_dir * pi / 180) + y * cos(wind_dir * pi / 180)
    ) %>%
    rowwise() %>%
    mutate(
      H_lofted = if (x_rot > 0) {
        calc_plume_rise_vsmoke(
          x_km = x_rot,
          F = F,
          u = wind_speed,
          stability_class = stability_class,
          plume_base = plume_base,
          mixing_height = mixing_height,
          ignition_method = ignition_method
        )
      } else {
        plume_base
      },
      
      concentration_flaming = if (x_rot > 0) {
        calc_split_plume_conc_vsmoke(
          x_km = x_rot,
          y_m = y_rot * 1000,
          z_m = 2,
          Q = Q_flaming,
          u = wind_speed,
          H_lofted = H_lofted,
          stability_class = stability_class,
          plume_rise_fraction = plume_rise_fraction,
          mixing_height = mixing_height,
          curtain_layers = 7,
          ground_release_height = 3,
          sigma_y_min = 14,
          sigma_z_min = 6
        ) * 1e6
      } else {
        0
      },
      
      concentration_smolder = if (x_rot > 0) {
        calc_split_plume_conc_vsmoke(
          x_km = x_rot,
          y_m = y_rot * 1000,
          z_m = 2,
          Q = Q_smolder,
          u = max(wind_speed * 0.85, 0.5),
          H_lofted = max(H_lofted * 0.45, plume_base),
          stability_class = stability_class,
          plume_rise_fraction = plume_rise_fraction_smolder,
          mixing_height = mixing_height,
          curtain_layers = 5,
          ground_release_height = 2,
          sigma_y_min = 18,
          sigma_z_min = 8
        ) * 1e6
      } else {
        0
      }
    ) %>%
    ungroup() %>%
    mutate(
      concentration = concentration_flaming + concentration_smolder + background_pm25,
      distance = sqrt(x_rot^2 + y_rot^2),
      lat_grid = lat + (y / 111.32),
      lon_grid = lon + (x / (111.32 * cos(lat * pi / 180))),
      resolution_km = resolution_km,
      lat_half = (resolution_km / 2) / 111.32,
      lon_half = (resolution_km / 2) / (111.32 * cos(lat * pi / 180)),
      lat_min = lat_grid - lat_half,
      lat_max = lat_grid + lat_half,
      lon_min = lon_grid - lon_half,
      lon_max = lon_grid + lon_half,
      H_eff = H_lofted,
      aqi_bin = reclass_aqi(concentration),
      aqi_bin = factor(
        aqi_bin,
        levels = c(
          "0–50 (Good)",
          "51–100 (Moderate)",
          "101–150 (USG)",
          "151–200 (Unhealthy)",
          "201–300 (Very Unhealthy)",
          "301+ (Hazardous)"
        ),
        ordered = TRUE
      )
    ) %>%
    filter(!is.na(aqi_bin)) %>%
    count_neighbors(threshold_col = "concentration", x_col = "x", y_col = "y", thresh = 12.1)
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
      duff_factor   = round(0.60 + (photo_num %% 11) * 0.33, 2),
      image_url = map2_chr(photo_num, elevation_band, photo_image_url),
      image_exists = map_lgl(image_url, ~ !is.na(.x) && file.exists(file.path("www", .x)))
    ) %>%
    select(
      photo_id, photo_num, site_type, ecozone,
      elevation_band, aspect_band, vegetation_type,
      litter_factor, duff_factor,
      image_url, image_exists
    )
}

photo_guide_options <- build_photo_guide_options()
photo_source_note <- sprintf(
  "Mapped %d guide photos; found %d local PNG files in /www.",
  nrow(photo_guide_options),
  sum(photo_guide_options$image_exists)
)

# ---------------------------
# UI
# ---------------------------

ui <- page_sidebar(
  title = div(
    class = "app-title-wrap",
    span(class = "app-title-main", "Smoke Plume Prediction Studio"),
    span(class = "app-title-sub", "VSMOKE-closer prescribed fire screening view")
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
    numericInput("duration", "Duration of active flaming fire (hours):", value = 4, min = 0.5, max = 24, step = 0.5),
    selectInput(
      "ignition_method", "Ignition method:",
      choices = c("Backing/Spot", "Head/Aerial"),
      selected = "Backing/Spot"
    ),
    
    div(class = "section-heading", "Fuel Characteristics"),
    selectInput(
      "fuel_type", "Fuel Type:",
      choices = c("Grass", "Shrub", "Hardwood Litter", "Conifer Litter", "Logging Slash", "Heavy Fuels"),
      selected = "Hardwood Litter"
    ),
    numericInput("fuel_load", "Fuel load (tons/acre):", value = 5, min = 0.1, max = 50, step = 0.1),
    sliderInput("consumed_fraction", "Fuel consumed fraction", min = 0.05, max = 1.00, value = 0.30, step = 0.05),
    
    div(class = "section-heading", "Weather Conditions"),
    numericInput("wind_speed", "Transport wind speed (m/s):", value = 5, min = 0.5, max = 20, step = 0.5),
    selectInput(
      "wind_direction",
      "Wind direction smoke travels toward:",
      choices = c("North", "Northeast", "East", "Southeast", "South", "Southwest", "West", "Northwest"),
      selected = "Northeast"
    ),
    selectInput(
      "stability", "Atmospheric stability:",
      choices = c(
        "Very Unstable (A)" = "A", "Moderately Unstable (B)" = "B",
        "Slightly Unstable (C)" = "C", "Neutral (D)" = "D",
        "Slightly Stable (E)" = "E", "Moderately Stable (F)" = "F"
      ),
      selected = "D"
    ),
    numericInput("mixing_height", "Mixing height (m):", value = 1200, min = 100, max = 5000, step = 50),
    numericInput("plume_base", "Initial plume base (m):", value = 15, min = 0, max = 200, step = 1),
    sliderInput("convective_fraction", "Convective heat fraction", min = 0.10, max = 0.60, value = 0.35, step = 0.05),
    numericInput("background_pm25", "Background PM2.5 (µg/m³):", value = 5, min = 0, max = 100, step = 1),
    
    div(class = "section-heading", "VSMOKE-Style Controls"),
    sliderInput("plume_rise_fraction", "Plume rise fraction (-1 to 1)", min = -1.0, max = 1.0, value = -0.5, step = 0.05),
    sliderInput("flaming_fraction", "Flaming emission fraction", min = 0.30, max = 0.90, value = 0.65, step = 0.05),
    sliderInput("flaming_duration_fraction", "Fraction of burn duration dominated by flaming", min = 0.10, max = 0.80, value = 0.30, step = 0.05),
    sliderInput("smoke_opacity", "Smoke layer transparency", min = 0.10, max = 0.70, value = 0.30, step = 0.02),
    checkboxInput("show_good", "Show low-concentration 'Good' smoke on map", value = FALSE),
    
    br(),
    actionButton("predict", "Generate Smoke Prediction", class = "btn-primary btn-lg", style = "width: 100%;")
  ),
  
  tags$head(
    tags$style(HTML("
      .bslib-sidebar-layout > .sidebar {
        border-right: 1px solid #dbe4ea;
        background: linear-gradient(180deg, #ffffff 0%, #f8fbfc 100%);
      }
      .app-title-wrap { display:flex; flex-direction:column; line-height:1.2; }
      .app-title-main { font-size:1.2rem; font-weight:700; letter-spacing:0.01em; }
      .app-title-sub { font-size:0.8rem; color:#5c6d7a; font-weight:500; }
      .section-heading {
        font-size:0.84rem; letter-spacing:0.08em; text-transform:uppercase;
        font-weight:700; color:#1f6f5d; border-bottom:1px solid #e4ecef;
        padding-bottom:0.35rem; margin:1rem 0 0.6rem 0;
      }
      .calc-note { color: #4b5563; margin-bottom: 0.75rem; }
      .card { border:1px solid #d8e2e8; box-shadow:0 10px 24px rgba(15,23,42,0.05); }
      .leaflet-container { border-radius:0.8rem; border:1px solid #d9e3e9; }
      .nav-pills .nav-link.active { background-color:#1f6f5d; }

      .value-box .value-box-value,
      .value-box .value-box-title {
        word-break: break-word;
        overflow-wrap: anywhere;
      }
      .value-box .value-box-title {
        font-size: 0.9rem;
        line-height: 1.15;
      }
      .value-box .value-box-value {
        font-size: clamp(1rem, 1.45vw, 1.75rem);
        line-height: 1.15;
      }
      .value-box { min-height: 165px; }

      .photo-grid {
        display:grid;
        grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
        gap:0.75rem;
        margin-top:0.75rem;
      }
      .photo-item { border:1px solid #d8e2e8; border-radius:0.5rem; background:#fff; overflow:hidden; }
      .photo-item img { width:100%; height:110px; object-fit:cover; display:block; }
      .photo-caption { padding:0.5rem; font-size:0.8rem; color:#334155; font-weight:600; }
      .selected-photo-wrap { margin-top:0.75rem; }
      .selected-photo-wrap img { width:100%; border:1px solid #d8e2e8; border-radius:0.6rem; display:block; }
      .warn-missing { color:#b91c1c; font-weight:800; margin-top:0.5rem; }
    "))
  ),
  
  navset_card_tab(
    nav_panel(
      "Smoke Plume Map",
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            value_box(
              title = "Prediction Status",
              value = textOutput("status_text"),
              showcase = icon("wind"),
              theme = "primary"
            ),
            value_box(
              title = "Current Fuel Option",
              value = textOutput("selected_fuel_text"),
              showcase = icon("leaf"),
              theme = "secondary"
            ),
            value_box(
              title = "Wind Profile",
              value = textOutput("wind_profile_text"),
              showcase = icon("compass"),
              theme = "success"
            )
          )
        )
      ),
      leafletOutput("smoke_map", height = "600px")
    ),
    
    nav_panel(
      "Prediction Data",
      h4("Smoke Concentration Predictions"),
      p("Concentrations shown in µg/m³ at 2 m above ground level."),
      p("This version is closer to a VSMOKE-style screening view, but still not the original FORTRAN model."),
      DTOutput("prediction_table")
    ),
    
    nav_panel(
      "Diff/Litter Calculator",
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
      h4("Model Information"),
      tags$div(
        tags$h5("What changed:"),
        tags$ul(
          tags$li("Duration is treated as active flaming duration estimate."),
          tags$li("Fuel consumed fraction is included."),
          tags$li("Plume rise fraction follows VSMOKE-style semantics."),
          tags$li("Negative plume rise fraction curtains smoke upward, which is typical for prescribed burns."),
          tags$li("AQI mapping uses raw concentration instead of broad smoothing.")
        ),
        tags$h5("Important note:"),
        tags$p("This is a closer screening depiction, not a regulatory or exact VSMOKE clone.")
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
  
  observe({
    if (input$ignition_method == "Backing/Spot" && isTRUE(all.equal(input$plume_rise_fraction, 0.75))) {
      updateSliderInput(session, "plume_rise_fraction", value = -0.5)
    }
  })
  
  output$status_text <- renderText(values$status)
  output$selected_fuel_text <- renderText(
    paste(input$fuel_type, "|", input$fuel_load, "tons/acre |", round(100 * input$consumed_fraction), "% consumed")
  )
  output$wind_profile_text <- renderText(
    paste0(
      input$wind_speed, " m/s toward ", input$wind_direction,
      " | MH: ", input$mixing_height, " m",
      " | PRF: ", input$plume_rise_fraction
    )
  )
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
  output$duff_mass_text   <- renderText(paste0(round(photo_calc()$duff_mass, 2), " tons/acre"))
  output$total_mass_text  <- renderText(paste0(round(photo_calc()$total_mass, 2), " tons/acre"))
  
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
      input$latitude, input$longitude,
      input$acres, input$duration,
      input$fuel_type, input$fuel_load,
      input$consumed_fraction,
      input$wind_speed, input$wind_direction, input$stability,
      input$mixing_height, input$plume_base, input$convective_fraction,
      input$ignition_method, input$plume_rise_fraction,
      input$flaming_fraction, input$flaming_duration_fraction,
      input$background_pm25
    )
    
    values$burn_lat <- input$latitude
    values$burn_lon <- input$longitude
    
    wind_dir_deg <- wind_dir_to_degrees(input$wind_direction)
    
    tryCatch({
      values$prediction_data <- generate_smoke_plume(
        lat = input$latitude,
        lon = input$longitude,
        acres = input$acres,
        duration_hours = input$duration,
        fuel_type = input$fuel_type,
        tons_per_acre = input$fuel_load,
        wind_speed = input$wind_speed,
        wind_dir = wind_dir_deg,
        stability_class = input$stability,
        mixing_height = input$mixing_height,
        plume_base = input$plume_base,
        consumed_fraction = input$consumed_fraction,
        convective_fraction = input$convective_fraction,
        ignition_method = input$ignition_method,
        plume_rise_fraction = input$plume_rise_fraction,
        flaming_fraction = input$flaming_fraction,
        flaming_duration_fraction = input$flaming_duration_fraction,
        background_pm25 = input$background_pm25,
        resolution_km = 0.20
      )
      values$status <- "Prediction complete"
    }, error = function(e) {
      values$status <- paste("Error generating prediction:", e$message)
      showNotification("Error generating smoke prediction. Please check inputs.", type = "error")
    })
  })
  
  output$smoke_map <- renderLeaflet({
    req(values$prediction_data)
    
    df <- values$prediction_data
    
    aqi_levels <- c(
      "0–50 (Good)",
      "51–100 (Moderate)",
      "101–150 (USG)",
      "151–200 (Unhealthy)",
      "201–300 (Very Unhealthy)",
      "301+ (Hazardous)"
    )
    
    aqi_colors_all <- c(
      "#00E400",
      "#FFFF00",
      "#FF7E00",
      "#FF0000",
      "#8F3F97",
      "#7E0023"
    )
    names(aqi_colors_all) <- aqi_levels
    
    visible_levels <- if (isTRUE(input$show_good)) aqi_levels else aqi_levels[-1]
    threshold_min <- if (isTRUE(input$show_good)) 0.01 else 12.1
    
    pal <- colorFactor(
      palette = unname(aqi_colors_all),
      domain = aqi_levels,
      ordered = TRUE
    )
    
    map_df <- df %>%
      filter(
        x_rot > 0,
        concentration >= threshold_min,
        !is.na(aqi_bin)
      ) %>%
      mutate(
        keep_cell = case_when(
          concentration < 12.1 ~ TRUE,
          neighbor_count >= 2 ~ TRUE,
          x_rot <= 0.8 ~ TRUE,
          TRUE ~ FALSE
        )
      ) %>%
      filter(keep_cell) %>%
      mutate(
        aqi_bin = factor(aqi_bin, levels = aqi_levels, ordered = TRUE)
      )
    
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap)
    
    if (nrow(map_df) > 0) {
      for (i in seq_len(nrow(map_df))) {
        m <- m %>%
          addRectangles(
            lng1 = map_df$lon_min[i],
            lat1 = map_df$lat_min[i],
            lng2 = map_df$lon_max[i],
            lat2 = map_df$lat_max[i],
            fillColor = pal(as.character(map_df$aqi_bin[i])),
            fillOpacity = input$smoke_opacity,
            color = NA,
            stroke = FALSE,
            popup = HTML(paste0(
              "<b>AQI Category:</b> ", map_df$aqi_bin[i], "<br/>",
              "<b>Concentration:</b> ", round(map_df$concentration[i], 2), " µg/m³<br/>",
              "<b>Flaming contribution:</b> ", round(map_df$concentration_flaming[i], 2), " µg/m³<br/>",
              "<b>Smolder contribution:</b> ", round(map_df$concentration_smolder[i], 2), " µg/m³<br/>",
              "<b>Downwind distance:</b> ", round(map_df$x_rot[i], 2), " km<br/>",
              "<b>Crosswind offset:</b> ", round(map_df$y_rot[i], 2), " km<br/>",
              "<b>Plume top:</b> ", round(map_df$H_eff[i], 1), " m"
            ))
          )
      }
    }
    
    burn_radius_m <- sqrt(input$acres * 4046.86 / pi)
    
    m <- m %>%
      addCircleMarkers(
        lng = values$burn_lon,
        lat = values$burn_lat,
        radius = 8,
        color = "red",
        fillOpacity = 1,
        popup = "Burn location"
      ) %>%
      addCircles(
        lng = values$burn_lon,
        lat = values$burn_lat,
        radius = burn_radius_m,
        color = "orange",
        weight = 2,
        fill = FALSE,
        popup = "Burn Area"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = unname(aqi_colors_all[visible_levels]),
        labels = visible_levels,
        title = "Air Quality Index (PM2.5)",
        opacity = 0.9
      )
    
    if (nrow(map_df) > 0) {
      m <- m %>%
        fitBounds(
          lng1 = min(c(values$burn_lon, map_df$lon_min), na.rm = TRUE),
          lat1 = min(c(values$burn_lat, map_df$lat_min), na.rm = TRUE),
          lng2 = max(c(values$burn_lon, map_df$lon_max), na.rm = TRUE),
          lat2 = max(c(values$burn_lat, map_df$lat_max), na.rm = TRUE)
        )
    } else {
      m <- m %>%
        setView(lng = values$burn_lon, lat = values$burn_lat, zoom = 10)
    }
    
    m
  })
  
  output$prediction_table <- renderDT({
    req(values$prediction_data)
    
    table_data <- values$prediction_data %>%
      filter(x_rot > 0) %>%
      select(
        lat_grid, lon_grid,
        concentration, concentration_flaming, concentration_smolder,
        distance, x_rot, y_rot, H_eff, aqi_bin
      ) %>%
      rename(
        "Latitude" = lat_grid,
        "Longitude" = lon_grid,
        "Concentration (µg/m³)" = concentration,
        "Flaming Contribution (µg/m³)" = concentration_flaming,
        "Smolder Contribution (µg/m³)" = concentration_smolder,
        "Distance from source (km)" = distance,
        "Downwind distance (km)" = x_rot,
        "Crosswind offset (km)" = y_rot,
        "Plume Top (m)" = H_eff,
        "AQI Band" = aqi_bin
      ) %>%
      arrange(desc(`Concentration (µg/m³)`)) %>%
      mutate(
        Latitude = round(Latitude, 6),
        Longitude = round(Longitude, 6),
        `Concentration (µg/m³)` = round(`Concentration (µg/m³)`, 2),
        `Flaming Contribution (µg/m³)` = round(`Flaming Contribution (µg/m³)`, 2),
        `Smolder Contribution (µg/m³)` = round(`Smolder Contribution (µg/m³)`, 2),
        `Distance from source (km)` = round(`Distance from source (km)`, 2),
        `Downwind distance (km)` = round(`Downwind distance (km)`, 2),
        `Crosswind offset (km)` = round(`Crosswind offset (km)`, 2),
        `Plume Top (m)` = round(`Plume Top (m)`, 1)
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
        "Concentration (µg/m³)",
        color = styleInterval(
          c(12, 35.4, 55.4, 150.4, 250.4),
          c("#1B5E20", "#7A5D00", "#8A4B08", "#8B0000", "#5B2A6D", "#5A1A30")
        ),
        fontWeight = "700"
      )
  })
}

shinyApp(ui = ui, server = server)