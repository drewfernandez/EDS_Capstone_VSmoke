library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(htmltools)
library(httr2)
library(jsonlite)
library(base64enc)
library(rsconnect)


# ---------------------------
# OpenAI / image match helpers
# ---------------------------

Sys.setenv(OPENAI_API_KEY = "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")

.assert_shiny_fix_dependencies <- function() {
  pkgs <- c("magick", "dplyr", "tibble", "purrr", "jsonlite", "base64enc", "httr2")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(paste("Missing required packages:", paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

retry_openai_request <- function(expr_fn, max_attempts = 4, base_sleep = 0.7) {
  for (attempt in seq_len(max_attempts)) {
    result <- tryCatch(expr_fn(), error = function(e) e)
    
    if (!inherits(result, "error")) {
      return(result)
    }
    
    msg <- conditionMessage(result)
    
    if (!grepl("429|rate limit", msg, ignore.case = TRUE) || attempt == max_attempts) {
      stop(result)
    }
    
    Sys.sleep(base_sleep * attempt)
  }
}

extract_chat_text <- function(resp_json) {
  if (!is.null(resp_json$choices) &&
      length(resp_json$choices) >= 1 &&
      !is.null(resp_json$choices[[1]]$message$content)) {
    return(trimws(resp_json$choices[[1]]$message$content))
  }
  ""
}

openai_chat_raw <- function(model, messages, temperature = 0, max_completion_tokens = 120) {
  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(api_key)) {
    stop("OPENAI_API_KEY is not set.")
  }
  
  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    max_completion_tokens = max_completion_tokens
  )
  
  retry_openai_request(function() {
    resp <- httr2::request("https://api.openai.com/v1/chat/completions") |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(120) |>
      httr2::req_perform()
    
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  })
}

ask_openai_assistant <- function(prompt, top_matches, model = "gpt-5.4-mini") {
  top_matches <- normalize_ai_matches(top_matches)
  if (nrow(top_matches) == 0) {
    return("OpenAI recommendation error: no match results were available.")
  }
  
  match_lines <- paste0(
    top_matches$photo_id,
    " | ",
    top_matches$site_type,
    " | similarity=",
    ifelse(is.na(top_matches$similarity), "NA", sprintf("%.1f", top_matches$similarity)),
    "%"
  )
  
  user_content <- paste(
    "User request:", prompt,
    "\nTop matched guide photos:\n", paste0("- ", match_lines, collapse = "\n"),
    "\nGive a concise recommendation for which output photo to use and why."
  )
  
  response <- tryCatch(
    openai_chat_raw(
      model = model,
      temperature = 0.2,
      max_completion_tokens = 120,
      messages = list(
        list(
          role = "system",
          content = "You are a wildfire fuel photo assistant. Be brief, practical, and safety-minded."
        ),
        list(
          role = "user",
          content = user_content
        )
      )
    ),
    error = function(e) {
      structure(list(.error = e$message), class = "openai_error_wrapper")
    }
  )
  
  if (!is.null(response$.error)) {
    err <- as.character(response$.error)
    if (grepl("429|rate limit", err, ignore.case = TRUE)) {
      return("Recommendation skipped because the OpenAI rate limit was reached. The top match results below are still usable.")
    }
    return(paste("OpenAI recommendation error:", err))
  }
  
  txt <- extract_chat_text(response)
  if (nzchar(txt)) txt else "OpenAI returned no recommendation text."
}

safe_chr1 <- function(x, default = "") {
  if (is.null(x) || length(x) == 0) return(default)
  x <- as.character(x[[1]])
  if (is.na(x) || !nzchar(x)) default else x
}

safe_num1 <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  out <- suppressWarnings(as.numeric(x[[1]]))
  if (length(out) == 0 || is.na(out)) default else out
}

coalesce_match_metadata <- function(matches_df, reference_df = photo_guide_options) {
  if (is.null(matches_df) || !is.data.frame(matches_df) || nrow(matches_df) == 0) {
    return(tibble::tibble())
  }
  
  out <- tibble::as_tibble(matches_df)
  ref <- tibble::as_tibble(reference_df)
  
  if (!"photo_id" %in% names(out) || !"photo_id" %in% names(ref)) {
    return(out)
  }
  
  ref_small <- ref %>%
    dplyr::select(
      photo_id, site_type, ecozone, vegetation_type,
      elevation_band, aspect_band, litter_factor, duff_factor,
      hr1, hr10, hr100,
      litter_depth_ref, duff_depth_ref, fuel_height_ref,
      fine_woody, coarse_woody, total_woody,
      image_url, local_path, image_exists
    )
  
  joined <- dplyr::left_join(out, ref_small, by = "photo_id", suffix = c("", ".ref"))
  
  pick_col <- function(df, primary, fallback = NULL, default = NULL) {
    has_primary <- primary %in% names(df)
    has_fallback <- !is.null(fallback) && fallback %in% names(df)
    
    if (has_primary && has_fallback) {
      dplyr::coalesce(df[[primary]], df[[fallback]])
    } else if (has_primary) {
      df[[primary]]
    } else if (has_fallback) {
      df[[fallback]]
    } else {
      rep(default, nrow(df))
    }
  }
  
  joined %>%
    dplyr::mutate(
      site_type       = pick_col(., "site_type", "site_type.ref", "Guide photo"),
      ecozone         = pick_col(., "ecozone", "ecozone.ref", NA_character_),
      vegetation_type = pick_col(., "vegetation_type", "vegetation_type.ref", NA_character_),
      elevation_band  = pick_col(., "elevation_band", "elevation_band.ref", NA_character_),
      aspect_band     = pick_col(., "aspect_band", "aspect_band.ref", NA_character_),
      litter_factor   = suppressWarnings(as.numeric(pick_col(., "litter_factor", "litter_factor.ref", NA_real_))),
      duff_factor     = suppressWarnings(as.numeric(pick_col(., "duff_factor", "duff_factor.ref", NA_real_))),
      hr1             = suppressWarnings(as.numeric(pick_col(., "hr1", "hr1.ref", NA_real_))),
      hr10            = suppressWarnings(as.numeric(pick_col(., "hr10", "hr10.ref", NA_real_))),
      hr100           = suppressWarnings(as.numeric(pick_col(., "hr100", "hr100.ref", NA_real_))),
      litter_depth_ref = suppressWarnings(as.numeric(pick_col(., "litter_depth_ref", "litter_depth_ref.ref", NA_real_))),
      duff_depth_ref   = suppressWarnings(as.numeric(pick_col(., "duff_depth_ref", "duff_depth_ref.ref", NA_real_))),
      fuel_height_ref  = suppressWarnings(as.numeric(pick_col(., "fuel_height_ref", "fuel_height_ref.ref", NA_real_))),
      fine_woody      = suppressWarnings(as.numeric(pick_col(., "fine_woody", "fine_woody.ref", NA_real_))),
      coarse_woody    = suppressWarnings(as.numeric(pick_col(., "coarse_woody", "coarse_woody.ref", NA_real_))),
      total_woody     = suppressWarnings(as.numeric(pick_col(., "total_woody", "total_woody.ref", NA_real_))),
      image_url       = pick_col(., "image_url", "image_url.ref", NA_character_),
      local_path      = pick_col(., "local_path", "local_path.ref", NA_character_),
      image_exists    = pick_col(., "image_exists", "image_exists.ref", FALSE)
    ) %>%
    dplyr::select(-dplyr::any_of(c(
      "site_type.ref", "ecozone.ref", "vegetation_type.ref",
      "elevation_band.ref", "aspect_band.ref",
      "litter_factor.ref", "duff_factor.ref",
      "hr1.ref", "hr10.ref", "hr100.ref",
      "litter_depth_ref.ref", "duff_depth_ref.ref", "fuel_height_ref.ref",
      "fine_woody.ref", "coarse_woody.ref", "total_woody.ref",
      "image_url.ref", "local_path.ref", "image_exists.ref"
    )))
}

normalize_ai_matches <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble())
  }
  
  out <- tibble::as_tibble(df)
  
  if (!"photo_id" %in% names(out)) out$photo_id <- NA_character_
  if (!"site_type" %in% names(out)) out$site_type <- "Guide photo"
  if (!"ecozone" %in% names(out)) out$ecozone <- NA_character_
  if (!"vegetation_type" %in% names(out)) out$vegetation_type <- NA_character_
  if (!"elevation_band" %in% names(out)) out$elevation_band <- NA_character_
  if (!"aspect_band" %in% names(out)) out$aspect_band <- NA_character_
  if (!"litter_factor" %in% names(out)) out$litter_factor <- NA_real_
  if (!"duff_factor" %in% names(out)) out$duff_factor <- NA_real_
  if (!"hr1" %in% names(out)) out$hr1 <- NA_real_
  if (!"hr10" %in% names(out)) out$hr10 <- NA_real_
  if (!"hr100" %in% names(out)) out$hr100 <- NA_real_
  if (!"litter_depth_ref" %in% names(out)) out$litter_depth_ref <- NA_real_
  if (!"duff_depth_ref" %in% names(out)) out$duff_depth_ref <- NA_real_
  if (!"fuel_height_ref" %in% names(out)) out$fuel_height_ref <- NA_real_
  if (!"fine_woody" %in% names(out)) out$fine_woody <- NA_real_
  if (!"coarse_woody" %in% names(out)) out$coarse_woody <- NA_real_
  if (!"total_woody" %in% names(out)) out$total_woody <- NA_real_
  if (!"image_url" %in% names(out)) out$image_url <- NA_character_
  if (!"local_path" %in% names(out)) out$local_path <- NA_character_
  if (!"openai_reason" %in% names(out)) out$openai_reason <- ""
  
  if ("similarity" %in% names(out)) {
    out$similarity <- suppressWarnings(as.numeric(out$similarity))
  } else if ("openai_similarity" %in% names(out)) {
    out$similarity <- suppressWarnings(as.numeric(out$openai_similarity))
  } else {
    out$similarity <- NA_real_
  }
  
  out$photo_id <- as.character(out$photo_id)
  out$site_type <- ifelse(is.na(out$site_type) | !nzchar(as.character(out$site_type)), "Guide photo", as.character(out$site_type))
  out$ecozone <- as.character(out$ecozone)
  out$vegetation_type <- as.character(out$vegetation_type)
  out$elevation_band <- as.character(out$elevation_band)
  out$aspect_band <- as.character(out$aspect_band)
  out$image_url <- as.character(out$image_url)
  out$local_path <- as.character(out$local_path)
  out$litter_factor <- suppressWarnings(as.numeric(out$litter_factor))
  out$duff_factor <- suppressWarnings(as.numeric(out$duff_factor))
  out$hr1 <- suppressWarnings(as.numeric(out$hr1))
  out$hr10 <- suppressWarnings(as.numeric(out$hr10))
  out$hr100 <- suppressWarnings(as.numeric(out$hr100))
  out$litter_depth_ref <- suppressWarnings(as.numeric(out$litter_depth_ref))
  out$duff_depth_ref <- suppressWarnings(as.numeric(out$duff_depth_ref))
  out$fuel_height_ref <- suppressWarnings(as.numeric(out$fuel_height_ref))
  out$fine_woody <- suppressWarnings(as.numeric(out$fine_woody))
  out$coarse_woody <- suppressWarnings(as.numeric(out$coarse_woody))
  out$total_woody <- suppressWarnings(as.numeric(out$total_woody))
  out$openai_reason <- as.character(out$openai_reason)
  
  dplyr::arrange(out, dplyr::desc(similarity))
}

safe_img_vector <- function(path, size = 96) {
  .assert_shiny_fix_dependencies()
  
  img <- magick::image_read(path) |>
    magick::image_resize(sprintf("%dx%d!", size, size)) |>
    magick::image_convert(colorspace = "Gray")
  
  as.numeric(magick::image_data(img, channels = "gray"))
}

find_candidate_matches_local <- function(upload_path, reference_df, top_n = 20) {
  .assert_shiny_fix_dependencies()
  
  up_vec <- safe_img_vector(upload_path)
  
  reference_df |>
    dplyr::filter(image_exists, !is.na(local_path), file.exists(local_path)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      prefilter_distance = mean(abs(up_vec - safe_img_vector(local_path)))
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(prefilter_distance) |>
    dplyr::slice_head(n = top_n)
}

find_top_matches_openai <- function(
    upload_path,
    reference_df,
    model = "gpt-5.4-mini",
    top_n = 5,
    candidate_limit = 5
) {
  local_candidates <- find_candidate_matches_local(
    upload_path = upload_path,
    reference_df = reference_df,
    top_n = candidate_limit
  )
  
  if (nrow(local_candidates) == 0) {
    stop("No candidate guide photos were found.")
  }
  
  uploaded_ext <- tolower(tools::file_ext(upload_path))
  uploaded_mime <- ifelse(
    uploaded_ext %in% c("jpg", "jpeg"), "image/jpeg",
    ifelse(uploaded_ext == "webp", "image/webp", "image/png")
  )
  uploaded_uri <- base64enc::dataURI(file = upload_path, mime = uploaded_mime)
  
  results <- vector("list", nrow(local_candidates))
  
  for (i in seq_len(nrow(local_candidates))) {
    cand <- local_candidates[i, ]
    
    results[[i]] <- tryCatch({
      cand_path <- as.character(cand$local_path[[1]])
      cand_id <- as.character(cand$photo_id[[1]])
      
      if (!file.exists(cand_path)) {
        return(tibble::tibble(
          photo_id = cand_id,
          openai_similarity = NA_real_,
          openai_reason = "Candidate image file does not exist"
        ))
      }
      
      cand_mime <- if (grepl("\\.jpe?g$", cand_path, ignore.case = TRUE)) {
        "image/jpeg"
      } else if (grepl("\\.webp$", cand_path, ignore.case = TRUE)) {
        "image/webp"
      } else {
        "image/png"
      }
      
      cand_uri <- base64enc::dataURI(file = cand_path, mime = cand_mime)
      
      compare_prompt <- paste(
        "Compare these two forest-fuel photos.",
        "Return ONLY valid JSON with keys:",
        "similarity (0-100 number), reason (short string).",
        "Score visual similarity of vegetation/fuel-bed structure, litter/duff appearance, and ground cover."
      )
      
      resp <- tryCatch(
        openai_chat_raw(
          model = model,
          temperature = 0,
          max_completion_tokens = 80,
          messages = list(
            list(
              role = "user",
              content = list(
                list(type = "text", text = compare_prompt),
                list(type = "text", text = "Image A (uploaded reference):"),
                list(type = "image_url", image_url = list(url = uploaded_uri)),
                list(type = "text", text = paste("Image B (guide", cand_id, "):")),
                list(type = "image_url", image_url = list(url = cand_uri))
              )
            )
          )
        ),
        error = function(e) structure(list(.error = e$message), class = "openai_error_wrapper")
      )
      
      if (!is.null(resp$.error)) {
        return(tibble::tibble(
          photo_id = cand_id,
          openai_similarity = NA_real_,
          openai_reason = paste("OpenAI API error:", resp$.error)
        ))
      }
      
      txt <- extract_chat_text(resp)
      parsed <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
      
      if (is.null(parsed) || is.null(parsed$similarity)) {
        tibble::tibble(
          photo_id = cand_id,
          openai_similarity = NA_real_,
          openai_reason = paste("Invalid JSON returned:", txt)
        )
      } else {
        tibble::tibble(
          photo_id = cand_id,
          openai_similarity = as.numeric(parsed$similarity),
          openai_reason = as.character(if (!is.null(parsed$reason)) parsed$reason else "")
        )
      }
    }, error = function(e) {
      tibble::tibble(
        photo_id = as.character(cand$photo_id[[1]]),
        openai_similarity = NA_real_,
        openai_reason = paste("Candidate", i, "failed:", e$message)
      )
    })
  }
  
  scored <- dplyr::bind_rows(results)
  
  ranked <- local_candidates |>
    dplyr::left_join(scored, by = "photo_id") |>
    dplyr::mutate(
      openai_similarity = suppressWarnings(as.numeric(openai_similarity)),
      similarity = pmax(0, pmin(100, openai_similarity))
    ) |>
    dplyr::filter(!is.na(openai_similarity)) |>
    dplyr::arrange(dplyr::desc(similarity)) |>
    dplyr::slice_head(n = top_n)
  
  if (nrow(ranked) == 0) {
    stop("OpenAI did not return valid similarity scores.")
  }
  
  ranked
}

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

fuel_emission_factors <- list(
  "Grass" = 0.013,
  "Shrub" = 0.016,
  "Hardwood Litter" = 0.014,
  "Conifer Litter" = 0.016,
  "Logging Slash" = 0.020,
  "Heavy Fuels" = 0.022
)

LITTER_TONS_PER_ACRE_PER_INCH <- 1.38
DUFF_TONS_PER_ACRE_PER_INCH <- 4.84

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

fuel_moisture_profiles <- list(
  "Very Dry" = list(consumption_mult = 1.15, flaming_mult = 1.12, plume_mult = 1.10),
  "Dry" = list(consumption_mult = 1.08, flaming_mult = 1.05, plume_mult = 1.04),
  "Moderate" = list(consumption_mult = 1.00, flaming_mult = 1.00, plume_mult = 1.00),
  "Moist" = list(consumption_mult = 0.92, flaming_mult = 0.92, plume_mult = 0.95),
  "Very Moist" = list(consumption_mult = 0.82, flaming_mult = 0.85, plume_mult = 0.90)
)

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
                                 background_pm25 = 5,
                                 fuel_moisture = "Moderate") {
  
  total_fuel_tonnes_available <- acres * tons_per_acre * 0.907185
  total_fuel_tonnes_consumed <- total_fuel_tonnes_available * consumed_fraction
  
  emission_factor <- fuel_emission_factors[[fuel_type]]
  total_pm_tonnes <- total_fuel_tonnes_consumed * emission_factor
  total_pm_g <- total_pm_tonnes * 1e6
  
  flaming_hours <- max(duration_hours * flaming_duration_fraction, 0.1)
  smolder_hours <- max(duration_hours - flaming_hours, 0.1)
  
  pm_flaming_g <- total_pm_g * flaming_fraction
  pm_smolder_g <- total_pm_g * (1 - flaming_fraction)
  
  Q_flaming <- pm_flaming_g / (flaming_hours * 3600)
  Q_smolder <- pm_smolder_g / (smolder_hours * 3600)
  
  fuel_flaming_kg_s <- (total_fuel_tonnes_consumed * 1000 * flaming_fraction) / (flaming_hours * 3600)
  
  moisture_profile <- fuel_moisture_profiles[[fuel_moisture]]
  if (is.null(moisture_profile)) moisture_profile <- fuel_moisture_profiles[["Moderate"]]
  
  heat_mult <- if (ignition_method == "Head/Aerial") 1.25 else 0.85
  heat_release_mw <- fuel_flaming_kg_s * heat_content_kj_kg * convective_fraction * heat_mult * moisture_profile$plume_mult / 1000
  F <- calc_buoyancy_flux(heat_release_mw)
  
  plume_rise_fraction_smolder <- if (plume_rise_fraction < 0) plume_rise_fraction * 0.35 else plume_rise_fraction * 0.20
  
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
      } else plume_base,
      
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
      } else 0,
      
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
      } else 0
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
# Photo Guide Mapping (74 photos)
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

photo_guide_reference_data <- bind_rows(
  tibble(photo_num = 1, ecozone = "Low elevation pine", aspect_degrees = 129.9, elevation_ft = 1552, slope_pct = 12.9, hr1 = 0.3, litter_depth_ref = 1.6, hr10 = 0.6, duff_depth_ref = 1.4, hr100 = 1.2, fuel_height_ref = 2.3, fine_woody = 2.1, coarse_woody = 1.0, shrub_cover_pct = 0.9, total_woody = 3.1, vegetation_lt1ft_pct = 25.0),
  tibble(photo_num = 2, ecozone = "Low elevation pine", aspect_degrees = 96.0, elevation_ft = 1538, slope_pct = 10.8, hr1 = 0.2, litter_depth_ref = 2.0, hr10 = 0.4, duff_depth_ref = 1.0, hr100 = 1.2, fuel_height_ref = 2.5, fine_woody = 1.8, coarse_woody = 2.5, shrub_cover_pct = 3.4, total_woody = 4.3, vegetation_lt1ft_pct = 63.0),
  tibble(photo_num = 3, ecozone = "Low elevation pine", aspect_degrees = 79.6, elevation_ft = 1582, slope_pct = 5.1, hr1 = 0.5, litter_depth_ref = 1.4, hr10 = 1.9, duff_depth_ref = 1.9, hr100 = 0.6, fuel_height_ref = 2.3, fine_woody = 3.0, coarse_woody = 5.3, shrub_cover_pct = 25.1, total_woody = 8.3, vegetation_lt1ft_pct = 10.0),
  tibble(photo_num = 4, ecozone = "Acidic cove", aspect_degrees = 47.1, elevation_ft = 1209, slope_pct = 32.5, hr1 = 0.7, litter_depth_ref = 1.2, hr10 = 1.6, duff_depth_ref = 1.0, hr100 = 0.7, fuel_height_ref = 3.9, fine_woody = 3.0, coarse_woody = 2.5, shrub_cover_pct = 0.0, total_woody = 5.5, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 5, ecozone = "Dry mesic oak", aspect_degrees = 55.0, elevation_ft = 1457, slope_pct = 21.9, hr1 = 0.2, litter_depth_ref = 1.8, hr10 = 1.1, duff_depth_ref = 0.8, hr100 = 1.8, fuel_height_ref = 2.3, fine_woody = 3.1, coarse_woody = 2.6, shrub_cover_pct = 0.0, total_woody = 5.7, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 6, ecozone = "Low elevation pine", aspect_degrees = 133.6, elevation_ft = 1600, slope_pct = 15.2, hr1 = 0.1, litter_depth_ref = 1.3, hr10 = 0.6, duff_depth_ref = 0.7, hr100 = 2.9, fuel_height_ref = 2.4, fine_woody = 3.6, coarse_woody = 4.9, shrub_cover_pct = 16.0, total_woody = 8.5, vegetation_lt1ft_pct = 8.0),
  tibble(photo_num = 7, ecozone = "Low elevation pine", aspect_degrees = 127.0, elevation_ft = 1219, slope_pct = 8.7, hr1 = 0.2, litter_depth_ref = 1.4, hr10 = 0.9, duff_depth_ref = 1.1, hr100 = 4.1, fuel_height_ref = 2.3, fine_woody = 5.2, coarse_woody = 1.0, shrub_cover_pct = 0.3, total_woody = 6.2, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 8, ecozone = "Montane oak-hickory cove", aspect_degrees = 117.3, elevation_ft = 1154, slope_pct = 4.5, hr1 = 0.4, litter_depth_ref = 1.0, hr10 = 1.5, duff_depth_ref = 1.7, hr100 = 2.9, fuel_height_ref = 4.5, fine_woody = 4.8, coarse_woody = 13.4, shrub_cover_pct = 9.4, total_woody = 18.2, vegetation_lt1ft_pct = 11.0),
  tibble(photo_num = 9, ecozone = "Dry oak evergreen heath", aspect_degrees = 73.6, elevation_ft = 1408, slope_pct = 14.3, hr1 = 0.2, litter_depth_ref = 1.5, hr10 = 0.9, duff_depth_ref = 1.1, hr100 = 3.5, fuel_height_ref = 1.9, fine_woody = 4.6, coarse_woody = 0.9, shrub_cover_pct = 0.0, total_woody = 5.5, vegetation_lt1ft_pct = 95.0),
  tibble(photo_num = 10, ecozone = "Acidic cove", aspect_degrees = 101.0, elevation_ft = 1219, slope_pct = 23.1, hr1 = 0.6, litter_depth_ref = 1.0, hr10 = 0.7, duff_depth_ref = 0.4, hr100 = 4.9, fuel_height_ref = 3.3, fine_woody = 6.2, coarse_woody = 0.3, shrub_cover_pct = 0.0, total_woody = 6.5, vegetation_lt1ft_pct = 4.5),
  tibble(photo_num = 11, ecozone = "Low elevation pine", aspect_degrees = 90.9, elevation_ft = 2677, slope_pct = 13.5, hr1 = 0.3, litter_depth_ref = 1.2, hr10 = 1.0, duff_depth_ref = 0.8, hr100 = 0.0, fuel_height_ref = 1.2, fine_woody = 1.3, coarse_woody = 0.8, shrub_cover_pct = 52.7, total_woody = 2.1, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 12, ecozone = "Dry mesic oak", aspect_degrees = 47.5, elevation_ft = 2698, slope_pct = 25.2, hr1 = 0.2, litter_depth_ref = 1.1, hr10 = 0.4, duff_depth_ref = 1.5, hr100 = 0.6, fuel_height_ref = 1.6, fine_woody = 1.2, coarse_woody = 10.7, shrub_cover_pct = 57.6, total_woody = 11.9, vegetation_lt1ft_pct = 43.0),
  tibble(photo_num = 13, ecozone = "Pine-oak heath", aspect_degrees = 85.5, elevation_ft = 2027, slope_pct = 28.6, hr1 = 0.5, litter_depth_ref = 2.0, hr10 = 0.6, duff_depth_ref = 1.9, hr100 = 0.6, fuel_height_ref = 3.4, fine_woody = 1.7, coarse_woody = 7.4, shrub_cover_pct = 0.0, total_woody = 9.1, vegetation_lt1ft_pct = 62.5),
  tibble(photo_num = 14, ecozone = "Low elevation pine", aspect_degrees = 114.4, elevation_ft = 2675, slope_pct = 11.2, hr1 = 0.2, litter_depth_ref = 1.7, hr10 = 0.9, duff_depth_ref = 1.3, hr100 = 1.2, fuel_height_ref = 5.3, fine_woody = 2.3, coarse_woody = 1.1, shrub_cover_pct = 15.2, total_woody = 3.4, vegetation_lt1ft_pct = 35.0),
  tibble(photo_num = 15, ecozone = "Low elevation pine", aspect_degrees = 109.9, elevation_ft = 2692, slope_pct = 16.4, hr1 = 0.3, litter_depth_ref = 1.6, hr10 = 0.6, duff_depth_ref = 1.6, hr100 = 2.3, fuel_height_ref = 7.5, fine_woody = 3.2, coarse_woody = 15.9, shrub_cover_pct = 0.0, total_woody = 19.1, vegetation_lt1ft_pct = 75.0),
  tibble(photo_num = 16, ecozone = "Low elevation pine", aspect_degrees = 50.0, elevation_ft = 2463, slope_pct = 13.1, hr1 = 0.3, litter_depth_ref = 2.0, hr10 = 0.9, duff_depth_ref = 2.1, hr100 = 3.5, fuel_height_ref = 6.1, fine_woody = 4.7, coarse_woody = 1.5, shrub_cover_pct = 1.3, total_woody = 6.2, vegetation_lt1ft_pct = 27.0),
  tibble(photo_num = 17, ecozone = "Dry mesic oak", aspect_degrees = 125.1, elevation_ft = 2581, slope_pct = 18.8, hr1 = 0.4, litter_depth_ref = 1.8, hr10 = 2.7, duff_depth_ref = 0.7, hr100 = 7.0, fuel_height_ref = 2.6, fine_woody = 10.1, coarse_woody = 8.1, shrub_cover_pct = 0.0, total_woody = 18.2, vegetation_lt1ft_pct = 2.0),
  tibble(photo_num = 18, ecozone = "High elevation red oak", aspect_degrees = 80.4, elevation_ft = 5009, slope_pct = 27.9, hr1 = 0.2, litter_depth_ref = 3.0, hr10 = 1.6, duff_depth_ref = 2.4, hr100 = 0.0, fuel_height_ref = 3.2, fine_woody = 1.8, coarse_woody = 14.5, shrub_cover_pct = 0.0, total_woody = 16.3, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 19, ecozone = "Acidic cove", aspect_degrees = 108.5, elevation_ft = 3727, slope_pct = 21.9, hr1 = 0.5, litter_depth_ref = 1.3, hr10 = 0.4, duff_depth_ref = 2.0, hr100 = 0.6, fuel_height_ref = 4.1, fine_woody = 1.5, coarse_woody = 2.1, shrub_cover_pct = 84.3, total_woody = 3.6, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 20, ecozone = "Northern hardwood cove", aspect_degrees = 81.6, elevation_ft = 4049, slope_pct = 17.9, hr1 = 0.1, litter_depth_ref = 1.5, hr10 = 0.9, duff_depth_ref = 2.3, hr100 = 2.3, fuel_height_ref = 4.3, fine_woody = 3.3, coarse_woody = 12.7, shrub_cover_pct = 0.0, total_woody = 16.0, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 21, ecozone = "Acidic cove", aspect_degrees = 64.2, elevation_ft = 3580, slope_pct = 6.8, hr1 = 0.5, litter_depth_ref = 1.9, hr10 = 1.3, duff_depth_ref = 3.7, hr100 = 1.8, fuel_height_ref = 8.3, fine_woody = 3.6, coarse_woody = 0.0, shrub_cover_pct = 43.2, total_woody = 3.6, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 22, ecozone = "Acidic cove", aspect_degrees = 147.7, elevation_ft = 1086, slope_pct = 9.8, hr1 = 0.2, litter_depth_ref = 2.1, hr10 = 0.0, duff_depth_ref = 1.5, hr100 = 0.6, fuel_height_ref = 9.8, fine_woody = 0.8, coarse_woody = 1.1, shrub_cover_pct = 0.0, total_woody = 1.9, vegetation_lt1ft_pct = 1.5),
  tibble(photo_num = 23, ecozone = "Dry oak evergreen heath", aspect_degrees = 197.2, elevation_ft = 1856, slope_pct = 23.1, hr1 = 0.4, litter_depth_ref = 1.5, hr10 = 0.9, duff_depth_ref = 2.0, hr100 = 1.2, fuel_height_ref = 5.7, fine_woody = 2.5, coarse_woody = 21.3, shrub_cover_pct = 9.2, total_woody = 23.8, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 24, ecozone = "Low elevation pine", aspect_degrees = 140.0, elevation_ft = 1577, slope_pct = 6.6, hr1 = 0.4, litter_depth_ref = 0.6, hr10 = 1.5, duff_depth_ref = 0.3, hr100 = 0.6, fuel_height_ref = 1.0, fine_woody = 2.5, coarse_woody = 1.6, shrub_cover_pct = 0.0, total_woody = 4.1, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 25, ecozone = "Dry oak evergreen heath", aspect_degrees = 196.6, elevation_ft = 1416, slope_pct = 18.8, hr1 = 0.2, litter_depth_ref = 1.6, hr10 = 0.6, duff_depth_ref = 2.1, hr100 = 2.3, fuel_height_ref = 3.4, fine_woody = 3.1, coarse_woody = 7.6, shrub_cover_pct = 74.6, total_woody = 10.7, vegetation_lt1ft_pct = 2.0),
  tibble(photo_num = 26, ecozone = "Low elevation pine", aspect_degrees = 196.7, elevation_ft = 1541, slope_pct = 8.6, hr1 = 0.2, litter_depth_ref = 1.6, hr10 = 1.2, duff_depth_ref = 1.8, hr100 = 2.3, fuel_height_ref = 3.8, fine_woody = 3.7, coarse_woody = 0.0, shrub_cover_pct = 14.7, total_woody = 3.7, vegetation_lt1ft_pct = 10.0),
  tibble(photo_num = 27, ecozone = "Dry mesic oak", aspect_degrees = 185.9, elevation_ft = 1171, slope_pct = 24.0, hr1 = 0.3, litter_depth_ref = 0.7, hr10 = 1.4, duff_depth_ref = 0.7, hr100 = 4.7, fuel_height_ref = 2.2, fine_woody = 6.4, coarse_woody = 0.8, shrub_cover_pct = 0.0, total_woody = 7.2, vegetation_lt1ft_pct = 7.0),
  tibble(photo_num = 28, ecozone = "Low elevation pine", aspect_degrees = 183.8, elevation_ft = 2564, slope_pct = 4.1, hr1 = 0.2, litter_depth_ref = 1.2, hr10 = 0.4, duff_depth_ref = 1.1, hr100 = 0.0, fuel_height_ref = 1.7, fine_woody = 0.6, coarse_woody = 0.2, shrub_cover_pct = 0.0, total_woody = 0.8, vegetation_lt1ft_pct = 20.0),
  tibble(photo_num = 29, ecozone = "Dry oak evergreen heath", aspect_degrees = 220.4, elevation_ft = 2662, slope_pct = 14.4, hr1 = 0.3, litter_depth_ref = 2.1, hr10 = 0.6, duff_depth_ref = 1.3, hr100 = 0.0, fuel_height_ref = 5.4, fine_woody = 0.9, coarse_woody = 16.2, shrub_cover_pct = 0.0, total_woody = 17.1, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 30, ecozone = "Low elevation pine", aspect_degrees = 209.7, elevation_ft = 2457, slope_pct = 13.7, hr1 = 0.2, litter_depth_ref = 0.9, hr10 = 0.6, duff_depth_ref = 1.4, hr100 = 1.2, fuel_height_ref = 2.3, fine_woody = 2.0, coarse_woody = 0.5, shrub_cover_pct = 29.0, total_woody = 2.5, vegetation_lt1ft_pct = 2.0),
  tibble(photo_num = 31, ecozone = "Acidic cove", aspect_degrees = 225.1, elevation_ft = 3490, slope_pct = 1.9, hr1 = 0.3, litter_depth_ref = 2.1, hr10 = 0.9, duff_depth_ref = 3.1, hr100 = 1.8, fuel_height_ref = 5.6, fine_woody = 3.0, coarse_woody = 2.1, shrub_cover_pct = 39.3, total_woody = 5.1, vegetation_lt1ft_pct = 3.0),
  tibble(photo_num = 32, ecozone = "Montane oak-hickory slope", aspect_degrees = 220.5, elevation_ft = 3705, slope_pct = 24.9, hr1 = 0.4, litter_depth_ref = 1.7, hr10 = 1.5, duff_depth_ref = 0.5, hr100 = 0.7, fuel_height_ref = 4.8, fine_woody = 2.6, coarse_woody = 4.3, shrub_cover_pct = 0.0, total_woody = 6.9, vegetation_lt1ft_pct = 27.0),
  tibble(photo_num = 33, ecozone = "Acidic cove", aspect_degrees = 216.7, elevation_ft = 3529, slope_pct = 17.7, hr1 = 0.1, litter_depth_ref = 1.3, hr10 = 0.1, duff_depth_ref = 1.1, hr100 = 1.8, fuel_height_ref = 1.3, fine_woody = 2.0, coarse_woody = 7.2, shrub_cover_pct = 63.3, total_woody = 9.2, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 34, ecozone = "Acidic cove", aspect_degrees = 225.3, elevation_ft = 3742, slope_pct = 23.3, hr1 = 0.4, litter_depth_ref = 1.9, hr10 = 1.9, duff_depth_ref = 1.1, hr100 = 1.8, fuel_height_ref = 5.0, fine_woody = 4.1, coarse_woody = 1.9, shrub_cover_pct = 0.0, total_woody = 6.0, vegetation_lt1ft_pct = 19.0),
  tibble(photo_num = 35, ecozone = "Dry oak evergreen heath", aspect_degrees = 196.6, elevation_ft = 4194, slope_pct = 27.9, hr1 = 0.3, litter_depth_ref = 2.4, hr10 = 0.9, duff_depth_ref = 1.2, hr100 = 3.1, fuel_height_ref = 4.9, fine_woody = 4.3, coarse_woody = 17.6, shrub_cover_pct = 0.0, total_woody = 21.9, vegetation_lt1ft_pct = 3.0),
  tibble(photo_num = 36, ecozone = "Low elevation pine", aspect_degrees = 294.2, elevation_ft = 1050, slope_pct = 12.3, hr1 = 0.2, litter_depth_ref = 1.8, hr10 = 0.7, duff_depth_ref = 0.9, hr100 = 0.0, fuel_height_ref = 2.0, fine_woody = 0.9, coarse_woody = 1.4, shrub_cover_pct = 7.9, total_woody = 2.3, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 37, ecozone = "Dry mesic oak", aspect_degrees = 245.4, elevation_ft = 1467, slope_pct = 7.8, hr1 = 0.5, litter_depth_ref = 1.4, hr10 = 1.0, duff_depth_ref = 1.2, hr100 = 0.0, fuel_height_ref = 6.8, fine_woody = 1.5, coarse_woody = 0.0, shrub_cover_pct = 3.7, total_woody = 1.5, vegetation_lt1ft_pct = 72.0),
  tibble(photo_num = 38, ecozone = "Dry mesic oak", aspect_degrees = 245.4, elevation_ft = 1426, slope_pct = 7.8, hr1 = 0.2, litter_depth_ref = 1.4, hr10 = 0.6, duff_depth_ref = 0.5, hr100 = 2.9, fuel_height_ref = 1.9, fine_woody = 3.7, coarse_woody = 1.1, shrub_cover_pct = 28.8, total_woody = 4.8, vegetation_lt1ft_pct = 51.0),
  tibble(photo_num = 39, ecozone = "Low elevation pine", aspect_degrees = 243.5, elevation_ft = 3411, slope_pct = 9.4, hr1 = 0.2, litter_depth_ref = 1.4, hr10 = 0.3, duff_depth_ref = 1.0, hr100 = 0.0, fuel_height_ref = 3.2, fine_woody = 0.5, coarse_woody = 6.7, shrub_cover_pct = 30.5, total_woody = 7.2, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 40, ecozone = "Rich cove", aspect_degrees = 242.3, elevation_ft = 3435, slope_pct = 18.9, hr1 = 0.2, litter_depth_ref = 1.4, hr10 = 0.0, duff_depth_ref = 1.7, hr100 = 1.2, fuel_height_ref = 5.4, fine_woody = 1.4, coarse_woody = 25.9, shrub_cover_pct = 44.6, total_woody = 27.3, vegetation_lt1ft_pct = 4.0),
  tibble(photo_num = 41, ecozone = "Dry mesic oak", aspect_degrees = 280.7, elevation_ft = 3395, slope_pct = 13.8, hr1 = 0.2, litter_depth_ref = 1.0, hr10 = 1.6, duff_depth_ref = 0.7, hr100 = 0.6, fuel_height_ref = 4.2, fine_woody = 2.4, coarse_woody = 8.1, shrub_cover_pct = 2.0, total_woody = 10.5, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 42, ecozone = "Montane oak-hickory slope", aspect_degrees = 242.9, elevation_ft = 2200, slope_pct = 32.7, hr1 = 0.2, litter_depth_ref = 1.4, hr10 = 0.3, duff_depth_ref = 1.5, hr100 = 1.2, fuel_height_ref = 4.7, fine_woody = 1.7, coarse_woody = 25.7, shrub_cover_pct = 44.5, total_woody = 27.4, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 43, ecozone = "Dry mesic oak", aspect_degrees = 253.5, elevation_ft = 2142, slope_pct = 13.2, hr1 = 0.3, litter_depth_ref = 0.9, hr10 = 0.7, duff_depth_ref = 2.9, hr100 = 1.2, fuel_height_ref = 3.4, fine_woody = 2.2, coarse_woody = 0.4, shrub_cover_pct = 70.4, total_woody = 2.6, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 44, ecozone = "Dry mesic oak", aspect_degrees = 230.7, elevation_ft = 3065, slope_pct = 19.2, hr1 = 0.3, litter_depth_ref = 1.6, hr10 = 0.7, duff_depth_ref = 0.6, hr100 = 1.8, fuel_height_ref = 3.9, fine_woody = 2.8, coarse_woody = 0.9, shrub_cover_pct = 0.0, total_woody = 3.7, vegetation_lt1ft_pct = 77.0),
  tibble(photo_num = 45, ecozone = "Low elevation pine", aspect_degrees = 278.1, elevation_ft = 3459, slope_pct = 32.2, hr1 = 0.3, litter_depth_ref = 3.3, hr10 = 0.4, duff_depth_ref = 4.8, hr100 = 2.3, fuel_height_ref = 4.7, fine_woody = 3.0, coarse_woody = 3.6, shrub_cover_pct = 68.4, total_woody = 6.6, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 46, ecozone = "Acidic cove", aspect_degrees = 249.5, elevation_ft = 3491, slope_pct = 21.2, hr1 = 0.2, litter_depth_ref = 1.5, hr10 = 0.5, duff_depth_ref = 1.7, hr100 = 3.0, fuel_height_ref = 2.2, fine_woody = 3.7, coarse_woody = 4.3, shrub_cover_pct = 0.0, total_woody = 8.0, vegetation_lt1ft_pct = 4.0),
  tibble(photo_num = 47, ecozone = "Montane oak-hickory slope", aspect_degrees = 272.5, elevation_ft = 4152, slope_pct = 23.5, hr1 = 0.2, litter_depth_ref = 2.2, hr10 = 0.4, duff_depth_ref = 0.6, hr100 = 0.0, fuel_height_ref = 3.1, fine_woody = 0.6, coarse_woody = 1.2, shrub_cover_pct = 0.0, total_woody = 1.8, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 48, ecozone = "Montane oak-hickory slope", aspect_degrees = 245.5, elevation_ft = 4056, slope_pct = 41.6, hr1 = 0.1, litter_depth_ref = 2.4, hr10 = 0.3, duff_depth_ref = 2.5, hr100 = 0.7, fuel_height_ref = 3.4, fine_woody = 1.1, coarse_woody = 0.0, shrub_cover_pct = 107.7, total_woody = 1.1, vegetation_lt1ft_pct = 3.0),
  tibble(photo_num = 49, ecozone = "Dry oak evergreen heath", aspect_degrees = 273.3, elevation_ft = 3790, slope_pct = 19.7, hr1 = 0.2, litter_depth_ref = 2.1, hr10 = 0.3, duff_depth_ref = 0.6, hr100 = 2.5, fuel_height_ref = 5.2, fine_woody = 3.0, coarse_woody = 2.9, shrub_cover_pct = 0.0, total_woody = 5.9, vegetation_lt1ft_pct = 76.0),
  tibble(photo_num = 50, ecozone = "Rich cove", aspect_degrees = 256.9, elevation_ft = 3858, slope_pct = 29.7, hr1 = 0.3, litter_depth_ref = 2.5, hr10 = 1.2, duff_depth_ref = 1.5, hr100 = 1.2, fuel_height_ref = 4.8, fine_woody = 2.7, coarse_woody = 3.3, shrub_cover_pct = 0.0, total_woody = 6.0, vegetation_lt1ft_pct = 5.0),
  tibble(photo_num = 51, ecozone = "Northern hardwood cove", aspect_degrees = 280.4, elevation_ft = 4768, slope_pct = 8.7, hr1 = 0.3, litter_depth_ref = 2.2, hr10 = 0.7, duff_depth_ref = 1.6, hr100 = 1.8, fuel_height_ref = 15.9, fine_woody = 2.8, coarse_woody = 12.3, shrub_cover_pct = 0.0, total_woody = 15.1, vegetation_lt1ft_pct = 10.0),
  tibble(photo_num = 52, ecozone = "Acidic cove", aspect_degrees = 296.5, elevation_ft = 3693, slope_pct = 17.0, hr1 = 0.2, litter_depth_ref = 1.3, hr10 = 0.9, duff_depth_ref = 0.9, hr100 = 2.4, fuel_height_ref = 1.1, fine_woody = 3.5, coarse_woody = 6.6, shrub_cover_pct = 41.8, total_woody = 10.1, vegetation_lt1ft_pct = 2.0),
  tibble(photo_num = 53, ecozone = "Mixed oak rhododendron", aspect_degrees = 254.5, elevation_ft = 3895, slope_pct = 27.1, hr1 = 0.5, litter_depth_ref = 1.8, hr10 = 1.0, duff_depth_ref = 0.4, hr100 = 4.2, fuel_height_ref = 2.6, fine_woody = 5.7, coarse_woody = 5.9, shrub_cover_pct = 55.5, total_woody = 11.6, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 54, ecozone = "Northern hardwood slope", aspect_degrees = 255.4, elevation_ft = 4590, slope_pct = 26.4, hr1 = 0.2, litter_depth_ref = 2.2, hr10 = 1.2, duff_depth_ref = 0.7, hr100 = 2.9, fuel_height_ref = 3.6, fine_woody = 4.3, coarse_woody = 8.4, shrub_cover_pct = 0.0, total_woody = 12.7, vegetation_lt1ft_pct = 7.0),
  tibble(photo_num = 55, ecozone = "High elevation red oak", aspect_degrees = 288.8, elevation_ft = 4997, slope_pct = 17.1, hr1 = 0.2, litter_depth_ref = 1.6, hr10 = 1.2, duff_depth_ref = 1.6, hr100 = 5.3, fuel_height_ref = 7.5, fine_woody = 6.7, coarse_woody = 18.8, shrub_cover_pct = 0.0, total_woody = 25.5, vegetation_lt1ft_pct = 10.0),
  tibble(photo_num = 56, ecozone = "Acidic cove", aspect_degrees = 279.8, elevation_ft = 4009, slope_pct = 16.2, hr1 = 0.3, litter_depth_ref = 1.9, hr10 = 2.4, duff_depth_ref = 1.0, hr100 = 11.9, fuel_height_ref = 4.3, fine_woody = 14.6, coarse_woody = 13.7, shrub_cover_pct = 0.0, total_woody = 28.3, vegetation_lt1ft_pct = 7.0),
  tibble(photo_num = 57, ecozone = "Pine-oak heath", aspect_degrees = 317.0, elevation_ft = 1859, slope_pct = 19.9, hr1 = 0.1, litter_depth_ref = 2.4, hr10 = 0.3, duff_depth_ref = 1.1, hr100 = 2.4, fuel_height_ref = 2.5, fine_woody = 2.8, coarse_woody = 0.4, shrub_cover_pct = 0.0, total_woody = 3.2, vegetation_lt1ft_pct = 5.5),
  tibble(photo_num = 58, ecozone = "Dry mesic oak", aspect_degrees = 8.7, elevation_ft = 1296, slope_pct = 31.0, hr1 = 0.2, litter_depth_ref = 2.0, hr10 = 1.9, duff_depth_ref = 6.2, hr100 = 1.8, fuel_height_ref = 5.3, fine_woody = 3.9, coarse_woody = 3.4, shrub_cover_pct = 2.3, total_woody = 7.3, vegetation_lt1ft_pct = 14.0),
  tibble(photo_num = 59, ecozone = "Dry mesic oak", aspect_degrees = 28.2, elevation_ft = 1270, slope_pct = 25.0, hr1 = 0.2, litter_depth_ref = 1.4, hr10 = 1.6, duff_depth_ref = 1.3, hr100 = 2.9, fuel_height_ref = 4.1, fine_woody = 4.7, coarse_woody = 4.4, shrub_cover_pct = 0.0, total_woody = 9.1, vegetation_lt1ft_pct = 35.0),
  tibble(photo_num = 60, ecozone = "Low elevation pine", aspect_degrees = 38.0, elevation_ft = 1267, slope_pct = 19.7, hr1 = 0.3, litter_depth_ref = 1.6, hr10 = 2.1, duff_depth_ref = 2.8, hr100 = 2.9, fuel_height_ref = 6.8, fine_woody = 5.3, coarse_woody = 7.6, shrub_cover_pct = 37.9, total_woody = 12.9, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 61, ecozone = "Dry mesic oak", aspect_degrees = 22.4, elevation_ft = 2769, slope_pct = 21.3, hr1 = 0.4, litter_depth_ref = 2.1, hr10 = 0.7, duff_depth_ref = 1.7, hr100 = 0.6, fuel_height_ref = 5.7, fine_woody = 1.7, coarse_woody = 2.1, shrub_cover_pct = 6.7, total_woody = 3.8, vegetation_lt1ft_pct = 100.0),
  tibble(photo_num = 62, ecozone = "Acidic cove", aspect_degrees = 42.5, elevation_ft = 2084, slope_pct = 11.5, hr1 = 0.4, litter_depth_ref = 1.5, hr10 = 1.0, duff_depth_ref = 2.3, hr100 = 0.6, fuel_height_ref = 6.5, fine_woody = 2.0, coarse_woody = 1.0, shrub_cover_pct = 42.0, total_woody = 3.0, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 63, ecozone = "Rich cove", aspect_degrees = 352.5, elevation_ft = 2244, slope_pct = 22.2, hr1 = 0.2, litter_depth_ref = 2.2, hr10 = 0.7, duff_depth_ref = 1.8, hr100 = 1.2, fuel_height_ref = 3.8, fine_woody = 2.1, coarse_woody = 1.4, shrub_cover_pct = 0.0, total_woody = 3.5, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 64, ecozone = "Acidic cove", aspect_degrees = 320.9, elevation_ft = 3428, slope_pct = 3.3, hr1 = 0.4, litter_depth_ref = 2.6, hr10 = 0.2, duff_depth_ref = 1.9, hr100 = 3.0, fuel_height_ref = 6.2, fine_woody = 3.6, coarse_woody = 17.3, shrub_cover_pct = 43.1, total_woody = 20.9, vegetation_lt1ft_pct = 26.0),
  tibble(photo_num = 65, ecozone = "Low elevation pine", aspect_degrees = 38.8, elevation_ft = 2782, slope_pct = 17.4, hr1 = 0.3, litter_depth_ref = 1.5, hr10 = 1.5, duff_depth_ref = 1.7, hr100 = 2.3, fuel_height_ref = 3.1, fine_woody = 4.1, coarse_woody = 3.1, shrub_cover_pct = 0.0, total_woody = 7.2, vegetation_lt1ft_pct = 83.0),
  tibble(photo_num = 66, ecozone = "Acidic cove", aspect_degrees = 318.8, elevation_ft = 3442, slope_pct = 4.7, hr1 = 0.7, litter_depth_ref = 2.5, hr10 = 0.9, duff_depth_ref = 2.4, hr100 = 4.7, fuel_height_ref = 5.7, fine_woody = 6.3, coarse_woody = 3.1, shrub_cover_pct = 32.6, total_woody = 9.4, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 67, ecozone = "Acidic cove", aspect_degrees = 17.6, elevation_ft = 3555, slope_pct = 24.1, hr1 = 0.2, litter_depth_ref = 2.7, hr10 = 0.6, duff_depth_ref = 1.2, hr100 = 0.0, fuel_height_ref = 3.9, fine_woody = 0.8, coarse_woody = 6.0, shrub_cover_pct = 0.0, total_woody = 6.8, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 68, ecozone = "Northern hardwood cove", aspect_degrees = 28.9, elevation_ft = 4031, slope_pct = 21.1, hr1 = 0.2, litter_depth_ref = 2.7, hr10 = 0.3, duff_depth_ref = 2.7, hr100 = 0.6, fuel_height_ref = 2.2, fine_woody = 1.1, coarse_woody = 5.7, shrub_cover_pct = 0.0, total_woody = 6.8, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 69, ecozone = "Acidic cove", aspect_degrees = 317.9, elevation_ft = 4043, slope_pct = 7.9, hr1 = 0.4, litter_depth_ref = 2.6, hr10 = 0.2, duff_depth_ref = 2.3, hr100 = 1.4, fuel_height_ref = 4.2, fine_woody = 2.0, coarse_woody = 1.6, shrub_cover_pct = 29.3, total_woody = 3.6, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 70, ecozone = "Montane oak-hickory slope", aspect_degrees = 25.6, elevation_ft = 4138, slope_pct = 23.0, hr1 = 0.6, litter_depth_ref = 2.1, hr10 = 1.3, duff_depth_ref = 1.6, hr100 = 1.2, fuel_height_ref = 4.2, fine_woody = 3.1, coarse_woody = 9.0, shrub_cover_pct = 0.0, total_woody = 12.1, vegetation_lt1ft_pct = 0.0),
  tibble(photo_num = 71, ecozone = "Acidic cove", aspect_degrees = 355.7, elevation_ft = 3638, slope_pct = 14.1, hr1 = 0.4, litter_depth_ref = 1.8, hr10 = 2.1, duff_depth_ref = 2.1, hr100 = 1.2, fuel_height_ref = 5.6, fine_woody = 3.7, coarse_woody = 1.0, shrub_cover_pct = 0.0, total_woody = 4.7, vegetation_lt1ft_pct = 3.0),
  tibble(photo_num = 72, ecozone = "Acidic cove", aspect_degrees = 26.4, elevation_ft = 3683, slope_pct = 18.1, hr1 = 0.5, litter_depth_ref = 1.9, hr10 = 1.1, duff_depth_ref = 1.6, hr100 = 2.4, fuel_height_ref = 3.6, fine_woody = 4.0, coarse_woody = 0.5, shrub_cover_pct = 74.2, total_woody = 4.5, vegetation_lt1ft_pct = 7.0),
  tibble(photo_num = 73, ecozone = "Rich cove", aspect_degrees = 37.7, elevation_ft = 3867, slope_pct = 31.2, hr1 = 0.4, litter_depth_ref = 1.7, hr10 = 1.3, duff_depth_ref = 1.5, hr100 = 3.5, fuel_height_ref = 3.5, fine_woody = 5.2, coarse_woody = 17.3, shrub_cover_pct = 0.0, total_woody = 22.5, vegetation_lt1ft_pct = 1.0),
  tibble(photo_num = 74, ecozone = "Northern hardwood slope", aspect_degrees = 356.4, elevation_ft = 5314, slope_pct = 24.6, hr1 = 0.3, litter_depth_ref = 1.8, hr10 = 1.2, duff_depth_ref = 2.5, hr100 = 3.5, fuel_height_ref = 13.5, fine_woody = 5.0, coarse_woody = 7.8, shrub_cover_pct = 0.0, total_woody = 12.8, vegetation_lt1ft_pct = 1.0)
)

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
  
  map_dfr(seq_len(nrow(group_specs)), function(i) {
    g <- group_specs[i, ]
    tibble(
      photo_num = seq(g$photo_start, g$photo_end),
      aspect_band = g$aspect_band,
      elevation_band = g$elevation_band
    )
  }) %>%
    left_join(photo_guide_reference_data, by = "photo_num") %>%
    mutate(
      photo_id = sprintf("P%02d", photo_num),
      site_type = paste("PHOTO", photo_num),
      vegetation_type = ecozone,
      litter_factor = LITTER_TONS_PER_ACRE_PER_INCH,
      duff_factor = DUFF_TONS_PER_ACRE_PER_INCH,
      image_url = map2_chr(photo_num, elevation_band, photo_image_url),
      local_path = ifelse(is.na(image_url), NA_character_, file.path("www", image_url)),
      image_exists = !is.na(local_path) & file.exists(local_path)
    ) %>%
    select(
      photo_id, photo_num, site_type, ecozone,
      elevation_band, aspect_band, vegetation_type,
      litter_factor, duff_factor,
      hr1, hr10, hr100,
      litter_depth_ref, duff_depth_ref, fuel_height_ref,
      fine_woody, coarse_woody, total_woody,
      aspect_degrees, elevation_ft, slope_pct,
      shrub_cover_pct, vegetation_lt1ft_pct,
      image_url, local_path, image_exists
    )
}

photo_guide_options <- build_photo_guide_options()
photo_source_note <- sprintf(
  "Mapped %d guide photos; found %d local PNG files in /www.",
  nrow(photo_guide_options),
  sum(photo_guide_options$image_exists)
)

ui <- page_sidebar(
  title = div(
    class = "app-title-wrap",
    span(class = "app-title-main", "Smoke Plume Prediction Studio"),
    span(class = "app-title-sub", "VSMOKE-closer prescribed fire screening view")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
  ),
  sidebar = sidebar(
    width = 340,
    
    div(class = "section-heading", "Location"),
    numericInput("latitude", "latitude:", value = 40.7128, min = -90, max = 90, step = 0.0001),
    numericInput("longitude", "longitude:", value = -74.0060, min = -180, max = 180, step = 0.0001),
    
    div(class = "section-heading", "Burn Characteristics"),
    numericInput("acres", "Burn size (acres):", value = 100, min = 1, max = 50000, step = 1),
    numericInput("duration", "Burn duration (hours):", value = 4, min = 0.25, max = 48, step = 0.25),
    selectInput(
      "fuel_type", "Fuel type:",
      choices = c("Grass", "Shrub", "Hardwood Litter", "Conifer Litter", "Logging Slash", "Heavy Fuels"),
      selected = "Hardwood Litter"
    ),
    selectInput(
      "fuel_load_source",
      "Fuel loading source:",
      choices = c(
        "Manual entry" = "manual",
        "Use Duff/Litter total" = "calculator_total",
        "Use AI photo match total" = "ai_total"
      ),
      selected = "manual"
    ),
    uiOutput("fuel_load_ui"),
    sliderInput("consumed_fraction", "Fuel consumed fraction", min = 0.05, max = 1.00, value = 0.30, step = 0.05),
    selectInput(
      "ignition_method",
      "Ignition method:",
      choices = c("Backing/Spot", "Head/Aerial"),
      selected = "Backing/Spot"
    ),
    
    div(class = "section-heading", "Weather Conditions"),
    numericInput("wind_speed", "Transport wind speed (m/s):", value = 5, min = 0.5, max = 25, step = 0.5),
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
    selectInput(
      "fuel_moisture",
      "Surrounding fuels / duff condition:",
      choices = c("Very Dry", "Dry", "Moderate", "Moist", "Very Moist"),
      selected = "Moderate"
    ),
    
    div(class = "section-heading", "VSMOKE-Style Controls"),
    sliderInput("plume_rise_fraction", "Plume rise fraction (-1 to 1)", min = -1.0, max = 1.0, value = -0.5, step = 0.05),
    sliderInput("flaming_fraction", "Flaming emission fraction", min = 0.30, max = 0.90, value = 0.65, step = 0.05),
    sliderInput("flaming_duration_fraction", "Fraction of burn duration dominated by flaming", min = 0.10, max = 0.80, value = 0.30, step = 0.05),
    sliderInput("smoke_opacity", "Smoke layer transparency", min = 0.10, max = 0.70, value = 0.30, step = 0.02),
    checkboxInput("show_good", "Show low-concentration 'Good' smoke on map", value = FALSE),
    
    br(),
    actionButton("predict", "Generate Smoke Prediction", class = "btn-primary btn-lg", style = "width: 100%;", onclick = "var el=document.getElementById('prediction-loading-overlay'); if (el) el.style.display='flex';")
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
        font-size:0.84rem; letter-spacing:0.04em; text-transform:uppercase;
        font-weight:700; color:#567; margin-top:0.9rem; margin-bottom:0.5rem;
      }
      .calc-note { color:#5c6d7a; font-size:0.92rem; }
      .photo-grid {
        display:grid;
        grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
        gap:12px;
        margin-top:0.75rem;
      }
      .photo-card {
        border:1px solid #d8e2e8;
        border-radius:12px;
        background:#fff;
        overflow:hidden;
        box-shadow:0 2px 6px rgba(0,0,0,0.06);
        padding: 0;
        height: 100%;
      }
      .photo-thumb {
        width:100%;
        height:190px;
        object-fit:cover;
        display:block;
        cursor:zoom-in;
      }
      .photo-caption {
        padding:0.5rem;
        font-size:0.8rem;
        color:#334155;
        font-weight:600;
      }
      .selected-photo-wrap, .preview-card {
        margin-top:0.75rem;
        background:#ffffff;
        border:1px solid #d9d9d9;
        border-radius:12px;
        padding:14px;
        box-shadow:0 2px 6px rgba(0,0,0,0.05);
      }
      .selected-photo-wrap img, .preview-card img {
        width:100%;
        max-height:420px;
        object-fit:contain;
        border:1px solid #d8e2e8;
        border-radius:10px;
        display:block;
        background:#fff;
      }
      .warn-missing { color:#b91c1c; font-weight:800; margin-top:0.5rem; }
      .result-panel {
        border-radius:10px;
        padding:18px;
        color:white;
        font-weight:600;
        min-height:160px;
        margin-bottom:14px;
        white-space:pre-line;
        display:flex;
        flex-direction:column;
        justify-content:center;
      }
      .result-dark { background:#227b68; }
      .result-light { background:#58c49a; }
      .result-gold { background:#f0c35a; color:#274060; }
      .result-box {
        background:#f8f9fa;
        border:1px solid #dee2e6;
        border-radius:10px;
        padding:12px;
        white-space:pre-wrap;
      }
      .ai-two-col {
        display:grid;
        grid-template-columns:380px 1fr;
        gap:18px;
        align-items:start;
      }
      .match-box-photo-wrap {
        width:100%;
        text-align:center;
        background:transparent;
      }
      .match-box-photo {
        width:auto;
        max-width:100%;
        max-height:320px;
        height:auto;
        object-fit:contain;
        border-radius:10px;
        background:transparent;
        border:none;
        cursor:zoom-in;
        display:inline-block;
      }
      .gallery-search-wrap {
        max-width: 420px;
        margin-bottom: 12px;
      }
      .ai-thumb-small {
        width:42px;
        height:42px;
        object-fit:cover;
        border-radius:8px;
        border:1px solid #d8e2e8;
        display:block;
        margin:auto;
      }
      .loading-overlay {
        position: fixed;
        inset: 0;
        background: rgba(255,255,255,0.45);
        display: none;
        align-items: center;
        justify-content: center;
        z-index: 3000;
        pointer-events: all;
      }
      .loading-overlay-card {
        min-width: 260px;
        max-width: 360px;
        text-align: center;
        background: rgba(255,255,255,0.96);
        border: 1px solid #d8e2e8;
        border-radius: 18px;
        padding: 20px 24px;
        box-shadow: 0 10px 28px rgba(0,0,0,0.14);
      }
      .loading-overlay-gif {
        width: 96px;
        height: 96px;
        object-fit: contain;
        display: block;
        margin: 0 auto 12px auto;
      }
      .loading-overlay-text {
        color: #1f2937;
        font-weight: 700;
        font-size: 1rem;
        line-height: 1.35;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('show_prediction_loader', function(message) {
        var el = document.getElementById('prediction-loading-overlay');
        if (el) el.style.display = 'flex';
      });
      Shiny.addCustomMessageHandler('hide_prediction_loader', function(message) {
        var el = document.getElementById('prediction-loading-overlay');
        if (el) el.style.display = 'none';
      });
      Shiny.addCustomMessageHandler('show_ai_loader', function(message) {
        var el = document.getElementById('ai-loading-overlay');
        if (el) el.style.display = 'flex';
      });
      Shiny.addCustomMessageHandler('hide_ai_loader', function(message) {
        var el = document.getElementById('ai-loading-overlay');
        if (el) el.style.display = 'none';
      });
    "))
  ),
  
  navset_card_tab(
    nav_panel(
      "Smoke Plume Map",
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            
            div(
              class = "result-panel result-dark",
              tags$div("Prediction Status", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("status_text")
            ),
            
            div(
              class = "result-panel result-light",
              tags$div("Current Fuel Option", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("selected_fuel_text")
            ),
            
            div(
              class = "result-panel result-gold",
              tags$div("Wind Profile", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("wind_profile_text")
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
      "Duff/Litter Calculator",
      layout_columns(
        col_widths = c(4, 8),
        
        card(
          full_screen = TRUE,
          card_header("Select Vegetation Type + Photo"),
          div(class = "calc-note", textOutput("photo_source_text")),
          p(
            class = "calc-note",
            "Choose a vegetation type first. Then the photo list will filter to matching guide photos only."
          ),
          
          selectInput(
            "manual_vegetation_type",
            "Vegetation type:",
            choices = sort(unique(photo_guide_options$vegetation_type)),
            selected = sort(unique(photo_guide_options$vegetation_type))[1]
          ),
          
          numericInput("litter_depth", "Litter depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1),
          numericInput("duff_depth", "Duff depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1),
          
          uiOutput("manual_photo_select_ui"),
          uiOutput("selected_photo_view"),
          
          br(),
          h5("Filtered Photo Gallery"),
          tags$p(class = "calc-note", "Only photos matching the selected vegetation type are shown below."),
          uiOutput("photo_gallery")
        ),
        
        card(
          full_screen = TRUE,
          card_header("Estimated Fuel Loading"),
          p(class = "calc-note", "The selected photo supplies the pre-determined woody debris loads. Your entered litter and duff depths are converted using 1.38 tons/acre/inch for litter and 4.84 tons/acre/inch for duff, then combined into the fuel loading source total."),
          div(class = "result-panel result-light",
              tags$div("Litter mass", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("litter_mass_text")
          ),
          div(class = "result-panel result-gold",
              tags$div("Duff mass", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("duff_mass_text")
          ),
          div(class = "result-panel result-dark",
              tags$div("Total fuel loading source", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("total_mass_text")
          ),
          div(class = "result-panel result-light",
              tags$div("Fine woody debris (1+10+100-hr)", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("fine_woody_text")
          ),
          div(class = "result-panel result-gold",
              tags$div("Coarse woody debris (1,000-hr)", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("coarse_woody_text")
          ),
          div(class = "result-panel result-dark",
              tags$div("Total woody fuels", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("total_woody_text")
          ),
          DTOutput("photo_factor_table")
        )
      )
    ),
    
    nav_panel(
      "AI Photo Match Calculator",
      div(
        class = "ai-two-col",
        
        div(
          class = "preview-card",
          tags$h4("Upload a Photo to Match"),
          p("Upload a forest fuel / litter photo. OpenAI will compare it to the guide photos in your www folder and return the closest match. The matched photo supplies the pre-determined woody debris loads, and your litter/duff depths are converted using 1.38 and 4.84 tons/acre/inch."),
          fileInput(
            "ai_photo_upload",
            "Upload photo",
            accept = c(".png", ".jpg", ".jpeg", ".webp")
          ),
          numericInput("ai_litter_depth", "Litter depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1),
          numericInput("ai_duff_depth", "Duff depth (inches)", value = 1.0, min = 0, max = 12, step = 0.1),
          checkboxInput("show_ai_recommendation", "Show recommendation", value = FALSE),
          actionButton(
            "match_uploaded_photo",
            "Find Closest Guide Photo",
            class = "btn-primary",
            style = "width: 100%;",
            onclick = "var el=document.getElementById('ai-loading-overlay'); if (el) el.style.display='flex';"
          ),
          br(), br(),
          textOutput("ai_match_status"),
          uiOutput("uploaded_photo_preview")
        ),
        
        div(
          class = "preview-card",
          tags$h4("Best Match Result"),
          
          div(
            class = "result-panel result-dark",
            tags$div("Best Match", style = "font-weight:700; margin-bottom:12px;"),
            uiOutput("ai_best_match_box")
          ),
          
          div(class = "result-panel result-light",
              tags$div("Litter mass", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("ai_litter_mass_text")
          ),
          div(class = "result-panel result-gold",
              tags$div("Duff mass", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("ai_duff_mass_text")
          ),
          div(class = "result-panel result-dark",
              tags$div("Total fuel loading source", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("ai_total_mass_text")
          ),
          div(class = "result-panel result-light",
              tags$div("Fine woody debris (1+10+100-hr)", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("ai_fine_woody_text")
          ),
          div(class = "result-panel result-gold",
              tags$div("Coarse woody debris (1,000-hr)", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("ai_coarse_woody_text")
          ),
          div(class = "result-panel result-dark",
              tags$div("Total woody fuels", style = "font-weight:700; margin-bottom:10px;"),
              textOutput("ai_total_woody_text")
          ),
          
          conditionalPanel(
            condition = "input.show_ai_recommendation == true",
            tags$h4("Recommendation"),
            div(class = "result-box", textOutput("ai_recommendation"))
          ),
          
          br(),
          tags$h4("Top Matches"),
          DTOutput("ai_match_table")
        )
      )
    ),
    
    nav_panel(
      "Full Photo Gallery",
      h4("Full Guide Photo Gallery"),
      div(class = "calc-note", textOutput("photo_source_text")),
      div(
        class = "gallery-search-wrap",
        textInput("full_gallery_search", "Search photos", placeholder = "Search by photo ID, ecozone, vegetation, elevation, aspect, or site type")
      ),
      uiOutput("full_photo_gallery")
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
          tags$li("AQI mapping uses raw concentration instead of broad smoothing."),
          tags$li("The map now appears immediately, even before you generate a prediction."),
          tags$li("Clicking the map sets the burn location and updates the latitude/longitude inputs."),
          tags$li("The AI Photo Match Calculator uses GPT-5.4 mini with direct API requests."),
          tags$li("The AI button crash from validate() inside observeEvent is fixed."),
          tags$li("The full photo gallery is now on its own tab and has a search bar."),
          tags$li("The Best Match photo appears inside the green Best Match box."),
          tags$li("The separate Best Match Photo section at the bottom was removed."),
          tags$li("Recommendation can be turned on or off in the AI tab."),
          tags$li("Top Matches now include Elevation, Aspect, and a small photo thumbnail next to Similarity."),
          tags$li("White side bars around the best-match image were removed by centering the image without a white photo frame."),
          tags$li("All result boxes now use even heights."),
          tags$li("Photo viewing now happens inside the app with a modal preview.")
        ),
        tags$h5("Important note:"),
        tags$p("This is a closer screening depiction, not a regulatory or exact VSMOKE clone.")
      )
    )
  )
  ,div(
    id = "prediction-loading-overlay",
    class = "loading-overlay",
    div(
      class = "loading-overlay-card",
      tags$img(src = "Loading Circle Animation.gif", class = "loading-overlay-gif"),
      div(class = "loading-overlay-text", "Generating smoke prediction...")
    )
  )
  ,div(
    id = "ai-loading-overlay",
    class = "loading-overlay",
    div(
      class = "loading-overlay-card",
      tags$img(src = "Loading Circle Animation.gif", class = "loading-overlay-gif"),
      div(class = "loading-overlay-text", "Matching photo and building AI results...")
    )
  )
)

server <- function(input, output, session) {
  session$sendCustomMessage("hide_prediction_loader", list())
  session$sendCustomMessage("hide_ai_loader", list())
  
  values <- reactiveValues(
    prediction_data = NULL,
    burn_lat = 40.7128,
    burn_lon = -74.0060,
    status = "Map ready. Click the map or enter coordinates, then generate prediction.",
    is_generating = FALSE
  )
  
  ai_match_values <- reactiveValues(
    matches = NULL,
    recommendation = NULL,
    status = "Upload a photo, then click 'Find Closest Guide Photo'.",
    is_generating = FALSE
  )
  
  observe({
    if (input$ignition_method == "Backing/Spot" && isTRUE(all.equal(input$plume_rise_fraction, 0.75))) {
      updateSliderInput(session, "plume_rise_fraction", value = -0.5)
    }
  })
  
  observeEvent(list(input$latitude, input$longitude), {
    req(input$latitude, input$longitude)
    values$burn_lat <- input$latitude
    values$burn_lon <- input$longitude
  }, ignoreInit = FALSE)
  
  observeEvent(input$smoke_map_click, {
    click <- input$smoke_map_click
    req(click$lat, click$lng)
    
    values$burn_lat <- click$lat
    values$burn_lon <- click$lng
    values$prediction_data <- NULL
    
    updateNumericInput(session, "latitude", value = round(click$lat, 6))
    updateNumericInput(session, "longitude", value = round(click$lng, 6))
    
    showNotification(
      paste0("Burn location updated to ", round(click$lat, 6), ", ", round(click$lng, 6)),
      type = "message",
      duration = 3
    )
    
    values$status <- "Map ready. Click the map or enter coordinates, then generate prediction."
  })
  
  output$status_text <- renderText(values$status)
  output$prediction_loading_ui <- renderUI({
    if (!isTRUE(values$is_generating)) return(NULL)
    div(
      class = "loading-overlay",
      div(
        class = "loading-overlay-card",
        tags$img(src = "Loading Circle Animation.gif", class = "loading-overlay-gif"),
        div(class = "loading-overlay-text", "Generating smoke prediction...")
      )
    )
  })
  output$ai_loading_ui <- renderUI({
    if (!isTRUE(ai_match_values$is_generating)) return(NULL)
    div(
      class = "loading-overlay",
      div(
        class = "loading-overlay-card",
        tags$img(src = "Loading Circle Animation.gif", class = "loading-overlay-gif"),
        div(class = "loading-overlay-text", "Matching photo and building AI results...")
      )
    )
  })
  output$selected_fuel_text <- renderText(
    paste(input$fuel_type, "|", round(effective_fuel_load(), 2), "tons/acre |", fuel_load_source_label(), "|", round(100 * input$consumed_fraction), "% consumed")
  )
  output$wind_profile_text <- renderText(
    paste0(
      input$wind_speed, " m/s toward ", input$wind_direction,
      " | MH: ", input$mixing_height, " m",
      " | Fuels: ", input$fuel_moisture
    )
  )
  output$photo_source_text <- renderText(photo_source_note)
  
  output$fuel_load_ui <- renderUI({
    if (identical(input$fuel_load_source, "manual")) {
      return(numericInput("fuel_load", "Fuel loading (tons/acre):", value = 5, min = 0.1, max = 100, step = 0.1))
    }
    
    if (identical(input$fuel_load_source, "calculator_total")) {
      calc <- tryCatch(photo_calc(), error = function(e) NULL)
      if (is.null(calc) || is.null(calc$total_mass) || is.na(calc$total_mass)) {
        return(div(class = "calc-note", tags$strong("Calculator fuel loading source total unavailable."), " Select a calculator photo first."))
      }
      return(div(class = "calc-note", tags$strong("Using calculator fuel loading source total: "), paste0(round(calc$total_mass, 2), " tons/acre")))
    }
    
    calc <- tryCatch(ai_photo_calc(), error = function(e) NULL)
    if (is.null(calc) || is.null(calc$total_mass) || is.na(calc$total_mass)) {
      return(div(class = "calc-note", tags$strong("AI photo match fuel loading source total unavailable."), " Run the AI photo match first."))
    }
    div(class = "calc-note", tags$strong("Using AI photo match fuel loading source total: "), paste0(round(calc$total_mass, 2), " tons/acre"))
  })
  
  effective_fuel_load <- reactive({
    if (identical(input$fuel_load_source, "calculator_total")) {
      calc <- photo_calc()
      req(!is.null(calc$total_mass), !is.na(calc$total_mass))
      return(calc$total_mass)
    }
    if (identical(input$fuel_load_source, "ai_total")) {
      calc <- ai_photo_calc()
      req(!is.null(calc$total_mass), !is.na(calc$total_mass))
      return(calc$total_mass)
    }
    req(input$fuel_load)
    input$fuel_load
  })
  
  fuel_load_source_label <- reactive({
    if (identical(input$fuel_load_source, "calculator_total")) {
      return("Calculator fuel loading source total")
    }
    if (identical(input$fuel_load_source, "ai_total")) {
      return("AI photo match fuel loading source total")
    }
    "Manual"
  })
  
  manual_filtered <- reactive({
    photo_guide_options %>%
      filter(vegetation_type == input$manual_vegetation_type)
  })
  
  output$manual_photo_select_ui <- renderUI({
    filtered <- manual_filtered()
    selectInput(
      "manual_photo_id",
      "Choose photo:",
      choices = setNames(filtered$photo_id, paste(filtered$photo_id, "-", filtered$site_type, "(", filtered$ecozone, ")")),
      selected = filtered$photo_id[[1]]
    )
  })
  
  selected_photo <- reactive({
    req(input$manual_photo_id)
    photo_guide_options %>% filter(photo_id == input$manual_photo_id)
  })
  
  output$photo_gallery <- renderUI({
    filtered <- manual_filtered()
    
    div(
      class = "photo-grid",
      lapply(seq_len(nrow(filtered)), function(i) {
        img_id <- paste0("manual_gallery_img_", i)
        
        div(
          class = "photo-card",
          tags$img(
            src = filtered$image_url[i],
            id = img_id,
            class = "photo-thumb",
            alt = paste("Photo guide", filtered$photo_id[i])
          ),
          div(
            class = "photo-caption",
            paste(filtered$photo_id[i], "-", filtered$site_type[i])
          ),
          tags$script(HTML(sprintf(
            "document.getElementById('%s').onclick = function() {
               Shiny.setInputValue('selected_gallery_image', %s, {priority: 'event'});
             };",
            img_id,
            jsonlite::toJSON(list(
              src = filtered$image_url[i],
              label = paste(filtered$photo_id[i], "-", filtered$site_type[i]),
              photo_id = filtered$photo_id[i],
              ecozone = filtered$ecozone[i]
            ), auto_unbox = TRUE)
          )))
        )
      })
    )
  })
  
  full_gallery_filtered <- reactive({
    q <- trimws(tolower(input$full_gallery_search %||% ""))
    
    df <- photo_guide_options
    if (!nzchar(q)) return(df)
    
    df %>%
      filter(
        str_detect(tolower(photo_id), fixed(q)) |
          str_detect(tolower(site_type), fixed(q)) |
          str_detect(tolower(ecozone), fixed(q)) |
          str_detect(tolower(vegetation_type), fixed(q)) |
          str_detect(tolower(elevation_band), fixed(q)) |
          str_detect(tolower(aspect_band), fixed(q))
      )
  })
  
  output$full_photo_gallery <- renderUI({
    all_photos <- full_gallery_filtered()
    
    if (nrow(all_photos) == 0) {
      return(div(class = "warn-missing", "No photos match your search."))
    }
    
    div(
      class = "photo-grid",
      lapply(seq_len(nrow(all_photos)), function(i) {
        img_id <- paste0("full_gallery_img_", i)
        
        div(
          class = "photo-card",
          tags$img(
            src = all_photos$image_url[i],
            id = img_id,
            class = "photo-thumb",
            alt = paste("Photo guide", all_photos$photo_id[i])
          ),
          div(
            class = "photo-caption",
            paste(all_photos$photo_id[i], "-", all_photos$site_type[i])
          ),
          tags$script(HTML(sprintf(
            "document.getElementById('%s').onclick = function() {
               Shiny.setInputValue('selected_gallery_image', %s, {priority: 'event'});
             };",
            img_id,
            jsonlite::toJSON(list(
              src = all_photos$image_url[i],
              label = paste(all_photos$photo_id[i], "-", all_photos$site_type[i]),
              photo_id = all_photos$photo_id[i],
              ecozone = all_photos$ecozone[i]
            ), auto_unbox = TRUE)
          )))
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
      tags$img(
        src = img_rel,
        id = "selected_manual_photo",
        class = "photo-thumb",
        style = "height:auto; max-height:420px; object-fit:contain;",
        alt = paste("Selected", opt$photo_id[[1]])
      ),
      div(
        class = "photo-caption",
        paste(
          opt$photo_id[[1]], "-",
          opt$site_type[[1]], "|",
          opt$vegetation_type[[1]]
        )
      ),
      tags$script(HTML(sprintf(
        "document.getElementById('%s').onclick = function() {
           Shiny.setInputValue('selected_gallery_image', %s, {priority: 'event'});
         };",
        "selected_manual_photo",
        jsonlite::toJSON(list(
          src = img_rel,
          label = paste(opt$photo_id[[1]], "-", opt$site_type[[1]]),
          photo_id = opt$photo_id[[1]],
          ecozone = opt$ecozone[[1]]
        ), auto_unbox = TRUE)
      )))
    )
  })
  
  photo_calc <- reactive({
    opt <- selected_photo()
    req(nrow(opt) == 1)
    
    litter_factor <- LITTER_TONS_PER_ACRE_PER_INCH
    duff_factor <- DUFF_TONS_PER_ACRE_PER_INCH
    
    litter_mass <- input$litter_depth * litter_factor
    duff_mass <- input$duff_depth * duff_factor
    litter_duff_total <- litter_mass + duff_mass
    woody_total <- opt$total_woody[[1]]
    
    list(
      litter_mass = litter_mass,
      duff_mass = duff_mass,
      litter_duff_total = litter_duff_total,
      total_mass = litter_duff_total + woody_total,
      option = opt,
      litter_factor = litter_factor,
      duff_factor = duff_factor,
      fine_woody = opt$fine_woody[[1]],
      coarse_woody = opt$coarse_woody[[1]],
      total_woody = woody_total,
      ref_litter_depth = opt$litter_depth_ref[[1]],
      ref_duff_depth = opt$duff_depth_ref[[1]],
      ref_fuel_height = opt$fuel_height_ref[[1]],
      hr1 = opt$hr1[[1]],
      hr10 = opt$hr10[[1]],
      hr100 = opt$hr100[[1]]
    )
  })
  
  output$litter_mass_text <- renderText(paste0(round(photo_calc()$litter_mass, 2), " tons/acre"))
  output$duff_mass_text   <- renderText(paste0(round(photo_calc()$duff_mass, 2), " tons/acre"))
  output$total_mass_text  <- renderText(paste0(round(photo_calc()$total_mass, 2), " tons/acre"))
  output$fine_woody_text  <- renderText(paste0(round(photo_calc()$fine_woody, 2), " tons/acre"))
  output$coarse_woody_text <- renderText(paste0(round(photo_calc()$coarse_woody, 2), " tons/acre"))
  output$total_woody_text <- renderText(paste0(round(photo_calc()$total_woody, 2), " tons/acre"))
  
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
      `Custom litter + duff subtotal` = photo_calc()$litter_duff_total,
      `Guide litter depth (in)` = photo_calc()$ref_litter_depth,
      `Guide duff depth (in)` = photo_calc()$ref_duff_depth,
      `Guide fuel height (in)` = photo_calc()$ref_fuel_height,
      `1-hr woody` = photo_calc()$hr1,
      `10-hr woody` = photo_calc()$hr10,
      `100-hr woody` = photo_calc()$hr100,
      `Fine woody (1+10+100-hr)` = photo_calc()$fine_woody,
      `Coarse woody (1,000-hr)` = photo_calc()$coarse_woody,
      `Total woody fuels` = photo_calc()$total_woody,
      `Fuel loading source total` = photo_calc()$total_mass,
      `Image path` = opt$image_url
    )
    
    datatable(table_data, options = list(scrollX = TRUE, dom = "t"), rownames = FALSE)
  })
  
  show_photo_modal <- function(photo_id, src, label = NULL, ecozone = NULL) {
    showModal(
      modalDialog(
        title = paste("Photo Preview -", photo_id),
        easyClose = TRUE,
        size = "l",
        footer = NULL,
        tags$div(
          style = "text-align:center;",
          tags$img(
            src = src,
            style = "max-width:100%; max-height:80vh; border-radius:10px;"
          ),
          tags$hr(),
          if (!is.null(label) && nzchar(label)) tags$p(strong("Label: "), label),
          if (!is.null(ecozone) && nzchar(ecozone)) tags$p(strong("Ecozone: "), ecozone)
        )
      )
    )
  }
  
  observeEvent(input$selected_gallery_image, {
    info <- input$selected_gallery_image
    req(info)
    show_photo_modal(
      photo_id = info$photo_id,
      src = info$src,
      label = info$label,
      ecozone = info$ecozone
    )
  })
  
  observeEvent(input$ai_match_photo_click, {
    req(ai_match_values$matches)
    idx <- suppressWarnings(as.integer(input$ai_match_photo_click))
    req(!is.na(idx), idx >= 1, idx <= nrow(ai_match_values$matches))
    
    matches <- ai_match_values$matches %>%
      coalesce_match_metadata(photo_guide_options) %>%
      normalize_ai_matches()
    
    req(nrow(matches) >= idx)
    photo_row <- matches[idx, ]
    req(!is.na(photo_row$image_url), nzchar(photo_row$image_url))
    
    show_photo_modal(
      photo_id = photo_row$photo_id,
      src = photo_row$image_url,
      label = paste(photo_row$site_type, "-", photo_row$vegetation_type),
      ecozone = photo_row$ecozone
    )
  })
  
  output$ai_match_status <- renderText({
    ai_match_values$status
  })
  
  output$uploaded_photo_preview <- renderUI({
    req(input$ai_photo_upload)
    
    ext <- tolower(tools::file_ext(input$ai_photo_upload$name))
    mime <- if (ext %in% c("jpg", "jpeg")) {
      "image/jpeg"
    } else if (ext == "webp") {
      "image/webp"
    } else {
      "image/png"
    }
    
    img_src <- base64enc::dataURI(file = input$ai_photo_upload$datapath, mime = mime)
    
    div(
      class = "preview-card",
      tags$h4("Uploaded Photo"),
      tags$img(
        src = img_src,
        style = "max-width:100%; border:1px solid #d8e2e8; border-radius:0.6rem;"
      )
    )
  })
  
  observeEvent(input$match_uploaded_photo, {
    if (is.null(input$ai_photo_upload)) {
      session$sendCustomMessage("hide_ai_loader", list())
      showNotification("Upload a photo first.", type = "message")
      return(NULL)
    }
    ai_match_values$is_generating <- TRUE
    on.exit({
      ai_match_values$is_generating <- FALSE
      session$sendCustomMessage("hide_ai_loader", list())
    }, add = TRUE)
    
    ref_df <- photo_guide_options %>%
      filter(image_exists, !is.na(local_path), file.exists(local_path))
    
    if (nrow(ref_df) == 0) {
      ai_match_values$status <- "No guide photos were found in the www folder."
      ai_match_values$matches <- NULL
      ai_match_values$recommendation <- "No recommendation available."
      showNotification("No guide photos were found in the www folder.", type = "error")
      return(NULL)
    }
    
    if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
      ai_match_values$status <- "OPENAI_API_KEY is not set."
      ai_match_values$matches <- NULL
      ai_match_values$recommendation <- "No recommendation available."
      showNotification("OPENAI_API_KEY is not set.", type = "error")
      return(NULL)
    }
    
    ai_match_values$status <- "Matching uploaded image with OpenAI."
    ai_match_values$matches <- NULL
    ai_match_values$recommendation <- "Waiting for match results."
    
    local_candidates <- tryCatch(
      find_candidate_matches_local(
        upload_path = input$ai_photo_upload$datapath,
        reference_df = ref_df,
        top_n = 5
      ),
      error = function(e) tibble::tibble()
    )
    
    tryCatch({
      matches <- find_top_matches_openai(
        upload_path = input$ai_photo_upload$datapath,
        reference_df = ref_df,
        model = "gpt-5.4-mini",
        top_n = 5,
        candidate_limit = 5
      )
      
      matches <- matches %>%
        coalesce_match_metadata(photo_guide_options) %>%
        normalize_ai_matches()
      
      if (is.null(matches) || nrow(matches) == 0) {
        stop("OpenAI returned no matches.")
      }
      
      ai_match_values$matches <- matches
      ai_match_values$status <- "OpenAI match complete."
      
      ai_match_values$recommendation <- tryCatch(
        ask_openai_assistant(
          prompt = "Match this uploaded forest fuel photo to the closest guide photo and explain why.",
          top_matches = matches,
          model = "gpt-5.4-mini"
        ),
        error = function(e) paste("OpenAI recommendation error:", e$message)
      )
      
    }, error = function(e) {
      err_msg <- as.character(e$message)
      
      if (grepl("\\[429\\]|rate limit", err_msg, ignore.case = TRUE) && nrow(local_candidates) > 0) {
        fallback_matches <- local_candidates %>%
          dplyr::arrange(prefilter_distance) %>%
          dplyr::slice_head(n = 5) %>%
          dplyr::mutate(
            similarity = NA_real_,
            openai_similarity = NA_real_,
            openai_reason = paste0(
              "OpenAI API error: ", err_msg,
              " Local visual shortlist shown below while rate-limited."
            )
          ) %>%
          coalesce_match_metadata(photo_guide_options) %>%
          normalize_ai_matches()
        
        ai_match_values$matches <- fallback_matches
        ai_match_values$recommendation <- "Recommendation skipped because the OpenAI rate limit was reached. Local visual shortlist results are shown below and can still be used."
        ai_match_values$status <- "OpenAI rate-limited. Showing local visual shortlist."
      } else {
        ai_match_values$matches <- NULL
        ai_match_values$recommendation <- paste("OpenAI recommendation error:", err_msg)
        ai_match_values$status <- paste("OpenAI matching failed:", err_msg)
        showNotification(ai_match_values$status, type = "error")
      }
    })
  })
  
  best_ai_match <- reactive({
    matches <- ai_match_values$matches %>%
      coalesce_match_metadata(photo_guide_options) %>%
      normalize_ai_matches()
    req(nrow(matches) > 0)
    matches %>% dplyr::slice(1)
  })
  
  ai_photo_calc <- reactive({
    best <- best_ai_match()
    
    litter_factor <- LITTER_TONS_PER_ACRE_PER_INCH
    duff_factor <- DUFF_TONS_PER_ACRE_PER_INCH
    
    litter_mass <- input$ai_litter_depth * litter_factor
    duff_mass   <- input$ai_duff_depth * duff_factor
    litter_duff_total <- litter_mass + duff_mass
    woody_total <- safe_num1(best$total_woody)
    
    list(
      best = best,
      litter_mass = litter_mass,
      duff_mass = duff_mass,
      litter_duff_total = litter_duff_total,
      total_mass = litter_duff_total + woody_total,
      fine_woody = safe_num1(best$fine_woody),
      coarse_woody = safe_num1(best$coarse_woody),
      total_woody = woody_total,
      litter_factor = litter_factor,
      duff_factor = duff_factor,
      ref_litter_depth = safe_num1(best$litter_depth_ref),
      ref_duff_depth = safe_num1(best$duff_depth_ref),
      ref_fuel_height = safe_num1(best$fuel_height_ref),
      hr1 = safe_num1(best$hr1),
      hr10 = safe_num1(best$hr10),
      hr100 = safe_num1(best$hr100)
    )
  })
  
  output$ai_best_match_box <- renderUI({
    best <- best_ai_match()
    img_rel <- safe_chr1(best$image_url, "")
    reason_txt <- safe_chr1(best$openai_reason, "")
    
    if (!nzchar(img_rel)) {
      return(tags$div("Matched image unavailable."))
    }
    
    tags$div(
      tags$div(
        paste(safe_chr1(best$photo_id, "Unknown"), "-", safe_chr1(best$site_type, "Guide photo")),
        style = "margin-bottom:12px; font-weight:700;"
      ),
      tags$div(
        class = "match-box-photo-wrap",
        tags$img(
          src = img_rel,
          id = "ai_best_match_image_box",
          class = "match-box-photo",
          alt = safe_chr1(best$photo_id, "Matched photo")
        )
      ),
      if (nzchar(reason_txt)) {
        tags$div(
          style = paste(
            "margin-top:12px; padding:12px; border-radius:10px;",
            "background:rgba(255,255,255,0.14); color:white;",
            "line-height:1.45; font-weight:500;"
          ),
          tags$div("AI reason", style = "font-weight:700; margin-bottom:6px;"),
          reason_txt
        )
      },
      tags$script(HTML(sprintf(
        "document.getElementById('%s').onclick = function() {
           Shiny.setInputValue('selected_gallery_image', %s, {priority: 'event'});
         };",
        "ai_best_match_image_box",
        jsonlite::toJSON(list(
          src = img_rel,
          label = paste(safe_chr1(best$photo_id, "Unknown"), "-", safe_chr1(best$site_type, "Guide photo")),
          photo_id = safe_chr1(best$photo_id, "Unknown"),
          ecozone = safe_chr1(best$ecozone, "Unknown")
        ), auto_unbox = TRUE)
      )))
    )
  })
  
  output$ai_similarity_text <- renderText({
    best <- best_ai_match()
    sim_val <- safe_num1(best$similarity)
    reason_txt <- safe_chr1(best$openai_reason, "")
    if (is.na(sim_val) && grepl("Local visual shortlist", reason_txt, fixed = TRUE)) {
      "Local shortlist only"
    } else if (is.na(sim_val)) {
      "Similarity unavailable"
    } else {
      paste0(round(sim_val, 1), "%")
    }
  })
  
  output$ai_litter_mass_text <- renderText({
    calc <- ai_photo_calc()
    if (is.na(calc$litter_mass)) "Unavailable" else paste0(round(calc$litter_mass, 2), " tons/acre")
  })
  
  output$ai_duff_mass_text <- renderText({
    calc <- ai_photo_calc()
    if (is.na(calc$duff_mass)) "Unavailable" else paste0(round(calc$duff_mass, 2), " tons/acre")
  })
  
  output$ai_total_mass_text <- renderText({
    calc <- ai_photo_calc()
    if (is.na(calc$total_mass)) "Unavailable" else paste0(round(calc$total_mass, 2), " tons/acre")
  })
  
  output$ai_fine_woody_text <- renderText({
    calc <- ai_photo_calc()
    if (is.na(calc$fine_woody)) "Unavailable" else paste0(round(calc$fine_woody, 2), " tons/acre")
  })
  
  output$ai_coarse_woody_text <- renderText({
    calc <- ai_photo_calc()
    if (is.na(calc$coarse_woody)) "Unavailable" else paste0(round(calc$coarse_woody, 2), " tons/acre")
  })
  
  output$ai_total_woody_text <- renderText({
    calc <- ai_photo_calc()
    if (is.na(calc$total_woody)) "Unavailable" else paste0(round(calc$total_woody, 2), " tons/acre")
  })
  
  output$ai_recommendation <- renderText({
    if (is.null(ai_match_values$recommendation) || !nzchar(ai_match_values$recommendation)) {
      "No recommendation yet."
    } else {
      ai_match_values$recommendation
    }
  })
  
  output$ai_match_table <- renderDT({
    req(ai_match_values$matches)
    
    table_data <- ai_match_values$matches %>%
      coalesce_match_metadata(photo_guide_options) %>%
      normalize_ai_matches() %>%
      mutate(
        Photo = ifelse(
          is.na(image_url) | image_url == "",
          "",
          paste0(
            "<a href='#' class='ai-match-photo-link' data-row='", dplyr::row_number(), "' ",
            "onclick='Shiny.setInputValue(\"ai_match_photo_click\", this.dataset.row, {priority: \"event\"}); return false;'>",
            "<img src='", image_url, "' class='ai-thumb-small'></a>"
          )
        ),
        Similarity = ifelse(is.na(similarity), NA, round(similarity, 1))
      ) %>%
      transmute(
        Photo,
        `Photo ID` = photo_id,
        `Site Type` = site_type,
        Ecozone = ecozone,
        Elevation = elevation_band,
        Aspect = aspect_band,
        `Litter Factor` = litter_factor,
        `Duff Factor` = duff_factor,
        `Fine Woody` = fine_woody,
        `Coarse Woody` = coarse_woody,
        `Total Woody` = total_woody,
        `Similarity (%)` = Similarity,
        `OpenAI Reason` = openai_reason
      )
    
    DT::datatable(
      table_data,
      escape = FALSE,
      options = list(pageLength = 5, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  observeEvent(input$predict, {
    values$is_generating <- TRUE
    on.exit({
      values$is_generating <- FALSE
      session$sendCustomMessage("hide_prediction_loader", list())
    }, add = TRUE)
    values$status <- "Generating smoke plume prediction."
    
    req(
      input$latitude, input$longitude,
      input$acres, input$duration,
      input$fuel_type, input$fuel_load,
      input$consumed_fraction,
      input$wind_speed, input$wind_direction, input$stability,
      input$mixing_height, input$plume_base, input$convective_fraction,
      input$fuel_moisture,
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
        tons_per_acre = effective_fuel_load(),
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
        fuel_moisture = input$fuel_moisture,
        resolution_km = 0.20
      )
      values$status <- "Prediction complete"
    }, error = function(e) {
      values$status <- paste("Error generating prediction:", e$message)
      showNotification("Error generating smoke prediction. Please check inputs.", type = "error")
    })
  })
  
  output$smoke_map <- renderLeaflet({
    burn_lat <- values$burn_lat
    burn_lon <- values$burn_lon
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
    
    burn_radius_m <- sqrt(input$acres * 4046.86 / pi)
    
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopoMap)
    
    map_df <- NULL
    
    if (!is.null(df)) {
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
    }
    
    m <- m %>%
      addCircleMarkers(
        lng = burn_lon,
        lat = burn_lat,
        radius = 8,
        color = "red",
        fillOpacity = 1,
        popup = "Burn location (click map to move)"
      ) %>%
      addCircles(
        lng = burn_lon,
        lat = burn_lat,
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
    
    if (!is.null(map_df) && nrow(map_df) > 0) {
      m <- m %>%
        fitBounds(
          lng1 = min(c(burn_lon, map_df$lon_min), na.rm = TRUE),
          lat1 = min(c(burn_lat, map_df$lat_min), na.rm = TRUE),
          lng2 = max(c(burn_lon, map_df$lon_max), na.rm = TRUE),
          lat2 = max(c(burn_lat, map_df$lat_max), na.rm = TRUE)
        )
    } else {
      m <- m %>%
        setView(lng = burn_lon, lat = burn_lat, zoom = 10)
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
        "Total PM2.5 Concentration (µg/m³)" = concentration,
        "Flaming PM2.5 Contribution (µg/m³)" = concentration_flaming,
        "Smolder PM2.5 Contribution (µg/m³)" = concentration_smolder,
        "Distance from source (km)" = distance,
        "Downwind distance (km)" = x_rot,
        "Crosswind offset (km)" = y_rot,
        "Plume Top (m)" = H_eff,
        "AQI Band" = aqi_bin
      ) %>%
      arrange(desc(`Total PM2.5 Concentration (µg/m³)`)) %>%
      mutate(
        Latitude = round(Latitude, 6),
        Longitude = round(Longitude, 6),
        `Total PM2.5 Concentration (µg/m³)` = round(`Total PM2.5 Concentration (µg/m³)`, 2),
        `Flaming PM2.5 Contribution (µg/m³)` = round(`Flaming PM2.5 Contribution (µg/m³)`, 2),
        `Smolder PM2.5 Contribution (µg/m³)` = round(`Smolder PM2.5 Contribution (µg/m³)`, 2),
        `Distance from source (km)` = round(`Distance from source (km)`, 2),
        `Downwind distance (km)` = round(`Downwind distance (km)`, 2),
        `Crosswind offset (km)` = round(`Crosswind offset (km)`, 2),
        `Plume Top (m)` = round(`Plume Top (m)`, 1)
      )
    
    DT::datatable(
      table_data,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)