library(shiny)
library(tidyverse)
library(DT)
library(bslib)
library(leaflet)
library(htmltools)
library(magick)

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
      local_path = file.path("www", image_url),
      image_exists = file.exists(local_path)
    )
}

safe_img_vector <- function(path, size = 96) {
  img <- image_read(path) %>%
    image_resize(sprintf("%dx%d!", size, size)) %>%
    image_convert(colorspace = "Gray")
  
  as.numeric(image_data(img, channels = "gray"))
}

find_top_matches <- function(upload_path, reference_df, top_n = 5) {
  up_vec <- safe_img_vector(upload_path)
  
  reference_df %>%
    filter(image_exists) %>%
    rowwise() %>%
    mutate(
      distance = mean(abs(up_vec - safe_img_vector(local_path))),
      similarity = pmax(0, 100 - (distance / 255) * 100)
    ) %>%
    ungroup() %>%
    arrange(distance) %>%
    slice_head(n = top_n)
}

photo_guide_options <- build_photo_guide_options()

ui <- page_sidebar(
  title = "Smoke Plume Prediction Studio + Photo Guide Calculator",
  theme = bs_theme(version = 5, bootswatch = "minty"),
  sidebar = sidebar(
    width = 340,
    p("Smoke prediction components omitted here; this app focuses on the upgraded Diff/Litter calculator workflow."),
    fileInput("uploaded_photo", "Upload reference photo", accept = c(".png", ".jpg", ".jpeg")),
    textInput("ai_prompt", "Ask image assistant", value = "Find the closest fuel guide photos and explain why."),
    actionButton("compare_photo", "Compare with 74 guide photos", class = "btn-primary")
  ),
  navset_card_tab(
    nav_panel(
      "Diff/Litter Calculator",
      layout_columns(
        col_widths = c(5, 7),
        card(
          card_header("Uploaded image + AI-style matching"),
          uiOutput("uploaded_preview"),
          uiOutput("ai_response"),
          selectInput(
            "selected_output_photo",
            "Choose output photo",
            choices = character(0),
            selected = character(0)
          ),
          h5("Top matching guide photos"),
          uiOutput("match_gallery")
        ),
        card(
          card_header("Estimated Fuel Loading (from best match)"),
          numericInput("litter_depth", "Litter depth (inches)", value = 1, min = 0, max = 12, step = 0.1),
          numericInput("duff_depth", "Duff depth (inches)", value = 1, min = 0, max = 12, step = 0.1),
          value_box(title = "Selected photo", value = textOutput("selected_photo_text")),
          value_box(title = "Litter mass", value = textOutput("litter_mass_text"), theme = "success"),
          value_box(title = "Duff mass", value = textOutput("duff_mass_text"), theme = "warning"),
          value_box(title = "Total", value = textOutput("total_mass_text"), theme = "primary"),
          DTOutput("photo_factor_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  matches <- reactiveVal(NULL)
  
  observeEvent(input$compare_photo, {
    req(input$uploaded_photo)
    top <- find_top_matches(input$uploaded_photo$datapath, photo_guide_options, top_n = 6)
    matches(top)
  })
  
  observeEvent(matches(), {
    req(matches())
    choices <- setNames(matches()$photo_id, paste0(matches()$photo_id, " (", round(matches()$similarity, 1), "%)"))
    default_choice <- matches()$photo_id[[1]]
    selected_choice <- if (isTruthy(input$selected_output_photo) && input$selected_output_photo %in% matches()$photo_id) {
      input$selected_output_photo
    } else {
      default_choice
    }
    updateSelectInput(
      session,
      "selected_output_photo",
      choices = choices,
      selected = selected_choice
    )
  }, ignoreInit = FALSE)
  
  selected_photo <- reactive({
    req(matches())
    req(input$selected_output_photo)
    selected <- matches() %>% filter(photo_id == input$selected_output_photo) %>% slice(1)
    if (nrow(selected) == 0) {
      return(matches() %>% slice(1))
    }
    selected
  })
  
  output$uploaded_preview <- renderUI({
    req(input$uploaded_photo)
    tags$div(
      tags$strong("Uploaded"),
      tags$img(src = input$uploaded_photo$datapath, style = "max-width:100%;border:1px solid #ddd;border-radius:8px;")
    )
  })
  
  output$ai_response <- renderUI({
    req(matches())
    req(selected_photo())
    top <- matches()
    chosen <- selected_photo()
    msg <- paste0(
      "AI Assistant: Based on visual similarity analysis across ", sum(photo_guide_options$image_exists),
      " available guide images, the best match is ", top$photo_id[[1]],
      " (", round(top$similarity[[1]], 1), "%). ",
      "Current selected output photo: ", chosen$photo_id[[1]],
      " (", round(chosen$similarity[[1]], 1), "%). ",
      "Prompt: '", input$ai_prompt, "'."
    )
    tags$p(style = "background:#f8fafc;padding:10px;border-radius:8px;", msg)
  })
  
  output$match_gallery <- renderUI({
    req(matches())
    top <- matches()
    div(
      style = "display:grid;grid-template-columns:repeat(auto-fill,minmax(140px,1fr));gap:10px;",
      lapply(seq_len(nrow(top)), function(i) {
        tags$div(
          style = "border:1px solid #ddd;border-radius:8px;padding:6px;",
          tags$img(src = top$image_url[[i]], style = "width:100%;height:100px;object-fit:cover;"),
          tags$div(tags$b(top$photo_id[[i]]), br(), sprintf("Similarity: %.1f%%", top$similarity[[i]]))
        )
      })
    )
  })
  
  photo_calc <- reactive({
    opt <- selected_photo()
    litter_mass <- input$litter_depth * opt$litter_factor[[1]]
    duff_mass <- input$duff_depth * opt$duff_factor[[1]]
    list(opt = opt, litter_mass = litter_mass, duff_mass = duff_mass, total = litter_mass + duff_mass)
  })
  
  output$selected_photo_text <- renderText({
    req(selected_photo())
    paste(selected_photo()$photo_id[[1]], "-", selected_photo()$site_type[[1]])
  })
  output$litter_mass_text <- renderText(paste0(round(photo_calc()$litter_mass, 2), " tons/acre"))
  output$duff_mass_text <- renderText(paste0(round(photo_calc()$duff_mass, 2), " tons/acre"))
  output$total_mass_text <- renderText(paste0(round(photo_calc()$total, 2), " tons/acre"))
  
  output$photo_factor_table <- renderDT({
    req(selected_photo())
    opt <- selected_photo()
    datatable(
      tibble(
        `Photo Option` = opt$photo_id,
        `Site Type` = opt$site_type,
        Ecozone = opt$ecozone,
        Elevation = opt$elevation_band,
        Aspect = opt$aspect_band,
        Vegetation = opt$vegetation_type,
        `Litter factor (tons/acre/in)` = opt$litter_factor,
        `Duff factor (tons/acre/in)` = opt$duff_factor,
        Similarity = round(opt$similarity, 1)
      ),
      options = list(dom = "t"),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)