required_packages <- c(
  "shiny",
  "tidyverse",
  "DT",
  "bslib",
  "leaflet",
  "htmltools",
  "httr2",
  "jsonlite",
  "base64enc",
  "magick"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  message("Installing: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
} else {
  message("All packages already installed.")
}
