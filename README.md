# Smoke Plume Prediction Studio

An interactive R Shiny application for predicting smoke concentrations and air quality impacts from prescribed burns. Built on a Gaussian plume dispersion model based on VSmoke methodology, with an integrated photo-guide fuel loading calculator.

Developed as part of the **Environmental Data Science Capstone** at Virginia Tech.

---

## Overview

This app gives prescribed fire planners and land managers a browser-based tool to visualize smoke transport and estimate PM2.5 concentrations downwind of a burn — no command-line tools required.

**Key features:**

- **Interactive smoke plume map** — AQI-colored ellipse polygons showing predicted PM2.5 concentration zones downwind of the burn location, rendered on a live Esri basemap
- **Gaussian dispersion modeling** — implements Pasquill-Gifford stability classes (A–F) and fuel-specific PM2.5 emission factors across six fuel types
- **Prediction data table** — full grid of predicted concentrations (µg/m³) color-coded by AQI severity band
- **Duff/Litter fuel calculator** — estimates litter and duff fuel loading (tons/acre) from depth inputs using a photo guide keyed to elevation, aspect, and vegetation type
- **Model information tab** — in-app documentation of model assumptions, parameters, and limitations

---

## Getting Started

### Prerequisites

- [R](https://www.r-project.org/) (>= 4.1.0)
- [RStudio](https://posit.co/products/open-source/rstudio/) (recommended)

### Installation

1. **Clone the repository:**
```bash
   git clone https://github.com/drewfernandez/EDS_Capstone_VSmoke.git
   cd EDS_Capstone_VSmoke
```

2. **Install required R packages:**
```r
   source("requirements.R")
```

3. **Launch the app:**
```r
   shiny::runApp("App.R")
```
   Or open `App.R` in RStudio and click **Run App**.

> **Note:** The Duff/Litter Calculator tab requires fuel guide photos to be placed in `www/Fuel_Guide_Photos_1000_1999/`, `www/Fuel_Guide_Photos_2000_3499/`, and `www/Fuel_Guide_Photos_3500_plus/`. The rest of the app works normally without them.

---

## Usage

1. Enter a **burn location** (latitude/longitude), fire size in acres, and burn duration
2. Select a **fuel type** and fuel load (tons/acre)
3. Set **weather conditions** — wind speed, wind direction, and atmospheric stability class
4. Click **Generate Smoke Prediction** to run the model
5. Explore results across the Map, Data, and Calculator tabs

### AQI Concentration Bands

| AQI Category | PM2.5 (µg/m³) |
|---|---|
| Good | 0 – 12 |
| Moderate | 12 – 35.4 |
| Unhealthy for Sensitive Groups | 35.4 – 55.4 |
| Unhealthy | 55.4 – 150.4 |
| Very Unhealthy | 150.4 – 250.4 |
| Hazardous | 250.4+ |

---

## Model Background

The app implements a steady-state Gaussian plume model consistent with VSmoke methodology (Lavdas, 1996). Key components:

- **Dispersion coefficients** derived from Pasquill-Gifford stability classes (A–F) using standard sigma-y and sigma-z parameterizations
- **Emission rates** calculated from total fuel mass, fuel-type-specific PM2.5 emission factors, and burn duration
- **Plume rise** estimated from heat release based on fuel load and fire intensity
- **Concentration grid** computed over a 50 km × 50 km domain at 1 km resolution, rotated into wind-aligned coordinates

**Assumptions and limitations:**
- Flat terrain and steady-state atmospheric conditions
- Point/area source approximation with no explicit fire front behavior
- Ground-level (2 m) concentrations only

**Reference:**
> Lavdas, L.G. (1996). *Program VSMOKE – User's Manual*. USDA Forest Service, Southeastern Forest Experiment Station, General Technical Report SRS-6. Macon, GA.

---

## Contributors

| Name | GitHub |
|---|---|
| Keenan Pierce | [@keenanjp](https://github.com/keenanjp) |
| Matteo Pivato | [@Mattewolf](https://github.com/Mattewolf) |
| Drew Fernandez | [@drewfernandez](https://github.com/drewfernandez) |



---

## Acknowledgments

- USDA Forest Service for the original VSmoke model and documentation
- Course instructors and advisors for the EDS Capstone program at Virginia Tech.
