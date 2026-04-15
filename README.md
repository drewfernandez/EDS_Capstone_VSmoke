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
- **AI Photo Match Calculator** - Uses OpenAI to compare the fuel guide photos to a photo of your choice and finds the closest match. 

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

## How to use the AI Photo Match Calculator 
1. Upload a photo of your choice that you want to analyze
2. Input the litter and duff depth
3. check the "show recommendation" box
4. select "Find Closest Guide Photo"
5. View the results on the right panel

### AQI Concentration Bands

| AQI Category | PM2.5 (µg/m³) |
|---|---|
| Good | 0 – 50 |
| Moderate | 51 – 100 |
| Unhealthy for Sensitive Groups | 101 – 150 |
| Unhealthy | 151 – 200 |
| Very Unhealthy | 201 – 300 |
| Hazardous | 301+ |

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

| Name | GitHub | Email |
|---|---|---|
| Keenan Pierce | [@keenanjp](https://github.com/keenanjp) | keenanjpierce@icloud.com |
| Matteo Pivato | [@Mattewolf](https://github.com/Mattewolf) | Matteo@pivato.com |
| Drew Fernandez | [@drewfernandez](https://github.com/drewfernandez) | drewfernandez254@gmail.com |



---

## Acknowledgments

- USDA Forest Service for the original VSmoke model and documentation
- Course instructors and advisors for the EDS Capstone program at Virginia Tech.
- Dr. Adam Coates Photo Guide for Estimating Fuel Loading in the Southern Appalachian Mountains

---
# Future Features 

- Addition of a Topographic map and 3D model
