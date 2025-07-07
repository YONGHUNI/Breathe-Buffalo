# Breathe Buffalo

**Breathe Buffalo** is a dashboard-style web application developed for the UB Air Monitoring project. This dashboard is a key dissemination product for the community-based participatory research (CBPR) study focused on air quality in Buffalo’s African-American community, particularly on the east side of Buffalo, New York.

## Project Background

The east side of Buffalo’s African-American community faces a disproportionate burden of chronic disease compared to neighboring regions. Poor air quality is a well-known contributor to adverse health outcomes, but until recently, it has been difficult to assess community-specific air pollution exposures due to a lack of local monitoring. To address this gap, we partnered with the Buffalo Center for Health Equity (BCHE), a local environmental justice organization, and launched a two-year air monitoring initiative using CBPR principles. t Prototypes of study results were developed to report fine particulate matter (PM2.5) and volatile organic compound (VOC) levels. An interactive data dashboard—this very application—was iteratively co-designed and refined with input from BCHE members and study participants through virtual meetings and online surveys. This process emphasized the use of color, large font for key results, and a preference for maps over traditional plots, ensuring that the dashboard is accessible, engaging, and tailored to community needs.

The dashboard serves as a central tool to communicate findings from a ten-month community air monitoring campaign. Future dissemination efforts will also include public education events and virtual meetings to share results with the broader community.

## Key Features

-   **Real-time Air Quality Monitoring:** Visualize sensor data (PM2.5, VOC, etc.) on an interactive map using Leaflet.
-   **Zipcode-based Measurements:** View air quality metrics by zipcode, with gauges and bar charts for selected regions.
-   **Intuitive UI:** User-friendly interface built with shinydashboard (no sidebar, clean layout).
-   **Spatial Data Integration:** Utilizes the `sf` and `leaflet` packages to display and facilitate the analysis of spatial datasets.
-   **Rich Visualizations:** Supports various R visualization packages including plotly, ggplot2, DT for tables.

## Main Screens

-   **Location Map:** Real-time air quality by region, visualized on a map.
-   **Air Quality Gauges:** Gauges showing both selected zipcode and city-wide air quality.
-   **Color Ramp:** Visual color ramp indicating air quality levels (`Good` in green to `Hazardous` in red).

## Getting Started

### 1. Install Dependencies

The following R packages are required:

-   shiny, shinydashboard, shinybusy, flexdashboard, lubridate, leaflet, DT, sf, data.table, base64enc, jsonlite, DBI, RPostgres, plotly, ggplot2, rsconnect

The project uses `renv` for dependency management:

``` r
# In the repo root
renv::restore()
```

### 2. Prepare Environment Variables

Store sensitive information (e.g., DB credentials) in `.Renviron` and place it in the repo root.

### 3. Run the App

``` r
# From the repo root
shiny::runApp()
```

or run `app.R` directly.

## Deployment

- Use `config.R` for deployment settings (`rsconnect` to deploy to a server).
- Easily run and deploy from within RStudio.
- **Continuous Deployment:**  
  When you push to the `repo-main` branch, the app is automatically deployed to [shinyapps.io](https://www.shinyapps.io/) via GitHub Actions.



## Project Structure

```         
Breathe Buffalo/
├── app.R               # Main Shiny application code
├── config.R            # Deployment (rsconnect) configuration script
├── renv/               # R package dependency management (renv)
│   ├── activate.R
│   └── ...             # Other renv infrastructure files
├── renv.R              # renv bootstrap/init script
├── data/               # Project data assets
│   └── zip/
│       ├── target.gpkg     # Geopackage: spatial boundary/target data
│       └── zipBnd_EN.qmd   # QGIS metadata or spatial config
├── .Renviron           # (Not committed: for local secrets/env vars)
├── LICENSE             # License file
└── README.md           # Project documentation (this file)
```

Note:\
- Some files such as `.Renviron` are referenced in the code but not committed to version control. - The `renv/` folder may contain additional files (lock, settings, cached packages) created by the R package management system. - Additional files or folders may exist for logs, temporary files, or user-generated content if applicable.

## Related Links

-   [UB Air Quality Study (Project Overview)](https://ubairqualitystudy.github.io/EPA-Website/index.html)
-   [Repository for Data handling](https://github.com/YONGHUNI/rougue_PA_detector)

## License

See the [LICENSE](./LICENSE) file for details.
