# Shiny-FishAssess: Fisheries Stock Assessment Prep Tool

[![Shiny app](https://img.shields.io/badge/Shiny-v1.8.0-brightgreen)](https://shiny.rstudio.com/)  
[![R version](https://img.shields.io/badge/R-4.5.0-blue)](https://www.r-project.org/)  
[![License](https://img.shields.io/badge/License-MIT-yellowgreen)](https://opensource.org/licenses/MIT)

**Shiny-FishAssess** is an R Shiny application for preparing input data for fisheries stock assessments. It provides an interface to compile data, generate SS3 input files, and run model diagnostics including sensitivity analyses, bias tuning, and model comparisons.

## Key Features

* **Data Exploration:** Interactive dashboards for visualising and filtering catch, indices, length, and age data.
* **Species Selection:** Filter specific species using searchable dropdown menus.
* **Assessment Inputs:** Control inclusions for data types (Catch, Indices, Length, Age) and parameters (Biological, Fishery).
* **Parameter Specification:** Customise biological and fishery parameters. This includes support for von Bertalanffy and Schnute growth curves, time-varying parameters, and recruitment deviations.
* **SS3 Input Generation:** Generate formatted `datafile.dat`, `controlfile.ctl`, `starter.ss`, and `forecast.ss` files.
* **Batch & Parallel Processing:** Run single or batch SS3 models from uploaded ZIP files using parallel processing.
* **Sensitivity Analysis:** Run parallelised sensitivity tests including Jitter analysis, Retrospective analysis, and Likelihood profiles.
* **Bias & Tuning:** Automated tools for parallelised bias ramp adjustment and composition weighting (Francis and Dirichlet methods).
* **Model Comparison:** Compare multiple model runs with generated plots and summaries.
* **DPIRD Styling:** Standardised plot outputs tailored for DPIRD reporting.
* **Workspace Management:** Built-in tools to clear `.exe` files and manage output directories.

## Application Files

* `app.R`: Main Shiny application script containing UI and server logic.
* `SS_input.R`: Formats and generates SS3 input files.
* `SS_sensitivities.R`: Runs sensitivity analyses in parallel.
* `SS_bias_tuning.R`: Handles automated bias adjustment and data weighting.
* `BiolTable.R`: Generates the biological parameters table.
* `SSplotComparisonsREP.R`: Helper script for model comparison plots.
* `import_DPIRD_data.R`: **(Internal Only)** Script for importing raw database data. *Note: As this file is not public, the app defaults to "Restricted Mode" focusing on analysing existing SS3 model outputs.*

## Setup and Usage

1. **Prerequisites:** R (>= 4.2.0) and RStudio.
2. **Installation:** Clone or download this repository. Place the `Stock_Synthesis_latest/ss.exe` executable in the project root to run models locally.
3. **Running the App:** Open `app.R` in RStudio and use `shiny::runApp()`. Use the "Run External" option for best performance.
4. **Workflow:**
    * **Restricted Mode (Public Users):** Focuses on the SS3 Sensitivity Analysis and Bias and Tuning tabs. Upload existing SS3 model folders (zipped) to perform diagnostics, comparisons, and tuning.
    * **Full Mode (DPIRD Staff):** Requires the internal data script to load raw data, filter species/fleets, and generate new SS3 input files from the Data Preparation tabs.