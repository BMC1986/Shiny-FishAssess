# Shiny-FishAssess: Fisheries Stock Assessment Prep Tool

[![Shiny app](https://img.shields.io/badge/Shiny-v1.8.0-brightgreen)](https://shiny.rstudio.com/)  
[![R version](https://img.shields.io/badge/R-4.5.0-blue)](https://www.r-project.org/)  
[![License](https://img.shields.io/badge/License-MIT-yellowgreen)](https://opensource.org/licenses/MIT)

**Shiny-FishAssess** is an R Shiny application designed to streamline the preparation of input data for fisheries stock assessments. It provides a user-friendly interface to explore, filter, and compile essential data, generate SS3 input files, and perform advanced model diagnostics including sensitivity analyses, bias tuning, and model comparisons.

## Key Features

* **Data Exploration:** Interactive dashboards for visualising and filtering catch, indices, length, and age data.
* **Species Selection:** Select and focus on specific species using searchable dropdown menus.
* **Assessment Inputs:** Control which data types (Catch, Indices, Length, Age) and parameters (Biological, Fishery) to include.
* **Parameter Specification:** Customise biological and fishery parameters, including time-varying growth/selectivity, natural mortality, and recruitment deviations.
* **SS3 Input Generation:** Automatically generate formatted `datafile.dat`, `controlfile.ctl`, `starter.ss`, and `forecast.ss` files.
* **Sensitivity Analysis:** Run parallelised sensitivity tests including Jitter analysis, Retrospective analysis, and Likelihood profiles ($R_0$, $M$, $h$, Depletion).
* **Bias & Tuning:** Automated tools for bias ramp adjustment and composition weighting (Francis and Dirichlet methods).
* **Model Comparison:** Compare multiple model runs with generated plots and summaries.
* **Parallel Processing:** Leverages multiple cores for efficient batch processing of SS3 models.
* **DPIRD Styling:** Custom plot outputs tailored for reporting.

## Application Files

The repository includes the following key R scripts:

* `app.R`: The main Shiny application script containing UI and server logic.
* `SS_input.R`: Script to format and generate SS3 input files from the processed data.
* `SS_sensitivities.R`: Script to run sensitivity analyses (Jitter, Retro, Profiles) in parallel.
* `SS_bias_tuning.R`: Script handling automated bias adjustment and data weighting.
* `BiolTable.R`: Script to generate the biological parameters table.
* `SSplotComparisonsREP.R`: Helper script for generating model comparison plots.
* `import_DPIRD_data.R`: **(Internal Only)** Script for importing raw database data, available only to DPIRD staff. *Note: As this file is not part of the public repository, the app defaults to "Restricted Mode," which focuses on analysing and manipulating existing SS3 model outputs.*

## Setup and Usage

1.  **Prerequisites:**
    Ensure you have R (>= 4.2.0) and RStudio installed. You will need the following packages:
    ```r
    install.packages(c("shiny", "shinyWidgets", "shinyjs", "shinyFiles", "bslib", 
                       "dplyr", "ggplot2", "tidyr", "data.table", "stringr", 
                       "doParallel", "foreach", "processx", "fs", "zip", 
                       "DT", "kableExtra", "remotes"))
    
    # Install r4ss from GitHub
    remotes::install_github("r4ss/r4ss")
    ```

2.  **Installation:**
    * Clone or download this repository.
    * Place the `Stock_Synthesis_latest/ss.exe` executable in the project root if you intend to run models locally.

3.  **Running the App:**
    * Open `app.R` in RStudio.
    * Run using `shiny::runApp()` or the "Run App" button.
    * **Note:** For best performance, use the "Run External" option in RStudio.

4.  **Workflow:**
    * **Restricted Mode (Public Users):** Upon launch, the app will detect that the data import script is missing. The app will focus on the **SS3 Sensitivity Analysis** and **Bias and Tuning** tabs. You can upload existing SS3 model folders (zipped) to perform diagnostics, comparisons, and tuning.
    * **Full Mode (DPIRD Staff):** With the internal data script present, users can load raw data, filter species/fleets, and generate new SS3 input files from scratch via the "Data Preparation" tabs.

## Process Flowchart

```mermaid
%%{init: {
  'flowchart': {
    'curve': 'basis',
    'diagramPadding': 40,
    'nodeSpacing': 40,
    'rankSpacing': 60
  },
  'themeVariables': {
    'fontSize': '40px'
  }
}}%%
graph LR
  subgraph Data_Inputs
    direction LR
    A1_script["Import Script<br>(DPIRD Internal)"]:::rawdata
    A1_zip["Upload ZIP<br>(Existing Models)"]:::rawdata
  end

  subgraph User_Interface
    direction TB
    UI("Shiny UI<br>(app.R)"):::usercontrol
    UI_inputs("User Inputs<br>(Species, Filters, Params)"):::usercontrol
  end

  subgraph Processing_&_Analysis
    direction TB
    P1("SS3 Input Gen<br>(SS_input.R)"):::processing
    P2("Sensitivities<br>(SS_sensitivities.R)"):::processing
    P3("Bias & Tuning<br>(SS_bias_tuning.R)"):::processing
  end

  subgraph Outputs
    direction TB
    O1("SS3 Input Files<br>(.dat, .ctl, .ss)"):::ss3output
    O2("Model Runs<br>(Parallel)"):::ss3output
    O3("Comparisons & Plots"):::ss3output
    O4("ZIP Archive"):::ss3output
  end

  %% Action Nodes
  ActionGenerate([Generate Inputs]):::actionbutton
  ActionRun([Run/Tune Model]):::actionbutton

  %% Data Flow
  A1_script -.-> UI
  A1_zip -.-> UI
  
  UI --> UI_inputs
  UI_inputs --> P1
  UI_inputs --> P2
  UI_inputs --> P3

  P1 --> O1
  O1 --> O4
  
  P2 --> O2
  P3 --> O2
  
  O2 --> O3
  
  UI_inputs --> ActionGenerate --> O4
  UI_inputs --> ActionRun --> O2

  %% Styling
  classDef rawdata fill:#8E5F7E,stroke:#42797F,color:#FFFFFF,stroke-width:1.5px,border-radius:5px
  classDef usercontrol fill:#BFD7DF,stroke:#003F51,color:#000000,stroke-width:1.5px,border-radius:5px
  classDef processing fill:#CC9950,stroke:#003F51,color:#FFFFFF,stroke-width:1.5px,border-radius:5px
  classDef ss3output fill:#003F51,stroke:#BFD7DF,color:#FFFFFF,stroke-width:1.5px,border-radius:5px
  classDef actionbutton fill:#42797F,stroke:#42797F,color:#FFFFFF,stroke-width:1.5px,border-radius:10px

  class A1_script,A1_zip rawdata
  class UI,UI_inputs usercontrol
  class P1,P2,P3 processing
  class O1,O2,O3,O4 ss3output
  class ActionGenerate,ActionRun actionbutton

  style Data_Inputs fill:#F0F7F9,stroke:#003F51
  style User_Interface fill:#F0F7F9,stroke:#003F51
  style Processing_&_Analysis fill:#F0F7F9,stroke:#003F51
  style Outputs fill:#F0F7F9,stroke:#003F51
```