# Shiny-FishAssess: Fisheries Stock Assessment Prep Tool

[![Shiny app](https://img.shields.io/badge/Shiny-v1.7.4-brightgreen)](https://shiny.rstudio.com/)  
[![R version](https://img.shields.io/badge/R-4.5.0-blue)](https://www.r-project.org/)  
[![License](https://img.shields.io/badge/License-MIT-yellowgreen)](https://opensource.org/licenses/MIT)

**Shiny-FishAssess** is a R Shiny application designed to streamline the preparation of input data for fisheries stock assessments. It provides a user-friendly interface to explore, filter, and compile essential data from various sources, facilitating the creation of input files for stock assessment models like SS3.

## Key Features

* **Data Exploration:** Interactive dashboards for visualizing and filtering catch, indices, length, and age data.
* **Species Selection:** Easily select and focus on specific species using searchable dropdown menus.
* **Data Inclusion Control:** Choose which data types (Catch, Indices, Length, Age, Biological Parameters, Fishery Parameters) to include in the analysis.
* **Parameter Specification:** Define or customize biological and fishery parameters, such as natural mortality, steepness, and initial recruitment.
* **SS3 Input Generation:** Automatically generate formatted input files for the SS3 stock assessment model.
* **Data Download:** Download processed data and configurations in a convenient ZIP archive.
* **Console Output:** Monitor processing steps and any warnings or errors via an integrated console log.

## Application Files

The repository includes the following R scripts:

* `app.R`:  The main Shiny application script, defining the user interface and server logic.
* `import_data.R`:  Script for importing and preprocessing raw data from various sources (e.g., databases, CSV files).
* `BiolTable.R`: Script to generate the biological parameters table.
* `SS_input.R`:  Script to format and generate SS3 input files from the processed data.
* `SS3_sensitivities.R`:  Script to run run sensitivity analyses

## Data Sources

The application integrates data from several sources, including:

* Fixed Site Survey data (Kimberley)
* Pilbara and Kimberley biological databases
* WCD database
* Catch time-series data
* Indices time-series data
* Biological Unit Parameters database 

## Setup and Usage

1.  **Prerequisites:**
    * R (>= 4.2.0)
    * RStudio (recommended)
    * r4ss: Installed from GitHub: 
    
```S
remotes::install_github("r4ss/r4ss")
```

2.  **Installation:**
    * Clone or download the repository to your local machine.
    * Place the necessary data files in the appropriate directories as expected by the R scripts.
    * Ensure all the required R packages are installed.
        
    
3.  **Running the App:**
    * Open the `app.R` script in RStudio (or your preferred R environment).
    * Run the application using the `shiny::runApp()` function or the "Run App" button in RStudio.
    * Recommend using the "`Run External`" option within the "`Run App`" button 

4.  **Workflow:**
    * Use the sidebar panel to select the species, data types, and parameters for your assessment.
    * Explore the data in the main panel's tabs (Catch, Indices, Length, Age, etc.).
    * Adjust parameters and filtering options as needed.
    * Click the "Generate SS3 Files" button to generate and download the SS3 input files and a summary of your selections.

## SS3 Input Files

The application generates the following files for use with the SS3 stock assessment model:

* `datafile.dat`:  The main data file containing catch, indices, and biological information.
* `controlfile.ctl`:  The control file specifying model settings and parameters.
* `starter.ss`:   The SS3 starter file.
* `forecast.ss`:  The forecast file for future projections.

## Important Notes

* Ensure that your data files are correctly formatted and located as expected by the `import_data.R` script.
* Carefully review the console output for any warnings or errors during data processing.
* This tool is designed to aid in data preparation; users are responsible for the validity and appropriateness of the data and assessment settings.

## Process flowchart

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
  subgraph Raw_Data_Inputs
    direction LR
    A1_sql["SQL Server"]:::rawdata
    A1_mdb["Access DBs"]:::rawdata
    A1_csv["CSV Files<br>(Catch, Indices)"]:::rawdata
    A1_rdata["XLSX Files<br>(Testing)"]:::rawdata
  end

  subgraph User_Interface
    direction TB
    UI("Shiny UI<br>(app.R)"):::usercontrol
    UI_inputs("User Inputs<br>(Species, Filters)"):::usercontrol
  end

  subgraph Data_Processing
    direction TB
    P1("Import Data<br>(import_data.R)"):::processing
    P2("Bio Parameters<br>(BiolTable.R)"):::processing
    P3("Filter & Aggregate<br>(app.R server)"):::processing
    P4("SS3 Input Files<br>(SS_input.R)"):::processing
  end

  subgraph Intermediate_Data
    direction TB
    D1("Fixedsite Data"):::interdata
    D2("Merged Bio Data"):::interdata
    D3("Catch TS"):::interdata
    D4("Effort TS"):::interdata
    D5("FIS Age Data"):::interdata
    D6("Bio Params Table"):::interdata
    D7("Fishery Parameters"):::interdata
    D8("Length Comps"):::interdata
    D9("Age Comps"):::interdata
    D10("CPUE/Survey"):::interdata
    D11("SS3 Catch Data"):::interdata
  end

  subgraph SS3_Model
    direction TB
    O1("starter.ss"):::ss3output
    O2("controlfile.ctl"):::ss3output
    O3("datafile.dat"):::ss3output
    O4("forecast.ss"):::ss3output
    M1{"SS3 Run<br>(r4ss::run)"}:::ss3output
    O5("SS3 Plots<br>(r4ss::SS_plots)"):::ss3output
    O6("ZIP Archive"):::ss3output
  end

  %% Action Nodes
  ActionRunModel([Run Model]):::actionbutton
  ActionGenerateFiles([Generate Files]):::actionbutton
  ActionBookmark([Bookmark]):::actionbutton

  %% Data Flow
  A1_sql --> P1
  A1_mdb --> P1
  A1_csv --> P1
  A1_rdata --> P1

  UI --> UI_inputs

  P1 --> D1
  P1 --> D2
  P1 --> D3
  P1 --> D4
  P1 --> D5
  
  A1_rdata --> P2
  UI_inputs --> P2
  P2 --> D6

  UI_inputs --> P3
  D1 --> P3
  D2 --> P3
  D3 --> P3
  D4 --> P3
  D5 --> P3
  D6 --> P3
  P3 --> D7
  P3 --> D8
  P3 --> D9
  P3 --> D10
  P3 --> D11

  D6 --> P4
  D7 --> P4
  D8 --> P4
  D9 --> P4
  D10 --> P4
  D11 --> P4
  UI_inputs --> P4

  P4 --> O1
  P4 --> O2
  P4 --> O3
  P4 --> O4
  
  UI_inputs --> ActionRunModel --> M1
  O1 --> M1
  O2 --> M1
  O3 --> M1
  O4 --> M1
  M1 --> O5
  
  UI_inputs --> ActionGenerateFiles --> O6
  O1 --> O6
  O2 --> O6
  O3 --> O6
  O4 --> O6
  P3 -->|Summary| O6
  UI_inputs --> ActionBookmark --> O6

  %% Styling - DPIRD Theme
  classDef rawdata fill:#8E5F7E,stroke:#42797F,color:#FFFFFF,stroke-width:1.5px,stroke-dasharray:5,border-radius:5px
  classDef usercontrol fill:#BFD7DF,stroke:#003F51,color:#000000,stroke-width:1.5px,border-radius:5px
  classDef processing fill:#CC9950,stroke:#003F51,color:#FFFFFF,stroke-width:1.5px,border-radius:5px
  classDef interdata fill:#94B237,stroke:#DEE2E6,color:#000000,stroke-width:1.5px,border-radius:5px
  classDef ss3output fill:#003F51,stroke:#BFD7DF,color:#FFFFFF,stroke-width:1.5px,border-radius:5px
  classDef actionbutton fill:#42797F,stroke:#42797F,color:#FFFFFF,stroke-width:1.5px,border-radius:10px,padding:5px

  class A1_sql,A1_mdb,A1_csv,A1_rdata rawdata
  class UI,UI_inputs usercontrol
  class P1,P2,P3,P4 processing
  class D1,D2,D3,D4,D5,D6,D7,D8,D9,D10,D11 interdata
  class O1,O2,O3,O4,M1,O5,O6 ss3output
  class ActionRunModel,ActionGenerateFiles,ActionBookmark actionbutton


  %% Subgraph specific styling
  style Raw_Data_Inputs fill:#F0F7F9,stroke:#003F51,color:#003F51,stroke-width:1.5px,border-radius:5px
  style User_Interface fill:#F0F7F9,stroke:#003F51,color:#003F51,stroke-width:1.5px,border-radius:5px
  style Data_Processing fill:#F0F7F9,stroke:#003F51,color:#003F51,stroke-width:1.5px,border-radius:5px
  style Intermediate_Data fill:#F0F7F9,stroke:#003F51,color:#003F51,stroke-width:1.5px,border-radius:5px
  style SS3_Model fill:#F0F7F9,stroke:#003F51,color:#003F51,stroke-width:1.5px,border-radius:5px

```
