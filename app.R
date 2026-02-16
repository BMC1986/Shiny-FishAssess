# 
# ███████ ██   ██ ██ ███    ██ ██    ██       ███████ ██ ███████ ██   ██  █████  ███████ ███████ ███████ ███████ ███████ 
# ██      ██   ██ ██ ████   ██  ██  ██        ██      ██ ██      ██   ██ ██   ██ ██      ██      ██      ██      ██      
# ███████ ███████ ██ ██ ██  ██   ████   █████ █████   ██ ███████ ███████ ███████ ███████ ███████ █████   ███████ ███████ 
#      ██ ██   ██ ██ ██  ██ ██    ██          ██      ██      ██ ██   ██ ██   ██      ██      ██ ██           ██      ██ 
# ███████ ██   ██ ██ ██   ████    ██          ██      ██ ███████ ██   ██ ██   ██ ███████ ███████ ███████ ███████ ███████ 

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(shinyWidgets)
  library(ggplot2)
  library(lemon)
  library(ggthemes)
  library(knitr)
  library(tidyr)
  library(zip)
  library(shinyjs)
  library(kableExtra)
  library(lubridate)
  library(DT)
  library(data.table)
  library(stringr)
  library(ggpubr)
  library(RODBC)
  library(pingr)
  library(RefManageR)
  library(htmltools)
  library(splitstackshape)
  library(r4ss) # From github
  library(bslib)
  library(callr)
  library(shinyFiles)
  library(fs)
  library(doParallel)
  library(foreach)
  library(jsonlite)
  library(tibble)
  library(future)
  library(future.apply)
  library(processx)
  library(tidyverse)
})

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

# --- START: New Configuration and Data Loading Block ---
# Default to FALSE. We will load data interactively in the Server function.
SENSITIVE_DATA_LOADED <- FALSE 

# --- DEFINE FALLBACK DATA STRUCTURES (ALWAYS RUN INITIALLY) ---
# Define all global objects needed by the rest of app.R with minimal, valid structure
# This prevents subsequent code from crashing due to missing objects.
Fixedsiteonly <- data.frame(SpeciesName = character(0), Fork.Length = numeric(0), Location = character(0), 
                            CSIRO = integer(0), year = integer(0), Discarded. = character(0), 
                            Sector = character(0), 
                            stringsAsFactors = FALSE)

Merged_Kim_Pilb <- data.frame(SpeciesName = character(0), FL_mm = numeric(0), TL_mm = numeric(0),
                              Fin_Yr_Age = numeric(0), BioRegion = character(0), Zone = character(0),
                              Location = character(0), year = integer(0), Sector = character(0),
                              stringsAsFactors = FALSE)

FISages <- data.frame(SpeciesName = character(0), IntAge = numeric(0), year = integer(0), 
                      Location = character(0), stringsAsFactors = FALSE)

WCD_data <- data.frame(SpeciesName = character(0), CSIRO = integer(0), stringsAsFactors = FALSE)

catch_ts <- data.frame(Specstock = character(0), year = integer(0), stringsAsFactors = FALSE)

effort_ts <- data.frame(Specstock = character(0), year = integer(0), fleet = character(0), 
                        obs = numeric(0), se_log = numeric(0), stringsAsFactors = FALSE)

# Mock the mandatory object created by BiolTable.R source
bupBiologicalUnitParam <- data.frame(BiologicalUnitName = character(0), RSSpeciesCode = integer(0), 
                                     stringsAsFactors = FALSE) 

# Mock the reference object
temp_bib_file <- tempfile(fileext = ".bib")
writeLines("@article{dummy2025, title={No External Data}, author={Dummy, A}, year={2025}, journal={TEST}}", temp_bib_file)
bib_data <- RefManageR::ReadBib(file = temp_bib_file)
unlink(temp_bib_file)

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

message("INFO: App initialized with empty/dummy data sets. Waiting for user to load data.")
# --- END: New Configuration and Data Loading Block ---

all_species <- unique(c(Fixedsiteonly$SpeciesName, Merged_Kim_Pilb$SpeciesName))


ui <- function(request) {
  
  # Define DPIRD colours from the blue route 
  dpird_dark_blue <- "#003F51"  # PANTONE 7477 C
  dpird_mid_blue <- "#42797F"   # PANTONE 7475 C
  dpird_light_blue <- "#BFD7DF" # PANTONE 552 C
  dpird_black <- "#000000"
  dpird_white <- "#FFFFFF"
  dpird_sidebar_bg <- "#F0F7F9" 
  dpird_sidebar_border <- dpird_light_blue
  
  app_theme <- bs_theme(
    version = 5,
    bg = dpird_white,
    fg = dpird_black,
    primary = dpird_dark_blue,
    secondary = dpird_mid_blue,
    base_font = font_collection(font_google("Arial", local = FALSE), "sans-serif"), 
    heading_font = font_collection(font_google("Arial", local = FALSE), "sans-serif"), 
    "font-size-base" = "0.8rem",
    "accordion-button-active-bg" = dpird_light_blue,
    "accordion-button-active-color" = dpird_dark_blue,
    "btn-border-radius" = "0.25rem",
    "input-border-color" = dpird_mid_blue
  )
  
  # --- CSS FOR THE SPLIT LAYOUT AND OTHER CUSTOMIZATIONS ---
  custom_dpird_css <- HTML(paste0("
    /* Make body and html take full height for the splitter */
    html, body {
      height: 100%;
      overflow: hidden; /* Prevent double scrollbars */
    }
    
    /* Main container for the split layout */
    .split-container {
      display: flex;
      flex-direction: row;
      height: calc(100vh - 80px); /* Full viewport height minus title panel */
    }

    /* Style for the individual split panels */
    .split {
      padding: 10px;
      overflow-y: auto; /* Allow scrolling within panels */
    }

    /* Gutter (the draggable handle) style */
    .gutter {
      background-color: #eee;
      background-repeat: no-repeat;
      background-position: 50%;
    }
    .gutter.gutter-horizontal {
      background-image: url('data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAUAAAAeCAYAAADkftS9AAAAIklEQVQoU2M4c+bMfxl7ACYgdD4AbkKBG3AoGkYMAgAAg1gHwmuG4gAAAABJRU5ErkJggg==');
      cursor: col-resize;
    }

    /* Main Title Panel Styling */
    .shiny-title-panel h2 {
      font-family: 'Arial', sans-serif;
      font-weight: 600;
      color: ", dpird_dark_blue, ";
      margin-bottom: 20px;
    }

    /* General Heading Styling */
    h4, h5 {
      font-family: 'Arial', sans-serif;
      font-weight: 600;
      color: ", dpird_dark_blue, ";
    }
    h5 { font-size: 0.85rem; }
    h4 { font-size: 1.1rem; }

    /* Sidebar Panel Styling */
    .sidebar-panel .well {
      background-color: ", dpird_sidebar_bg, ";
      border: 1px solid ", dpird_sidebar_border, ";
      border-radius: 0.25rem;
    }

    /* Main Content WellPanels */
    .main-panel .wellPanel, .main-panel .well {
      background-color: #F8F9FA;
      border: 1px solid #DEE2E6;
      border-radius: 0.25rem;
      margin-top: 15px;
    }
    
    /* (The rest of your extensive CSS remains the same...) */
    .btn, .btn-primary, .btn-secondary, .btn-default { font-size: 0.75rem; padding: 0.275rem 0.525rem; line-height: 1.2; }
    .btn-primary { background-color: ", dpird_dark_blue, " !important; border-color: ", dpird_dark_blue, " !important; color: ", dpird_white, " !important; }
    .btn-primary:hover, .btn-primary:focus { background-color: darken(", dpird_dark_blue, ", 10%) !important; border-color: darken(", dpird_dark_blue, ", 10%) !important; }
    .btn-secondary, .btn-default { background-color: ", dpird_mid_blue, " !important; border-color: ", dpird_mid_blue, " !important; color: ", dpird_white, " !important; }
    .btn-secondary:hover, .btn-secondary:focus, .btn-default:hover, .btn-default:focus { background-color: darken(", dpird_mid_blue, ", 10%) !important; border-color: darken(", dpird_mid_blue, ", 10%) !important; }
    #download_btn { font-weight: bold; padding: 0.3rem 0.6rem; }
    #clear_console_btn { background-color: transparent !important; border-color: transparent !important; color: ", dpird_mid_blue, " !important; padding: 0.2rem 0.4rem; }
    #clear_console_btn:hover { color: ", dpird_dark_blue, " !important; }
    .nav-tabs .nav-link.active, .nav-tabs .nav-item.show .nav-link { color: ", dpird_dark_blue, "; background-color: ", dpird_white, "; border-color: ", dpird_dark_blue, " ", dpird_dark_blue, " ", dpird_white, "; font-weight: bold; }
    .nav-tabs .nav-link { color: ", dpird_mid_blue, "; border-bottom-color: ", dpird_light_blue, "; }
    .nav-tabs .nav-link:hover { border-color: ", dpird_light_blue, " ", dpird_light_blue, " ", dpird_light_blue, "; color: ", dpird_dark_blue, "; background-color: #f8f9fa; }
    .bootstrap-select .dropdown-menu { border: 1px solid ", dpird_dark_blue, "; }
    .bootstrap-select > .dropdown-toggle { border: 1px solid ", dpird_mid_blue, "; background-color: ", dpird_white, "; color: ", dpird_black, "; }
    .bootstrap-select > .dropdown-toggle:hover { border-color: ", dpird_dark_blue, "; }
    #console_output { background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 0.25rem; padding: 10px; font-family: monospace; }
    .sidebar-panel a { color: ", dpird_dark_blue, "; text-decoration: none; font-weight: bold; }
    .sidebar-panel a:hover { color: ", dpird_mid_blue, "; text-decoration: underline; }
    .form-control { font-size: 0.75rem; padding: 0.2rem 0.4rem; height: 1.8rem; line-height: 1.2; }
    .shiny-input-container.file-input .form-control { height: 1.8rem; padding: 0.2rem 0.4rem; }
    .form-control.textarea { font-size: 0.75rem; padding: 0.2rem 0.4rem; line-height: 1.2; height: auto; resize: vertical; }
    textarea.form-control[rows='3'] { height: calc(3 * 1.2rem + 0.4rem + 2px); }
    textarea.form-control[rows='4'] { height: calc(4 * 1.2rem + 0.4rem + 2px); }
    textarea.form-control[rows='5'] { height: calc(5 * 1.2rem + 0.4rem + 2px); }
    .selectize-input { font-size: 0.75rem; padding: 0.2rem 0.4rem; height: 1.8rem; line-height: 1.2; }
    .selectize-dropdown { font-size: 0.75rem; }
    .shiny-input-container.file-input .btn { height: 1.8rem; padding: 0.2rem 0.4rem; font-size: 0.75rem; line-height: 1.2; }
    # This updated CSS block styles both tabs together
    .nav-tabs .nav-link[data-value=\"ss3sensitivity_tab\"],
    .nav-tabs .nav-link[data-value=\"bias_tuning_tab\"] { 
        background-color: #dde8eb; 
        border-bottom-color: ", dpird_light_blue, "; 
    }
    .nav-tabs .nav-link[data-value=\"ss3sensitivity_tab\"]:hover,
    .nav-tabs .nav-link[data-value=\"bias_tuning_tab\"]:hover { 
        background-color: #e8f0f2; 
        border-color: ", dpird_light_blue, " ", dpird_light_blue, " ", dpird_light_blue, "; 
    }
    .nav-tabs .nav-link[data-value=\"ss3sensitivity_tab\"].active,
    .nav-tabs .nav-link[data-value=\"bias_tuning_tab\"].active { 
        color: ", dpird_dark_blue, "; 
        background-color: ", dpird_white, "; 
        border-color: ", dpird_dark_blue, " ", dpird_dark_blue, " ", dpird_white, "; 
        font-weight: normal; 
    }
    
    /* --- FIX CHECKBOX SPACING --- */
    
    /* 1. Remove bottom margin from the first group (Catch, Indices, etc.) */
    #data_include_before { 
        margin-bottom: 0px !important; 
    }

    /* 2. TIGHTEN THE CONDITIONAL INPUT (The Middle Section) */
    
    /* Remove margins from the UI Output container */
    #conditional_age_ui {
        margin-top: 0px !important;
        margin-bottom: 0px !important;
    }
    
    /* CRITICAL FIX: Remove margins from the hidden form-group wrapper */
    #conditional_age_ui .form-group {
        margin-top: 5px !important;
        margin-bottom: 5px !important; 
    }

    /* Remove margins from the specific checkbox div itself */
    #conditional_age_ui .checkbox {
        margin-top: 0px !important;
        margin-bottom: 0px !important;
    }

    /* 3. Remove top margin from the last group (Bio/Fishery Params) */
    #data_include_after { 
        margin-top: 0px !important; 
    }
    
    /* Remove the empty space reserved for the blank label */
    #data_include_after > label {
        display: none;
    }
 "))
  
  fluidPage(
    theme = app_theme,
    useShinyjs(), 
    tags$head(
      tags$script(src="https://unpkg.com/split.js/dist/split.min.js"),
      tags$style(custom_dpird_css),
      tags$script(
        HTML(
          'MathJax = {
            tex: {inlineMath: [["$", "$"], ["\\\\(", "\\\\)"]]}
          };'
        )
      ),
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js")
    ),
    titlePanel(
      windowTitle = "Shiny-FishAssess",
      title = "Shiny-FishAssess"
    ),
    
    div(class = "split-container",
        div(id = "sidebar", class="split sidebar-panel",
            wellPanel(
              uiOutput("load_data_ui"),
              br(),
              h4("Assessment Inputs"),
              pickerInput("species_select", "Select Species to get started",
                          choices = c(setNames(sort(all_species), sort(all_species))),
                          selected = NULL,
                          multiple = FALSE,
                          options = list(`live-search` = TRUE)),
              checkboxGroupInput("data_include_before", "Inputs:",
                                 choices = c("Catch", "Indices" = "Effort", "Length", "Age"),
                                 selected = c("Catch", "Effort", "Length", "Age")),
              uiOutput("conditional_age_ui"),
              checkboxGroupInput("data_include_after", NULL,
                                 choices = c("Biological Parameters", "Fishery Parameters"),
                                 selected = c("Biological Parameters", "Fishery Parameters")),
              
              tags$b("Single area SS3 model:"),
              br(),
              h6("(Review Input Tabs)"),
              downloadButton("download_btn", "Generate SS3 inputs", class = "btn-primary"),
              br(),
              br(),
              tags$b("Run SS model:"),
              br(),
              fileInput("upload_zip", "Upload ZIP File(s)",
                        accept = c(".zip"),
                        multiple = TRUE,
                        placeholder = "Select one or more previously downloaded ZIP files"),
              uiOutput("conditional_run_ss3_button_ui"),
              # checkboxInput("nohess_option", "–nohess", value = TRUE),
              # br(),
              actionButton("open_task_mgr_btn", "Task Manager", icon = icon("tasks"), class = "btn-secondary"),
              br(),
              div(style = "display: flex; justify-content: space-between; align-items: center;",
                  h5("Console:"),
                  actionButton("clear_console_btn", label = NULL, icon = icon("trash-alt"))
              ),
              div(style = "overflow-y: scroll; overflow-x: auto;",
                  verbatimTextOutput("console_output") # Removed placeholder argument
              ),
              # br(),br(),
              tags$div(
                tags$a("v0.81", href = "changelog.html", target = "_blank"),
                style="text-align: right; font-size: 0.9em; margin-top:15px;"
              )
            )
        ),
        div(id = "main", class="split main-panel",
            h4("Assessment Data Dashboard"),
            tabsetPanel(id = "primaryTabs", 
                        tabPanel("Catch", value = "catch_tab",
                                 br(),
                                 textOutput("catch_subset_text"),
                                 uiOutput("catch_species_select_ui"),
                                 actionButton("catch_refresh_btn", "Refresh", icon = icon("sync") , class = "btn-default"),
                                 br(),
                                 uiOutput("catch_sector_select_ui"),
                                 plotOutput("catch_plot", width = "800px", height = "600px")
                        ),
                        tabPanel("Indices",value = "indices_tab",
                                 br(),
                                 textOutput("effort_subset_text"),
                                 uiOutput("effort_species_select_ui"),
                                 actionButton("effort_refresh_btn", "Refresh", icon = icon("sync"), class = "btn-default"),
                                 br(),
                                 uiOutput("effort_fleet_select_ui"),
                                 plotOutput("effort_plot", width = "800px")
                        ),
                        tabPanel("Length",value = "length_tab",
                                 fluidPage(
                                   fluidRow(
                                     column(12,
                                            div(style = "padding: 15px 0;",
                                                checkboxInput("use_fis_length", "Use length data from FIS", value = TRUE),
                                                checkboxInput("use_released_fis_data", "Use discarded length data from FIS", value = TRUE),
                                                checkboxInput("use_bio_length", "Use length data from Biological databases", value = FALSE),
                                                radioButtons("length_metric", "Length Metric:",
                                                             choices = c("Fork Length (FL)" = "FL", "Total Length (TL)" = "TL"),
                                                             selected = "FL",
                                                             inline = TRUE),
                                                numericInput("length_class_input", "Length Class Interval (mm):",
                                                             value = 10, min = 1, max = 100, step = 1, width = '15%'),
                                                numericInput("min_length_input", "Minimum Length (mm, applies to length and age)",
                                                             value = 0, min = 0, step = 1, width = '15%'),
                                                numericInput("min_sample_size_length", "Minimum Sample Size (n):",
                                                             value = 1, min = 0, step = 1, width = '15%'),
                                                radioButtons("length_color_by", "Colour Plots By:",
                                                             choices = c("BioRegion", "Zone", "Location", "Sector", "Sex"),
                                                             selected = "Sex",
                                                             inline = TRUE),
                                                actionButton("refresh_btn", "Refresh", icon = icon("sync"), class = "btn-default")
                                            )
                                     )
                                   ),
                                   uiOutput("length_section1"),
                                   uiOutput("length_section2"),
                                   fluidRow(
                                     column(12,
                                            h5("Debug Information"),
                                            verbatimTextOutput("length_debug")
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Age", value = "age_tab",
                                 fluidPage(
                                   fluidRow(
                                     column(12,
                                            div(style = "padding: 15px 0;",
                                                checkboxInput("use_fis_age", "Use age data from FIS", value = TRUE),
                                                checkboxInput("use_bio_age", "Use age data from Biological databases", value = FALSE),
                                                radioButtons("age_color_by", "Colour Plots By:",
                                                             choices = c("BioRegion", "Zone", "Location", "Sector", "Sex"),
                                                             selected = "BioRegion",
                                                             inline = TRUE),
                                                actionButton("age_refresh_btn", "Refresh", icon = icon("sync"), class = "btn-default")
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            wellPanel( 
                                              h5("FIS Age Data"),
                                              uiOutput("age_availability_message"),
                                              uiOutput("year_select_age_ui"),
                                              pickerInput("location_select_age", "Select Locations for FIS Age Data",
                                                          choices = NULL,
                                                          selected = NULL,
                                                          multiple = TRUE,
                                                          options = list(`actions-box` = TRUE, `live-search` = TRUE)),
                                              uiOutput("fis_age_plot_ui"),
                                              style = "margin-bottom: 20px;" 
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            wellPanel( 
                                              h5("Biological and Historical Age Data"),
                                              uiOutput("year_select_bio_age_ui"),
                                              textOutput("bio_age_sectors"),
                                              uiOutput("bio_age_plot_ui"),
                                              style = "margin-bottom: 20px;"
                                            )
                                     )
                                   ),
                                   fluidRow(
                                     column(12,
                                            h5("Debug Information"),
                                            verbatimTextOutput("age_debug")
                                     )
                                   )
                                 )
                        ),
                        tabPanel("Biological Parameters",value = "bioparams_tab",
                                 br(),
                                 textOutput("bio_subset_text"),
                                 uiOutput("bio_species_select_ui"),
                                 # --- NEW: Checkbox to force show all ---
                                 checkboxInput("show_all_bio_species", "Force show all species", value = FALSE),
                                 # ---------------------------------------
                                 actionButton("bio_refresh_btn", "Refresh", icon = icon("sync"), class = "btn-default"),
                                 uiOutput("bio_warning_ui"),
                                 br(), br(), br(),
                                 uiOutput("bio_caption"),
                                 uiOutput("bio_table"),
                                 br(),
                                 uiOutput("bibliography_output"),
                                 fluidRow(
                                   column(width = 6,
                                          plotOutput("bioSizeAtAgePlot", height="450px", width="100%")
                                   ),
                                   column(width = 6,
                                          plotOutput("bioWeightPlot", height="450px", width="100%")
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 6,
                                          plotOutput("bioMaturityPlot", height="450px", width="100%")
                                   )
                                 )
                        ),
                        tabPanel("SS3 Model Options", value = "ss3options_tab",
                                 br(),
                                 p("Note: Default model setup estimates $R_0$, length-based logistic selectivity and retention."),
                                 checkboxInput("use_initial_catch", "Use Initial Catch", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_initial_catch == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("year0catch", "Year 0 Catch:", value = 0.1, min = 0, step = 1),
                                       numericInput("year0catchse", "Year 0 Catch SE:", value = 0.2, min = 0, step = 0.1),
                                       numericInput("initf", "Initial $F$:", value = 0.1, min = 0, step = 0.01),
                                       selectizeInput("year0fleet", "Fleet:", choices = NULL, selected = "Foreign.Trawl", options = list(create = TRUE, placeholder = "Select or type fleet"))
                                   )
                                 ),
                                 hr(),
                                 h5("Modify biological and fishery parameters"),
                                 checkboxInput("use_1_sex_model", "Use 1-sex model", value = FALSE),
                                 # --- NEW: Custom Max Size/Age Inputs ---
                                 checkboxInput("use_custom_max_size", "Specify Custom Max Population Size", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_max_size == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("custom_max_size", "Max Population Size (cm):", value = 100, min = 1, step = 1),
                                       helpText("Overrides the automatically calculated max length bin.")
                                   )
                                 ),
                                 checkboxInput("use_custom_max_age", "Specify Custom Max Population Age", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_max_age == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("custom_max_age", "Max Population Age (years):", value = 40, min = 1, step = 1),
                                       helpText("Overrides the max observed age. Sets the accumulator age.")
                                   )
                                 ),
                                 # -------------------------------------
                                 checkboxInput("use_custom_M", "Specify Custom Natural Mortality ($M$)", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_M == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("custom_M", "Natural Mortality ($M$):", value = 0.2, min = 0, max = 1, step = 0.01)
                                   )
                                 ),
                                 
                                 checkboxInput("use_age_post_settlement", "Specify Age Post-Settlement", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_age_post_settlement == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("age_post_settlement", "Age Post-Settlement:", value = 0, min = 0, step = 1)
                                   )
                                 ),
                                 checkboxInput("use_cv_growth_pattern", "Specify CV Growth Pattern", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_cv_growth_pattern == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("cv_growth_pattern", "CV Growth Pattern:", value = 0, min = 0, step = 1)
                                   )
                                 ),
                                 checkboxInput("use_custom_h", "Specify Custom Steepness ($h$)", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_h == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("custom_h", "Steepness ($h$):", value = 0.8, min = 0.2, max = 1, step = 0.01)
                                   )
                                 ),
                                 checkboxInput("use_custom_R0", "Specify Custom Initial Recruitment log($R_0$)", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_R0 == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("custom_R0", "Initial Recruitment log($R_0$):", value = 8.5, min = 0, step = 100)
                                   )
                                 ),
                                 checkboxInput("use_custom_sigma_r", "Specify Custom SigmaR", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_sigma_r == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("custom_sigma_r", "SigmaR:", value = 0.6, min = 0, step = 0.1)
                                   )
                                 ),
                                 checkboxInput("use_recdevs_range", "Specify recruitment deviation years", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_recdevs_range == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("first_year_recr_devs", "First year of main recr_devs:", value = 0, min = 0, step = 1),
                                       numericInput("last_year_recr_devs", "Last year of main recr_devs:", value = 0, min = 0, step = 1)
                                   )
                                 ),
                                 checkboxInput("estimate_growth_params", "Estimate Growth Parameters", value = FALSE),
                                 checkboxInput("use_time_varying_params", "Use Time-varying parameters", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_time_varying_params == true",
                                   div(
                                     style = "margin-left: 20px;",
                                     textAreaInput("time_varying_params_text", "Begin and end years of blocks:",
                                                   value = "", placeholder = "Enter years separated by spaces. Start each new block on a new line. For example:\n2002 2006\n2007 2009\n",
                                                   rows = 5, width = "25%"),
                                     checkboxInput("use_time_varying_growth_all", "Growth-all params", value = FALSE),
                                     checkboxInput("use_time_varying_growth_L_at_Amin", "Growth-L_at_Amin", value = FALSE),
                                     checkboxInput("use_time_varying_growth_L_at_Amax", "Growth-L_at_Amax", value = FALSE),
                                     checkboxInput("use_time_varying_growth_vbK", "Growth-vbK", value = FALSE),
                                     checkboxInput("use_time_varying_selectivity", "Selectivity", value = FALSE),
                                     checkboxInput("use_time_varying_retention", "Retention", value = FALSE)
                                   )
                                 ),
                                 hr(),
                                 h5("Data Weighting"),
                                 checkboxInput("use_custom_bias_adj", "Use Custom Bias Adjustments", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_custom_bias_adj == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("last_early_yr_nobias_adj", "Last early year no bias adjustment:", value = 1972.4, min = 0, step = 1),
                                       numericInput("first_yr_fullbias_adj", "First year full bias adjustment:", value = 2002.0, min = 0, step = 1),
                                       numericInput("last_yr_fullbias_adj", "Last year full bias adjustment:", value = 2019.6, min = 0, step = 1),
                                       numericInput("first_recent_yr_nobias_adj", "First recent year no bias adjustment:", value = 2020.3, min = 0, step = 1),
                                       numericInput("Use_max_bias_adj_in_MPD", "Use max bias adjustment in MPD:", value = 0.9245, min = 0, step = 0.1)
                                   )
                                 ),
                                 checkboxInput("estimate_Dirichlet", "Estimate Dirichlet ($ln(DM\\theta)$)", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.estimate_Dirichlet == true",
                                   div(style = "padding-left: 30px;",
                                       textAreaInput("dirichlet_textbox_value", "Dirichlet Parameter Lines:",
                                                     value = "", placeholder = "Enter values here...",
                                                     rows = 3, width = "50%")
                                   )
                                 ),
                                 checkboxInput("use_Q_extraSD", "Use Q_extraSD", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_Q_extraSD == true",
                                   div(style = "padding-left: 30px;",
                                       helpText("This option includes a parameter that will contain a value to be added to the input standard deviation of the survey variability.\n
                                  see https://nmfs-ost.github.io/ss3-doc/SS330_User_Manual_release.html#Qsetup"),
                                       numericInput("Q_extraSD", "Q_extraSD:", value = 0.05, min = 0, step = 0.01)
                                   )
                                 ),
                                 checkboxInput("use_francis_weighting", "Francis Weighting", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.use_francis_weighting == true",
                                   div(style = "padding-left: 30px;",
                                       textAreaInput("francis_weighting_value", "Francis Weighting Lines",
                                                     value = "",
                                                     placeholder = "Enter variance adjustments factors:... e.g.\nTip: Looks for suggested values in suggested_tunings.ss from a previous model run\n4    1   5.328853    #           1   5.328853 0.340902    5.328853   3.376112  28.765341 0.340902  len FISTrap\n5    1   0.255742    #           1   0.255742 0.334749    0.255742   0.149936   1.756688 0.334749  age FISTrap",
                                                     rows = 4, width = "75%")
                                   )
                                 )
                        ),
                        tabPanel("SS3 Sensitivity Analysis", value = "ss3sensitivity_tab",
                                 br(),
                                 shinyDirButton("dir", "Select Model Folder", "Please select a folder"),
                                 tags$b("Selected folder:"),
                                 verbatimTextOutput("dir_path_display", placeholder = TRUE),
                                 # hr(),
                                 actionButton("run_sensitivities_btn", "Run Sensitivities", class = "btn-primary"),

                                 hr(),
                                 h5("Jitter Analysis"),
                                 checkboxInput("jitter_checkbox", "Run Jitters", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.jitter_checkbox == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("njitters", "njitters", value = 25, min = 1, step = 1),
                                       numericInput("jitter_fraction", "jitter fraction", value = 0.1, min = 0, step = 0.01)
                                   )
                                 ),
                                 # hr(),
                                 h5("Retrospective analysis"),
                                 checkboxInput("retro_checkbox", "Run Retrospective analysis", value = FALSE),
                                 conditionalPanel(
                                   condition = "input.retro_checkbox == true",
                                   div(style = "padding-left: 30px;",
                                       numericInput("nyears", "nyears", value = 5, min = 1, step = 1)
                                   )
                                 ),
                                 # hr(),
                                 h5("Likelihood Profiles"),
                                 # checkboxInput("use_par_file_in_profile", "- use par file", value = FALSE),
                                 checkboxInput("use_par_file_in_profile", 
                                               label = HTML("<b>Option: use par file</b>"), 
                                               value = FALSE),
                                 checkboxInput("r0_profile", "$R_0$ (including Piner plots)", value = FALSE),
                                 checkboxInput("M_profile", "$M$", value = FALSE),
                                 checkboxInput("h_profile", "$h$", value = FALSE),
                                 checkboxInput("l_amax_fem_profile", "$L$ at $A_{max}$ (Fem)", value = FALSE),
                                 checkboxInput("l_amax_mal_profile", "$L$ at $A_{max}$ (Mal)", value = FALSE),
                                 checkboxInput("k_fem_profile", "VonBert $K$ (Fem)", value = FALSE),
                                 checkboxInput("k_mal_profile", "VonBert $K$ (Mal)", value = FALSE),
                                 checkboxInput("final_depletion_profile", "Final Depletion", value = FALSE),
                                 checkboxInput("current_spawning_biomass_profile", "Current spawning biomass", value = FALSE),
                                 # hr(), # Added horizontal rule
                                 h5("Fixed Parameter Scenarios"), # Added new heading
                                 checkboxInput("fixed_param_scenarios", "$M$, $h$, $\\sigma_R$", value = FALSE),
                                 # Add these two lines immediately after:
                                 checkboxInput("comp_weight_scenarios", "Compostion data weighting (double, halve)", value = FALSE),
                                 checkboxInput("index_weight_scenarios", "Index data weighting (double, halve)", value = FALSE),

                                 
                                 # --- START: New Code for Model Comparison ---
                                 hr(),
                                 h5("Model Comparison"),
                                 p("Use the button below to select model folders one by one. The selected folders will appear in the list below."),
                                 textInput("comparison_name", "Comparison Name (optional)", placeholder = "e.g., Sensitivity_Run_1"),
                                 shinyDirButton("add_comparison_dir_btn", "Select a Model Folder", "Please select a folder"),
                                 actionButton("clear_comparison_dirs_btn", "Clear List", icon = icon("trash-alt")),
                                 tags$b("Selected folders for comparison:"),
                                 verbatimTextOutput("comparison_dirs_display", placeholder = TRUE),
                                 actionButton("run_comparison_btn", "Run Comparison", class = "btn-primary"),
                                 # --- END: New Code for Model Comparison ---
                                 # --- START: New Button to delete exe files ---
                                 hr(),
                                 h5("Folder Cleanupn"),
                                 p("Use the button below to delete all exe files in the output folder."),
                                 actionButton("delete_exes_btn", "Delete .exe files from 'output' folder", class = "btn-danger")
                                 # --- END: New Button to delete exe files --
                        ),
                        tabPanel("Bias and Tuning", value = "bias_tuning_tab",
                                 fluidPage(
                                   h3("Bias Ramp and Composition Weighting"),
                                   p("This tool automates model tuning for bias ramp adjustments and data weighting for multiple models in parallel."),
                                   hr(),
                                   wellPanel(
                                     h4("1. Select Model Folders"),
                                     p("Use the button below to select the parent folder(s) of the model(s) you wish to tune."),
                                     shinyDirButton(
                                       "bias_tuning_add_dir_btn",
                                       "Select a Model Folder",
                                       "Select a model for tuning"
                                     ),
                                     actionButton("bias_tuning_clear_dirs_btn", "Clear List", icon = icon("trash-alt")),
                                     tags$b("Selected folders for tuning:"),
                                     verbatimTextOutput("bias_tuning_dirs_display"),
                                     hr(),
                                     h4("2. Configure Tuning Options"),
                                     checkboxGroupInput("tuning_weighting_method", "Composition Weighting Method(s):",
                                                        choices = c("Francis" = "francis", "Dirichlet" = "dirichlet"),
                                                        selected = "francis", inline = TRUE),
                                     p("Note that Dirichlet does not apply to size comps."),
                                     
                                     # # Conditional panel for Dirichlet options
                                     # conditionalPanel(
                                     #   condition = "input.tuning_weighting_method == 'dirichlet'",
                                     #   div(style="padding: 10px; border: 1px solid #ddd; border-radius: 5px; margin-top: 10px; background-color: #f9f9f9;",
                                     #       h5("Dirichlet Multinomial Options"),
                                     #       p("Note: The logic for applying Dirichlet weighting needs to be implemented in the script."),
                                     #       numericInput("dirichlet_param1", "DM Parameter 1 (Placeholder):", value = 1.0, step = 0.1),
                                     #       numericInput("dirichlet_param2", "DM Parameter 2 (Placeholder):", value = 100, step = 10)
                                     #   )
                                     # ),
                                     
                                     hr(),
                                     h4("3. Run Process"),
                                     p("Choose to run a single step or the full automated sequence."),
                                     div(style="display: flex; gap: 10px; margin-top: 15px;",
                                         actionButton("run_single_bias_adj", "Run Single Bias Ramp Adj.", class = "btn-secondary"),
                                         actionButton("run_full_tuning_sequence", "Run Full Tuning Sequence", class = "btn-primary")
                                     )
                                   )
                                 )
                        )
            )
        )
    ),
    
    # --- JAVASCRIPT TO ACTIVATE THE SPLITTER ---
    tags$script(HTML("
      Split(['#sidebar', '#main'], {
        sizes: [25, 75], // Initial sizes in percentages
        minSize: 250,    // Minimum size in pixels
        gutterSize: 8,
        cursor: 'col-resize'
      });
    "))
  )
}

# Helper functions for plotting biological parameters -------------------------------------------

# helper_bio_plots.R (or in app.R)

# Calculate von Bertalanffy Growth (no change from previous)
calculate_vb_lengths <- function(Linf, K, t0, ages) {
  if (any(is.na(c(Linf, K, t0))) || !is.numeric(Linf) || !is.numeric(K) || !is.numeric(t0)) {
    return(rep(NA, length(ages)))
  }
  Linf * (1 - exp(-K * (as.numeric(ages) - t0)))
}

# Calculate Schnute Growth (New Function)
# Parameters based on your CSV: y1, y2, a (kgrowth), b (gamma/shape), t1, t2
calculate_schnute_lengths <- function(y1, y2, a_rate, b_shape, t1, t2, ages) {
  # Ensure all parameters are numeric and ages is numeric
  params <- c(y1, y2, a_rate, b_shape, t1, t2)
  if (any(sapply(params, function(p) !is.numeric(p) || is.na(p))) || !is.numeric(ages)) {
    return(rep(NA, length(ages)))
  }
  if (t1 == t2) { # Avoid division by zero if t1 equals t2
    return(rep(y1, length(ages))) # Or handle as an error/warning
  }
  
  len_at_age <- numeric(length(ages))
  
  # Term for proportion of growth between t1 and t2
  # (1 - exp(-a_rate * (age - t1))) / (1 - exp(-a_rate * (t2 - t1)))
  
  denominator_val <- 1 - exp(-a_rate * (t2 - t1))
  
  for (i in seq_along(ages)) {
    age <- ages[i]
    frac_val <- NA
    
    if (abs(a_rate) < 1e-6) { # If 'a_rate' is effectively zero, linear interpolation proportion
      if ((t2 - t1) == 0) frac_val <- 0.5 # Or handle error, should be caught by t1==t2 above
      else frac_val <- (age - t1) / (t2 - t1)
    } else if (abs(denominator_val) < 1e-9) { # Denominator is effectively zero (should be rare if a_rate != 0 and t1 != t2)
      # This case implies -a_rate * (t2-t1) is a multiple of 2*pi*i for complex exp, or very large for real exp
      # Or if a_rate * (t2-t1) is very large, so exp(-term) -> 0, making denom = 1.
      # If denom is truly ~0 because exp(-a(t2-t1)) ~ 1 (and a!=0), implies a(t2-t1) ~0, handled by above.
      # Let's assume if denom is ~0 and a_rate !=0, it means the growth period is very short relative to rate,
      # or rate is such that it completes/saturates.
      # Simplified: if denom is zero, it might mean full transition or linear.
      # Using the linear approximation if denominator is problematic.
      if ((t2 - t1) == 0) frac_val <- 0.5
      else frac_val <- (age - t1) / (t2 - t1) # Fallback, though ideally covered
    } else {
      numerator_val <- 1 - exp(-a_rate * (age - t1))
      frac_val <- numerator_val / denominator_val
    }
    
    # Ensure frac_val is within reasonable bounds if it's an interpolator, typically [0,1] for ages between t1 and t2
    # However, Schnute can extrapolate, so we allow it.
    
    if (abs(b_shape) < 1e-6) { # b_shape (gamma) is effectively zero
      if (y1 <= 0 || y2 <= 0) { # Log form requires positive y1, y2
        len_at_age[i] <- NA # Or handle appropriately
        next
      }
      len_at_age[i] <- y1 * exp(log(y2 / y1) * frac_val)
    } else { # b_shape is not zero
      # Handle y1 = 0 case specifically, as y1^b_shape can be problematic (e.g. 0^negative)
      if (abs(y1) < 1e-9 && b_shape > 0) { # If y1 is zero and b_shape is positive
        len_at_age[i] <- y2 * (frac_val ^ (1 / b_shape))
      } else if (abs(y1) < 1e-9 && b_shape <= 0) { # y1 is zero and b_shape is not positive
        len_at_age[i] <- NA # Undefined or problematic
      }
      else { # y1 is not zero
        term1_pow_b <- y1^b_shape
        term2_pow_b <- y2^b_shape
        inside_bracket <- term1_pow_b + (term2_pow_b - term1_pow_b) * frac_val
        
        if(inside_bracket < 0 && (1/b_shape) %% 1 != 0 && b_shape %% 2 == 0) { # e.g. (-ve)^(0.5)
          len_at_age[i] <- NA
        } else if (inside_bracket == 0 && (1/b_shape) < 0) { # e.g. 0^(-ve)
          len_at_age[i] <- Inf # or NA
        }
        else {
          # sign(inside_bracket) needed if raising negative number to non-integer power
          len_at_age[i] <- sign(inside_bracket) * (abs(inside_bracket)^(1 / b_shape))
        }
      }
    }
  }
  return(len_at_age)
}


# Weight at Length and Maturity functions (no change from previous response)
calculate_weight_at_length <- function(a, b, len_values) {
  if (any(is.na(c(a, b))) || !is.numeric(a) || !is.numeric(b)) {
    return(rep(NA, length(len_values)))
  }
  a * (as.numeric(len_values)^b)
}

calculate_logistic_maturity <- function(L50_or_A50, L95_or_A95, values) {
  if (any(is.na(c(L50_or_A50, L95_or_A95))) ||
      !is.numeric(L50_or_A50) || !is.numeric(L95_or_A95) ||
      (L95_or_A95 - L50_or_A50 == 0)) {
    return(rep(NA, length(values)))
  }
  1 / (1 + exp(-log(19) * (as.numeric(values) - L50_or_A50) / (L95_or_A95 - L50_or_A50)))
}

# --- Plot 1: Size at Age (bio1_sizeatage.png equivalent) ---
render_biology_size_at_age <- function(bio_params,
                                       max_age_override = NULL,
                                       main_title_base = "Size at Age") {
  if (is.null(bio_params) || nrow(bio_params) != 1) {
    plot(0, type="n", axes=F, xlab="", ylab="", main="Select a Biological Unit."); return()
  }
  
  growth_curve_type_str <- bio_params$GrowthCurve %||% ""
  growth_measure_type <- bio_params$GrowthMeasurementType %||% "Length"
  
  # Female parameters
  gf_p1 <- bio_params$GrowthParamsFemale_1
  gf_p2 <- bio_params$GrowthParamsFemale_2
  gf_p3 <- bio_params$GrowthParamsFemale_3
  gf_p4 <- bio_params$GrowthParamsFemale_4
  gf_p5 <- bio_params$GrowthParamsFemale_5
  gf_p6 <- bio_params$GrowthParamsFemale_6
  
  # Male parameters
  gm_p1 <- bio_params$GrowthParamsMale_1
  gm_p2 <- bio_params$GrowthParamsMale_2
  gm_p3 <- bio_params$GrowthParamsMale_3
  gm_p4 <- bio_params$GrowthParamsMale_4
  gm_p5 <- bio_params$GrowthParamsMale_5
  gm_p6 <- bio_params$GrowthParamsMale_6
  
  has_male_params <- all(!is.na(c(gm_p1, gm_p2, gm_p3))) # VBGF needs 3, Schnute needs at least these for basic function
  if(grepl("Schnute", growth_curve_type_str, ignore.case = TRUE) && has_male_params) {
    has_male_params <- all(!is.na(c(gm_p4, gm_p5, gm_p6)))
  }
  nsexes <- ifelse(has_male_params, 2, 1)
  
  plot_max_age <- if(!is.null(max_age_override) && is.numeric(max_age_override)) max_age_override else (bio_params$MaxObservedAge %||% 20)
  ages <- 0:ceiling(plot_max_age)
  
  len_F <- NULL; len_M <- NULL; main_plot_title <- main_title_base
  plot_ylim_max <- 1 # default
  
  if (grepl("von Bertalanffy", growth_curve_type_str, ignore.case = TRUE)) {
    main_plot_title <- paste(main_title_base, "(von Bertalanffy)")
    if (any(is.na(c(gf_p1, gf_p2, gf_p3)))) {
      plot(0, type="n", axes=F, xlab="", ylab="", main="Female VBGF parameters missing."); return()
    }
    len_F <- calculate_vb_lengths(Linf=gf_p1, K=gf_p2, t0=gf_p3, ages=ages)
    plot_ylim_max <- gf_p1 * 1.15 # Linf
    if (nsexes == 2) {
      if(any(is.na(c(gm_p1, gm_p2, gm_p3)))) { nsexes <- 1 } else {
        len_M <- calculate_vb_lengths(Linf=gm_p1, K=gm_p2, t0=gm_p3, ages=ages)
        plot_ylim_max <- max(plot_ylim_max, gm_p1 * 1.15, na.rm = TRUE)
      }
    }
  } else if (grepl("Schnute", growth_curve_type_str, ignore.case = TRUE)) {
    main_plot_title <- paste(main_title_base, "(Schnute)")
    if (any(is.na(c(gf_p1, gf_p2, gf_p3, gf_p4, gf_p5, gf_p6)))) {
      plot(0, type="n", axes=F, xlab="", ylab="", main="Female Schnute parameters missing."); return()
    }
    len_F <- calculate_schnute_lengths(y1=gf_p1, y2=gf_p2, a_rate=gf_p3, b_shape=gf_p4, t1=gf_p5, t2=gf_p6, ages=ages)
    plot_ylim_max <- max(len_F, na.rm=TRUE) * 1.15 # Max observed from curve
    if(is.infinite(plot_ylim_max) || is.na(plot_ylim_max)) plot_ylim_max <- gf_p2 * 1.15 # approx with y2 if max fails
    
    if (nsexes == 2) {
      if(any(is.na(c(gm_p1, gm_p2, gm_p3, gm_p4, gm_p5, gm_p6)))) { nsexes <- 1} else {
        len_M <- calculate_schnute_lengths(y1=gm_p1, y2=gm_p2, a_rate=gm_p3, b_shape=gm_p4, t1=gm_p5, t2=gm_p6, ages=ages)
        plot_ylim_max <- max(plot_ylim_max, len_M, na.rm=TRUE) * 1.15
        if(is.infinite(plot_ylim_max) || is.na(plot_ylim_max)) plot_ylim_max <- max(gf_p2, gm_p2, na.rm=T) * 1.15
      }
    }
  } else {
    plot(0, type="n", axes=F, xlab="", ylab="", main=paste("Unknown GrowthCurve:", growth_curve_type_str)); return()
  }
  
  if (is.null(len_F) || all(is.na(len_F))) {
    plot(0, type="n", axes=F, xlab="", ylab="", main="Could not calculate female growth."); return()
  }
  if(is.infinite(plot_ylim_max) || is.na(plot_ylim_max) || plot_ylim_max <=0 ) plot_ylim_max <- max(c(1, len_F, len_M), na.rm=T) * 1.15 # Final fallback
  
  plot(ages, len_F, type = "n",
       ylim = c(0, plot_ylim_max), xlab = "Age (years)", ylab = paste(growth_measure_type),
       main = main_plot_title, las = 1, xaxs = "i", yaxs = "i", cex.lab=1.2, cex.axis=1.1, cex.main=1.3)
  abline(h = 0, col = "grey80")
  
  lines(ages, len_F, col = "red", lwd = 2.5, lty = 1)
  
  if (nsexes == 2 && !is.null(len_M) && !all(is.na(len_M))) {
    lines(ages, len_M, col = "blue", lwd = 2.5, lty = 2)
    legend("bottomright", legend = c("Female", "Male"), col = c("red", "blue"), lty = c(1,2), lwd = 2.5, bty = "n", cex=1.1)
  }
  grid(col="grey80"); box()
}

# --- Plot 2: Weight at Size (bio5_weightatsize.png equivalent) ---
render_biology_weight_at_size <- function(bio_params, main_title = "Weight-Length Relationship") {
  if (is.null(bio_params) || nrow(bio_params) != 1) {
    plot(0, type="n", axes=F, xlab="", ylab="", main="Select a Biological Unit."); return()
  }
  
  a_F <- bio_params$LengthWeightParamsFemale_1
  b_F <- bio_params$LengthWeightParamsFemale_2
  lw_measure_type <- bio_params$LengthWeightMeasurementType %||% "Length"
  lw_units <- bio_params$LengthWeightUnits %||% "g|mm"
  lw_unit_parts <- strsplit(lw_units, "\\|")[[1]]
  weight_unit <- lw_unit_parts[1]
  length_unit <- lw_unit_parts[2]
  
  has_male_params <- !is.na(bio_params$LengthWeightParamsMale_1) && !is.na(bio_params$LengthWeightParamsMale_2)
  nsexes <- ifelse(has_male_params, 2, 1)
  
  if (any(is.na(c(a_F, b_F)))) {
    plot(0, type="n", axes=F, xlab="", ylab="", main="Female W-L parameters missing or invalid."); return()
  }
  
  max_L_for_plot <- bio_params$MaximumLength %||% bio_params$GrowthParamsFemale_1 %||% 100 # Use max recorded or Linf or default
  length_range <- seq(0, max_L_for_plot, length.out = 200)
  
  wt_F <- calculate_weight_at_length(a_F, b_F, length_range)
  plot_ylim_max <- max(wt_F, na.rm = TRUE) * 1.15
  
  wt_M <- NULL
  if (nsexes == 2) {
    a_M <- bio_params$LengthWeightParamsMale_1
    b_M <- bio_params$LengthWeightParamsMale_2
    if(any(is.na(c(a_M, b_M)))){ has_male_params <- FALSE; nsexes <- 1} else {
      wt_M <- calculate_weight_at_length(a_M, b_M, length_range)
      plot_ylim_max <- max(plot_ylim_max, wt_M, na.rm = TRUE) * 1.15
    }
  }
  if(is.infinite(plot_ylim_max) || is.na(plot_ylim_max)) plot_ylim_max <- 1 # fallback
  
  plot(length_range, wt_F, type = "n",
       ylim = c(0, plot_ylim_max), xlab = paste(lw_measure_type, "(", length_unit, ")"), ylab = paste("Weight (", weight_unit, ")"),
       main = main_title, las = 1, xaxs = "i", yaxs = "i",  cex.lab=1.2, cex.axis=1.1, cex.main=1.3)
  abline(h = 0, col = "grey80")
  
  lines(length_range, wt_F, col = "red", lwd = 2.5, lty = 1)
  
  if (nsexes == 2 && !is.null(wt_M)) {
    lines(length_range, wt_M, col = "blue", lwd = 2.5, lty = 2)
    legend("topleft", legend = c("Female", "Male"), col = c("red", "blue"), lty = c(1,2), lwd = 2.5, bty = "n", cex=1.1)
  }
  grid(col="grey80"); box()
}

# --- Plot 3: Maturity (bio6_maturity.png equivalent) ---
render_biology_maturity <- function(bio_params, main_title = "Maturity Schedule") {
  if (is.null(bio_params) || nrow(bio_params) != 1) {
    plot(0, type="n", axes=F, xlab="", ylab="", main="Select a Biological Unit."); return()
  }
  
  # Determine if primarily Age or Length based from parameters provided for females
  # This assumes L50/L95 are in _1/_2 and A50/A95 are in _3/_4 if present
  # A more robust method would parse bio_params$MaturityCurve and bio_params$MaturityUnits
  param1_F <- NA; param2_F <- NA; plot_val_range <- NULL; xlab_custom <- "Value"
  
  # Check for Age-based parameters first for females
  if (!is.na(bio_params$MaturityParamsFemale_3) && !is.na(bio_params$MaturityParamsFemale_4)) {
    param1_F <- bio_params$MaturityParamsFemale_3 # A50
    param2_F <- bio_params$MaturityParamsFemale_4 # A95
    max_age_plot <- bio_params$MaxObservedAge %||% 20
    plot_val_range <- 0:ceiling(max_age_plot)
    xlab_custom <- "Age (years)"
  } else if (!is.na(bio_params$MaturityParamsFemale_1) && !is.na(bio_params$MaturityParamsFemale_2)) { # Then Length-based
    param1_F <- bio_params$MaturityParamsFemale_1 # L50
    param2_F <- bio_params$MaturityParamsFemale_2 # L95
    max_L_for_plot <- bio_params$MaximumLength %||% bio_params$GrowthParamsFemale_1 %||% 100
    plot_val_range <- seq(0, max_L_for_plot, length.out = 200)
    mat_measure_type <- bio_params$MaturityMeasurementType %||% "Length"
    mat_units_parts <- strsplit(bio_params$MaturityUnits %||% "mm", "\\|")[[1]]
    xlab_custom <- paste(mat_measure_type, "(", mat_units_parts[1], ")") # Use first unit part
  } else {
    plot(0, type="n", axes=F, xlab="", ylab="", main="Female maturity parameters missing."); return()
  }
  
  mat_F <- calculate_logistic_maturity(param1_F, param2_F, plot_val_range)
  
  # Male maturity (similar logic)
  has_male_params <- FALSE; mat_M <- NULL
  if(!is.na(bio_params$MaturityParamsMale_3) && !is.na(bio_params$MaturityParamsMale_4)){ # Age-based Male
    param1_M <- bio_params$MaturityParamsMale_3; param2_M <- bio_params$MaturityParamsMale_4
    if(!is.na(param1_M) && !is.na(param2_M)) {
      mat_M <- calculate_logistic_maturity(param1_M, param2_M, plot_val_range) # Use same range as female for simplicity
      if(!all(is.na(mat_M))) has_male_params <- TRUE
    }
  } else if (!is.na(bio_params$MaturityParamsMale_1) && !is.na(bio_params$MaturityParamsMale_2)){ # Length-based Male
    param1_M <- bio_params$MaturityParamsMale_1; param2_M <- bio_params$MaturityParamsMale_2
    if(!is.na(param1_M) && !is.na(param2_M)) {
      mat_M <- calculate_logistic_maturity(param1_M, param2_M, plot_val_range)
      if(!all(is.na(mat_M))) has_male_params <- TRUE
    }
  }
  nsexes <- ifelse(has_male_params, 2, 1)
  
  plot(plot_val_range, mat_F, type="n", ylim=c(0,1.05),
       xlab=xlab_custom, ylab="Proportion Mature",
       main=main_title, las=1, xaxs="i", yaxs="i", cex.lab=1.2, cex.axis=1.1, cex.main=1.3)
  abline(h=c(0,1), col="grey80")
  
  lines(plot_val_range, mat_F, col="red", lwd=2.5, lty=1)
  
  if(nsexes == 2 && !is.null(mat_M) && !all(is.na(mat_M))){
    lines(plot_val_range, mat_M, col="blue", lwd=2.5, lty=2)
    legend("bottomright", legend=c("Female", "Male"), col=c("red","blue"), lty=c(1,2), lwd=2.5, bty="n", cex=1.1)
  }
  grid(col="grey80"); box()
}

# --- Helper Function: Generate Custom DPIRD Plots ---
generate_DPIRD_plots <- function(replist, output_dir) {
  
  # 1. Setup Directory
  dpird_dir <- file.path(output_dir, "DPIRD_plots")
  if (!dir.exists(dpird_dir)) dir.create(dpird_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load ggplot2 if not already loaded
  if (!"package:ggplot2" %in% search()) suppressPackageStartupMessages(library(ggplot2))
  
  # Retrieve the model end year to distinguish history from forecast
  end_year <- replist$endyr
  
  # =========================================================================
  # PLOT 1: FRACTION OF UNFISHED SPAWNING BIOMASS (Depletion)
  # =========================================================================
  
  dq <- replist$derived_quants
  bratio_rows <- grep("^Bratio_\\d+$", rownames(dq))
  
  if (length(bratio_rows) > 0) {
    df <- dq[bratio_rows, ]
    df$Label <- rownames(df)
    df$Year <- as.numeric(sub("Bratio_", "", df$Label))
    df <- df[order(df$Year), ]
    
    # Calculate Intervals
    df$lo_95 <- pmax(0, df$Value - 1.96 * df$StdDev)
    df$hi_95 <- df$Value + 1.96 * df$StdDev
    z_60 <- qnorm(0.8) 
    df$lo_60 <- pmax(0, df$Value - z_60 * df$StdDev)
    df$hi_60 <- df$Value + z_60 * df$StdDev
    
    # Split into Historical and Forecast data frames
    # Overlapping at end_year ensures the lines connect
    df_hist <- df[df$Year <= end_year, ]
    df_fore <- df[df$Year >= end_year, ]
    
    p_dep <- ggplot(data = df, aes(x = Year, y = Value)) +
      # Reference Lines
      geom_hline(yintercept = 0.4, linetype = "dashed", color = "darkgreen", linewidth = 0.7) + 
      annotate("text", x = min(df$Year), y = 0.4, label = "Target (0.4)", hjust = 0, vjust = -0.5, color = "darkgreen", size = 3.5, fontface = "bold") +
      geom_hline(yintercept = 0.3, linetype = "dashed", color = "orange", linewidth = 0.7) + 
      annotate("text", x = min(df$Year), y = 0.3, label = "Threshold (0.3)", hjust = 0, vjust = -0.5, color = "orange", size = 3.5, fontface = "bold") +
      geom_hline(yintercept = 0.2, linetype = "dashed", color = "red", linewidth = 0.7) + 
      annotate("text", x = min(df$Year), y = 0.2, label = "Limit (0.2)", hjust = 0, vjust = -0.5, color = "red", size = 3.5, fontface = "bold") +
      
      # Historical Ribbons (Darker/Standard)
      geom_ribbon(data = df_hist, aes(ymin = lo_95, ymax = hi_95), fill = "grey80", alpha = 0.6) +
      geom_ribbon(data = df_hist, aes(ymin = lo_60, ymax = hi_60), fill = "grey60", alpha = 0.6) +
      
      # Forecast Ribbons (Lighter/Different to distinguish)
      geom_ribbon(data = df_fore, aes(ymin = lo_95, ymax = hi_95), fill = "grey95", alpha = 0.6) +
      geom_ribbon(data = df_fore, aes(ymin = lo_60, ymax = hi_60), fill = "grey85", alpha = 0.6) +
      
      # Historical Line (Solid)
      geom_line(data = df_hist, linewidth = 1.2, color = "black", linetype = "solid") +
      
      # Forecast Line (Dotted)
      geom_line(data = df_fore, linewidth = 1.2, color = "black", linetype = "dotted") +
      
      # Formatting
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Fraction of Unfished Spawning Biomass", 
           subtitle = "Solid: Historical | Dotted: Forecast (lighter shading)", 
           y = "Fraction of Unfished", x = "Year") +
      theme_classic(base_family = "Arial") +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(face = "bold", size = 12),
            panel.grid.major.y = element_line(color = "grey95"))
    
    ggsave(filename = file.path(dpird_dir, "Depletion_DPIRD_with_intervals.png"), plot = p_dep, width = 9, height = 6, dpi = 300)
  } else {
    warning("DPIRD Plot: No Bratio_YEAR derived quantities found.")
  }
  
  # =========================================================================
  # PLOT 2: FISHING MORTALITY (F)
  # =========================================================================
  
  # 1. Extract F Data (rows like "F_2023")
  f_rows <- grep("^F_\\d+$", rownames(dq))
  
  if (length(f_rows) > 0) {
    df_f <- dq[f_rows, ]
    df_f$Label <- rownames(df_f)
    df_f$Year <- as.numeric(sub("F_", "", df_f$Label))
    df_f <- df_f[order(df_f$Year), ]
    
    # 2. Intervals
    df_f$lo_95 <- pmax(0, df_f$Value - 1.96 * df_f$StdDev)
    df_f$hi_95 <- df_f$Value + 1.96 * df_f$StdDev
    z_60 <- qnorm(0.8) 
    df_f$lo_60 <- pmax(0, df_f$Value - z_60 * df_f$StdDev)
    df_f$hi_60 <- df_f$Value + z_60 * df_f$StdDev
    
    # Split into Historical and Forecast data frames
    df_f_hist <- df_f[df_f$Year <= end_year, ]
    df_f_fore <- df_f[df_f$Year >= end_year, ]
    
    # 3. Calculate Reference Points based on Natural Mortality (M)
    NatM <- NA
    m_match <- grep("NatM", replist$parameters$Label)
    if (length(m_match) > 0) {
      NatM <- replist$parameters$Value[m_match[1]]
    }
    
    p_f <- ggplot(data = df_f, aes(x = Year, y = Value))
    
    # Add Reference Lines if M was found
    if (!is.na(NatM)) {
      F_targ <- (2/3) * NatM
      F_thresh <- NatM
      F_lim <- 1.5 * NatM
      
      p_f <- p_f +
        # Target (2/3 M) - Green
        geom_hline(yintercept = F_targ, linetype = "dashed", color = "darkgreen", linewidth = 0.7) + 
        annotate("text", x = min(df_f$Year), y = F_targ, label = paste0("Target (", round(F_targ, 3), ")"), 
                 hjust = 0, vjust = -0.5, color = "darkgreen", size = 3.5, fontface = "bold") +
        # Threshold (M) - Orange
        geom_hline(yintercept = F_thresh, linetype = "dashed", color = "orange", linewidth = 0.7) + 
        annotate("text", x = min(df_f$Year), y = F_thresh, label = paste0("Threshold (", round(F_thresh, 3), ")"), 
                 hjust = 0, vjust = -0.5, color = "orange", size = 3.5, fontface = "bold") +
        # Limit (1.5 M) - Red
        geom_hline(yintercept = F_lim, linetype = "dashed", color = "red", linewidth = 0.7) + 
        annotate("text", x = min(df_f$Year), y = F_lim, label = paste0("Limit (", round(F_lim, 3), ")"), 
                 hjust = 0, vjust = -0.5, color = "red", size = 3.5, fontface = "bold")
    } else {
      warning("DPIRD Plot: Could not find Natural Mortality parameter to calculate F reference points.")
    }
    
    # Add Ribbons and Lines
    p_f <- p_f +
      # Historical Ribbons
      geom_ribbon(data = df_f_hist, aes(ymin = lo_95, ymax = hi_95), fill = "grey80", alpha = 0.6) +
      geom_ribbon(data = df_f_hist, aes(ymin = lo_60, ymax = hi_60), fill = "grey60", alpha = 0.6) +
      
      # Forecast Ribbons (Lighter)
      geom_ribbon(data = df_f_fore, aes(ymin = lo_95, ymax = hi_95), fill = "grey95", alpha = 0.6) +
      geom_ribbon(data = df_f_fore, aes(ymin = lo_60, ymax = hi_60), fill = "grey85", alpha = 0.6) +
      
      # Historical Line (Solid)
      geom_line(data = df_f_hist, linewidth = 1.2, color = "black", linetype = "solid") +
      
      # Forecast Line (Dotted)
      geom_line(data = df_f_fore, linewidth = 1.2, color = "black", linetype = "dotted") +
      
      # Formatting
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(title = "Fishing Mortality (F)", 
           subtitle = "Solid: Historical | Dotted: Forecast (lighter shading)", 
           y = "Fishing Mortality (F)", x = "Year") +
      theme_classic(base_family = "Arial") +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            axis.title = element_text(face = "bold", size = 12),
            panel.grid.major.y = element_line(color = "grey95"))
    
    ggsave(filename = file.path(dpird_dir, "F_value_DPIRD_with_intervals.png"), plot = p_f, width = 9, height = 6, dpi = 300)
    message(paste("DPIRD Plots generated in:", dpird_dir))
    
  } else {
    warning("DPIRD Plot: No F_YEAR derived quantities found.")
  }
}

# Server ------------------------------------------------------------------



# Define the server logic
server <- function(input, output, session) {
  
  # --- 1. REACTIVE DATA BINDINGS ---
  # Explicitly initialize local variables from the global environment first.
  # This prevents them from starting as NULL and crashing the app before data is loaded.
  Fixedsiteonly <- if(exists("Fixedsiteonly", envir = .GlobalEnv)) .GlobalEnv$Fixedsiteonly else data.frame()
  Merged_Kim_Pilb <- if(exists("Merged_Kim_Pilb", envir = .GlobalEnv)) .GlobalEnv$Merged_Kim_Pilb else data.frame()
  FISages <- if(exists("FISages", envir = .GlobalEnv)) .GlobalEnv$FISages else data.frame()
  WCD_data <- if(exists("WCD_data", envir = .GlobalEnv)) .GlobalEnv$WCD_data else data.frame()
  catch_ts <- if(exists("catch_ts", envir = .GlobalEnv)) .GlobalEnv$catch_ts else data.frame()
  effort_ts <- if(exists("effort_ts", envir = .GlobalEnv)) .GlobalEnv$effort_ts else data.frame()
  bupBiologicalUnitParam <- if(exists("bupBiologicalUnitParam", envir = .GlobalEnv)) .GlobalEnv$bupBiologicalUnitParam else data.frame()
  bib_data <- if(exists("bib_data", envir = .GlobalEnv)) .GlobalEnv$bib_data else NULL
  
  # Now make these local variables reactive
  makeReactiveBinding("Fixedsiteonly")
  makeReactiveBinding("Merged_Kim_Pilb")
  makeReactiveBinding("FISages")
  makeReactiveBinding("WCD_data")
  makeReactiveBinding("catch_ts")
  makeReactiveBinding("effort_ts")
  makeReactiveBinding("bupBiologicalUnitParam")
  makeReactiveBinding("bib_data")
  
  # Tracks if the full data has been loaded
  data_loaded_trigger <- reactiveVal(FALSE)
  
  # --- 2. LOAD DATA BUTTON LOGIC ---
  output$load_data_ui <- renderUI({
    # Only show button if the file exists and we haven't loaded it yet
    if (file.exists("import_DPIRD_data.R") && !data_loaded_trigger()) {
      actionButton("btn_load_import_data", "Load Full Data (import_DPIRD_data.R)", 
                   icon = icon("database"), 
                   class = "btn-success", 
                   style = "width: 100%; margin-bottom: 10px;")
    } else if (data_loaded_trigger()) {
      div(style="text-align: center; color: green; font-weight: bold; margin-bottom: 10px;",
          icon("check"), " Full Data Loaded")
    } else {
      # File doesn't exist
      div(style="text-align: center; color: orange; font-style: italic; margin-bottom: 10px;",
          "Running in restricted mode (No data script found)")
    }
  })
  
  observeEvent(input$btn_load_import_data, {
    req(file.exists("import_DPIRD_data.R"))
    
    showModal(modalDialog("Loading large datasets, please wait...", footer=NULL))
    
    tryCatch({
      # Source into a temporary environment first to avoid clutter
      temp_env <- new.env()
      source("import_DPIRD_data.R", local = temp_env)
      
      # Update the global/server-scope reactive variables
      if(exists("Fixedsiteonly", envir = temp_env)) Fixedsiteonly <<- temp_env$Fixedsiteonly
      if(exists("Merged_Kim_Pilb", envir = temp_env)) Merged_Kim_Pilb <<- temp_env$Merged_Kim_Pilb
      if(exists("FISages", envir = temp_env)) FISages <<- temp_env$FISages
      if(exists("WCD_data", envir = temp_env)) WCD_data <<- temp_env$WCD_data
      if(exists("catch_ts", envir = temp_env)) catch_ts <<- temp_env$catch_ts
      if(exists("effort_ts", envir = temp_env)) effort_ts <<- temp_env$effort_ts
      if(exists("bupBiologicalUnitParam", envir = temp_env)) bupBiologicalUnitParam <<- temp_env$bupBiologicalUnitParam
      if(exists("bib_data", envir = temp_env)) bib_data <<- temp_env$bib_data
      
      data_loaded_trigger(TRUE)
      
      # Trigger a species picker update now that we have data
      # (Accessing the first species to set a default if needed)
      all_new_species <- unique(c(Fixedsiteonly$SpeciesName, Merged_Kim_Pilb$SpeciesName))
      updatePickerInput(session, "species_select", choices = sort(all_new_species))
      
      removeModal()
      showNotification("Data loaded successfully!", type = "message")
      
    }, error = function(e) {
      removeModal()
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 10)
    })
  })
  
  # --- 3. UI VISIBILITY CONTROL ---
  # Modified to listen to data_loaded_trigger() instead of the static SENSITIVE_DATA_LOADED
  observe({
    if (!data_loaded_trigger()) {
      # --- RESTRICTED MODE ---
      # Hide all content tabs except SS3 Sensitivity and Bias/Tuning
      tabs_to_hide <- c("catch_tab", "indices_tab", "length_tab", "age_tab", "bioparams_tab", "ss3options_tab")
      
      for (tab_id in tabs_to_hide) {
        if (tab_id != "ss3sensitivity_tab" && tab_id != "bias_tuning_tab") {
          shinyjs::hide(selector = paste0("li a[data-value=", tab_id, "]"))
        }
      }
      
      shinyjs::hide("species_select")
      shinyjs::hide("data_include_before")
      shinyjs::hide("conditional_age_ui")
      shinyjs::hide("data_include_after")
      shinyjs::hide("download_btn")
      
      shinyjs::hide(selector = "#sidebar h4:contains('Assessment Inputs')")
      shinyjs::hide(selector = "#sidebar h5:contains('Refresh all tabs before downloading.')")
      shinyjs::hide(selector = "#sidebar b:contains('Single area SS3 model:')")
      shinyjs::hide(selector = "#sidebar br:eq(2)") 
      
      updateTabsetPanel(session, "primaryTabs", selected = "ss3sensitivity_tab")
      
    } else {
      # --- FULL MODE (Data Loaded) ---
      shinyjs::show(selector = "li a[data-value]") 
      shinyjs::show("species_select")
      shinyjs::show("data_include_before")
      shinyjs::show("data_include_after")
      shinyjs::show("download_btn")
      shinyjs::show(selector = "#sidebar h4:contains('Assessment Inputs')")
      
      # Re-evaluate logic for 'Age' checkbox based on new data
      # (Triggers the observe logic later in the script)
    }
  }, priority = 100)
  
  # Set the global theme for all ggplot2 plots to use Arial font
  theme_set(theme_minimal(base_family = "Arial"))
  
  # --- CONSOLE LOGGING SETUP (Standalone reactiveVal) ---
  console_log <- reactiveVal(character(0))
  refresh_trigger <- reactiveVal(0)
  
  # Consolidated append_to_log function
  append_to_log <- function(message_text) {
    if (!is.character(message_text)) {
      message_text <- tryCatch(as.character(message_text), error = function(e) "Log: Non-character message could not be converted.")
    }
    # new_log_entry <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", message_text)
    new_log_entry <- message_text
    current_log_val <- console_log()
    console_log(c(current_log_val, new_log_entry))
  }
  
  # In your server function, modify the output$console_output definition
  output$console_output <- renderText({
    log_entries <- console_log()
    if (length(log_entries) == 0) {
      "Console is ready..." # Your custom placeholder text goes here
    } else {
      paste(log_entries, collapse = "\n")
    }
  })
  
  observeEvent(input$clear_console_btn, {
    console_log(character(0))
  })
  # --- END CONSOLE LOGGING SETUP ---
  
  rv <- reactiveValues(
    retained_years = NULL, released_years = NULL, kim_pilb_years = NULL,
    bio_age_years = NULL, retained_locations = NULL, released_locations = NULL,
    kim_pilb_locations = NULL, age_locations = NULL, bio_age_locations = NULL,
    kim_pilb_zones = NULL, bio_age_zones = NULL, kim_pilb_bioregions = NULL,
    bio_age_bioregions = NULL,
    kim_pilb_sectors = NULL, # ADD THIS LINE
    sensitivity_process = NULL , 
    bias_tuning_process = NULL,
    tuning_folders = character(0),
    comparison_process = NULL,
    comparison_folders = character(0) # <-- ADD THIS LINE
  )
  
  
  
  observeEvent(input$open_task_mgr_btn, {
    # This command is specific to Windows.
    if (Sys.info()['sysname'] == "Windows") {
      tryCatch({
        # shell.exec is the most reliable way to open applications on Windows.
        shell.exec("taskmgr.exe")
        append_to_log("Request to open Task Manager sent.")
      }, error = function(e) {
        append_to_log(paste("Error: Failed to open Task Manager -", e$message))
        showNotification("Could not open Task Manager.", type = "error")
      })
    } else {
      # Provide helpful feedback for non-Windows users.
      showNotification("This feature is only available on Windows.", type = "warning")
      append_to_log("Info: 'Open Task Manager' button is for Windows only.")
    }
  })
  
  # Observer for the 'Delete .exe files' button
  observeEvent(input$delete_exes_btn, {
    # Define the path to the output folder
    output_folder_path <- file.path(getwd(), "output")
    
    # Check if the directory exists
    if (!dir.exists(output_folder_path)) {
      showNotification("The 'output' folder does not exist.", type = "warning")
      append_to_log("Attempted to delete .exe files, but the 'output' folder was not found.")
      return()
    }
    
    # List all files with the .exe extension in the folder (including subfolders)
    exe_files_to_delete <- list.files(
      path = output_folder_path,
      pattern = "\\.exe$",
      full.names = TRUE,
      ignore.case = TRUE,
      recursive = TRUE # This is the key change
    )
    
    # Check if any .exe files were found
    if (length(exe_files_to_delete) == 0) {
      showNotification("No .exe files were found in the 'output' folder.", type = "message")
      append_to_log("No .exe files found to delete in 'output' folder.")
      return()
    }
    
    # Delete the files
    tryCatch({
      file.remove(exe_files_to_delete)
      # Filter for files that were actually deleted
      deleted_count <- length(exe_files_to_delete) - sum(file.exists(exe_files_to_delete))
      showNotification(
        paste0("Successfully deleted ", deleted_count, " .exe file(s) from the 'output' folder."),
        type = "message"
      )
      append_to_log(paste0("Deleted the following files: ", paste(basename(exe_files_to_delete), collapse = ", ")))
    }, error = function(e) {
      showNotification(
        paste("Error deleting files:", e$message),
        type = "error"
      )
      append_to_log(paste("Error deleting .exe files:", e$message))
    })
  })
  
  # download_success is still used by run_ss3_ui logic
  download_success <- reactiveVal(FALSE)
  
  
  
  
  bookmark_name_for_onBookmarked <- reactiveVal(NULL)
  bookmark_list_update_trigger <- reactiveVal(0)
  
  
  # Reactive value to store the path of the downloaded ZIP file
  downloaded_zip_path <- reactiveVal(NULL)
  
  
  # --- START: FOLDER SELECTION SERVER LOGIC ---
  # Define a path to the app's 'output' subfolder
  output_folder_path <- file.path(getwd(), "output")
  # Create the directory if it doesn't exist
  if (!dir.exists(output_folder_path)) { dir.create(output_folder_path) }
  
  # Define the locations the user can browse from.
  # We add our new output folder path to the list.
  volumes <- c(
    `App Output Folder` = output_folder_path,
    Home = fs::path_home(),
    getVolumes()()
  )
  
  shinyDirChoose(
    input,
    "dir",
    roots = volumes,
    session = session
  )
  # --- END: FOLDER SELECTION SERVER LOGIC ---
  
  # In the server() function, with other reactive expressions
  
  # --- START: REACTIVES FOR FOLDER PATH ---
  # Reactive to store and parse the chosen path
  selected_folder_path <- reactive({
    req(is.list(input$dir))
    parseDirPath(volumes, input$dir)
  })
  
  # Display the selected path in the UI
  output$dir_path_display <- renderText({
    path <- selected_folder_path()
    if (length(path) == 0) {
      "No folder selected"
    } else {
      path
    }
  })
  # --- END: REACTIVES FOR FOLDER PATH ---
  
  # --- START: New Sensitivity Run Logic ---
  
  # Reactive expression to gather sensitivity options
  sensitivity_options <- reactive({
    path <- selected_folder_path()
    list(
      model_folder = if (length(path) > 0) path else NULL, # <-- ADD THIS
      jitter = input$jitter_checkbox,
      njitters = input$njitters,
      jitter_fraction = input$jitter_fraction,
      profile_use_par = input$use_par_file_in_profile,
      profile_r0 = input$r0_profile,
      profile_m = input$M_profile,
      profile_h = input$h_profile,
      profile_l_amax_fem = input$l_amax_fem_profile,
      profile_l_amax_mal = input$l_amax_mal_profile,
      profile_k_fem = input$k_fem_profile,
      profile_k_mal = input$k_mal_profile,
      profile_final_depletion = input$final_depletion_profile,
      profile_current_spawning_biomass = input$current_spawning_biomass_profile,
      retrospective = input$retro_checkbox,
      retro_nyears = input$nyears,
      fixed_param_scenarios = input$fixed_param_scenarios,
      comp_weight_scenario = input$comp_weight_scenarios,
      index_weight_scenario = input$index_weight_scenarios
    )
  })
  
  # Observer for the 'Run Sensitivities' button
  observeEvent(input$run_sensitivities_btn, {
    # Prevent starting a new process if one is already running
    if (!is.null(rv$sensitivity_process) && rv$sensitivity_process$is_alive()) {
      showNotification("A sensitivity analysis is already running.", type = "warning")
      return()
    }
    
    sens_opts <- sensitivity_options()
    append_to_log("--- Starting sensitivity runs in background... ---")
    
    background_r_function <- function(options) {
      # This code runs in a new R session.
      # Use print() or cat() to send messages back to the main app.
      print("Background process initiated.")
      sensitivity_options <- options
      source("SS_sensitivities.R", local = TRUE)
      return("Background script execution finished.")
    }
    
    # Store the process object in our reactive value
    rv$sensitivity_process <- r_bg(
      func = background_r_function,
      args = list(options = sens_opts),
      supervise = TRUE
    )
    
    showNotification(
      "Sensitivity analysis started in the background. See console log for progress.",
      type = "message",
      duration = 8
    )
  })
  
  # Create a timer that will check for output every 500ms
  output_poller <- reactiveTimer(500)
  

  
  # --- END: New Sensitivity Run Logic ---
  
  # In the server function
  
  # --- START: New Refactored Function for Model Comparison ---
  # --- SERVER-SIDE LOGIC FOR FOLDER SELECTION ---
  # Define the default path.
  # Note: This path is hardcoded as requested. For better portability, you could use
  bias_tuning_default_path <- output_folder_path
  
  bias_tuning_volumes <- c(Repository = bias_tuning_default_path)
  

  
  # --- Main process for Bias Ramp and Francis Tuning ---
  # Create a reactive value to store the log text for this specific tab
  tuning_log_text <- reactiveVal("")
  
  # --- FINAL, CORRECTED observeEvent block for Bias and Tuning ---
  

  
  # Render the log output in the new tab's main panel
  output$bias_tuning_log <- renderText({
    tuning_log_text()
  })
  # --- END OF BIAS AND TUNING SERVER LOGIC ---
  
  # --- Renders the log text in the UI ---
  output$log_output <- renderText({
    log_text()
  })
  
  # Renders the status (can be the same as the log or a separate summary)
  output$status_output <- renderText({
    log_text()
  })
  
  start_model_comparison <- function(folder_paths, comparison_name_prefix) {
    # This function encapsulates the logic to start a comparison run.
    # It can be called from the manual button or the automatic batch process.
    
    if (length(folder_paths) < 2) {
      showNotification("Please select at least two model folders to compare.", type = "warning")
      append_to_log("Comparison start failed: less than two folders provided.")
      return()
    }
    
    if (!is.null(rv$comparison_process) && rv$comparison_process$is_alive()) {
      showNotification("A model comparison is already running.", type = "warning")
      return()
    }
    
    append_to_log("--- Starting model comparison in background... ---")
    
    comp_name <- comparison_name_prefix
    if (is.null(comp_name) || comp_name == "") {
      comp_name <- paste0("comp_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    } else {
      # Sanitize the name and add a timestamp
      comp_name <- paste0(gsub("[^A-Za-z0-9_]", "_", comp_name), "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    }
    
    # This background function now includes a tryCatch for robust error logging
    background_comparison_function <- function(folder_paths, filename_prefix) {
      suppressPackageStartupMessages({
        library(r4ss)
      })
      
      cat("Background comparison process initiated.\n")
      cat("Folders to compare:\n", paste(folder_paths, collapse = "\n"), "\n")
      
      tryCatch({
        model_names <- basename(folder_paths)
        
        plotdir <- file.path(getwd(), "output", "comparison", filename_prefix)
        if (!dir.exists(plotdir)) {
          dir.create(plotdir, recursive = TRUE)
          cat("Created plot directory:", plotdir, "\n")
        }
        
        dirs_exist <- dir.exists(folder_paths)
        if(!all(dirs_exist)){
          cat("ERROR: The following model directories do not exist:\n")
          cat(paste(folder_paths[!dirs_exist], collapse="\n"))
          stop("One or more selected model directories not found.")
        }
        
        cat("Reading model outputs...\n")
        models_list <- setNames(lapply(folder_paths, function(d) r4ss::SS_output(dir = d, verbose = FALSE, printstats = FALSE)), model_names)
        
        cat("Summarizing model results...\n")
        mod.sum <- r4ss::SSsummarize(models_list)
        
        cat("Generating comparison plots...\n")
        r4ss::SSplotComparisons(
          mod.sum,
          plot = FALSE,
          print = TRUE,
          plotdir = plotdir,
          btarg = 0.4,
          minbthresh = 0.2,
          filenameprefix = paste0(filename_prefix, "_"),
          legend = TRUE,
          legendlabels = model_names,
          png = TRUE
        )
        
        cat("Generating REP comparison plots...\n")
        
        # NatMort = models_list$test_AgeErr0.5$MGparmAdj$NatM_uniform_Fem_GP_1[1]
        
        # NatMort_for_REP_Plot = 0.11
        
        NatMort_for_REP_Plot = models_list[[1]]$MGparmAdj$NatM_uniform_Fem_GP_1[1]
        
        cat("NatMort is taken from first model selected, i.e. ", NatMort_for_REP_Plot, "\n")
        # Ftarg = 2/3*NatMort
        # Fthresh = NatMort
        # Flim = 3/2*NatMort
        source("SSplotComparisonsREP.R")
        
        SSplotComparisons_REP(summaryoutput=mod.sum,
                             plot = FALSE,
                             print = TRUE,
                             plotdir = plotdir,
                             btarg = 0.4,
                             minbthresh = 0.2,
                             filenameprefix = paste0(filename_prefix, "_REP_"),
                             legend=T,
                             Ftarg = 2/3*NatMort_for_REP_Plot,
                             Fthresh=NatMort_for_REP_Plot,
                             Flim=3/2*NatMort_for_REP_Plot,
                             legendlabels=model_names,
                             png = TRUE)
        
        cat("Comparison plots generated successfully.\n")
        
        cat("Output saved in:", plotdir, "\n")
        
        return(paste("Comparison", filename_prefix, "complete."))
        
      }, error = function(e) {
        cat("\n--- ERROR IN BACKGROUND COMPARISON ---\n")
        cat("An error occurred during the comparison process.\n")
        cat("FULL ERROR MESSAGE:", e$message, "\n")
        cat("------------------------------------\n")
        stop("Background comparison failed. See console log for the full error message.")
      })
    }
    
    rv$comparison_process <- r_bg(
      func = background_comparison_function,
      args = list(folder_paths = folder_paths, filename_prefix = comp_name),
      supervise = TRUE
    )
    
    showNotification(
      paste("Model comparison", comp_name, "started. See console for progress."),
      type = "message",
      duration = 8
    )
  }
  # --- END: New Refactored Function for Model Comparison ---
  
  # --- START: New Model Comparison Server Logic ---
  
  # Server-side logic for the 'add_comparison_dir_btn' file explorer
  shinyDirChoose(
    input,
    "add_comparison_dir_btn",
    roots = volumes, # 'volumes' is already defined for the other shinyDirChoose
    session = session
  )
  
  # START - FRANCIS AND BIAS TUNE LOGIC
  
  # Reactive value to store the log
  log_text <- reactiveVal("")
  
  # In your server function, add this new code block.
  
  # --- START: New Parallel Bias and Tuning Server Logic ---
  
  # Server-side logic for the folder selection pop-up
  shinyDirChoose(
    input,
    "bias_tuning_add_dir_btn",
    roots = volumes, # 'volumes' is already defined elsewhere in the app
    session = session
  )
  
  # Observer to add a selected folder to the tuning list
  observeEvent(input$bias_tuning_add_dir_btn, {
    req(is.list(input$bias_tuning_add_dir_btn))
    path <- parseDirPath(volumes, input$bias_tuning_add_dir_btn)
    
    if (length(path) > 0) {
      if (!path %in% rv$tuning_folders) {
        rv$tuning_folders <- c(rv$tuning_folders, path)
        append_to_log(paste("Added to tuning list:", path))
      } else {
        showNotification("That folder is already in the tuning list.", type = "warning")
      }
    }
  })
  
  # Observer for the 'Clear List' button
  observeEvent(input$bias_tuning_clear_dirs_btn, {
    rv$tuning_folders <- character(0)
    append_to_log("Cleared the parallel tuning list.")
  })
  
  # Render the display for the list of selected folders
  output$bias_tuning_dirs_display <- renderText({
    if (length(rv$tuning_folders) == 0) {
      "No folders selected."
    } else {
      paste(rv$tuning_folders, collapse = "\n")
    }
  })
  
  # Helper function to launch a parallel tuning process
  # This avoids duplicating the foreach setup for both buttons
  # In your server function, replace the entire launch_tuning_process function.
  
  launch_tuning_process <- function(jobs_df, task_name) {
    if (is.null(jobs_df) || nrow(jobs_df) == 0) {
      showNotification("No tuning jobs to run.", type = "error")
      return()
    }
    
    append_to_log(paste("--- Starting Parallel Task:", task_name, "for", nrow(jobs_df), "jobs... ---"))
    
    app_dir <- getwd()
    exe_folder <- file.path(app_dir, "Stock_Synthesis_latest")
    exe_name <- if (file.exists(file.path(exe_folder, "ss.exe"))) "ss.exe" else "ss3.exe"
    exe_path <- file.path(exe_folder, exe_name)
    
    if (!file.exists(exe_path)) {
      msg <- paste("CRITICAL ERROR: Stock Synthesis executable not found in:", exe_folder)
      append_to_log(msg)
      showNotification(msg, type = "error", duration = 10)
      return()
    }
    
    n_cores <- parallel::detectCores() - 1
    if (n_cores < 1) n_cores <- 1
    append_to_log(paste("Setting up parallel processing with", n_cores, "cores."))
    
    my_cluster <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(my_cluster)
    
    on.exit({
      append_to_log("Stopping parallel cluster for tuning.")
      try(parallel::stopCluster(my_cluster), silent = TRUE)
    }, add = TRUE)
    
    showNotification(
      paste("Parallel task '", task_name, "' started for", nrow(jobs_df), "jobs. See console for progress."),
      type = "message",
      duration = 8
    )
    
    # Use foreach to run the tuning script for each job in the data frame
    results <- foreach(
      i = 1:nrow(jobs_df),
      .packages = c('r4ss', 'processx', 'stringr'), 
      .errorhandling = 'pass'
    ) %dopar% {
      
      job_details <- as.list(jobs_df[i, ])
      
      # Define the options for the script for this specific worker
      bias_tuning_options <- c(job_details, list(
        exe_path = exe_path,
        exe_name = exe_name,
        tuning_dir = file.path(job_details$model_dir, "tuning")
      ))
      
      log_output <- capture.output({
        source("SS_bias_tuning.R", local = TRUE)
      })
      
      # Check for success based on the task, using the dynamic folder names from the script
      base_folder_name <- basename(bias_tuning_options$model_dir)
      
      success <- if (bias_tuning_options$run_step == "full_sequence") {
        weighting_suffix <- if (!is.na(bias_tuning_options$weighting_method)) {
          switch(bias_tuning_options$weighting_method, "francis" = "_WtFr", "dirichlet" = "_WtDir", "")
        } else {""}
        full_prefix <- paste0(base_folder_name, weighting_suffix)
        final_dir_name <- paste0(full_prefix, "_final_model")
        final_dir_path <- file.path(bias_tuning_options$tuning_dir, final_dir_name)
        dir.exists(final_dir_path) && file.exists(file.path(final_dir_path, "Report.sso"))
        
      } else if (bias_tuning_options$run_step == "bias_ramp_only") {
        base_output_dir_name <- paste0(base_folder_name, "_bias_adj")
        all_dirs <- list.dirs(bias_tuning_options$tuning_dir, full.names = FALSE, recursive = FALSE)
        matching_dirs <- all_dirs[startsWith(all_dirs, base_output_dir_name)]
        if (length(matching_dirs) > 0) {
          any(sapply(matching_dirs, function(d) file.exists(file.path(bias_tuning_options$tuning_dir, d, "Report.sso"))))
        } else {
          FALSE
        }
      } else {
        FALSE
      }
      
      return(list(log = log_output, success = success, folder = job_details$model_dir, method = job_details$weighting_method))
    }
    
    # Process results after the parallel loop
    append_to_log(paste("--- Parallel task '", task_name, "' finished. Consolidating results. ---"))
    
    all_successful <- TRUE
    for (res in results) {
      method_info <- if (!is.na(res$method)) paste0("(", res$method, ")") else ""
      append_to_log(paste("\n--- Log for:", res$folder, method_info, "---"))
      if (inherits(res, "error")) {
        append_to_log(paste("TUNING ERROR for", res$folder, ":", as.character(res)))
        all_successful <- FALSE
      } else {
        append_to_log(res$log)
        if(isTRUE(res$success)) {
          append_to_log(paste("SUCCESS: Task completed for", res$folder, method_info))
        } else {
          append_to_log(paste("FAILURE: Task did not complete successfully for", res$folder, method_info))
          all_successful <- FALSE
        }
      }
      append_to_log("--- End Log ---")
    }
    
    if(all_successful){
      showNotification(paste("Task '", task_name, "' completed successfully for all jobs."), type = "message", duration = 8)
    } else {
      showNotification(paste("One or more jobs failed during task '", task_name, "'. Check console for details."), type = "warning", duration = 10)
    }
  }
  
  # Observer for the NEW "Run Single Bias Ramp Adj." button
  # In your server function, replace the existing observers for the two tuning buttons.
  
  # Observer for the "Run Single Bias Ramp Adj." button
  observeEvent(input$run_single_bias_adj, {
    folders <- rv$tuning_folders
    if (is.null(folders) || length(folders) == 0) {
      showNotification("Please select at least one model folder.", type = "error")
      return()
    }
    
    # Create a data frame of jobs for this task
    jobs_df <- data.frame(
      model_dir = folders,
      run_step = "bias_ramp_only",
      weighting_method = NA_character_, # Not used for this step, but column is needed
      stringsAsFactors = FALSE
    )
    
    launch_tuning_process(jobs_df, "Single Bias Adj")
  })
  
  # Observer for the "Run Full Tuning Sequence" button
  observeEvent(input$run_full_tuning_sequence, {
    selected_methods <- input$tuning_weighting_method
    folders <- rv$tuning_folders
    
    if (is.null(selected_methods) || length(selected_methods) == 0) {
      showNotification("Please select at least one weighting method.", type = "warning")
      return()
    }
    if (is.null(folders) || length(folders) == 0) {
      showNotification("Please select at least one model folder.", type = "error")
      return()
    }
    
    # Create a job for each combination of folder and selected method
    jobs_df <- expand.grid(model_dir = folders, weighting_method = selected_methods, stringsAsFactors = FALSE)
    jobs_df$run_step <- "full_sequence"
    
    launch_tuning_process(jobs_df, "Full Tuning Sequence")
  })
  
  # --- END: New Parallel Bias and Tuning Server Logic ---
  
  # END - FRANCIS AND BIAS TUNE LOGIC
  
  # Render the log text
  output$log_output <- renderText({
    log_text()
  })
  
  # Render the status
  output$status_output <- renderText({
    log_text()
  })
  
  # Observer to add a selected folder to the comparison list
  observeEvent(input$add_comparison_dir_btn, {
    req(is.list(input$add_comparison_dir_btn))
    path <- parseDirPath(volumes, input$add_comparison_dir_btn)
    
    if (length(path) > 0) {
      # Check for duplicates before adding
      if (!path %in% rv$comparison_folders) {
        rv$comparison_folders <- c(rv$comparison_folders, path)
        append_to_log(paste("Added to comparison list:", path))
      } else {
        showNotification("That folder is already in the list.", type = "warning")
      }
    }
  })
  
  # Observer for the 'Clear List' button
  observeEvent(input$clear_comparison_dirs_btn, {
    rv$comparison_folders <- character(0)
    append_to_log("Cleared the model comparison list.")
  })
  
  # Render the display for the list of selected folders
  output$comparison_dirs_display <- renderText({
    if (length(rv$comparison_folders) == 0) {
      "No folders selected."
    } else {
      paste(rv$comparison_folders, collapse = "\n")
    }
  })
  
  # app.R (inside the 'server' function)
  
  # In the server function
  
  # MODIFIED: Observer for the 'Run Comparison' button now calls the refactored function
  observeEvent(input$run_comparison_btn, {
    start_model_comparison(
      folder_paths = rv$comparison_folders,
      comparison_name_prefix = input$comparison_name
    )
  })

  
  # Observer to poll the background processes and log their output
  
  observeEvent(output_poller(), {
    # --- Check sensitivity process ---
    if (!is.null(rv$sensitivity_process)) {
      
      # Read standard output
      new_output <- rv$sensitivity_process$read_output_lines()
      if (length(new_output) > 0) { append_to_log(new_output) }
      
      # Read standard error (CRITICAL for crashes)
      new_error <- rv$sensitivity_process$read_error_lines()
      if (length(new_error) > 0) { append_to_log(paste("SENSITIVITY:", new_error)) }
      
      # Check if process has finished/died
      if (!rv$sensitivity_process$is_alive()) {
        # READ ONE LAST TIME to capture the final crash message
        final_output <- rv$sensitivity_process$read_output_lines()
        final_error <- rv$sensitivity_process$read_error_lines()
        
        if (length(final_output) > 0) append_to_log(final_output)
        if (length(final_error) > 0) append_to_log(paste("FINAL ERROR:", final_error))
        
        append_to_log("--- Sensitivity analysis process ended ---")
        rv$sensitivity_process <- NULL # Reset
      }
    }
    
    # --- Check comparison process ---
    if (!is.null(rv$comparison_process)) {
      # Same logic for comparison process
      new_output_comp <- rv$comparison_process$read_output_lines()
      if (length(new_output_comp) > 0) { append_to_log(new_output_comp) }
      
      new_error_comp <- rv$comparison_process$read_error_lines()
      if (length(new_error_comp) > 0) { append_to_log(paste("COMPARISON ERROR:", new_error_comp)) }
      
      if (!rv$comparison_process$is_alive()) { 
        final_out <- rv$comparison_process$read_output_lines()
        final_err <- rv$comparison_process$read_error_lines()
        if(length(final_out)>0) append_to_log(final_out)
        if(length(final_err)>0) append_to_log(paste("FINAL COMP ERROR:", final_err))
        
        rv$comparison_process <- NULL 
      }
    }
  })
  
  # Custom handler to capture all output
  observe({
    withCallingHandlers({
      # Combine checkbox groups
      data_include <- reactive({
        c(input$data_include_before, input$data_include_after)
      })
      
      # Reactive value for Global.Lengthclass
      Global.Lengthclass <- reactive({
        input$length_class_input
      })
      
      # Reactive Fixedsiteonly data
      fixedsiteonly_reactive <- reactive({
        req(input$species_select)
        # Evaluate the input value outside the dplyr pipe
        current_interval <- input$length_class_input
        
        

        
        data <- Fixedsiteonly %>%
        mutate(LengthClass = round_any(Fork.Length, accuracy = Global.Lengthclass(), f = floor))

        if (!"Location" %in% colnames(data)) {
          data$Location <- "Unknown"
        }
        if (!"Zone" %in% colnames(data)) {
          data$Zone <- "Unknown"
        }
        if (!"BioRegion" %in% colnames(data)) {
          data$BioRegion <- "Unknown"
        }
        if (!"Sector" %in% colnames(data)) {
          data$Sector <- "Unknown"
        }
        if (!"Sex" %in% colnames(data)) { 
          data$Sex <- "U"                
        }                                
        data
      })
      
      # Modify merged_kim_pilb_reactive
      merged_kim_pilb_reactive <- reactive({
        req(input$species_select) # <--- ADD THIS LINE
        current_interval <- input$length_class_input
        length_col <- ifelse(input$length_metric == "FL", "FL_mm", "TL_mm")
        data <- Merged_Kim_Pilb
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        data <- data %>%
          mutate(LengthClass = round_any(.data[[length_col]], accuracy = Global.Lengthclass(), f = floor)) %>%
          mutate(fleet = Sector)
        
        # data <- data %>%
        #   mutate(LengthClass = floor(.data[[length_col]] / current_interval) * current_interval) %>%
        #   mutate(fleet = Sector)
        # Add default columns
        if (!"Location" %in% colnames(data)) data$Location <- "Unknown"
        if (!"Zone" %in% colnames(data)) data$Zone <- "Unknown"
        if (!"BioRegion" %in% colnames(data)) data$BioRegion <- "Unknown"
        if (!"Sex" %in% colnames(data)) data$Sex <- "U"
        # message("merged_kim_pilb_reactive: nrow = ", nrow(data), ", columns = ", paste(colnames(data), collapse = ", "))
        data
      })
      

      # Reactive to get CSIRO code(s) for the selected species
      selected_csiro <- reactive({
        req(input$species_select)
        if (input$species_select == "All") {
          unique(c(Fixedsiteonly$CSIRO, Merged_Kim_Pilb$CSIRO, WCD_data$CSIRO))
        } else {
          unique(c(
            Fixedsiteonly$CSIRO[Fixedsiteonly$SpeciesName == input$species_select],
            Merged_Kim_Pilb$CSIRO[Merged_Kim_Pilb$SpeciesName == input$species_select],
            WCD_data$CSIRO[WCD_data$SpeciesName == input$species_select]
          ))
        }
      })
      
      # Reactive FISages data
      fisages_reactive <- reactive({
        # Safety check: if FISages is NULL or not a dataframe, return an empty structure immediately
        if (is.null(FISages) || !is.data.frame(FISages)) {
          return(data.frame(SpeciesName = character(), IntAge = numeric(), year = integer(), Location = character()))
        }
        
        # CRITICAL FIX: Check if IntAge column exists BEFORE trying to filter by it.
        # This prevents the "object 'IntAge' not found" crash if the data is empty/malformed.
        if (!"IntAge" %in% colnames(FISages)) {
          return(data.frame(SpeciesName = character(), IntAge = numeric(), year = integer(), Location = character()))
        }
        
        # Now it is safe to filter
        data <- FISages %>%
          filter(!is.na(IntAge))
        
        if (!is.numeric(data$IntAge)) {
          message("fisages_reactive: IntAge column exists but is non-numeric")
          return(data.frame())
        }
        
        # --- Updated column check using safe methods ---
        if (!"Location" %in% colnames(data)) {
          data <- tibble::add_column(data, Location = "Unknown")
        }
        if (!"Zone" %in% colnames(data)) {
          data <- tibble::add_column(data, Zone = "Unknown")
        }
        if (!"BioRegion" %in% colnames(data)) {
          data <- tibble::add_column(data, BioRegion = "Unknown")
        }
        if (!"Sector" %in% colnames(data)) {
          data <- tibble::add_column(data, Sector = "Unknown")
        }
        if (!"Sex" %in% colnames(data)) {
          data <- tibble::add_column(data, Sex = "U")                
        }
        # --- END: Updated column check using safe methods ---
        
        data
      })
      
      
      # Base reactive data for Age
      base_data_age <- reactive({
        if (!input$use_fis_age || !"Age" %in% data_include()) {
          message("base_data_age: Skipped due to use_fis_age = ", input$use_fis_age, 
                  ", Age in data_include = ", "Age" %in% data_include())
          return(data.frame())
        }
        data <- fisages_reactive()
        if ("IntAge" %in% colnames(data)) {
          data <- data %>%
            mutate(IntAge = as.character(IntAge)) %>% # Convert to character to handle factors
            mutate(IntAge = suppressWarnings(as.numeric(IntAge))) %>% # Convert to numeric
            filter(!is.na(IntAge), IntAge >= 0) # Keep valid, non-negative ages
        } else {
          message("base_data_age: IntAge column missing")
          return(data.frame())
        }
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        selected_types <- data_include()
        cols_to_keep <- c("SpeciesName", "year", "Location", "IntAge", "Sex", "FL", "TL")
        cols_available <- intersect(cols_to_keep, colnames(data))
        if (!"Location" %in% colnames(data)) {
          message("base_data_age: Location column missing, adding default")
          data$Location <- "Unknown"
          cols_available <- c(cols_available, "Location")
        }
        data <- data %>% 
          mutate(fleet = "FIS") %>%
          select(any_of(cols_available), fleet)
        data
      })
      
      base_data_bio_age <- reactive({
        data <- merged_kim_pilb_reactive()
        if (!input$use_bio_age || !"Age" %in% input$data_include_before) {
          message("base_data_bio_age: Returning empty due to use_bio_age FALSE or Age not included")
          return(data.frame())
        }
        length_col <- ifelse(input$length_metric == "FL", "FL_mm", "TL_mm")
        
        current_interval <- input$length_class_input

        data <- data %>%
          filter(Fin_Yr_Age != -9, !is.na(Fin_Yr_Age),
                 !is.na(.data[[length_col]]), .data[[length_col]] >= input$min_length_input) %>%
          mutate(IntAge = as.integer(Fin_Yr_Age),
                 LengthClass = round_any(.data[[length_col]], accuracy = Global.Lengthclass(), f = floor))
        
        # data <- data %>%
        #   filter(Fin_Yr_Age != -9, !is.na(Fin_Yr_Age),
        #          !is.na(.data[[length_col]]), .data[[length_col]] >= input$min_length_input) %>%
        #   mutate(IntAge = as.integer(Fin_Yr_Age),
        #          LengthClass = floor(.data[[length_col]] / current_interval) * current_interval)
        
        if (!"Location" %in% colnames(data)) {
          data$Location <- "Unknown"
        }
        if (!"Zone" %in% colnames(data)) {
          data$Zone <- "Unknown"
        }
        if (!"BioRegion" %in% colnames(data)) {
          data$BioRegion <- "Unknown"
        }
        data
      })
      
      # Base reactive data for Catch
      base_catch_data <- reactive({
        req(input$catch_sector_select)
        data <- catch_ts
        csiro_codes <- selected_csiro()
        if ("CSIRO" %in% names(data)) {
          # data <- data %>% filter(CSIRO %in% csiro_codes)
        }
        if (!"All" %in% input$catch_species_select) {
          data <- data %>% filter(Specstock %in% input$catch_species_select)
        } else if (input$species_select != "All") {
          data <- data %>% filter(Specstock == input$species_select)
        }
        selected_sectors <- input$catch_sector_select[!input$catch_sector_select %in% "CSIRO"]
        if (length(selected_sectors) == 0) {
          data %>% select(Specstock, year)
        } else {
          data %>% select(Specstock, year, all_of(selected_sectors))
        }
      })
      
      # Base reactive data for Effort
      base_effort_data <- reactive({
        if (!"Effort" %in% input$data_include_before || is.null(input$effort_species_select) || length(input$effort_species_select) == 0) {
          return(NULL)
        }
        req(input$effort_fleet_select)
        data <- effort_ts
        if (!"All" %in% input$effort_species_select) {
          data <- data %>% filter(Specstock %in% input$effort_species_select)
        } else if (input$species_select != "All") {
          data <- data %>% filter(Specstock == input$species_select)
        }
        if (nrow(data) > 0) {
          data %>% 
            select(Specstock, year, fleet, obs, se_log) %>%
            filter(fleet %in% input$effort_fleet_select)
        } else {
          return(NULL)
        }
      })
      
      
      has_age_data <- reactive({
        fis_data <- fisages_reactive()
        bio_data <- merged_kim_pilb_reactive()
        
        # Check FIS data availability first
        fis_age_available <- FALSE
        if (input$use_fis_age && nrow(fis_data) > 0) {
          fis_age_available <- nrow(fis_data %>% 
                                      filter(SpeciesName == input$species_select | input$species_select == "All")) > 0
        }
        
        # Check Biological data availability
        bio_age_available <- FALSE
        if (input$use_bio_age && nrow(bio_data) > 0) {
          bio_age_available <- nrow(bio_data %>% 
                                      filter((SpeciesName == input$species_select | input$species_select == "All") & 
                                               Fin_Yr_Age != -9 & !is.na(Fin_Yr_Age))) > 0
        }
        
        fis_age_available || bio_age_available
      })
      
      # Enable/disable Age checkbox
      observe({
        if (has_age_data()) {
          shinyjs::enable(selector = "#data_include_before input[value='Age']")
          if (!"Age" %in% input$data_include_before) {
            updateCheckboxGroupInput(session, "data_include_before", 
                                     selected = c(input$data_include_before, "Age"))
          }
        } else {
          shinyjs::disable(selector = "#data_include_before input[value='Age']")
          updateCheckboxGroupInput(session, "data_include_before", 
                                   selected = setdiff(input$data_include_before, "Age"))
        }
      })
      
      # Render age availability message
      output$age_availability_message <- renderUI({
        if (!has_age_data()) {
          div("No age data available for the selected species.", 
              style = "color: red; font-style: italic;")
        } else {
          NULL
        }
      })
      
      # Reactive values for dropdowns
      bio_species_choices <- reactiveVal(NULL)
      catch_species_choices <- reactiveVal(NULL)
      effort_species_choices <- reactiveVal(NULL)
      
      # Populate bio species dropdown
      observe({
        local({
          create_biol_table <- FALSE
          source("BiolTable.R", local = TRUE)
          if (!exists("bupBiologicalUnitParam") || is.null(bupBiologicalUnitParam$BiologicalUnitName)) {
            stop("BiolTable.R did not create 'bupBiologicalUnitParam' with 'BiologicalUnitName'")
          }
          
          # Default to the full loaded dataset
          bio_data <- bupBiologicalUnitParam
          
          # --- MODIFIED: Only filter by CSIRO if the "Show All" checkbox is NOT checked ---
          if (!isTRUE(input$show_all_bio_species)) {
            csiro_codes <- selected_csiro()
            if ("CSIRO" %in% names(bio_data) && any(bio_data$CSIRO %in% csiro_codes)) {
              bio_data <- bio_data %>% filter(CSIRO %in% csiro_codes)
            }
          }
          # --------------------------------------------------------------------------------
          
          unique_bio_species <- c(setNames(sort(unique(bio_data$BiologicalUnitName)),
                                           sort(unique(bio_data$BiologicalUnitName))))
          bio_species_choices(unique_bio_species)
        })
      })
      
      # Populate catch species dropdown
      observe({
        csiro_codes <- selected_csiro()
        catch_data <- catch_ts
        if ("CSIRO" %in% names(catch_data) && any(catch_data$CSIRO %in% csiro_codes)) {
          catch_data <- catch_data %>% filter(CSIRO %in% csiro_codes)
        } else {
        }
        unique_catch_species <- c(setNames(sort(unique(catch_data$Specstock)),
                                           sort(unique(catch_data$Specstock))))
        catch_species_choices(unique_catch_species)
      })
      
      # Populate effort species dropdown
      observe({
        csiro_codes <- selected_csiro()
        effort_data <- effort_ts
        if ("CSIRO" %in% names(effort_data) && any(effort_data$CSIRO %in% csiro_codes)) {
          effort_data <- effort_data %>% filter(CSIRO %in% csiro_codes)
        }
        unique_effort_species <- c(setNames(sort(unique(effort_data$Specstock)),
                                            sort(unique(effort_data$Specstock))))
        effort_species_choices(unique_effort_species)
      })
      
      # Render dynamic UI for Catch tab
      output$catch_species_select_ui <- renderUI({
        req(catch_species_choices())
        pickerInput("catch_species_select", "Select Catch Species",
                    choices = catch_species_choices(),
                    selected = "All",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      })
      
      # Render dynamic UI for Effort tab
      output$effort_species_select_ui <- renderUI({
        req(effort_species_choices())
        pickerInput("effort_species_select", "Select Indices Species",
                    choices = effort_species_choices(),
                    selected = "All",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      })
      
      # Render dynamic UI for Biological Parameters tab
      output$bio_species_select_ui <- renderUI({
        req(bio_species_choices())
        pickerInput("bio_species_select", "Select Biological Unit",
                    choices = bio_species_choices(),
                    selected = NULL,
                    multiple = FALSE,
                    options = list(`live-search` = TRUE))
      })
      
      # Debug text for Catch tab
      output$catch_subset_text <- renderText({
        csiro_codes <- selected_csiro()
        paste("Tab subset by CSIRO:", paste(csiro_codes, collapse = ", "))
      })
      
      # Debug text for Effort tab
      output$effort_subset_text <- renderText({
        csiro_codes <- selected_csiro()
        paste("Tab subset by CSIRO:", paste(csiro_codes, collapse = ", "))
      })
      
      # Debug text for Biological Parameters tab
      output$bio_subset_text <- renderText({
        csiro_codes <- selected_csiro()
        paste("Tab subset by CSIRO:", paste(csiro_codes, collapse = ", "))
      })
      
      # Dynamic UI for Catch sectors
      output$catch_sector_select_ui <- renderUI({
        req(input$species_select, input$catch_species_select)
        data <- catch_ts
        if (!"All" %in% input$catch_species_select) {
          data <- data %>% filter(Specstock %in% input$catch_species_select)
        } else if (input$species_select != "All") {
          data <- data %>% filter(Specstock == input$species_select)
        }
        sector_sums <- colSums(data[, !names(data) %in% c("Specstock", "year", "CSIRO"), drop = FALSE], na.rm = TRUE)
        sector_choices <- names(sector_sums)[sector_sums > 0]
        if (length(sector_choices) == 0) {
          return(div("No sectors with catch > 0 for selected species (excluding CSIRO).", style = "color: red;"))
        }
        checkboxGroupInput("catch_sector_select", "Select Sectors for Catch",
                           choices = sector_choices,
                           selected = sector_choices,
                           inline = TRUE)
      })
      
      # Dynamic UI for Effort fleets
      output$effort_fleet_select_ui <- renderUI({
        req(input$species_select, input$effort_species_select)
        data <- effort_ts
        if (!"All" %in% input$effort_species_select) {
          data <- data %>% filter(Specstock %in% input$effort_species_select)
        } else if (input$species_select != "All") {
          data <- data %>% filter(Specstock == input$species_select)
        }
        fleet_choices <- sort(unique(data$fleet))
        if (length(fleet_choices) == 0) {
          return(div("No fleets with data for selected species.", style = "color: red;"))
        }
        checkboxGroupInput("effort_fleet_select", "Select Fleets for Indices",
                           choices = fleet_choices,
                           selected = fleet_choices,
                           inline = TRUE)
      })
      
      # Dynamic UI for Length years (Retained Catch)
      output$year_select_retained <- renderUI({
        selected_locations <- input$location_select_retained
        
        # Base data for the species
        data <- fixedsiteonly_reactive()
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        data <- data %>% filter(Discarded. == "No")
        
        # Filter data for sample size calculation based on selected locations
        filtered_data <- data
        if (!is.null(selected_locations)) {
          filtered_data <- filtered_data %>% filter(Location %in% selected_locations)
        }
        
        # Compute sample sizes per year from the dynamically filtered data
        sample_sizes <- filtered_data %>%
          group_by(year) %>%
          summarise(n = n(), .groups = "drop")
        
        # Get all possible year choices from the base data
        year_choices <- sort(unique(data$year))
        
        # Create labels with the correct sample sizes
        choices <- setNames(as.character(year_choices),
                            sapply(year_choices, function(y) {
                              n_val <- sample_sizes$n[sample_sizes$year == y]
                              if (length(n_val) == 0) n_val <- 0
                              paste0(y, " (n = ", n_val, ")")
                            }))
        pickerInput("year_select_retained", "Select Years for Retained Catch (Discarded = No)",
                    choices = choices,
                    selected = rv$retained_years,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      })
      
      # Dynamic UI for Length years (Released Catch)
      output$year_select_released <- renderUI({
        selected_locations <- input$location_select_released
        
        # Base data for the species
        data <- fixedsiteonly_reactive()
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        data <- data %>% filter(Discarded. == "Yes")
        
        # Filter data for sample size calculation based on selected locations
        filtered_data <- data
        if (!is.null(selected_locations)) {
          filtered_data <- filtered_data %>% filter(Location %in% selected_locations)
        }
        
        # Compute sample sizes per year from the dynamically filtered data
        sample_sizes <- filtered_data %>%
          group_by(year) %>%
          summarise(n = n(), .groups = "drop")
        
        # Get all possible year choices from the base data
        year_choices <- sort(unique(data$year))
        
        # Create labels with the correct sample sizes
        choices <- setNames(as.character(year_choices),
                            sapply(year_choices, function(y) {
                              n_val <- sample_sizes$n[sample_sizes$year == y]
                              if (length(n_val) == 0) n_val <- 0
                              paste0(y, " (n = ", n_val, ")")
                            }))
        pickerInput("year_select_released", "Select Years for Released Catch (Discarded = Yes)",
                    choices = choices,
                    selected = rv$released_years,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      })
      
      # Dynamic UI for Age years (FIS)
      output$year_select_age_ui <- renderUI({
        selected_locations <- input$location_select_age
        
        # Base data for the species
        data <- fisages_reactive()
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        
        # Filter data for sample size calculation based on selected locations
        filtered_data <- data
        if (!is.null(selected_locations)) {
          filtered_data <- filtered_data %>% filter(Location %in% selected_locations)
        }
        
        # Compute sample sizes per year from the dynamically filtered data
        sample_sizes <- filtered_data %>%
          group_by(year) %>%
          summarise(n = n(), .groups = "drop")
        
        # Get all possible year choices from the base data
        year_choices <- if (nrow(data) > 0) sort(unique(data$year)) else character(0)
        
        # Create labels with the correct sample sizes
        choices <- setNames(as.character(year_choices), 
                            sapply(year_choices, function(y) {
                              n_val <- sample_sizes$n[sample_sizes$year == y]
                              if (length(n_val) == 0) n_val <- 0
                              paste0(y, " (n = ", n_val, ")")
                            }))
        
        # Update location choices based on base data
        location_choices <- sort(unique(data$Location[!is.na(data$Location)]))
        if (length(location_choices) == 0) location_choices <- "Unknown"
        updatePickerInput(session, "location_select_age",
                          choices = location_choices,
                          selected = location_choices)
        
        pickerInput("year_select_age", "Select Years for FIS Age Data",
                    choices = choices,
                    selected = intersect(year_choices, rv$retained_years),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE))
      })
      
      # --- START: New code for Biological and Historical Age Data ---
      
      # This defines the UI placeholders. It's simple and doesn't create a reactive loop.
      output$year_select_bio_age_ui <- renderUI({
        tagList(
          pickerInput("year_select_bio_age", "Select Years for Biological Age Data",
                      choices = NULL, selected = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("bioregion_select_bio_age", "Select BioRegions for Biological Age Data",
                      choices = NULL, selected = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("zone_select_bio_age", "Select Zones for Biological Age Data",
                      choices = NULL, selected = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("location_select_bio_age", "Select Locations for Biological Age Data",
                      choices = NULL, selected = NULL, multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE))
        )
      })
      
      # Corrected observer for Biological Age filters, now triggered ONLY by species change or the refresh button.
      observeEvent(list(input$species_select, input$age_refresh_btn), {
        req(input$species_select)
        
        base_data <- merged_kim_pilb_reactive() %>%
          filter(Fin_Yr_Age != -9, !is.na(Fin_Yr_Age))
        if (input$species_select != "All") {
          base_data <- base_data %>% filter(SpeciesName == input$species_select)
        }
        
        if (nrow(base_data) == 0) {
          updatePickerInput(session, "year_select_bio_age", choices = character(0), selected = character(0))
          updatePickerInput(session, "bioregion_select_bio_age", choices = character(0), selected = character(0))
          updatePickerInput(session, "zone_select_bio_age", choices = character(0), selected = character(0))
          updatePickerInput(session, "location_select_bio_age", choices = character(0), selected = character(0))
          return()
        }
        
        # Get selections from reactive values (rv) to support state restoration
        sel_bioregions <- rv$bio_age_bioregions
        sel_zones      <- rv$bio_age_zones
        sel_locations  <- rv$bio_age_locations
        sel_years      <- rv$bio_age_years
        
        # --- Determine new CHOICES based on current selections ---
        bioregion_choices <- sort(unique(base_data$BioRegion[!is.na(base_data$BioRegion)])) %||% "Unknown"
        sel_bioregions <- intersect(sel_bioregions, bioregion_choices)
        
        data_for_choices <- base_data %>% filter(if (length(sel_bioregions) > 0) BioRegion %in% sel_bioregions else TRUE)
        zone_choices <- sort(unique(data_for_choices$Zone[!is.na(data_for_choices$Zone)])) %||% "Unknown"
        sel_zones <- intersect(sel_zones, zone_choices)
        
        data_for_choices <- data_for_choices %>% filter(if (length(sel_zones) > 0) Zone %in% sel_zones else TRUE)
        location_choices <- sort(unique(data_for_choices$Location[!is.na(data_for_choices$Location)])) %||% "Unknown"
        sel_locations <- intersect(sel_locations, location_choices)
        
        # --- Update pickers with new choices and validated selections ---
        updatePickerInput(session, "bioregion_select_bio_age", choices = bioregion_choices, selected = sel_bioregions)
        updatePickerInput(session, "zone_select_bio_age", choices = zone_choices, selected = sel_zones)
        updatePickerInput(session, "location_select_bio_age", choices = location_choices, selected = sel_locations)
        
        # --- Update Year counts ---
        year_choices_all <- sort(unique(as.character(base_data$year)))
        data_for_year_counts <- data_for_choices %>% filter(if (length(sel_locations) > 0) Location %in% sel_locations else TRUE)
        
        sample_sizes <- data_for_year_counts %>% group_by(year) %>% summarise(n = n(), .groups = "drop") %>% mutate(year = as.character(year))
        
        year_labels <- sapply(year_choices_all, function(y) {
          n_val <- sample_sizes$n[sample_sizes$year == y]
          if (length(n_val) == 0 || is.na(n_val)) n_val <- 0
          paste0(y, " (n = ", n_val, ")")
        })
        
        sel_years <- intersect(sel_years, year_choices_all)
        updatePickerInput(session, "year_select_bio_age", choices = setNames(year_choices_all, year_labels), selected = sel_years)
        
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      
      # Corrected observer for Biological Length filters, now triggered ONLY by species change or the refresh button.
      observeEvent(list(input$species_select, input$refresh_btn), {
        req(input$species_select)
        
        base_data <- merged_kim_pilb_reactive()
        if (input$species_select != "All" && nrow(base_data) > 0) {
          base_data <- base_data %>% filter(SpeciesName == input$species_select)
        }
        
        # Get selections from reactive values (rv) to support state restoration
        sel_sectors    <- rv$kim_pilb_sectors
        sel_years      <- rv$kim_pilb_years
        sel_bioregions <- rv$kim_pilb_bioregions
        sel_zones      <- rv$kim_pilb_zones
        sel_locations  <- rv$kim_pilb_locations
        
        # --- Determine the NEW choices based on the CURRENT selections ---
        # Sector choices are based only on the species
        sector_choices <- sort(unique(base_data$Sector[!is.na(base_data$Sector)])) %||% "Unknown"
        sel_sectors <- intersect(sel_sectors, sector_choices) 
        
        data_for_choices <- base_data %>%
          filter(if (length(sel_sectors) > 0) Sector %in% sel_sectors else TRUE)
        
        bioregion_choices <- sort(unique(data_for_choices$BioRegion[!is.na(data_for_choices$BioRegion)])) %||% "Unknown"
        sel_bioregions <- intersect(sel_bioregions, bioregion_choices)
        
        data_for_choices <- data_for_choices %>%
          filter(if (length(sel_bioregions) > 0) BioRegion %in% sel_bioregions else TRUE)
        
        zone_choices <- sort(unique(data_for_choices$Zone[!is.na(data_for_choices$Zone)])) %||% "Unknown"
        sel_zones <- intersect(sel_zones, zone_choices)
        
        data_for_choices <- data_for_choices %>%
          filter(if (length(sel_zones) > 0) Zone %in% sel_zones else TRUE)
        
        location_choices <- sort(unique(data_for_choices$Location[!is.na(data_for_choices$Location)])) %||% "Unknown"
        sel_locations <- intersect(sel_locations, location_choices)
        
        # --- Update all pickers with the new choices and validated selections ---
        updatePickerInput(session, "sector_select_bio", choices = sector_choices, selected = sel_sectors)
        updatePickerInput(session, "bioregion_select_bio", choices = bioregion_choices, selected = sel_bioregions)
        updatePickerInput(session, "zone_select_bio", choices = zone_choices, selected = sel_zones)
        updatePickerInput(session, "location_select_bio", choices = location_choices, selected = sel_locations)
        
        # --- Update Year (with dynamic counts based on all other filters) ---
        year_choices_all <- sort(unique(as.character(base_data$year)))
        data_for_year_counts <- data_for_choices %>%
          filter(if (length(sel_locations) > 0) Location %in% sel_locations else TRUE)
        
        sample_sizes <- data_for_year_counts %>% group_by(year) %>% summarise(n = n(), .groups = "drop") %>% mutate(year = as.character(year))
        
        year_labels <- sapply(year_choices_all, function(y) {
          n_val <- sample_sizes$n[sample_sizes$year == y]; if (length(n_val) == 0 || is.na(n_val)) n_val <- 0; paste0(y, " (n=", n_val, ")")
        })
        sel_years <- intersect(sel_years, year_choices_all)
        updatePickerInput(session, "year_select2", choices = setNames(year_choices_all, year_labels), selected = sel_years)
        
      }, ignoreNULL = TRUE, ignoreInit = TRUE)
      
      output$bio_age_table <- renderTable({
        req(base_data_bio_age())
        if (nrow(base_data_bio_age()) == 0 || !input$use_bio_age) {
          return(NULL)
        }
        age_summary <- base_data_bio_age() %>%
          group_by(year) %>%
          dplyr::summarize(n = paste("n =", n()))
        age_summary
      })
      
      output$bio_age_histogram <- renderPlot({
        req(bio_age_plot_data())
        data <- bio_age_plot_data()
        if (nrow(data) == 0 || !input$use_bio_age) {
          return(ggplot() + annotate("text", x = 0, y = 0, label = "No biological age data available") + theme_void())
        }
        render_age_histogram(data, age_col = "IntAge")
      })
      
      output$bio_age_plot_ui <- renderUI({
        height <- bio_age_plot_height()
        plotOutput("bio_age_histogram", width = "800px", height = height)
      })
      
      output$length_debug <- renderText({
        paste(
          "use_fis_length:", input$use_fis_length,
          "\nuse_bio_length:", input$use_bio_length,
          "\nLength in data_include_before:", "Length" %in% input$data_include_before,
          "\nSelected Species:", input$species_select,
          "\nRetained Years Selected:", paste(input$year_select_retained %||% "None", collapse = ", "),
          "\nReleased Years Selected:", paste(input$year_select_released %||% "None", collapse = ", "),
          "\nBio Years Selected:", paste(input$year_select2 %||% "None", collapse = ", "),
          "\nBioRegions Selected:", paste(input$bioregion_select_bio %||% "None", collapse = ", "),
          "\nZones Selected:", paste(input$zone_select_bio %||% "None", collapse = ", "),
          "\nLocations Selected:", paste(input$location_select_bio %||% "None", collapse = ", "),
          "\nRetained Data Rows:", nrow(length_plot_data1_retained()),
          "\nReleased Data Rows:", nrow(length_plot_data1_released()),
          "\nBio Data Rows:", nrow(length_plot_data2())
        )
      })
      
      output$age_debug <- renderText({
        paste(
          "use_fis_age:", is.logical(input$use_fis_age), " (value = ", input$use_fis_age, ")",
          "\nuse_bio_age:", is.logical(input$use_bio_age), " (value = ", input$use_bio_age, ")",
          "\nAge in data_include_before:", "Age" %in% input$data_include_before,
          "\nSelected Species:", input$species_select,
          "\nFIS Age Rows (base_data_age):", nrow(base_data_age()),
          "\nBio Age Rows (base_data_bio_age):", nrow(base_data_bio_age()),
          "\nFIS Age Years Selected:", if (is.null(input$year_select_age)) "NULL" else paste(input$year_select_age, collapse = ", "),
          "\nBio Age Years Selected:", if (is.null(input$year_select_bio_age)) "NULL" else paste(input$year_select_bio_age, collapse = ", "),
          "\nrv$retained_years:", paste(rv$retained_years, collapse = ", "),
          "\nrv$bio_age_years:", paste(rv$bio_age_years, collapse = ", ")
        )
      })
      
      # Biological Parameters table reactive
      # Biological Parameters table reactive
      bio_tab <- eventReactive(list(input$bio_refresh_btn, refresh_trigger()), {
        
        # --- MODIFIED: Reverted to standard selection logic ---
        # Ensure a specific species is selected from the dropdown
        req(input$bio_species_select)
        
        local({
          selected_bio_species <- input$bio_species_select
          # ----------------------------------------------------
          
          create_biol_table <- TRUE
          source("BiolTable.R", local = TRUE)
          if (!exists("result") || !is.list(result) || is.null(result$tabdat) || is.null(result$parameters_s)) {
            message("BiolTable.R did not return a valid 'result' list")
            tabdat <- data.frame(Message = "Error: BiolTable.R failed to provide valid data")
            parameters_s <- data.frame(Message = "No parameters available")
            caption_text <- "Error loading biological parameters"
          } else {
            tabdat <- result$tabdat
            parameters_s <- result$parameters_s
            caption_text <- result$caption_text
            
            if (!is.data.frame(tabdat)) {
              tabdat <- as.data.frame(tabdat, stringsAsFactors = FALSE)
            }
            if (nrow(tabdat) == 0 || ncol(tabdat) == 0) {
              tabdat <- data.frame(Message = "No biological data available for selected biological unit")
            }
            if (!is.data.frame(parameters_s)) {
              parameters_s <- as.data.frame(parameters_s, stringsAsFactors = FALSE)
            }
            if (nrow(parameters_s) == 0 || ncol(parameters_s) == 0) {
              parameters_s <- data.frame(Message = "No biological parameters available")
            }
          }
          return(result)
        })
      }, ignoreNULL = FALSE)
      
      
      # Reactive for the selected biological parameters from tabdat
      selected_bio_parameters <- reactive({
        req(input$bio_species_select) # Ensures a species is selected
        bio_data_list <- bio_tab() 
        
        params_df <- bio_data_list$parameters_s 
        
        if (!is.null(params_df) && nrow(params_df) == 1) {
          return(params_df)
        } else {
          return(NULL) 
        }
      })
      
      # In server function of app.R
      output$bioSizeAtAgePlot <- renderPlot({
        params <- selected_bio_parameters() # Your reactive holding the selected row of tabdat
        req(params) 
        render_biology_size_at_age(bio_params = params) 
      })
      
      output$bioWeightPlot <- renderPlot({
        params <- selected_bio_parameters()
        req(params)
        render_biology_weight_at_size(bio_params = params)
      })
      
      output$bioMaturityPlot <- renderPlot({
        params <- selected_bio_parameters()
        req(params)
        render_biology_maturity(bio_params = params)
      })
      
      # Warning reactive for missing biological parameters
      bio_warning <- reactive({
        result <- bio_tab()
        req(result$parameters_s)
        key_params <- c("GrowthParamsFemale_1", "GrowthParamsMale_1", "NaturalMortality", 
                        "LengthWeightParamsFemale_1", "MaturityParamsFemale_1", "MaturityParamsFemale_2")
        param_data <- result$parameters_s
        missing_params <- sapply(key_params, function(param) {
          !(param %in% colnames(param_data)) || all(is.na(param_data[[param]]))
        })
        has_sexchange_1 <- "SexChangeParamsFemale_1" %in% colnames(param_data) && 
          !all(is.na(param_data[["SexChangeParamsFemale_1"]]))
        has_sexchange_2 <- "SexChangeParamsFemale_2" %in% colnames(param_data) && 
          !all(is.na(param_data[["SexChangeParamsFemale_2"]]))
        warning_msg <- character(0)
        if (any(missing_params)) {
          missing_names <- key_params[missing_params]
          warning_msg <- paste("Warning: Missing key biological parameters:", 
                               paste(missing_names, collapse = ", "),
                               "\n\n. SS_outputs and Download button won't work.")
        }
        if (xor(has_sexchange_1, has_sexchange_2)) {
          sexchange_warning <- "Warning: Incomplete Sex change parameters provided. \n\n. SS_outputs and Download button won't work."
          warning_msg <- if (length(warning_msg) > 0) {
            paste(warning_msg, sexchange_warning, sep = "\n\n")
          } else {
            sexchange_warning
          }
        }
        if (length(warning_msg) > 0) {
          return(warning_msg)
        } else {
          return(NULL)
        }
      })
      
      # Render warning UI
      output$bio_warning_ui <- renderUI({
        warning_text <- bio_warning()
        if (!is.null(warning_text)) {
          div(warning_text, style = "color: red; font-weight: bold;")
        } else {
          NULL
        }
      })
      
      # Base reactive data for Plot 1 (Fixedsiteonly)
      base_data1 <- reactive({
        if (!input$use_fis_length || !"Length" %in% data_include()) {
          return(data.frame())
        }
        data <- fixedsiteonly_reactive()
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        # Apply minimum length filter
        if (!is.null(input$min_length_input) && input$min_length_input > 0) {
          data <- data %>% filter(Fork.Length >= input$min_length_input)
        }
        if (!is.null(input$use_released_fis_data) && !isTRUE(input$use_released_fis_data)) {
          data <- data %>% filter(Discarded. != "Yes")
          message("base_data1: Filtered out Discarded == 'Yes' records.")
        }
        
        selected_types <- data_include()
        cols_to_keep <- c("SpeciesName", "year", "Location", "Discarded.", "Sex")
        if ("Length" %in% selected_types) cols_to_keep <- c(cols_to_keep, "Fork.Length", "LengthClass")
        if ("Catch" %in% selected_types && "Catch" %in% colnames(data)) cols_to_keep <- c(cols_to_keep, "Catch")
        if ("Effort" %in% selected_types && "Effort" %in% colnames(data)) cols_to_keep <- c(cols_to_keep, "Effort")
        if ("Age" %in% selected_types && "Age" %in% colnames(data)) cols_to_keep <- c(cols_to_keep, "Age")
        data %>% select(any_of(cols_to_keep))
      })
      
      # Base reactive data for Plot 2 (Merged_Kim_Pilb Length)
      base_data2 <- reactive({
        data <- merged_kim_pilb_reactive()
        if (!input$use_bio_length || !"Length" %in% input$data_include_before) {
          return(data.frame())
        }
        length_col <- ifelse(input$length_metric == "FL", "FL_mm", "TL_mm")
        current_interval <- input$length_class_input
        
        data <- data %>%
          filter(!is.na(.data[[length_col]]), .data[[length_col]] >= input$min_length_input) %>%
          mutate(LengthClass = round_any(.data[[length_col]], accuracy = Global.Lengthclass(), f = floor))
        
        # data <- data %>%
        #   filter(!is.na(.data[[length_col]]), .data[[length_col]] >= input$min_length_input) %>%
        #   mutate(LengthClass = floor(.data[[length_col]] / current_interval) * current_interval)
        # Ensure Zone and BioRegion are present
        if (!"Zone" %in% colnames(data)) {
          data$Zone <- "Unknown"
        }
        if (!"BioRegion" %in% colnames(data)) {
          data$BioRegion <- "Unknown"
        }
        data
      })
      
      
      # EventReactive data for Catch plot
      catch_plot_data <- eventReactive(list(input$catch_refresh_btn, refresh_trigger()), {
        data <- base_catch_data()
        data
      })
      
      # EventReactive data for Effort plot
      effort_plot_data <- eventReactive(list(input$effort_refresh_btn, refresh_trigger()), {
        base_effort_data()
      })
      
      observe({
        updateCheckboxInput(session, "use_fis_age", value = TRUE)
      })
      
      #Disable Inputs When No Data is Available
      observe({
        fis_data <- fixedsiteonly_reactive()
        year_choices <- sort(unique(fis_data$year)) %||% character(0)
        if (length(year_choices) == 0) {
          shinyjs::disable("year_select_retained")
          shinyjs::disable("year_select_released")
        } else {
          shinyjs::enable("year_select_retained")
          shinyjs::enable("year_select_released")
        }
      })
      
      
      # Initial UI update to ensure all years are selected on load
      # Initialize rv with all available data after data loads
      observe({
        fis_data <- fixedsiteonly_reactive()
        kim_pilb_data <- merged_kim_pilb_reactive()
        bio_age_data <- merged_kim_pilb_reactive() %>% filter(Fin_Yr_Age != -9, !is.na(Fin_Yr_Age))
        
        # FIS data
        rv$retained_years <- sort(unique(fis_data$year))
        rv$released_years <- sort(unique(fis_data$year))
        rv$retained_locations <- sort(unique(fis_data$Location[!is.na(fis_data$Location)])) %||% "Unknown"
        rv$released_locations <- sort(unique(fis_data$Location[!is.na(fis_data$Location)])) %||% "Unknown"
        
        # Biological data
        rv$kim_pilb_years <- sort(unique(kim_pilb_data$year))
        rv$kim_pilb_bioregions <- sort(unique(kim_pilb_data$BioRegion[!is.na(kim_pilb_data$BioRegion)])) %||% "Unknown"
        rv$kim_pilb_zones <- sort(unique(kim_pilb_data$Zone[!is.na(kim_pilb_data$Zone)])) %||% "Unknown"
        rv$kim_pilb_locations <- sort(unique(kim_pilb_data$Location[!is.na(kim_pilb_data$Location)])) %||% "Unknown"
        
        # Bio age data
        rv$bio_age_years <- sort(unique(bio_age_data$year))
        rv$bio_age_bioregions <- sort(unique(bio_age_data$BioRegion[!is.na(bio_age_data$BioRegion)])) %||% "Unknown"
        rv$bio_age_zones <- sort(unique(bio_age_data$Zone[!is.na(bio_age_data$Zone)])) %||% "Unknown"
        rv$bio_age_locations <- sort(unique(bio_age_data$Location[!is.na(bio_age_data$Location)])) %||% "Unknown"
      }, priority = 10)
      
      # Ensure minimum selections for years
      observe({
        if (is.null(input$year_select_retained) || length(input$year_select_retained) == 0) {
          available_years_retained <- sort(unique(fixedsiteonly_reactive() %>% 
                                                    filter(SpeciesName == input$species_select | input$species_select == "All", Discarded. == "No") %>% 
                                                    pull(year)))
          if (length(available_years_retained) > 0) {
            updatePickerInput(session, "year_select_retained", selected = available_years_retained)
            rv$retained_years <- available_years_retained
          }
        }
        
        if (is.null(input$year_select_released) || length(input$year_select_released) == 0) {
          available_years_released <- sort(unique(fixedsiteonly_reactive() %>% 
                                                    filter(SpeciesName == input$species_select | input$species_select == "All", Discarded. == "Yes") %>% 
                                                    pull(year)))
          if (length(available_years_released) > 0) {
            updatePickerInput(session, "year_select_released", selected = available_years_released)
            rv$released_years <- available_years_released
          }
        }
      })
      
      
      # Update selections when species changes
      observeEvent(input$species_select, {
        # FIS data
        fis_data <- fixedsiteonly_reactive()
        if (input$species_select != "All") {
          fis_data <- fis_data %>% filter(SpeciesName == input$species_select)
        }
        fis_year_choices <- sort(unique(fis_data$year)) %||% character(0)
        fis_location_choices <- sort(unique(fis_data$Location[!is.na(fis_data$Location)])) %||% "Unknown"
        rv$retained_years <- fis_year_choices
        rv$released_years <- fis_year_choices
        rv$retained_locations <- fis_location_choices
        rv$released_locations <- fis_location_choices
        updatePickerInput(session, "year_select_retained", choices = fis_year_choices, selected = fis_year_choices)
        updatePickerInput(session, "year_select_released", choices = fis_year_choices, selected = fis_year_choices)
        updatePickerInput(session, "location_select_retained", choices = fis_location_choices, selected = fis_location_choices)
        updatePickerInput(session, "location_select_released", choices = fis_location_choices, selected = fis_location_choices)
        
        # Biological data
        kim_pilb_data <- merged_kim_pilb_reactive()
        if (input$species_select != "All") {
          kim_pilb_data <- kim_pilb_data %>% filter(SpeciesName == input$species_select)
        }
        kim_pilb_sector_choices <- sort(unique(kim_pilb_data$Sector[!is.na(kim_pilb_data$Sector)])) %||% "Unknown"
        kim_pilb_year_choices <- sort(unique(kim_pilb_data$year)) %||% character(0)
        kim_pilb_bioregion_choices <- sort(unique(kim_pilb_data$BioRegion[!is.na(kim_pilb_data$BioRegion)])) %||% "Unknown"
        kim_pilb_zone_choices <- sort(unique(kim_pilb_data$Zone[!is.na(kim_pilb_data$Zone)])) %||% "Unknown"
        kim_pilb_location_choices <- sort(unique(kim_pilb_data$Location[!is.na(kim_pilb_data$Location)])) %||% "Unknown"
        
        rv$kim_pilb_sectors <- kim_pilb_sector_choices
        rv$kim_pilb_years <- kim_pilb_year_choices
        rv$kim_pilb_bioregions <- kim_pilb_bioregion_choices
        rv$kim_pilb_zones <- kim_pilb_zone_choices
        rv$kim_pilb_locations <- kim_pilb_location_choices
        
        updatePickerInput(session, "sector_select_bio", choices = kim_pilb_sector_choices, selected = kim_pilb_sector_choices)
        updatePickerInput(session, "year_select2", choices = kim_pilb_year_choices, selected = kim_pilb_year_choices)
        updatePickerInput(session, "bioregion_select_bio", choices = kim_pilb_bioregion_choices, selected = kim_pilb_bioregion_choices)
        updatePickerInput(session, "zone_select_bio", choices = kim_pilb_zone_choices, selected = kim_pilb_zone_choices)
        updatePickerInput(session, "location_select_bio", choices = kim_pilb_location_choices, selected = kim_pilb_location_choices)
        
        # Bio age data
        bio_age_data <- merged_kim_pilb_reactive() %>% filter(Fin_Yr_Age != -9, !is.na(Fin_Yr_Age))
        if (input$species_select != "All") {
          bio_age_data <- bio_age_data %>% filter(SpeciesName == input$species_select)
        }
        bio_age_year_choices <- sort(unique(bio_age_data$year)) %||% character(0)
        bio_age_bioregion_choices <- sort(unique(bio_age_data$BioRegion[!is.na(bio_age_data$BioRegion)])) %||% "Unknown"
        bio_age_zone_choices <- sort(unique(bio_age_data$Zone[!is.na(bio_age_data$Zone)])) %||% "Unknown"
        bio_age_location_choices <- sort(unique(bio_age_data$Location[!is.na(bio_age_data$Location)])) %||% "Unknown"
        rv$bio_age_years <- bio_age_year_choices
        rv$bio_age_bioregions <- bio_age_bioregion_choices
        rv$bio_age_zones <- bio_age_zone_choices
        rv$bio_age_locations <- bio_age_location_choices
        updatePickerInput(session, "year_select_bio_age", choices = bio_age_year_choices, selected = bio_age_year_choices)
        updatePickerInput(session, "bioregion_select_bio_age", choices = bio_age_bioregion_choices, selected = bio_age_bioregion_choices)
        updatePickerInput(session, "zone_select_bio_age", choices = bio_age_zone_choices, selected = bio_age_zone_choices)
        updatePickerInput(session, "location_select_bio_age", choices = bio_age_location_choices, selected = bio_age_location_choices)
      })
      
      # Sync rv with checkbox inputs
      observeEvent(input$year_select_retained, {
        rv$retained_years <- input$year_select_retained
      })
      
      observeEvent(input$year_select_released, {
        rv$released_years <- input$year_select_released
      })
      
      observeEvent(input$year_select2, {
        rv$kim_pilb_years <- input$year_select2
      })
      
      observeEvent(input$year_select_bio_age, {
        rv$bio_age_years <- input$year_select_bio_age
      })
      
      observeEvent(input$location_select_retained, {
        rv$retained_locations <- input$location_select_retained
      })
      
      observeEvent(input$location_select_released, {
        rv$released_locations <- input$location_select_released
      })
      
      observeEvent(input$location_select_bio, {
        rv$kim_pilb_locations <- input$location_select_bio
      })
      
      observeEvent(input$location_select_age, {
        rv$age_locations <- input$location_select_age
      })
      
      observeEvent(input$location_select_bio_age, {
        rv$bio_age_locations <- input$location_select_bio_age
      })
      
      observeEvent(input$zone_select_bio, {
        rv$kim_pilb_zones <- input$zone_select_bio
      })
      
      observeEvent(input$bioregion_select_bio, {
        rv$kim_pilb_bioregions <- input$bioregion_select_bio
      })
      
      observeEvent(input$zone_select_bio_age, {
        rv$bio_age_zones <- input$zone_select_bio_age
      })
      
      observeEvent(input$bioregion_select_bio_age, {
        rv$bio_age_bioregions <- input$bioregion_select_bio_age
      })
      
      observeEvent(input$sector_select_bio, {
        rv$kim_pilb_sectors <- input$sector_select_bio
      })
      
      
      
      # Update selections after refresh
      observeEvent(input$refresh_btn, {
        retained_data <- length_plot_data1_retained()
        released_data <- length_plot_data1_released()
        
        retained_available <- if (!is.null(retained_data) && nrow(retained_data) > 0) {
          sort(unique(retained_data$year))
        } else {
          character(0)
        }
        released_available <- if (!is.null(released_data) && nrow(released_data) > 0) {
          sort(unique(released_data$year))
        } else {
          character(0)
        }
        
        rv$retained_years <- intersect(rv$retained_years, retained_available)
        if (length(rv$retained_years) == 0 && length(retained_available) > 0) {
          rv$retained_years <- retained_available
        }
        
        rv$released_years <- intersect(rv$released_years, released_available)
        if (length(rv$released_years) == 0 && length(released_available) > 0) {
          rv$released_years <- released_available
        }
        
        data <- fixedsiteonly_reactive()
        if (input$species_select != "All") {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        year_choices <- sort(unique(data$year))
        
        updateCheckboxGroupInput(session, "year_select_retained",
                                 choices = year_choices,
                                 selected = rv$retained_years,
                                 inline = TRUE)
        updateCheckboxGroupInput(session, "year_select_released",
                                 choices = year_choices,
                                 selected = rv$released_years,
                                 inline = TRUE)
      })
      
      # EventReactive data for Length Plot 1 (combined)
      length_plot_data1 <- eventReactive(list(input$refresh_btn, refresh_trigger()), {
        data <- base_data1()
        selected_years <- union(rv$retained_years, rv$released_years)
        data %>% filter(year %in% selected_years)
        
        
      })

      # EventReactive data for Length Plot 1 - Retained Catch
      length_plot_data1_retained <- eventReactive(list(input$refresh_btn, refresh_trigger()), {
        current_use_fis_length <- isolate(input$use_fis_length)
        current_data_include_before <- isolate(input$data_include_before)
        current_base_data1 <- isolate(base_data1()) 
        
        if (!isTRUE(current_use_fis_length) || !"Length" %in% current_data_include_before) {
          message("length_plot_data1_retained: Skipped due to use_fis_length FALSE or Length not included")
          return(data.frame())
        }
        data <- current_base_data1
        if (nrow(data) == 0) {
          message("length_plot_data1_retained: No FIS data available")
          return(data.frame())
        }
        
        valid_years <- intersect(isolate(rv$retained_years), unique(data$year))
        valid_locations <- intersect(isolate(rv$retained_locations), unique(data$Location))
        
        if (length(valid_years) == 0 || length(valid_locations) == 0) {
          message("length_plot_data1_retained: No valid years or locations selected for current data")
          return(data.frame())
        }
        data <- data %>%
          filter(year %in% valid_years,
                 Discarded. == "No",
                 Location %in% valid_locations)
        
        # --- NEW: Minimum Sample Size Filter ---
        if (!is.null(input$min_sample_size_length) && input$min_sample_size_length > 0) {
          years_with_enough_data <- data %>%
            group_by(year) %>%
            summarise(n = n()) %>%
            filter(n >= input$min_sample_size_length) %>%
            pull(year)
          
          data <- data %>% filter(year %in% years_with_enough_data)
        }
        # ---------------------------------------
        
        data
      })
      
      length_plot_data1_released <- eventReactive(list(input$refresh_btn, refresh_trigger()), {
        current_use_fis_length <- isolate(input$use_fis_length)
        current_data_include_before <- isolate(input$data_include_before)
        if (!isTRUE(current_use_fis_length) || !"Length" %in% current_data_include_before) {
          message("length_plot_data1_released: Skipped (Overall FIS length off or Length not in data_include).")
          return(data.frame())
        }
        
        if (is.null(input$use_released_fis_data) || !isTRUE(input$use_released_fis_data)) {
          message("length_plot_data1_released: Skipped ('Use Released FIS data' is unchecked).")
          return(data.frame())
        }
        
        data <- isolate(base_data1())
        
        data_released <- data %>% filter(Discarded. == "Yes")
        
        if (nrow(data_released) == 0) {
          message("length_plot_data1_released: No 'Discarded. == Yes' data available.")
          return(data.frame())
        }
        
        valid_years <- intersect(isolate(rv$released_years), unique(data_released$year))
        valid_locations <- intersect(isolate(rv$released_locations), unique(data_released$Location))
        
        if (length(valid_years) == 0 || length(valid_locations) == 0) {
          message("length_plot_data1_released: No valid years or locations selected for current 'Discarded == Yes' data.")
          return(data.frame())
        }
        
        data_released <- data_released %>%
          filter(year %in% valid_years,
                 Location %in% valid_locations)
        
        # --- NEW: Minimum Sample Size Filter ---
        if (!is.null(input$min_sample_size_length) && input$min_sample_size_length > 0) {
          years_with_enough_data <- data_released %>%
            group_by(year) %>%
            summarise(n = n()) %>%
            filter(n >= input$min_sample_size_length) %>%
            pull(year)
          
          data_released <- data_released %>% filter(year %in% years_with_enough_data)
        }
        # ---------------------------------------
        
        message("length_plot_data1_released: Rows after filtering = ", nrow(data_released))
        data_released
      })
      
      length_plot_data2 <- eventReactive(
        list(input$refresh_btn, refresh_trigger()), {
          current_use_bio_length <- isolate(input$use_bio_length)
          current_data_include_before <- isolate(input$data_include_before)
          
          if (!isTRUE(current_use_bio_length) || !"Length" %in% current_data_include_before) {
            return(data.frame()) # Return empty data frame
          }
          
          data_from_base <- isolate(base_data2())
          
          if (nrow(data_from_base) == 0) {
            return(data.frame()) # Return empty data frame
          }
          
          sel_sectors <- isolate(input$sector_select_bio)
          sel_years <- isolate(input$year_select2)
          sel_bioregions <- isolate(input$bioregion_select_bio)
          sel_zones <- isolate(input$zone_select_bio)
          sel_locations <- isolate(input$location_select_bio)
          
          if (is.null(sel_sectors) || length(sel_sectors) == 0 ||
              is.null(sel_years) || length(sel_years) == 0 ||
              is.null(sel_bioregions) || length(sel_bioregions) == 0 ||
              is.null(sel_zones) || length(sel_zones) == 0 ||
              is.null(sel_locations) || length(sel_locations) == 0) {
            return(data.frame()) # Return empty data frame
          }
          
          # Filter by the current selections from the pickers
          filtered_data <- data_from_base %>%
            filter(Sector %in% sel_sectors,
                   as.character(year) %in% as.character(sel_years),
                   BioRegion %in% sel_bioregions,
                   Zone %in% sel_zones,
                   Location %in% sel_locations)
          
          # --- NEW: Minimum Sample Size Filter ---
          if (!is.null(input$min_sample_size_length) && input$min_sample_size_length > 0) {
            years_with_enough_data <- filtered_data %>%
              group_by(year) %>%
              summarise(n = n()) %>%
              filter(n >= input$min_sample_size_length) %>%
              pull(year)
            
            filtered_data <- filtered_data %>% filter(year %in% years_with_enough_data)
          }
          # ---------------------------------------
          
          if (nrow(filtered_data) == 0) {
            return(data.frame()) # Return empty data frame if filters yield no results
          }
          
          return(filtered_data)
        })
      
      # EventReactive data for Length Plot 3 (FRDC88)
      length_plot_data3 <- eventReactive(list(input$refresh_btn, refresh_trigger()), {
        base_data3() %>% filter(year %in% rv$frdc88_years)
      })
      
      age_plot_data <- eventReactive(list(input$age_refresh_btn, refresh_trigger()), {
        data <- base_data_age()
        if (!is.logical(input$use_fis_age)) {
          message("age_plot_data: Invalid use_fis_age type, treating as FALSE")
          input$use_fis_age <- FALSE
        }
        if (nrow(data) == 0 || !input$use_fis_age) {
          message("age_plot_data: Returning empty due to no data or use_fis_age FALSE")
          return(data.frame())
        }
        if (is.null(input$year_select_age) || length(input$year_select_age) == 0) {
          message("age_plot_data: No years selected, using all years")
          if (!"IntAge" %in% colnames(data) || !is.numeric(data$IntAge)) {
            message("age_plot_data: IntAge column missing or non-numeric")
            return(data.frame())
          }
          data <- data %>% filter(Location %in% rv$age_locations)
          return(data)
        }
        valid_years <- as.character(input$year_select_age)
        data <- data %>% 
          filter(year %in% valid_years,
                 Location %in% rv$age_locations)
        data
      })
      
      # Corrected bio_age_plot_data to use the correct `input$` values
      bio_age_plot_data <- eventReactive(list(input$age_refresh_btn, refresh_trigger()), {
        # This now correctly uses the 'input' values directly, but only runs when the refresh button is clicked.
        data <- base_data_bio_age()
        if (nrow(data) == 0 || !input$use_bio_age) {
          message("bio_age_plot_data: Returning empty due to no data or use_bio_age FALSE")
          return(data.frame())
        }
        
        # Use isolate() to read the current values of the pickers when the button is pressed.
        sel_years <- isolate(input$year_select_bio_age)
        sel_bioregions <- isolate(input$bioregion_select_bio_age)
        sel_zones <- isolate(input$zone_select_bio_age)
        sel_locations <- isolate(input$location_select_bio_age)
        
        if (is.null(sel_years) || is.null(sel_bioregions) || is.null(sel_zones) || is.null(sel_locations)) {
          return(data.frame()) # Return empty if any filter is not yet available
        }
        
        data <- data %>%
          filter(year %in% sel_years,
                 BioRegion %in% sel_bioregions,
                 Zone %in% sel_zones,
                 Location %in% sel_locations)
        
        data
      })
      
      # Render UI for conditional age-at-length
      output$conditional_age_ui <- renderUI({
        checkboxInput("conditional_age_select", "Conditional age-at-length", value = FALSE)
      })
      
      # Enable/disable conditional checkbox
      observe({
        if ("Age" %in% input$data_include_before) {
          shinyjs::enable("conditional_age_select")
        } else {
          shinyjs::disable("conditional_age_select")
          updateCheckboxInput(session, "conditional_age_select", value = FALSE)
        }
      })
      
      # Render Length table
      output$data_table <- renderTable({
        req(input$use_fis_data) # Ensure checkbox is ticked
        data_to_summarize <- base_data1() 
        
        if (nrow(data_to_summarize) == 0) {
          return(data.frame(Message = "No FIS data available for this species."))
        }
        
        LFplot.cor <- data_to_summarize %>%
          group_by(year) %>%
          dplyr::summarize(n = paste("n =", n()), Year = unique(year)) %>%
          ungroup()
        
        if (nrow(LFplot.cor) == 0){
          return(data.frame(Message = "No FIS data for currently selected years."))
        }
        
        LFplot.cor
      })
      
      # Render Age table
      output$age_table <- renderTable({
        req(base_data_age())
        data <- base_data_age()
        if (!is.logical(input$use_fis_age)) {
          message("age_table: Invalid use_fis_age type, treating as FALSE")
          input$use_fis_age <- FALSE
        }
        if (nrow(data) == 0 || !input$use_fis_age) {
          message("age_table: Returning NULL due to no data or use_fis_age FALSE")
          return(NULL)
        }
        age_summary <- data %>%
          group_by(year) %>%
          summarise(n = paste("n =", n()), .groups = "drop")
        age_summary
      })
      
      # Render Biological Parameters table
      output$bio_table <- renderUI({
        result <- bio_tab()
        if (is.null(result) || is.null(result$tabdat)) {
          return(HTML("<p>No biological data available.</p>"))
        }
        req(result$tabdat)
        if (!is.data.frame(result$tabdat) || ncol(result$tabdat) == 0) {
          return(HTML("<p>Error: Biological data is not in a valid table format.</p>"))
        }
        result$tabdat[] <- lapply(result$tabdat, function(col) {
          gsub("__([^__]+)__", "<strong>\\1</strong>", col)
        })
        options(knitr.kable.NA = '')
        table_html <- knitr::kable(
          result$tabdat,
          align = 'lccl',
          format = "html",
          escape = FALSE
        ) %>%
          as.character()
        HTML(paste(table_html, "<script>MathJax.typeset()</script>"))
      })
      
      # Render the caption as Markdown
      output$bio_caption <- renderUI({
        result <- bio_tab()
        caption_text <- result$caption_text
        if (!is.null(caption_text) && nchar(caption_text) > 0) {
          html_caption <- markdown::markdownToHTML(text = caption_text, fragment.only = TRUE)
          HTML(html_caption)
        } else {
          HTML("<p>No caption available.</p>")
        }
      })
      
      # Output the formatted bibliography as HTML
      output$bibliography_output <- renderUI({
        result <- bio_tab()
        if (!is.null(result$bibliography_string)) {
          HTML(result$bibliography_string)
        } else {
          HTML("<p>No bibliography available.</p>")
        }
      })
      
      # Output the bibliography as a DataTable
      output$bibliography_table <- renderDT({
        result <- bio_tab()
        bib_data <- tryCatch({
          suppressWarnings(RefManageR::ReadBib("interim_data/References.bib"))
        }, warning = function(w) {
          message("Warnings while reading BibTeX file (DataTable): ", w$message)
          RefManageR::ReadBib("interim_data/References.bib")
        }, error = function(e) {
          message("Error reading BibTeX file (DataTable): ", e$message)
          NULL
        })
        
        if (!is.null(bib_data)) {
          message("BibTeX entries loaded (DataTable): ", paste(names(bib_data), collapse = ", "))
        } else {
          message("No BibTeX entries loaded (DataTable).")
        }
        
        if (!is.null(bib_data) && length(bib_data) > 0) {
          # Extract cited keys from caption_text
          bib_keys <- character(0)
          if (!is.null(result$caption_text) && !is.na(result$caption_text) && nzchar(result$caption_text)) {
            extracted_keys <- str_extract_all(result$caption_text, "@[A-Za-z0-9_:]+")[[1]]
            bib_keys <- sub("^@", "", extracted_keys)
            if (length(bib_keys) == 0 || all(bib_keys == "")) {
              bib_keys <- str_extract_all(result$caption_text, "(?<=@)[A-Za-z0-9_:]+(?=[;\\]])")[[1]]
            }
          }
          
          # Log extracted keys
          message("DataTable caption_text: ", if (is.null(result$caption_text)) "NULL" else result$caption_text)
          message("DataTable extracted BibTeX keys: ", paste(bib_keys, collapse = ", "))
          
          # Check for missing references
          if (length(bib_keys) > 0) {
            missing_keys <- bib_keys[!bib_keys %in% names(bib_data)]
            if (length(missing_keys) > 0) {
              message("Warning: Cited references not found in .bib file (DataTable): ", paste(missing_keys, collapse = ", "))
            }
            # Debug matching keys
            matching_keys <- bib_keys[bib_keys %in% names(bib_data)]
            message("Matching BibTeX keys (DataTable): ", paste(matching_keys, collapse = ", "))
          } else {
            missing_keys <- character(0)
            matching_keys <- character(0)
            message("No BibTeX keys to match (DataTable).")
          }
          
          # Filter bib_data
          bib_data <- bib_data[names(bib_data) %in% bib_keys]
          
          # Debug filtered result
          message("Filtered BibTeX entries (DataTable): ", paste(names(bib_data), collapse = ", "))
          
          if (length(bib_data) > 0) {
            # Explicitly cite keys
            tryCatch({
              for (key in names(bib_data)) {
                RefManageR::Cite(bib_data, key, .opts = list(cite.style = "apa", style = "text"))
              }
            }, warning = function(w) {
              message("Warning in citing keys (DataTable): ", w$message)
            }, error = function(e) {
              message("Error in citing keys (DataTable): ", e$message)
            })
            
            bibliography_df <- data.frame(
              Reference = sapply(bib_data, function(x) {
                tryCatch({
                  citation <- paste(capture.output(print(x)), collapse = " ")
                  citation <- sub("^\\[1\\]\\s*", "", citation)
                  citation <- gsub("_([^_]+)_", "<i>\\1</i>", citation)
                  if (!is.null(x$url)) {
                    citation <- paste(citation, sprintf("<a href='%s' target='_blank'>%s</a>", x$url, x$url))
                  }
                  citation <- sub("\\.$", "", citation)
                  return(citation)
                }, error = function(e) {
                  paste("Error formatting entry:", names(x))
                })
              })
            )
            if (length(missing_keys) > 0) {
              bibliography_df <- rbind(
                bibliography_df,
                data.frame(Reference = paste("Missing references:", paste(missing_keys, collapse = ", ")))
              )
            }
            DT::datatable(
              bibliography_df,
              options = list(pageLength = 10, escape = FALSE),
              rownames = FALSE
            )
          } else {
            DT::datatable(
              data.frame(Message = paste(
                "No references cited in the caption or found in .bib file.",
                if (length(missing_keys) > 0) paste("Missing:", paste(missing_keys, collapse = ", ")) else ""
              ))
            )
          }
        } else {
          DT::datatable(data.frame(Message = "No bibliography available or all entries ignored due to missing fields."))
        }
      })
      
      # This reactive expression generates the list of available fleets.
      reactive_catch_sector_choices <- reactive({
        data_for_fleet_calc <- catch_ts
        
        if (!is.null(input$catch_species_select) && length(input$catch_species_select) > 0) {
          if (!"All" %in% input$catch_species_select) {
            data_for_fleet_calc <- catch_ts %>% 
              dplyr::filter(Specstock %in% input$catch_species_select)
          } else if (!is.null(input$species_select) && input$species_select != "All") {
            data_for_fleet_calc <- catch_ts %>% 
              dplyr::filter(Specstock == input$species_select)
          }
        } else if (!is.null(input$species_select) && input$species_select != "All") {
          data_for_fleet_calc <- catch_ts %>% 
            dplyr::filter(Specstock == input$species_select)
        }
        
        if (nrow(data_for_fleet_calc) == 0 && nrow(catch_ts) > 0) {
          data_for_fleet_calc <- catch_ts
        }
        
        potential_fleet_cols <- setdiff(names(data_for_fleet_calc), c("Specstock", "year", "CSIRO"))
        
        if (length(potential_fleet_cols) == 0) {
          return(c("Foreign.Trawl"))
        }
        
        data_for_sums <- data_for_fleet_calc[, potential_fleet_cols, drop = FALSE]
        for(col_name in potential_fleet_cols) {
          if(!is.numeric(data_for_sums[[col_name]])) {
            data_for_sums[[col_name]] <- as.numeric(as.character(data_for_sums[[col_name]]))
            data_for_sums[[col_name]][is.na(data_for_sums[[col_name]])] <- 0
          }
        }
        data_for_sums[is.na(data_for_sums)] <- 0 
        
        sector_sums <- colSums(data_for_sums, na.rm = FALSE)
        
        active_sector_choices <- names(sector_sums)[sector_sums > 0] 
        
        all_defined_fleets <- potential_fleet_cols
        
        final_choices <- sort(unique(c("Foreign.Trawl", active_sector_choices, all_defined_fleets)))
        
        if (length(final_choices) == 0) {
          return(c("Foreign.Trawl"))
        }
        
        return(final_choices)
      })
      
      # Observer to update the year0fleet selectizeInput choices and selection
      observeEvent(reactive_catch_sector_choices(), {
        fleets <- reactive_catch_sector_choices()
        
        current_value <- isolate(input$year0fleet)
        
        if (is.null(current_value) || !nzchar(trimws(current_value))) {
          current_value <- "Foreign.Trawl" 
        }
        
        all_choices <- unique(c(fleets, current_value))
        all_choices <- all_choices[!is.na(all_choices) & nzchar(trimws(all_choices))]
        
        if (length(all_choices) == 0) {
          all_choices <- c("Foreign.Trawl")
        }
        
        selected_val <- current_value
        if (!(current_value %in% all_choices)) {
          if ("Foreign.Trawl" %in% all_choices) {
            selected_val <- "Foreign.Trawl"
          } else {
            selected_val <- all_choices[1]
          }
        }
        
        updateSelectizeInput(session, "year0fleet",
                             choices = all_choices,
                             selected = selected_val,
                             server = FALSE)
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
      
      
      # Function to generate catch plot
      render_catch_plot <- function(data) {
        if (nrow(data) == 0) {
          return(ggplot() + annotate("text", x = 0, y = 0, label = "No catch data available") + theme_void())
        }
        if (ncol(data) <= 2) {
          return(ggplot() + annotate("text", x = 0, y = 0, label = "No non-zero catch sectors available for selected species") + theme_void())
        }
        catch_long <- data %>%
          pivot_longer(cols = -c(Specstock, year), names_to = "Sector", values_to = "Catch", values_drop_na = TRUE) %>%
          filter(Catch > 0)
        if (nrow(catch_long) == 0) {
          return(ggplot() + annotate("text", x = 0, y = 0, label = "No catch data available") + theme_void())
        }
        
        dpird_dark_blue <- "#003F51"
        dpird_mid_blue <- "#42797F"
        dpird_light_blue <- "#BFD7DF"
        
        sector_colors <- c(
          "Charter" = dpird_mid_blue,
          "Commercial" = dpird_dark_blue,
          "Commercial.Trap" = "#94B237",
          "Commercial.Line" = "#568B8E",
          "Foreign.Trawl" = "#8E5F7E",
          "Foreign.Line" = "#D9B852",
          "Recreational" = dpird_light_blue,
          "Foreign.Trawl.Japan" = "#B1894D",
          "Foreign.Trawl.Russia" = "#7F7F7F",
          "CSIRO.Research.Trawl" = "#6B5045",
          "Foreign.Trawl.Taiwan" = "#CC9950",
          "Foreign.Trawl.China" = "#4A3F3F",
          "Commonwealth" = "#3F7C4E",
          "Foreign" = "#B97C52",
          "Commercial.Monthly" = "#F2CC8F",
          "Commercial.daily" = "#E0B384",
          "Commercial.GillNet" = "#E6D7AD",
          "Commercial.OpenAccess" = "#6A8C7F",
          "Commercial.Estuarine" = "#A0C4B8",
          "Commercial.Trawl" = "#7D9D9C"
        )
        
        desired_levels <- c(
          "Commercial", "Commercial.Trap", "Commercial.Line", "Recreational", "Charter",
          "Foreign", "Foreign.Trawl", "Foreign.Line", "Foreign.Trawl.Japan", "Foreign.Trawl.Russia", 
          "Foreign.Trawl.Taiwan", "Foreign.Trawl.China", "Commonwealth", "CSIRO.Research.Trawl",
          "Commercial.Monthly", "Commercial.daily","Commercial.GillNet","Commercial.OpenAccess",
          "Commercial.Estuarine","Commercial.Trawl"
        )
        catch_long$Sector <- factor(catch_long$Sector, levels = desired_levels)
        ggplot(catch_long, aes(x = year, y = Catch, fill = Sector)) +
          geom_bar(stat = "identity", position = "stack") +
          labs(x = "Year", y = "Catch (tonnes)",
               title = paste("Catch by Sector for",
                             if(length(unique(data$Specstock)) > 1) "Multiple Species" else unique(data$Specstock))) +
          theme_minimal(base_family = "Arial") +
          theme(legend.position = "bottom", legend.title = element_blank(), legend.key.width = unit(0.5, "cm")) +
          scale_fill_manual(values = sector_colors) +
          guides(fill = guide_legend(ncol = 3))
      }
      
      
      render_effort_plot <- function(data) {
        if (is.null(data) || nrow(data) == 0) {
          return(list(
            plot = ggplot() + 
              annotate("text", x = 0, y = 0, label = "No indices data available for selected species/fleets") + 
              theme_void(),
            height = 400
          ))
        }
        
        dpird_dark_blue <- "#003F51"
        dpird_mid_blue <- "#42797F"
        dpird_light_blue <- "#BFD7DF"
        
        
        sector_colors_dpird <- c(
          "Charter" = dpird_mid_blue,
          "Commercial" = dpird_dark_blue,
          "Commercial.Trap" = "#94B237",
          "Commercial.Line" = "#568B8E",
          "Foreign.Trawl" = "#8E5F7E",
          "Foreign.Line" = "#D9B852",
          "Recreational" = dpird_light_blue,
          "Foreign.Trawl.Japan" = "#B1894D",
          "Foreign.Trawl.Russia" = "#7F7F7F",
          "CSIRO.Research.Trawl" = "#6B5045",
          "Foreign.Trawl.Taiwan" = "#CC9950",
          "Foreign.Trawl.China" = "#4A3F3F",
          "Commonwealth" = "#3F7C4E",
          "Foreign" = "#B97C52",
          "Commercial.Monthly" = "#F2CC8F",
          "Commercial.daily" = "#E0B384",
          "Commercial.GillNet" = "#E6D7AD",
          "Commercial.OpenAccess" = "#6A8C7F",
          "Commercial.Estuarine" = "#A0C4B8",
          "Commercial.Trawl" = "#7D9D9C"
        )
        
        set1_colors_hex <- c(
          "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
          "#FFFF33", "#A65628", "#F781BF", "#999999"
        )
        
        current_fleets <- unique(data$fleet)
        
        combined_colors <- character(0)
        
        set1_idx <- 1
        
        for (fleet_name in current_fleets) {
          if (fleet_name %in% names(sector_colors_dpird)) {
            combined_colors[fleet_name] <- sector_colors_dpird[fleet_name]
          } else {
            combined_colors[fleet_name] <- set1_colors_hex[set1_idx]
            set1_idx <- set1_idx + 1
            if (set1_idx > length(set1_colors_hex)) {
              set1_idx <- 1
            }
          }
        }
        
        num_fleets <- length(current_fleets)
        base_height <- 100
        height_per_fleet <- 150
        plot_height <- max(400, base_height + height_per_fleet * num_fleets)
        
        plot <- ggplot(data, aes(x = year, y = obs, color = fleet)) +
          geom_line(linewidth = 1) +
          geom_point(size = 2) +
          geom_errorbar(aes(ymin = obs * exp(-se_log * 1.96), ymax = obs * exp(se_log * 1.96)), width = 0.2) +
          facet_grid(fleet ~ ., scales = "free_y") +
          labs(x = "Year", y = "Catch Rate (Standardised)",
               title = paste("Indices by Fleet for",
                             if(length(unique(data$Specstock)) > 1) "Multiple Species" else unique(data$Specstock))) +
          theme_minimal(base_family = "Arial") +
          theme(legend.position = "none",
                strip.text.y = element_text(face = "bold")) +
          scale_color_manual(values = combined_colors)
        
        return(list(plot = plot, height = plot_height))
      }
      
      # Function to generate length histogram
      render_length_histogram <- function(data, length_col = "LengthClass") {
        fill_col <- input$length_color_by
        
        if (nrow(data) == 0) {
          return(ggplot() + 
                   annotate("text", x = 0, y = 0, 
                            label = "No length data available.\nPlease select at least one year and location.") + 
                   theme_void())
        }
        if (is.null(data[[length_col]]) || !(fill_col %in% colnames(data)) || all(is.na(data[[fill_col]]))) {
          return(ggplot() + 
                   annotate("text", x = 0, y = 0, 
                            label = paste("Invalid data: Length column or", fill_col, "missing.\nCheck your selections.")) + 
                   theme_void())
        }
        
        data[[fill_col]] <- as.factor(data[[fill_col]])
        LFplot.cor <- data %>%
          group_by(year) %>%
          dplyr::summarize(n = paste("n =", n()), Year = unique(year))
        LFplot.ymax <- 0.1
        MaxLen <- max(data[[length_col]], na.rm = TRUE)
        MinLen <- min(data[[length_col]], na.rm = TRUE)
        valid_fill_values <- unique(data[[fill_col]][!is.na(data[[fill_col]])])
        num_fill_values <- length(valid_fill_values)
        legend_ncol <- if (num_fill_values > 0) min(num_fill_values, 3) else 1
        
        suppressWarnings({
          ggplot(data, aes(x = .data[[length_col]], fill = .data[[fill_col]])) +
            geom_bar(aes(y = after_stat(count) / tapply(after_stat(count), after_stat(PANEL), sum)[after_stat(PANEL)]),
                     colour = "black", na.rm = TRUE) +
            labs(x = paste("Length (mm)", ifelse(input$length_metric == "FL", "Fork Length", "Total Length")),
                 y = "Proportion",
                 fill = fill_col) +
            # facet_rep_wrap(year ~ ., scales = "fixed", ncol = 1, dir = "v") +
            
            facet_wrap(~ year, scales = "free_x", ncol = 1, dir = "v") +
            theme_classic(base_family = "Arial") +
            theme(strip.background = element_blank(), strip.text = element_blank(),
                  legend.title.position = "top", legend.key.width = unit(0.5, "cm"),
                  legend.position = "bottom") +
            guides(fill = guide_legend(ncol = legend_ncol)) +
            geom_text(data = LFplot.cor,
                      aes(x = Inf, y = Inf, label = paste(Year, n, sep = "\n")),
                      hjust = 1.1, vjust = 1, size = 4, colour = "black", inherit.aes = FALSE) +
            geom_rangeframe(data = data.frame(x = c(floor(MinLen / 100) * 100, MaxLen + 100),
                                              y = c(0, LFplot.ymax)),
                            aes(x, y), sides = "bl", inherit.aes = FALSE) +
            coord_capped_cart(left = "both", bottom = "both") +
            scale_x_continuous(limits = c(floor(MinLen / 100) * 100, MaxLen + 100)) +
            scale_y_continuous()
        })
      }
      
      # Function to generate age histogram
      render_age_histogram <- function(data, age_col = "IntAge") {
        fill_col <- input$age_color_by
        
        if (nrow(data) == 0 || !age_col %in% colnames(data) || !is.numeric(data[[age_col]]) || !(fill_col %in% colnames(data)) || all(is.na(data[[fill_col]]))) {
          message("render_age_histogram: Returning empty plot due to invalid data or color column")
          return(ggplot() + annotate("text", x = 0, y = 0, label = paste("No age data available or invalid color column:", fill_col)) + theme_void())
        }
        
        data[[fill_col]] <- as.factor(data[[fill_col]])
        
        age_summary <- data %>%
          group_by(year) %>%
          dplyr::summarize(n = paste("n =", n()), Year = unique(year), .groups = "drop")
        
        MaxAge <- max(data[[age_col]], na.rm = TRUE)
        MinAge <- min(data[[age_col]], na.rm = TRUE)
        plot_min_age <- 0
        plot_max_age <- ceiling(MaxAge + 0.5)
        
        valid_fill_values <- unique(data[[fill_col]][!is.na(data[[fill_col]])])
        num_fill_values <- length(valid_fill_values)
        legend_ncol <- if (num_fill_values > 0) min(num_fill_values, 3) else 1
        
        # ggplot(data, aes(x = .data[[age_col]], fill = .data[[fill_col]])) +
        #   geom_bar(aes(y = after_stat(count) / tapply(after_stat(count), after_stat(PANEL), sum)[after_stat(PANEL)]),
        #            colour = "black") +
        #   labs(x = "Age (years)", y = "Proportion", fill = fill_col) +
        #   facet_rep_wrap(year ~ ., scales = "fixed", ncol = 1, dir = "v") +
        #   theme_classic(base_family = "Arial") +
        #   theme(strip.background = element_blank(), strip.text = element_blank(),
        ggplot(data, aes(x = .data[[age_col]], fill = .data[[fill_col]])) +
          geom_bar(aes(y = after_stat(count) / tapply(after_stat(count), after_stat(PANEL), sum)[after_stat(PANEL)]),
                   colour = "black") +
          labs(x = "Age (years)", y = "Proportion", fill = fill_col) +
          facet_wrap(~ year, scales = "fixed", ncol = 1, dir = "v") +      # <--- FIXED
          theme_classic(base_family = "Arial") +
          theme(strip.background = element_blank(), strip.text = element_blank(),
                legend.title.position = "top", legend.key.width = unit(0.5, "cm"),
                legend.position = "bottom") +
          guides(fill = guide_legend(ncol = legend_ncol)) +
          geom_text(data = age_summary,
                    aes(x = Inf, y = Inf, label = paste(Year, n, sep = "\n")),
                    hjust = 1.1, vjust = 1, size = 4, colour = "black", inherit.aes = FALSE) +
          geom_rangeframe(data = data.frame(x = c(plot_min_age, plot_max_age),
                                            y = c(0, 0.1)),
                          aes(x, y), sides = "bl", inherit.aes = FALSE) +
          coord_capped_cart(left = "both", bottom = "both") +
          scale_x_continuous(limits = c(plot_min_age, plot_max_age), breaks = seq(0, ceiling(MaxAge), by = 1)) +
          scale_y_continuous()
      }
      
      # Render Catch Plot
      output$catch_plot <- renderPlot({
        req(catch_plot_data())
        render_catch_plot(catch_plot_data())
      })
      
      # Render Effort Plot
      output$effort_plot <- renderPlot({
        req(effort_plot_data())
        result <- render_effort_plot(effort_plot_data())
        result$plot
      }, height = function() {
        result <- render_effort_plot(effort_plot_data())
        result$height
      })
      # Helper function to calculate plot height
      calculate_plot_height <- function(data) {
        if (nrow(data) == 0) return(400)
        num_years <- length(unique(data$year))
        base_height <- 200
        height_per_facet <- 150
        max(base_height + height_per_facet * num_years, 400)
      }
      
      fis_age_plot_height <- reactive({
        data <- tryCatch({
          age_plot_data()
        }, error = function(e) {
          message("fis_age_plot_height: Error in age_plot_data: ", e$message)
          data.frame()
        })
        num_years <- if (nrow(data) > 0 && "year" %in% colnames(data)) length(unique(data$year)) else 0
        height <- max(400, num_years * 200)
        height
      })
      
      bio_age_plot_height <- reactive({
        data <- bio_age_plot_data()
        num_years <- if (nrow(data) > 0) length(unique(data$year)) else 0
        height <- max(400, num_years * 200)
        height
      })
      
      # FIS Section (Table and Plot 1)
      output$length_section1 <- renderUI({
        if (!input$use_fis_length) {
          return(NULL)
        }
        
        fis_data_for_selectors <- fixedsiteonly_reactive()
        if (input$species_select != "All" && nrow(fis_data_for_selectors) > 0) {
          fis_data_for_selectors <- fis_data_for_selectors %>% filter(SpeciesName == input$species_select)
        }
        
        location_choices_retained <- sort(unique(fis_data_for_selectors %>% filter(Discarded. == "No") %>% pull(Location)))
        if (length(location_choices_retained) == 0) location_choices_retained <- "Unknown"
        
        location_choices_released <- sort(unique(fis_data_for_selectors %>% filter(Discarded. == "Yes") %>% pull(Location)))
        if (length(location_choices_released) == 0) location_choices_released <- "Unknown"
        
        plot_height_retained <- calculate_plot_height(length_plot_data1_retained())
        plot_height_released <- calculate_plot_height(length_plot_data1_released())
        
        retained_ui <- tagList(
          h5("Retained Catch Length FIS (Discarded = No)"),
          uiOutput("year_select_retained"),
          pickerInput("location_select_retained", "Select Locations for Retained Catch",
                      choices = location_choices_retained,
                      selected = rv$retained_locations %||% location_choices_retained,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          plotOutput("length_histogram1_retained", width = "800px", height = plot_height_retained)
        )
        
        released_ui_elements <- NULL
        if (!is.null(input$use_released_fis_data) && isTRUE(input$use_released_fis_data)) {
          released_ui_elements <- tagList(
            h5("Released Catch Length FIS (Discarded = Yes)"),
            uiOutput("year_select_released"),
            pickerInput("location_select_released", "Select Locations for Released Catch",
                        choices = location_choices_released,
                        selected = rv$released_locations %||% location_choices_released,
                        multiple = TRUE,
                        options = list(`actions-box` = TRUE, `live-search` = TRUE)),
            plotOutput("length_histogram1_released", width = "800px", height = plot_height_released)
          )
        }
        
        wellPanel(
          h5("Length Samples FIS (Fisheries Independent Survey)"),
          tableOutput("data_table"), 
          hr(),
          retained_ui,
          if (!is.null(released_ui_elements)) hr(),
          released_ui_elements,
          style = "margin-bottom: 20px;"
        )
      })
      
      output$length_section2 <- renderUI({
        req(input$species_select)
        
        data <- merged_kim_pilb_reactive()
        if (input$species_select != "All" && nrow(data) > 0) {
          data <- data %>% filter(SpeciesName == input$species_select)
        }
        
        initial_sector_choices <- if(nrow(data) > 0) sort(unique(data$Sector[!is.na(data$Sector)])) else character(0)
        if(length(initial_sector_choices) == 0 && nrow(data) > 0) initial_sector_choices <- "Unknown"
        
        initial_year_choices_vals <- if(nrow(data) > 0) sort(unique(as.character(data$year))) else character(0)
        initial_bioregion_choices <- if(nrow(data) > 0) sort(unique(data$BioRegion[!is.na(data$BioRegion)])) else character(0)
        if(length(initial_bioregion_choices) == 0 && nrow(data) > 0) initial_bioregion_choices <- "Unknown"
        initial_zone_choices <- if(nrow(data) > 0) sort(unique(data$Zone[!is.na(data$Zone)])) else character(0)
        if(length(initial_zone_choices) == 0 && nrow(data) > 0) initial_zone_choices <- "Unknown"
        initial_location_choices <- if(nrow(data) > 0) sort(unique(data$Location[!is.na(data$Location)])) else character(0)
        if(length(initial_location_choices) == 0 && nrow(data) > 0) initial_location_choices <- "Unknown"
        
        selected_sectors_init <- rv$kim_pilb_sectors %||% initial_sector_choices
        selected_years_init <- rv$kim_pilb_years %||% initial_year_choices_vals
        selected_bioregions_init <- rv$kim_pilb_bioregions %||% initial_bioregion_choices
        selected_zones_init <- rv$kim_pilb_zones %||% initial_zone_choices
        selected_locations_init <- rv$kim_pilb_locations %||% initial_location_choices
        
        year_labels_init <- sapply(initial_year_choices_vals, function(y) paste0(y, " (n=...)"))
        if(length(initial_year_choices_vals) == 0) year_labels_init <- character(0)
        
        wellPanel(
          h5("Length Histogram Biological databases"),
          pickerInput("sector_select_bio", "Select Sectors for Biological Samples",
                      choices = initial_sector_choices,
                      selected = selected_sectors_init,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("year_select2", "Select Years for Biological samples",
                      choices = if(length(initial_year_choices_vals) > 0) setNames(initial_year_choices_vals, year_labels_init) else character(0),
                      selected = selected_years_init,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("bioregion_select_bio", "Select BioRegions for Biological Samples",
                      choices = initial_bioregion_choices,
                      selected = selected_bioregions_init,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("zone_select_bio", "Select Zones for Biological Samples",
                      choices = initial_zone_choices,
                      selected = selected_zones_init,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          pickerInput("location_select_bio", "Select Locations for Biological Samples",
                      choices = initial_location_choices,
                      selected = selected_locations_init,
                      multiple = TRUE,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE)),
          textOutput("bio_length_sectors"),
          uiOutput("length_histogram2_ui"),
          style = "margin-bottom: 20px;"
        )
      })
      
      output$bio_age_sectors <- renderText({
        data <- bio_age_plot_data()
        if (nrow(data) == 0 || !input$use_bio_age) {
          return("No biological age data available (no sectors).")
        }
        sectors <- sort(unique(data$Sector[!is.na(data$Sector)]))
        if (length(sectors) == 0) {
          return("No sectors available in filtered biological age data.")
        }
        paste("Unique Sectors in Biological Age Data:", paste(sectors, collapse = ", "))
      })
      
      output$bio_length_sectors <- renderText({
        data <- length_plot_data2()
        if (nrow(data) == 0 || !input$use_bio_length) {
          return("No biological length data available (no sectors).")
        }
        sectors <- sort(unique(data$Sector[!is.na(data$Sector)]))
        if (length(sectors) == 0) {
          return("No sectors available in filtered biological length data.")
        }
        paste("Unique Sectors in Biological Length Data:", paste(sectors, collapse = ", "))
      })
      
      
      # Render Retained Catch Plot
      output$length_histogram1_retained <- renderPlot({
        req(length_plot_data1_retained())
        plot <- render_length_histogram(length_plot_data1_retained(), length_col = "LengthClass")
        if (!is.null(plot)) {
          plot <- plot + labs(title = "Retained Catch")
        } else {
          plot <- ggplot() + annotate("text", x = 0, y = 0, label = "No retained catch data available") + theme_void()
        }
        plot
      })
      
      # Render Released Catch Plot
      output$length_histogram1_released <- renderPlot({
        req(length_plot_data1_released())
        plot <- render_length_histogram(length_plot_data1_released(), length_col = "LengthClass")
        if (!is.null(plot)) {
          plot <- plot + labs(title = "Released Catch")
        } else {
          plot <- ggplot() + annotate("text", x = 0, y = 0, label = "No released catch data available") + theme_void()
        }
        plot
      })
      
      output$length_histogram2_ui <- renderUI({
        plot_data <- length_plot_data2()
        req(plot_data)
        
        dynamic_height <- calculate_plot_height(plot_data)
        
        plotOutput("length_histogram2", width = "800px", height = paste0(dynamic_height, "px"))
      })
      
      output$length_histogram2 <- renderPlot({
        req(length_plot_data2())
        render_length_histogram(length_plot_data2(), length_col = "LengthClass")
      })
      
      output$fis_age_plot_ui <- renderUI({
        req(input$use_fis_age, age_plot_data())
        height <- tryCatch({
          fis_age_plot_height()
        }, error = function(e) {
          400
        })
        plotOutput("age_histogram", width = "800px", height = height)
      })
      
      output$age_histogram <- renderPlot({
        req(age_plot_data(), input$use_fis_age)
        data <- age_plot_data()
        if (nrow(data) == 0 || !input$use_fis_age) {
          return(ggplot() + annotate("text", x = 0, y = 0, label = "No FIS age data available") + theme_void())
        }
        render_age_histogram(data, age_col = "IntAge")
      })
      
      fishery_params <- reactive({
        params <- data.frame(
          use_initial_catch = input$use_initial_catch,
          use_1_sex_model = input$use_1_sex_model,
          # --- NEW: Add flags to dataframe ---
          use_custom_max_size = input$use_custom_max_size,
          use_custom_max_age = input$use_custom_max_age,
          # -----------------------------------
          use_custom_M = input$use_custom_M,
          use_custom_sigma_r = input$use_custom_sigma_r,
          use_age_post_settlement = input$use_age_post_settlement,
          use_cv_growth_pattern = input$use_cv_growth_pattern,
          use_custom_h = input$use_custom_h,
          use_custom_R0 = input$use_custom_R0,
          use_recdevs_range = input$use_recdevs_range,
          use_custom_bias_adj = input$use_custom_bias_adj,
          estimate_growth_params = input$estimate_growth_params,
          estimate_Dirichlet = input$estimate_Dirichlet,
          use_francis_weighting = input$use_francis_weighting,
          use_Q_extraSD = input$use_Q_extraSD,
          use_time_varying_params = input$use_time_varying_params
        )
        if (input$use_initial_catch) {
          params <- cbind(params, data.frame(
            year0catch = input$year0catch,
            year0catchse = input$year0catchse,
            initf = input$initf,
            year0fleet = input$year0fleet
          ))
        }
        
        # --- NEW: Add values to dataframe if selected ---
        if (input$use_custom_max_size) {
          params <- cbind(params, data.frame(
            custom_max_size = input$custom_max_size
          ))
        }
        if (input$use_custom_max_age) {
          params <- cbind(params, data.frame(
            custom_max_age = input$custom_max_age
          ))
        }
        # ------------------------------------------------
        
        if (input$use_custom_M) {
          params <- cbind(params, data.frame(
            custom_M = input$custom_M
          ))
        }
        if (input$use_custom_sigma_r) {
          params <- cbind(params, data.frame(
            custom_sigma_r = input$custom_sigma_r
          ))
        }
        if (input$use_age_post_settlement) {
          params <- cbind(params, data.frame(
            age_post_settlement = input$age_post_settlement
          ))
        }
        if (input$use_cv_growth_pattern) {
          params <- cbind(params, data.frame(
            cv_growth_pattern = input$cv_growth_pattern
          ))
        }
        if (input$use_custom_h) {
          params <- cbind(params, data.frame(
            custom_h = input$custom_h
          ))
        }
        if (input$use_custom_R0) {
          params <- cbind(params, data.frame(
            custom_R0 = input$custom_R0
          ))
        }
        if (input$use_recdevs_range) {
          params <- cbind(params, data.frame(
            first_year_recr_devs = input$first_year_recr_devs,
            last_year_recr_devs = input$last_year_recr_devs
          ))
        }
        if (input$use_custom_bias_adj) {
          params <- cbind(params, data.frame(
            last_early_yr_nobias_adj = input$last_early_yr_nobias_adj,
            first_yr_fullbias_adj = input$first_yr_fullbias_adj,
            last_yr_fullbias_adj = input$last_yr_fullbias_adj,
            first_recent_yr_nobias_adj = input$first_recent_yr_nobias_adj,
            Use_max_bias_adj_in_MPD = input$Use_max_bias_adj_in_MPD
          ))
        }
        if (input$use_francis_weighting) {
          params <- cbind(params, data.frame(
            francis_weighting_value = input$francis_weighting_value
          ))
        }
        if (input$estimate_Dirichlet) {
          params <- cbind(params, data.frame(
            dirichlet_textbox_value = input$dirichlet_textbox_value
          ))
        }
        if (input$use_Q_extraSD) {
          params <- cbind(params, data.frame(
            Q_extraSD = input$Q_extraSD
          ))
        }
        if (input$use_time_varying_params) {
          params <- cbind(params, data.frame(
            time_varying_params_text = input$time_varying_params_text,
            use_time_varying_growth_all = input$use_time_varying_growth_all,
            use_time_varying_growth_L_at_Amin = input$use_time_varying_growth_L_at_Amin,
            use_time_varying_growth_L_at_Amax = input$use_time_varying_growth_L_at_Amax,
            use_time_varying_growth_vbK = input$use_time_varying_growth_vbK,
            use_time_varying_selectivity = input$use_time_varying_selectivity,
            use_time_varying_retention = input$use_time_varying_retention
          ))
        }
        params
      })
      
      # Download handler
      output$download_btn <- downloadHandler(
        filename = function() {
          selected_data <- data_include()
          data_codes <- c("Catch" = "C", "Effort" = "I", "Length" = "L", "Age" = "A")
          data_string_base <- paste(na.omit(data_codes[selected_data]), collapse = "")
          conditional_age_checked <- isTRUE(input$conditional_age_select)
          data_string <- if (conditional_age_checked) {
            paste0(data_string_base, "CndA")
          } else {
            data_string_base
          }
          if (data_string == "") data_string <- "None"
          species <- gsub(" ", "_", input$species_select)
          date_str <- format(Sys.Date(), "%Y-%m-%d")
          paste0(species, "_SS-", data_string, "_", date_str, ".zip")
        },
        content = function(file) {
          temp_file <- tempfile(fileext = ".txt")
          sink(temp_file, append = TRUE)
          on.exit({
            sink()
            if (file.exists(temp_file)) {
              log_content <- readLines(temp_file, warn = FALSE)
              append_to_log(log_content)
              unlink(temp_file)
            }
          })
          
          tryCatch({
            temp_dir <- tempdir()
            temp_files <- c()
            
            # --- START: Save App State ---
            app_state <- list()
            
            all_input_names <- names(isolate(reactiveValuesToList(input))) 
            
            # append_to_log(paste("DEBUG SAVE: All input names available at save time:", paste(all_input_names, collapse=", ")))
            problematic_bio_age_inputs <- c("year_select_bio_age", "bioregion_select_bio_age", "zone_select_bio_age", "location_select_bio_age")
            
            
            for (input_name in all_input_names) {
              
              if (input_name %in% problematic_bio_age_inputs) {
                current_val_for_problematic_input <- isolate(input[[input_name]])
                # append_to_log(paste("DEBUG SAVE: Checking problematic input '", input_name, "'. Value: '", paste(current_val_for_problematic_input, collapse=","), "'. Is NULL? ", is.null(current_val_for_problematic_input)))
              }
              
              if (!grepl("btn$|button$", input_name, ignore.case = TRUE) &&
                  input_name != "upload_zip" &&
                  !startsWith(input_name, "selectized") && 
                  !startsWith(input_name, ".shinylive_") && 
                  !grepl("reactable_state_change", input_name) && 
                  !grepl("^shinydashboard", input_name) && 
                  !grepl("navstrip", input_name) && 
                  !(input_name %in% c("valid", "errors", "shinyjs_resettable_id")) 
              ) {
                app_state[[paste0("input_", input_name)]] <- isolate(input[[input_name]])
              }
            }
            append_to_log("Captured input values for state saving.")
            
            app_state$rv_values <- isolate(reactiveValuesToList(rv))
            append_to_log("Captured reactiveValues (rv) for state saving.")
            
            temp_app_state_file <- file.path(temp_dir, "app_state.RDS")
            tryCatch({
              
              append_to_log(paste("--- DEBUG SAVE: Final keys in app_state before saveRDS:", paste(sort(names(app_state)), collapse=", ")))
              problematic_keys_to_check <- paste0("input_", c("year_select_bio_age", "bioregion_select_bio_age", "zone_select_bio_age", "location_select_bio_age"))
              for (key_to_check in problematic_keys_to_check) {
                if (key_to_check %in% names(app_state)) {
                  append_to_log(paste0("  DEBUG SAVE: Key '", key_to_check, "' IS in app_state. Is its value NULL? ", is.null(app_state[[key_to_check]])))
                } else {
                  append_to_log(paste0("  DEBUG SAVE: Key '", key_to_check, "' IS MISSING from app_state before saveRDS!"))
                }
              }
              
              
              saveRDS(app_state, file = temp_app_state_file)
              if (file.exists(temp_app_state_file)) {
                temp_files <- c(temp_files, temp_app_state_file)
                append_to_log("Included app_state.RDS in files to ZIP.")
              } else {
                append_to_log("Warning: app_state.RDS was not created.")
              }
            }, error = function(e_save_rds) {
              append_to_log(paste("Error writing app_state.RDS:", e_save_rds$message))
            })
            # --- END: Save App State ---
            
            
            
            # FIS Length Data
            # FIS Length Data
            temp_file1 <- NULL
            fixedsite_data <- NULL
            if (input$use_fis_length && "Length" %in% input$data_include_before) {
              fixedsite_data <- base_data1() %>%
                filter(
                  (year %in% rv$retained_years & Location %in% rv$retained_locations & Discarded. == "No") |
                    (year %in% rv$released_years & Location %in% rv$released_locations & Discarded. == "Yes")
                )
            # temp_file1 <- NULL
            # fixedsite_data <- NULL
            # if (input$use_fis_length && "Length" %in% input$data_include_before) {
            #   fixedsite_data <- base_data1() %>%
            #     filter(
            #       (year %in% rv$retained_years & Discarded. == "No") |
            #         (year %in% rv$released_years & Discarded. == "Yes")
            #     )
              if (!is.null(fixedsite_data) && nrow(fixedsite_data) > 0) {
                temp_file1 <- file.path(temp_dir, "fixedsiteonly_data.csv")
                tryCatch({
                  write.csv(fixedsite_data, temp_file1, row.names = FALSE)
                  if (file.exists(temp_file1)) {
                    temp_files <- c(temp_files, temp_file1)
                    append_to_log("Included fixedsiteonly_data.csv")
                  } else {
                    append_to_log("Warning: fixedsiteonly_data.csv was not created")
                    temp_file1 <- NULL
                  }
                }, error = function(e) {
                  append_to_log(paste("Error writing fixedsiteonly_data.csv:", e$message))
                  temp_file1 <<- NULL
                })
              } else {
                append_to_log("No fixedsiteonly data available")
              }
            } else {
              append_to_log("Skipped fixedsiteonly data (use_fis_length is FALSE or Length not included)")
            }
            
            # Biological Length Data
            temp_file2 <- NULL
            merged_data <- length_plot_data2()
            if (!is.null(merged_data) && nrow(merged_data) > 0 && input$use_bio_length) {
              temp_file2 <- file.path(temp_dir, "merged_kim_pilb_data.csv")
              tryCatch({
                write.csv(merged_data, temp_file2, row.names = FALSE)
                if (file.exists(temp_file2)) {
                  temp_files <- c(temp_files, temp_file2)
                  append_to_log("Included merged_kim_pilb_data.csv")
                } else {
                  append_to_log("Warning: merged_kim_pilb_data.csv was not created")
                  temp_file2 <- NULL
                }
              }, error = function(e) {
                append_to_log(paste("Error writing merged_kim_pilb_data.csv:", e$message))
                temp_file2 <<- NULL
              })
            } else {
              append_to_log("Skipped merged_kim_pilb data (no data or use_bio_length is FALSE)")
            }
            
            # Catch Data
            temp_file3 <- NULL
            catch_df <- base_catch_data()
            if (!is.null(catch_df) && nrow(catch_df) > 0) {
              temp_file3 <- file.path(temp_dir, "catch_data.csv")
              tryCatch({
                write.csv(catch_df, temp_file3, row.names = FALSE)
                if (file.exists(temp_file3)) {
                  temp_files <- c(temp_files, temp_file3)
                  append_to_log("Included catch_data.csv")
                } else {
                  append_to_log("Warning: catch_data.csv was not created")
                  temp_file3 <- NULL
                }
              }, error = function(e) {
                append_to_log(paste("Error writing catch_data.csv:", e$message))
                temp_file3 <<- NULL
              })
            } else {
              append_to_log("Skipped catch data (no data available)")
            }
            
            # Effort Data
            temp_file4 <- NULL
            effort_df <- base_effort_data()
            if (!is.null(effort_df) && nrow(effort_df) > 0) {
              temp_file4 <- file.path(temp_dir, "effort_data.csv")
              tryCatch({
                write.csv(effort_df, temp_file4, row.names = FALSE)
                if (file.exists(temp_file4)) {
                  temp_files <- c(temp_files, temp_file4)
                  append_to_log("Included effort_data.csv")
                } else {
                  append_to_log("Warning: effort_data.csv was not created")
                  temp_file4 <- NULL
                }
              }, error = function(e) {
                append_to_log(paste("Error writing effort_data.csv:", e$message))
                temp_file4 <<- NULL
              })
            } else {
              append_to_log("Skipped effort data (no data available)")
            }
            
            # Biological Parameters
            temp_file5 <- NULL
            bio_result <- bio_tab()
            if (!is.null(bio_result$parameters_s) && nrow(bio_result$parameters_s) > 0) {
              temp_file5 <- file.path(temp_dir, "biological_parameters.csv")
              tryCatch({
                write.csv(bio_result$parameters_s, temp_file5, row.names = FALSE)
                if (file.exists(temp_file5)) {
                  temp_files <- c(temp_files, temp_file5)
                  append_to_log("Included biological_parameters.csv")
                } else {
                  append_to_log("Warning: biological_parameters.csv was not created")
                  temp_file5 <- NULL
                }
              }, error = function(e) {
                append_to_log(paste("Error writing biological_parameters.csv:", e$message))
                temp_file5 <<- NULL
              })
            } else {
              append_to_log("Skipped biological parameters (no data available)")
            }
            
            # Biological Age Data
            temp_file_bio_age <- NULL
            if (isTRUE(input$use_bio_age) && "Age" %in% input$data_include_before) {
              bio_age_data_to_save <- bio_age_plot_data()
              if (!is.null(bio_age_data_to_save) && nrow(bio_age_data_to_save) > 0) {
                
                if ("FL_mm" %in% colnames(bio_age_data_to_save)) {
                  bio_age_data_to_save <- bio_age_data_to_save %>%
                    rename(FL = FL_mm)
                }
                if ("TL_mm" %in% colnames(bio_age_data_to_save)) {
                  bio_age_data_to_save <- bio_age_data_to_save %>%
                    rename(TL = TL_mm)
                }
                
                if (!"FL" %in% colnames(bio_age_data_to_save)) {
                  bio_age_data_to_save$FL <- NA_real_
                }
                if (!"TL" %in% colnames(bio_age_data_to_save)) {
                  bio_age_data_to_save$TL <- NA_real_
                }
                if (!"Sex" %in% colnames(bio_age_data_to_save)) {
                  bio_age_data_to_save$Sex <- NA_character_
                }
                
                temp_file_bio_age <- file.path(temp_dir, "bio_age_data.csv")
                tryCatch({
                  write.csv(bio_age_data_to_save, temp_file_bio_age, row.names = FALSE)
                  if (file.exists(temp_file_bio_age)) {
                    temp_files <- c(temp_files, temp_file_bio_age)
                    append_to_log("Included bio_age_data.csv")
                  } else {
                    append_to_log("Warning: bio_age_data.csv was not created")
                    temp_file_bio_age <- NULL
                  }
                }, error = function(e) {
                  append_to_log(paste("Error writing bio_age_data.csv:", e$message))
                  temp_file_bio_age <<- NULL
                })
              } else {
                append_to_log("Skipped biological age data (no data available)")
              }
            } else {
              append_to_log("Skipped biological age data (use_bio_age is FALSE or Age not included)")
            }
            
            # FIS Age Data
            temp_file_fis_age <- NULL
            if (isTRUE(input$use_fis_age) && "Age" %in% input$data_include_before) {
              fis_age_data_to_save <- age_plot_data()
              if (!is.null(fis_age_data_to_save) && nrow(fis_age_data_to_save) > 0) {
                temp_file_fis_age <- file.path(temp_dir, "fis_age_data.csv")
                tryCatch({
                  write.csv(fis_age_data_to_save, temp_file_fis_age, row.names = FALSE)
                  if (file.exists(temp_file_fis_age)) {
                    temp_files <- c(temp_files, temp_file_fis_age)
                    append_to_log("Included fis_age_data.csv")
                  } else {
                    append_to_log("Warning: fis_age_data.csv was not created")
                    temp_file_fis_age <- NULL
                  }
                }, error = function(e) {
                  append_to_log(paste("Error writing fis_age_data.csv:", e$message))
                  temp_file_fis_age <<- NULL
                })
              } else {
                append_to_log("Skipped FIS age data (no data available)")
              }
            } else {
              append_to_log("Skipped FIS age data (use_fis_age is FALSE or Age not included)")
            }
            
            # Fishery Parameters
            temp_file7 <- NULL
            fishery_data <- fishery_params()
            if (!is.null(fishery_data) && nrow(fishery_data) > 0) {
              temp_file7 <- file.path(temp_dir, "fishery_parameters.csv")
              tryCatch({
                write.csv(fishery_data, temp_file7, row.names = FALSE)
                if (file.exists(temp_file7)) {
                  temp_files <- c(temp_files, temp_file7)
                  append_to_log("Included fishery_parameters.csv")
                } else {
                  append_to_log("Warning: fishery_parameters.csv was not created")
                  temp_file7 <- NULL
                }
              }, error = function(e) {
                append_to_log(paste("Error writing fishery_parameters.csv:", e$message))
                temp_file7 <<- NULL
              })
            } else {
              append_to_log("Skipped fishery parameters (no data available)")
            }
            
            # Helper function for formatting summary lines
            format_selection <- function(label, value) {
              val_str <- if (is.null(value) || length(value) == 0) {
                "None selected"
              } else {
                paste(value, collapse = ", ")
              }
              paste0("  ", label, ": ", val_str)
            }
            
            # Summary File
            temp_summary_file <- file.path(temp_dir, "selections_summary.txt")
            
            
            summary_text <- c(
              "Shiny-FishAssess Selections Summary",
              paste("Generated on:", Sys.Date()),
              "",
              "--- Main Selections ---",
              format_selection("Selected Species (Main)", input$species_select),
              format_selection("Data Included (top checkboxes)", input$data_include_before),
              format_selection("Conditional age-at-length", input$conditional_age_select),
              format_selection("Data Included (bottom checkboxes)", input$data_include_after),
              format_selection("Run with -nohess", input$nohess_option),
              "",
              
              "--- Catch Tab ---",
              format_selection("Catch Species", input$catch_species_select),
              format_selection("Catch Sectors", input$catch_sector_select),
              "",
              
              "--- Indices Tab ---",
              format_selection("Indices Species", input$effort_species_select),
              format_selection("Indices Fleets", input$effort_fleet_select),
              "",
              
              "--- Length Tab ---",
              format_selection("Use FIS length data", input$use_fis_length),
              format_selection("Use discarded length data from FIS", input$use_released_fis_data),
              format_selection("Use length data from Biological databases", input$use_bio_length),
              format_selection("Length Metric", input$length_metric),
              format_selection("Length Class Interval (mm)", input$length_class_input),
              format_selection("Minimum Length (mm)", input$min_length_input),
              format_selection("Colour Plots By", input$length_color_by),
              format_selection("FIS Retained Years", input$year_select_retained),
              format_selection("FIS Retained Locations", input$location_select_retained),
              format_selection("FIS Released Years", input$year_select_released),
              format_selection("FIS Released Locations", input$location_select_released),
              format_selection("Biological Sample Sectors", input$sector_select_bio),
              format_selection("Biological Sample Years", input$year_select2),
              format_selection("Biological Sample BioRegions", input$bioregion_select_bio),
              format_selection("Biological Sample Zones", input$zone_select_bio),
              format_selection("Biological Sample Locations", input$location_select_bio),
              "",
              
              "--- Age Tab ---",
              format_selection("Use age data from FIS", input$use_fis_age),
              format_selection("Use age data from Biological databases", input$use_bio_age),
              format_selection("Colour Plots By", input$age_color_by),
              format_selection("FIS Age Years", input$year_select_age),
              format_selection("FIS Age Locations", input$location_select_age),
              format_selection("Biological Age Years", input$year_select_bio_age),
              format_selection("Biological Age BioRegions", input$bioregion_select_bio_age),
              format_selection("Biological Age Zones", input$zone_select_bio_age),
              format_selection("Biological Age Locations", input$location_select_bio_age),
              "",
              
              "--- Biological Parameters Tab ---",
              format_selection("Selected Biological Unit", input$bio_species_select),
              "",
              
              "--- SS3 Model Options Tab ---",
              paste("  Use Initial Catch:", input$use_initial_catch),
              if (isTRUE(input$use_initial_catch)) c(
                paste("    Year 0 Catch:", input$year0catch),
                paste("    Year 0 Catch SE:", input$year0catchse),
                paste("    Initial F:", input$initf),
                paste("    Year 0 Fleet:", input$year0fleet)
              ),
              paste("  Use 1-sex model:", input$use_1_sex_model),
              paste("  Specify Custom Natural Mortality (M):", input$use_custom_M),
              if (isTRUE(input$use_custom_M)) paste("    Natural Mortality (M):", input$custom_M),
              paste("  Specify Custom SigmaR:", input$use_custom_sigma_r),
              if (isTRUE(input$use_custom_sigma_r)) paste("    SigmaR:", input$custom_sigma_r),
              paste("  Specify Age Post-Settlement:", input$use_age_post_settlement),
              if (isTRUE(input$use_age_post_settlement)) paste("    Age Post-Settlement:", input$age_post_settlement),
              paste("  Specify CV Growth Pattern:", input$use_cv_growth_pattern),
              if (isTRUE(input$use_cv_growth_pattern)) paste("    CV Growth Pattern:", input$cv_growth_pattern),
              paste("  Specify Custom Steepness (h):", input$use_custom_h),
              if (isTRUE(input$use_custom_h)) paste("    Steepness (h):", input$custom_h),
              paste("  Specify Custom Initial Recruitment log(R0):", input$use_custom_R0),
              if (isTRUE(input$use_custom_R0)) paste("    Initial Recruitment log(R0):", input$custom_R0),
              paste("  Specify recruitment deviation years:", input$use_recdevs_range),
              if (isTRUE(input$use_recdevs_range)) c(
                paste("    First year of main recr_devs:", input$first_year_recr_devs),
                paste("    Last year of main recr_devs:", input$last_year_recr_devs)
              ),
              paste("  Estimate Growth Parameters:", input$estimate_growth_params),
              paste("  Use Time-varying parameters:", input$use_time_varying_params),
              if (isTRUE(input$use_time_varying_params)) c(
                paste("    Block years:", gsub("\n", "; ", input$time_varying_params_text)),
                paste("    Growth-all params:", input$use_time_varying_growth_all),
                paste("    Growth-L_at_Amin:", input$use_time_varying_growth_L_at_Amin),
                paste("    Growth-L_at_Amax:", input$use_time_varying_growth_L_at_Amax),
                paste("    Growth-vbK:", input$use_time_varying_growth_vbK),
                paste("    Selectivity:", input$use_time_varying_selectivity),
                paste("    Retention:", input$use_time_varying_retention)
              ),
              paste("  Use Custom Bias Adjustments:", input$use_custom_bias_adj),
              if (isTRUE(input$use_custom_bias_adj)) c(
                paste("    Last early year no bias adjustment:", input$last_early_yr_nobias_adj),
                paste("    First year full bias adjustment:", input$first_yr_fullbias_adj),
                paste("    Last year full bias adjustment:", input$last_yr_fullbias_adj),
                paste("    First recent year no bias adjustment:", input$first_recent_yr_nobias_adj),
                paste("    Use max bias adjustment in MPD:", input$Use_max_bias_adj_in_MPD)
              ),
              paste("  Estimate Dirichlet (ln(DM_theta)):", input$estimate_Dirichlet),
              if (isTRUE(input$estimate_Dirichlet)) paste("    Dirichlet Parameter Lines:", gsub("\n", "; ", input$dirichlet_textbox_value)),
              paste("  Use Q_extraSD:", input$use_Q_extraSD),
              if (isTRUE(input$use_Q_extraSD)) paste("    Q_extraSD:", input$Q_extraSD),
              paste("  Francis Weighting:", input$use_francis_weighting),
              if (isTRUE(input$use_francis_weighting)) paste("    Francis Weighting Lines:", gsub("\n", "; ", input$francis_weighting_value))
            )
            
            
            
            
            tryCatch({
              writeLines(summary_text, temp_summary_file)
              if (file.exists(temp_summary_file)) {
                temp_files <- c(temp_files, temp_summary_file)
                append_to_log("Included selections_summary.txt")
              } else {
                append_to_log("Warning: selections_summary.txt was not created")
              }
            }, error = function(e) {
              append_to_log(paste("Error writing selections_summary.txt:", e$message))
            })
            
            
            
            # Prepare SS_input.R execution
            species_name <- input$species_select
            outputdir <- paste0("SS3_input_files/", species_name, "/")
            
            if (file.exists("SS_input.R")) {
              file_list <- list(
                fixedsiteonly = temp_file1,
                merged_kim_pilb = temp_file2,
                catch_data = temp_file3,
                effort_data = temp_file4,
                bio_params = temp_file5,
                fis_age_data = temp_file_fis_age,
                bio_age_data = temp_file_bio_age,
                fishery_params = temp_file7,
                species_name = species_name,
                length_class = input$length_class_input,
                SS_use_cond_age_length = isTRUE(input$conditional_age_select),
                SS_use_fis_age = isTRUE("Age" %in% input$data_include_before) && isTRUE(input$use_fis_age),
                SS_use_bio_age = isTRUE("Age" %in% input$data_include_before) && isTRUE(input$use_bio_age),
                SS_use_any_age = isTRUE("Age" %in% input$data_include_before) && (isTRUE(input$use_fis_age) || isTRUE(input$use_bio_age)),
                SS_use_length = isTRUE("Length" %in% input$data_include_before) && isTRUE(input$use_fis_length),
                SS_use_bio_length = isTRUE(input$use_bio_length)
              )
              
              local({
                SS_input_env <- new.env()
                SS_input_env$file_list <- file_list
                source("SS_input.R", local = SS_input_env)
              })
              
              ss_files <- c(
                file.path(outputdir, "controlfile.ctl"),
                file.path(outputdir, "datafile.dat"),
                file.path(outputdir, "starter.ss"),
                file.path(outputdir, "forecast.ss")
              )
              existing_ss_files <- ss_files[file.exists(ss_files)]
              if (length(existing_ss_files) > 0) {
                temp_files <- c(temp_files, existing_ss_files)
                append_to_log("Included SS3 input files")
              } else {
                append_to_log("Warning: No SS3 input files were generated by SS_input.R")
              }
            } else {
              append_to_log("Error: SS_input.R not found")
            }
            
            if (length(temp_files) == 0) {
              stop("No files were created to include in the ZIP.")
            }
            
            valid_temp_files <- temp_files[file.exists(temp_files)]
            if (length(valid_temp_files) == 0) {
              stop("No valid files available to include in the ZIP.")
            }
            if (length(valid_temp_files) < length(temp_files)) {
              missing_files <- setdiff(temp_files, valid_temp_files)
              append_to_log(paste("Warning: Some files could not be found for zipping:", paste(missing_files, collapse = ", ")))
            }
            
            zips_dir <- file.path(getwd(), "zips")
            dir.create(zips_dir, showWarnings = FALSE)
            selected_data <- data_include()
            data_codes <- c("Catch" = "C", "Effort" = "I", "Length" = "L", "Age" = "A")
            data_string_base <- paste(na.omit(data_codes[selected_data]), collapse = "")
            conditional_age_checked <- isTRUE(input$conditional_age_select)
            data_string <- if (conditional_age_checked) {
              paste0(data_string_base, "CndA")
            } else {
              data_string_base
            }
            if (data_string == "") data_string <- "None"
            species <- gsub(" ", "_", input$species_select)
            date_str <- format(Sys.Date(), "%Y-%m-%d")
            zip_filename <- paste0(species, "_SS-", data_string, "_", date_str, ".zip")
            persistent_zip <- file.path(zips_dir, zip_filename)
            
            tryCatch({
              zip::zipr(persistent_zip, valid_temp_files)
              file.copy(persistent_zip, file, overwrite = TRUE)
              downloaded_zip_path(persistent_zip)
              append_to_log(paste("ZIP file created and stored in zips/ as:", zip_filename))
            }, error = function(e) {
              append_to_log(paste("Error creating ZIP file:", e$message))
              stop(paste("Failed to create ZIP file:", e$message))
            })
            
            unlink(valid_temp_files[!valid_temp_files %in% ss_files])
            
            download_success(TRUE)
            append_to_log("Download completed successfully")
          }, error = function(e) {
            download_success(FALSE)
            append_to_log(paste("Download failed:", e$message))
          })
        },
        contentType = "application/zip"
      )
      
      
      
      # Helper function to extract ZIP and validate SS3 files
      extract_and_validate_zip <- function(zip_path, temp_dir, species_name) {
        append_to_log("Extracting ZIP file...")
        unzip_dir <- file.path(temp_dir, "zip_extract")
        dir.create(unzip_dir, showWarnings = FALSE)
        tryCatch({
          zip::unzip(zip_path, exdir = unzip_dir)
          append_to_log("ZIP file extracted successfully")
        }, error = function(e) {
          append_to_log(paste("Error extracting ZIP file:", e$message))
          return(NULL)
        })
        
        ss_files <- c("controlfile.ctl", "datafile.dat", "starter.ss", "forecast.ss")
        
        input_dir <- file.path(unzip_dir, paste0("SS3_input_files/", species_name))
        if (!dir.exists(input_dir)) {
          input_dir <- unzip_dir
        }
        found_files <- file.path(input_dir, ss_files)
        
        if(!all(file.exists(found_files))) {
          alt_found_files <- file.path(unzip_dir, ss_files)
          if(all(file.exists(alt_found_files))) {
            input_dir <- unzip_dir
            found_files <- alt_found_files
            append_to_log(paste("SS3 files found directly in", unzip_dir))
          }
        }
        
        missing_files <- ss_files[!file.exists(found_files)]
        
        if (length(missing_files) > 0) {
          append_to_log(paste("Error: Missing required SS3 input files:", paste(missing_files, collapse = ", ")))
          append_to_log(paste("Searched in:", input_dir, "and", unzip_dir))
          append_to_log(paste("Files found in input_dir:", paste(list.files(input_dir), collapse=", ")))
          append_to_log(paste("Files found in unzip_dir:", paste(list.files(unzip_dir), collapse=", ")))
          return(NULL)
        }
        
        list(input_dir = input_dir, ss_files = found_files)
      }
      
      
      # --- Conditional Run SS3 Button UI ---
      output$conditional_run_ss3_button_ui <- renderUI({
        if (!is.null(input$upload_zip) && nrow(input$upload_zip) > 0) {
          if (nrow(input$upload_zip) == 1) {
            tagList(
            actionButton("run_uploaded_ss3_btn", "Run SS3 for Single ZIP"),
            checkboxInput("nohess_option", "–nohess", value = TRUE)
            )
          } else {
            # MODIFIED: Added a checkbox for the batch run
            tagList(
              actionButton("run_batch_ss3_btn", "Run SS3 for All ZIPs"),
              checkboxInput("nohess_option", "–nohess", value = TRUE),
              checkboxInput("run_comparison_after_batch", "Run model comparison after batch completes", value = TRUE)
            )
          }
        } else {
          NULL
        }
      })
      # --- End Conditional Run SS3 Button UI ---
      
      
      
      # --- START: Observer for Loading State from Uploaded ZIP ---
      observeEvent(input$upload_zip, {
        req(input$upload_zip)
        
        # --- START: NEW CONDITIONAL CHECK ---
        if (isFALSE(SENSITIVE_DATA_LOADED)) {
          append_to_log("Skipping application state restoration: SENSITIVE_DATA_LOADED is FALSE.")
          showNotification(
            "Application state restoration skipped. Data loading was disabled at startup.",
            type = "warning",
            duration = 10
          )
          return() # Exit the observer early
        }
        # --- END: NEW CONDITIONAL CHECK ---
        
        if (nrow(input$upload_zip) == 1) {
          zip_file_to_restore_path <- input$upload_zip$datapath[1]
          zip_filename_for_log <- input$upload_zip$name[1]
          append_to_log(paste("Single ZIP file uploaded:", zip_filename_for_log, "- attempting state restoration with TAB ITERATION."))
          
          restore_temp_dir <- tempfile(pattern = "restore_zip_")
          dir.create(restore_temp_dir)
          

          
          # [Existing TryCatch block starts here, including unzipping, readRDS, and all update calls]
          tryCatch({
            unzip(zip_file_to_restore_path, exdir = restore_temp_dir)
            append_to_log(paste("Successfully unzipped", zip_filename_for_log, "to", restore_temp_dir))
            
            app_state_file_path <- file.path(restore_temp_dir, "app_state.RDS")
            append_to_log(paste("DEBUG: app_state_file_path has been defined as:", app_state_file_path))
            
            if (file.exists(app_state_file_path)) {
              append_to_log(paste("Found app_state.RDS in", zip_filename_for_log))
              loaded_state <- readRDS(app_state_file_path)
              
              # STEP 1: Restore rv reactiveValues
              if (!is.null(loaded_state$rv_values)) {
                current_rv_names <- names(isolate(reactiveValuesToList(rv)))
                for (name_val in names(loaded_state$rv_values)) {
                  if (name_val %in% current_rv_names) rv[[name_val]] <- loaded_state$rv_values[[name_val]]
                }
                append_to_log("Restored rv reactiveValues from app_state.RDS.")
              } else {
                append_to_log("No rv_values found in app_state.RDS.")
              }
              
              restored_inputs_count <- 0 
              safe_update_input <- function(update_function_name, input_id, choices_arg_name = "value", choices_val = NULL, extra_args = list()) {
                input_state_name <- paste0("input_", input_id)
                append_to_log(paste0("Attempting restore for: '", input_id, "' (state key: '", input_state_name, "')"))
                if (input_state_name %in% names(loaded_state)) {
                  val_to_set <- loaded_state[[input_state_name]]
                  append_to_log(paste0("  Found '", input_state_name, "'. Value to set: '", paste(val_to_set, collapse=","), "'"))
                  args_list <- list(session, input_id)
                  if (update_function_name == "updateSelectizeInput" && choices_arg_name == "selected" && is.null(choices_val) && "choices" %in% names(formals(updateSelectizeInput))) {
                    args_list[["selected"]] <- val_to_set
                  } else {
                    args_list[[choices_arg_name]] <- val_to_set
                    if (!is.null(choices_val)) args_list[["choices"]] <- choices_val
                  }
                  if (length(extra_args) > 0) args_list <- c(args_list, extra_args)
                  tryCatch({
                    do.call(update_function_name, args_list)
                    append_to_log(paste0("  SUCCESS: Restored '", input_id, "'"))
                    restored_inputs_count <<- restored_inputs_count + 1
                  }, error = function(e_update) {
                    append_to_log(paste0("  ERROR restoring '", input_id, "' with value '", paste(val_to_set, collapse=","), "': ", e_update$message))
                  })
                } else {
                  append_to_log(paste0("  State for '", input_id, "' (", input_state_name, ") not found in loaded_state."))
                }
              }
              
              # STEP 2: Restore ALL PRIMARY/STATIC inputs first
              append_to_log("--- Restoring ALL PRIMARY/STATIC inputs from all tabs ---")
              all_spp_sorted <- sort(all_species) 
              safe_update_input("updatePickerInput", "species_select", choices_arg_name = "selected", choices_val = c(setNames(all_spp_sorted, all_spp_sorted)))
              
              # Sidebar
              safe_update_input("updateCheckboxGroupInput", "data_include_before", choices_arg_name = "selected")
              safe_update_input("updateCheckboxInput", "conditional_age_select", choices_arg_name = "value")
              safe_update_input("updateCheckboxGroupInput", "data_include_after", choices_arg_name = "selected")
              safe_update_input("updateCheckboxInput", "nohess_option", choices_arg_name = "value")
              
              # Length Tab
              safe_update_input("updateCheckboxInput", "use_fis_length", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_released_fis_data", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_bio_length", choices_arg_name = "value")
              safe_update_input("updateRadioButtons", "length_metric", choices_arg_name = "selected")
              safe_update_input("updateNumericInput", "length_class_input", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "min_length_input", choices_arg_name = "value")
              safe_update_input("updateRadioButtons", "length_color_by", choices_arg_name = "selected")
              
              # Age Tab
              safe_update_input("updateCheckboxInput", "use_fis_age", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_bio_age", choices_arg_name = "value")
              safe_update_input("updateRadioButtons", "age_color_by", choices_arg_name = "selected")
              
              # SS3 Model Options Tab
              safe_update_input("updateCheckboxInput", "use_initial_catch", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "year0catch", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "year0catchse", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "initf", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_1_sex_model", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_custom_M", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "custom_M", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_custom_sigma_r", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "custom_sigma_r", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_age_post_settlement", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "age_post_settlement", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_cv_growth_pattern", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "cv_growth_pattern", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_custom_h", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "custom_h", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_custom_R0", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "custom_R0", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_recdevs_range", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "first_year_recr_devs", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "last_year_recr_devs", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "estimate_growth_params", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_time_varying_params", choices_arg_name = "value")
              safe_update_input("updateTextAreaInput", "time_varying_params_text", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_time_varying_growth_all", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_time_varying_growth_L_at_Amin", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_time_varying_growth_L_at_Amax", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_time_varying_growth_vbK", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_time_varying_selectivity", choices_arg_name = "value")
              safe_update_input("updateCheckboxInput", "use_time_varying_retention", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_custom_bias_adj", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "last_early_yr_nobias_adj", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "first_yr_fullbias_adj", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "last_yr_fullbias_adj", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "first_recent_yr_nobias_adj", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "Use_max_bias_adj_in_MPD", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "estimate_Dirichlet", choices_arg_name = "value")
              safe_update_input("updateTextAreaInput", "dirichlet_textbox_value", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_Q_extraSD", choices_arg_name = "value")
              safe_update_input("updateNumericInput", "Q_extraSD", choices_arg_name = "value")
              
              safe_update_input("updateCheckboxInput", "use_francis_weighting", choices_arg_name = "value")
              safe_update_input("updateTextAreaInput", "francis_weighting_value", choices_arg_name = "value")
              
              # STEP 3: Trigger FIRST Refresh (to help populate choices for dynamic pickers on the *first* tab we switch to)
              append_to_log("--- Triggering FIRST refresh (after primary inputs restored) ---")
              refresh_trigger(refresh_trigger() + 1)
              
              # STEP 4: Iterate through tabs and restore their specific dynamic inputs
              tab_values <- c("catch_tab", "indices_tab", "length_tab", "age_tab", "bioparams_tab", "ss3options_tab")
              tab_update_delay_ms <- 1200
              
              current_tab_index <- 1
              
              process_next_tab <- function() {
                if (current_tab_index > length(tab_values)) {
                  append_to_log(paste("--- Finished all tab restorations. Final count of explicitly updated inputs:", restored_inputs_count, "---"))
                  refresh_trigger(refresh_trigger() + 1) # Final overall refresh
                  expected_restorations <- sum(startsWith(names(loaded_state), "input_")) 
                  if (restored_inputs_count >= expected_restorations * 0.75 && restored_inputs_count > 10) {
                    showNotification("Application state substantially restored.", type = "message", duration = 7)
                  } else {
                    showNotification(paste("State partially restored (", restored_inputs_count, "/", expected_restorations, " inputs attempted). Review selections.", sep=""), type = "warning", duration = 10)
                  }
                  return()
                }
                
                tab_to_update <- tab_values[current_tab_index]
                append_to_log(paste0("--- Switching to and updating Tab: '", tab_to_update, "' ---"))
                updateTabsetPanel(session, "primaryTabs", selected = tab_to_update)
                
                shinyjs::delay(ms = tab_update_delay_ms, {
                  append_to_log(paste0("--- Restoring dynamic inputs for Tab: '", tab_to_update, "' AFTER DELAY ---"))
                  
                  if (tab_to_update == "catch_tab") {
                    append_to_log("Restoring Catch Tab Pickers...")
                    safe_update_input("updatePickerInput", "catch_species_select", choices_arg_name = "selected", choices_val = isolate(catch_species_choices()))
                    safe_update_input("updateCheckboxGroupInput", "catch_sector_select", choices_arg_name = "selected")
                  } else if (tab_to_update == "indices_tab") {
                    append_to_log("Restoring Indices Tab Pickers...")
                    safe_update_input("updatePickerInput", "effort_species_select", choices_arg_name = "selected", choices_val = isolate(effort_species_choices()))
                    safe_update_input("updateCheckboxGroupInput", "effort_fleet_select", choices_arg_name = "selected")
                  } else if (tab_to_update == "length_tab") {
                    append_to_log("Restoring Length Tab Pickers...")
                    safe_update_input("updatePickerInput", "year_select_retained", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "location_select_retained", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "year_select_released", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "location_select_released", choices_arg_name = "selected")
                    # Biological Length Data Pickers
                    safe_update_input("updatePickerInput", "sector_select_bio", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "year_select2", choices_arg_name = "selected") 
                    safe_update_input("updatePickerInput", "bioregion_select_bio", choices_arg_name = "selected") 
                    safe_update_input("updatePickerInput", "zone_select_bio", choices_arg_name = "selected") 
                    safe_update_input("updatePickerInput", "location_select_bio", choices_arg_name = "selected") 
                  } else if (tab_to_update == "age_tab") {
                    append_to_log("Restoring Age Tab Pickers...")
                    safe_update_input("updatePickerInput", "year_select_age", choices_arg_name = "selected")     
                    safe_update_input("updatePickerInput", "location_select_age", choices_arg_name = "selected") 
                    safe_update_input("updatePickerInput", "year_select_bio_age", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "bioregion_select_bio_age", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "zone_select_bio_age", choices_arg_name = "selected")
                    safe_update_input("updatePickerInput", "location_select_bio_age", choices_arg_name = "selected")
                  } else if (tab_to_update == "bioparams_tab") {
                    append_to_log("Restoring Biological Parameters Tab Picker...")
                    safe_update_input("updatePickerInput", "bio_species_select", choices_arg_name = "selected", choices_val = isolate(bio_species_choices()))
                  } else if (tab_to_update == "ss3options_tab") {
                    append_to_log("Restoring SS3 Model Options year0fleet...")
                    safe_update_input("updateSelectizeInput", "year0fleet", choices_arg_name = "selected", choices_val = isolate(reactive_catch_sector_choices()))
                  }
                  
                  append_to_log(paste0("--- Triggering refresh after Tab: '", tab_to_update, "' ---"))
                  refresh_trigger(refresh_trigger() + 1)
                  current_tab_index <<- current_tab_index + 1
                  process_next_tab()
                })
              }
              
              process_next_tab()
              
            } else {
              append_to_log(paste("app_state.RDS not found in uploaded ZIP:", zip_filename_for_log))
              showNotification("Uploaded ZIP does not contain a saved application state (app_state.RDS).", type = "warning", duration = 10)
            }
          }, error = function(e_restore_main) { 
            append_to_log(paste("Major error during state restoration from ZIP (e.g., unzip or readRDS failed):", e_restore_main$message))
            showNotification(paste("Error processing ZIP for state restoration:", e_restore_main$message), type = "error", duration = 10)
          })
          
          # [Existing on.exit cleanup block]
          on.exit({
            unlink(restore_temp_dir, recursive = TRUE, force = TRUE)
            append_to_log(paste("Cleaned up temporary directory for restoration:", restore_temp_dir))
          }, add = TRUE)
          
        } else if (nrow(input$upload_zip) > 1) {
          append_to_log(paste(nrow(input$upload_zip), "ZIP files uploaded. State restoration from ZIP is only performed for single ZIP uploads."))
        }
      })
      # --- END: Observer for Loading State from Uploaded ZIP ---
      

      # Handler for batch running SS3 with multiple uploaded ZIP files (PARALLEL VERSION)
      observeEvent(input$run_batch_ss3_btn, {
        req(input$upload_zip)
        append_to_log("--- Starting Parallel SS3 Batch Processing ---")
        
        zip_files <- input$upload_zip
        if (nrow(zip_files) < 2) {
          append_to_log("Error: Batch processing requires at least two ZIP files.")
          return()
        }
        
        n_cores <- parallel::detectCores() - 1
        if (n_cores < 1) n_cores <- 1
        append_to_log(paste("Setting up parallel processing with", n_cores, "cores."))
        
        my_cluster <- parallel::makeCluster(n_cores)
        doParallel::registerDoParallel(my_cluster)
        
        on.exit({
          append_to_log("Stopping parallel cluster.")
          parallel::stopCluster(my_cluster)
        }, add = TRUE)
        
        nohess_val <- isolate(input$nohess_option)
        
        results <- foreach(
          i = seq_len(nrow(zip_files)),
          # 1. LOAD NECESSARY PACKAGES (ggplot2 is critical for DPIRD plots)
          .packages = c('r4ss', 'zip', 'stringr', 'ggplot2', 'dplyr'),
          # 2. EXPORT CUSTOM FUNCTION so workers can see it
          .export = c('generate_DPIRD_plots'),
          .errorhandling = 'pass'
        ) %dopar% {
          
          log_output <- c()
          output_dir <- NULL
          success_flag <- FALSE
          main_html_path <- NULL
          
          root_dir <- getwd()
          temp_dir_worker <- tempfile(pattern = "batch_worker_")
          dir.create(temp_dir_worker)
          
          tryCatch({
            current_zip_path <- zip_files$datapath[i]
            current_zip_filename <- zip_files$name[i]
            zip_basename <- tools::file_path_sans_ext(basename(current_zip_filename))
            species_name <- sub("_SS-.*", "", zip_basename)
            species_name <- gsub("_", " ", species_name)
            
            log_output <- c(log_output, paste("Worker", i, "processing:", current_zip_filename))
            
            output_dir <- file.path(root_dir, "output", zip_basename)
            
            # Define worker-specific zip extraction
            extract_and_validate_zip_worker <- function(zip_path, temp_dir, species) {
              unzip_dir <- file.path(temp_dir, "zip_extract")
              dir.create(unzip_dir, showWarnings = FALSE)
              zip::unzip(zip_path, exdir = unzip_dir)
              
              ss_files_to_find <- c("controlfile.ctl", "datafile.dat", "starter.ss", "forecast.ss")
              input_dir_check <- file.path(unzip_dir, paste0("SS3_input_files/", species))
              if (!dir.exists(input_dir_check)) input_dir_check <- unzip_dir 
              
              found_files_paths <- file.path(input_dir_check, ss_files_to_find)
              if (!all(file.exists(found_files_paths))) {
                stop(paste("Missing required SS3 files. Searched in:", input_dir_check))
              }
              return(input_dir_check)
            }
            
            input_dir <- extract_and_validate_zip_worker(current_zip_path, temp_dir_worker, species_name)
            log_output <- c(log_output, paste("Worker", i, "validated ZIP contents."))
            
            if (dir.exists(output_dir)) { unlink(output_dir, recursive = TRUE, force = TRUE) }
            dir.create(output_dir, recursive = TRUE)
            log_output <- c(log_output, paste0("Worker ", i, " output dir: ", output_dir))
            
            r4ss::run(
              dir = input_dir,
              exe = file.path(root_dir, "Stock_Synthesis_latest/ss"),
              extras = ifelse(nohess_val, "-nohess", ""),
              skipfinished = FALSE,
              show_in_console = FALSE
            )
            log_output <- c(log_output, paste("Worker", i, "finished r4ss::run."))
            
            replist <- r4ss::SS_output(dir = input_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
            r4ss::tune_comps(replist = replist, dir = input_dir, option = "Francis",plot = FALSE)
            
            all_ss3_files_to_copy <- list.files(input_dir, full.names = TRUE, recursive = TRUE, all.files = TRUE)
            if (length(all_ss3_files_to_copy) > 0) {
              file.copy(from = all_ss3_files_to_copy, to = output_dir, overwrite = TRUE, recursive = TRUE)
              log_output <- c(log_output, paste("Worker", i, "copied all model files to output directory."))
            }
            
            r4ss::SS_plots(replist, dir = output_dir, png = TRUE, html = TRUE, printfolder = "r4ss")
            
            # --- DPIRD PLOTS with Fixed Logging ---
            tryCatch({
              generate_DPIRD_plots(replist, output_dir)
              log_output <- c(log_output, paste("Worker", i, "generated DPIRD plots."))
            }, error = function(e_dpird) {
              # CRITICAL FIX: Use <<- to ensure the log update persists outside this error block
              log_output <<- c(log_output, paste("Worker", i, "DPIRD Plot Error:", e_dpird$message))
            })
            # ---------------------------------------
            
            # Retitle HTML files
            r4ss_plots_dir <- file.path(output_dir, "r4ss")
            if (dir.exists(r4ss_plots_dir)) {
              html_files_to_retitle <- list.files(path = r4ss_plots_dir, pattern = "\\.html$", full.names = TRUE)
              log_output <- c(log_output, paste("Worker", i, "found", length(html_files_to_retitle), "HTML file(s) to retitle."))
              for (html_file in html_files_to_retitle) {
                tryCatch({
                  content <- readLines(html_file, warn = FALSE)
                  content <- stringr::str_replace_all(content, "<title>SS Output</title>", paste0("<title>", zip_basename, "</title>"))
                  writeLines(content, html_file)
                }, error = function(e_retitle) {
                  log_output <<- c(log_output, paste("Worker", i, "failed to retitle", basename(html_file), ":", e_retitle$message))
                })
              }
            }
            
            main_html_path <- file.path(output_dir, "r4ss", "00_Summary.html")
            if(!file.exists(main_html_path)) main_html_path <- NULL
            
            log_output <- c(log_output, paste("Worker", i, "completed successfully."))
            success_flag <- TRUE
            
          }, error = function(e) {
            log_output <- c(log_output, paste("Worker", i, "ERROR:", e$message))
            success_flag <- FALSE
          })
          
          unlink(temp_dir_worker, recursive = TRUE)
          return(list(log = log_output, html = main_html_path, output_dir = output_dir, success = success_flag))
        }
        
        append_to_log("--- Parallel processing finished. Consolidating results. ---")
        
        html_files_to_open <- c()
        for (res in results) {
          if (inherits(res, "error")) {
            append_to_log(paste("A worker failed with error:", as.character(res)))
          } else {
            append_to_log(res$log)
            if (!is.null(res$html) && file.exists(res$html)) {
              html_files_to_open <- c(html_files_to_open, res$html)
            }
          }
        }
        
        if (length(html_files_to_open) > 0) {
          append_to_log("Opening summary HTML files for successful runs...")
          for(html_path in html_files_to_open) browseURL(html_path)
        }
        
        if (isTRUE(isolate(input$run_comparison_after_batch))) {
          append_to_log("--- Checking for automatic model comparison ---")
          successful_runs <- Filter(function(res) !inherits(res, "error") && isTRUE(res$success) && !is.null(res$output_dir), results)
          
          if (length(successful_runs) >= 2) {
            successful_output_dirs <- sapply(successful_runs, function(res) res$output_dir)
            append_to_log(paste("Found", length(successful_output_dirs), "successful model runs to compare."))
            Sys.sleep(2) 
            start_model_comparison(folder_paths = successful_output_dirs, comparison_name_prefix = "Batch_Comparison")
          } else {
            append_to_log("Skipping automatic comparison: Fewer than 2 successful model runs completed.")
          }
        }
      })
      
      # Handler for running SS3 with uploaded ZIP
      observeEvent(input$run_uploaded_ss3_btn, {
        req(input$upload_zip)
        append_to_log("Running SS3 model with selected ZIP file...")
        
        zip_path <- input$upload_zip$datapath[1]
        zip_filename <- input$upload_zip$name[1]
        zip_basename <- tools::file_path_sans_ext(basename(zip_filename))
        species_name <- sub("_SS-.*", "", zip_basename)
        species_name <- gsub("_", " ", species_name)
        
        if (nrow(input$upload_zip) > 1) {
          append_to_log("Warning: Multiple ZIP files uploaded. Processing only the first file. Use 'Run SS3 Batch' for all files.")
        }
        
        root_dir <- getwd()
        temp_dir <- tempdir()
        
        temp_file <- tempfile(fileext = ".txt")
        sink(temp_file, append = TRUE)
        on.exit({
          sink()
          if (file.exists(temp_file)) {
            log_content <- readLines(temp_file, warn = FALSE)
            append_to_log(log_content)
            unlink(temp_file)
          }
          setwd(root_dir)
        }, add = TRUE)
        
        tryCatch({
          zip_info <- extract_and_validate_zip(zip_path, temp_dir, species_name)
          if (is.null(zip_info)) {
            stop("Failed to extract or validate ZIP file contents")
          }
          input_dir <- zip_info$input_dir
          
          mydir <- normalizePath(getwd())
          append_to_log(paste("Processing species:", species_name))
          
          output_dir <- file.path(mydir, "output", zip_basename)
          append_to_log(paste0("Output will be saved to: ", output_dir))
          
          if (dir.exists(output_dir)) {
            unlink(output_dir, recursive = TRUE, force = TRUE)
          }
          dir.create(output_dir, recursive = TRUE)
          
          tryCatch({
            append_to_log(paste("Unzipping original file contents to:", output_dir))
            zip::unzip(zip_path, exdir = output_dir)
          }, error = function(e_unzip) {
            append_to_log(paste("Error unzipping file for archival:", e_unzip$message))
          })
          
          ss3_output_extensions <- c("sso", "par", "rep", "html", "log", "warning", "echo", "std", "cor", "psv", "cov", "dep", "hes", "hst", "key", "pva", "r0", "rdat", "sen", "ss_output", "ss3")
          files_to_delete <- list.files(
            path = input_dir,
            pattern = paste0("\\.(", paste(ss3_output_extensions, collapse = "|"), ")$"),
            full.names = TRUE,
            ignore.case = TRUE
          )
          if (length(files_to_delete) > 0) {
            file.remove(files_to_delete)
          }
          
          r4ss::run(
            dir = input_dir,
            exe = file.path(mydir, "Stock_Synthesis_latest/ss"),
            extras = ifelse(input$nohess_option, "-nohess", ""),
            skipfinished = FALSE,
            show_in_console = TRUE,
            verbose = TRUE
          )
          
          suppressWarnings(rm(replist, pos = .GlobalEnv))
          replist <- SS_output(
            dir = input_dir,
            verbose = FALSE,
            printstats = FALSE,
            covar = TRUE
          )
          
          tune_comps(replist=replist,
                     dir = input_dir,
                     option = "Francis",
                     plot = FALSE)
          
          tuning_file_path <- file.path(input_dir, "suggested_tuning.ss")
          if (file.exists(tuning_file_path)) {
            append_to_log(paste("--- Content of suggested_tuning.ss ---"))
            tuning_content <- readLines(tuning_file_path, warn = FALSE)
            for (line in tuning_content) {
              append_to_log(line)
            }
            append_to_log(paste("--- End of suggested_tuning.ss ---\n"))
          } else {
            append_to_log(paste("File not found after tune_comps:", tuning_file_path))
          }
          
          ss3_files_to_copy <- list.files(input_dir, pattern = "\\.(sso|par|rep|html|log|warning|echo|std|cor|psv|cov|dep|hes|hst|key|pva|r0|rdat|sen|ss_output|ss3|ss)$", full.names = TRUE, ignore.case = TRUE)
          if (length(ss3_files_to_copy) > 0) {
            file.copy(ss3_files_to_copy, output_dir, overwrite = TRUE)
            append_to_log(paste("Copied SS3 output files to:", output_dir))
          } else {
            append_to_log("Warning: No SS3 output files found in input_dir to copy.")
          }
          
          SS_plots(
            replist,
            pdf = FALSE,
            png = TRUE,
            html = TRUE,
            printfolder = "r4ss",
            dir = output_dir,
            pwidth = 6.5,
            pheight = 4,
            minbthresh = 0.2,
            fitrange = FALSE,
            forecastplot = TRUE
          )
          
          #DPIRD plots
          tryCatch({
            append_to_log("Generating custom DPIRD plots...")
            generate_DPIRD_plots(replist, output_dir)
            append_to_log("DPIRD plots generated successfully.")
          }, error = function(e) {
            append_to_log(paste("Error generating DPIRD plots:", e$message))
          })
          

          # Rename HTML files to match ZIP basename
          r4ss_plots_dir <- file.path(output_dir, "r4ss")
          if (dir.exists(r4ss_plots_dir)) {
            html_files <- list.files(path = r4ss_plots_dir, pattern = "\\.html$", full.names = TRUE)
            for (html_file_path in html_files) {
              tryCatch({
                content <- readLines(html_file_path, warn = FALSE)
                content <- str_replace(content, "<title>SS Output</title>", paste0("<title>", zip_basename, "</title>"))
                writeLines(content, html_file_path)
              }, error = function(e_retitle_single) {
                append_to_log(paste("Failed to retitle", basename(html_file_path), ":", e_retitle_single$message))
              })
            }
          }
          
          main_html <- file.path(output_dir, "00_Summary.html")
          if (file.exists(main_html)) {
            append_to_log(paste0("Opening HTML summary: ", main_html))
            browseURL(main_html)
          }
          
          append_to_log("SS3 model and plots completed successfully with selected ZIP")
        }, error = function(e) {
          append_to_log(paste("Error running SS3 model with selected ZIP:", e$message))
        })
      })
      
    }, message = function(m) {
      append_to_log(paste("App Message:", trimws(m$message))) 
    }, warning = function(w) {
      append_to_log(paste("App Warning:", trimws(w$message))) 
    }, error = function(e) {
      append_to_log(paste("App Error:", trimws(e$message)))
    })
  })
}


# Run the app
shinyApp(ui = ui, server = server, enableBookmarking = "server")