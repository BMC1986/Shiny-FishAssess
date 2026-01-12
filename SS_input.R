#
# 
# ███████ ███████     ██ ███    ██ ██████  ██    ██ ████████     ███████ ██ ██      ███████ ███████ 
# ██      ██          ██ ████   ██ ██   ██ ██    ██    ██        ██      ██ ██      ██      ██      
# ███████ ███████     ██ ██ ██  ██ ██████  ██    ██    ██        █████   ██ ██      █████   ███████ 
#      ██      ██     ██ ██  ██ ██ ██      ██    ██    ██        ██      ██ ██      ██           ██ 
# ███████ ███████     ██ ██   ████ ██       ██████     ██        ██      ██ ███████ ███████ ███████ 
#                                                                                                  


## Script to generate SS input files for the Siny R App

# Define directory and file-names ------------------------------------------

cat("\n!!!Start of output from SS_input.R!!!\n")

current_dir <- basename(getwd())

# outputdir <- "SS3_input_files/"
folder_name <- "SS3_input_files"

# Check if the folder exists
if (!dir.exists(folder_name)) {
  # Create the folder if it doesn't exist
  dir.create(folder_name)
  cat(paste0("Folder '", folder_name, "' created.\n"))
} else {
  # cat(paste0("Folder '", folder_name, "' already exists.\n"))
}

# Check if file_list exists in the environment
if (exists("file_list")) {
  # Predefine all data frames to avoid undefined object errors
  fixedsiteonly <- data.frame(
    SpeciesName = character(0),
    year = integer(0),
    Location = character(0),
    Discarded. = character(0),
    Fork.Length = integer(0),
    LengthClass = numeric(0)
  )
  merged_kim_pilb <- data.frame()
  catch_data <- data.frame()
  effort_data <- data.frame()
  
  effort_data <- data.frame(
    Specstock = character(0),
    year = integer(0),
    fleet = numeric(0),
    obs = numeric(0),
    se_log = numeric(0)
  )
  
  age_data <- data.frame()
  
  age_data <- data.frame(
    SpeciesName = character(0),
    year = integer(0),
    Location = character(0),
    IntAge= numeric(0),
    Sex = character(0),
    FL = numeric(0),
    TL = numeric(0),
    fleet = character(0)
  )
  
  bio_params <- data.frame()
  fishery_parameters <- data.frame()
  
  # Load the files if they exist
  if (!is.null(file_list$fixedsiteonly)) {
    fixedsiteonly <- read.csv(file_list$fixedsiteonly)
    cat("Loaded fixedsiteonly data with", nrow(fixedsiteonly), "rows\n")
  } else {
    cat("No FIS data detected\n")
  }
  
  
  # If not FL field. clear the object, causing issues
  if(!("Fork.Length" %in% names(fixedsiteonly))) {
    fixedsiteonly <- data.frame(
      SpeciesName = character(0),
      year = integer(0),
      Location = character(0),
      Discarded. = character(0),
      Fork.Length = integer(0),
      LengthClass = numeric(0)
    )
  }
  
  if (!is.null(file_list$merged_kim_pilb)) {
    merged_kim_pilb <- read.csv(file_list$merged_kim_pilb)
    cat("Loaded Biological length data with", nrow(merged_kim_pilb), "rows\n")
  } else {
    cat("No Biological length data detected\n")
  }

  if (!is.null(file_list$catch_data)) {
    catch_data <- read.csv(file_list$catch_data)
    cat("Loaded catch data with", nrow(catch_data), "rows\n")
  } else {
    cat("No catch data detected\n")
  }
  
  if (!is.null(file_list$effort_data)) {
    effort_data <- read.csv(file_list$effort_data)
    cat("Loaded effort data with", nrow(effort_data), "rows\n")
  } else {
    cat("No effort data detected\n")
  }

  
  if (!is.null(file_list$bio_params)) {
    bio_params <- read.csv(file_list$bio_params)
    cat("Loaded biological parameters with", nrow(bio_params), "rows\n")
  } else {
    cat("No bio params detected\n")
  }
  
  if (!is.null(file_list$fishery_params)) {
    fishery_parameters <- read.csv(file_list$fishery_params)
    cat("Loaded fishery parameters with", nrow(fishery_parameters), "rows\n")
  } else {
    cat("No fishery parameters detected\n")
  }
  
  # --- Load and Combine Age Data ---
  fis_age_data <- NULL
  bio_age_data <- NULL
  combined_age_data <- NULL
  
  # Load FIS age data if requested and file exists
  if (isTRUE(file_list$SS_use_fis_age) && !is.null(file_list$fis_age_data) && file.exists(file_list$fis_age_data)) {
    fis_age_data <- read.csv(file_list$fis_age_data)
    # Ensure fleet column exists (should be added in app.R)
    if (!"fleet" %in% names(fis_age_data)) {
      fis_age_data$fleet <- "FIS" # Fallback if missing
      cat("Warning: 'fleet' column missing in fis_age_data, added default 'FIS'\n")
    }
    cat("Loaded FIS age data with", nrow(fis_age_data), "rows\n")
  } else {
    cat("No FIS age data detected or age composition not selected\n")
  }
  
  # Load Biological age data if requested and file exists
  if (isTRUE(file_list$SS_use_bio_age) && !is.null(file_list$bio_age_data) && file.exists(file_list$bio_age_data)) {
    bio_age_data <- read.csv(file_list$bio_age_data)
    cat("'fleet' column missing in bio_age_data contains:", unique(bio_age_data$fleet),"\n")
    # Ensure fleet column exists (should be added in app.R)
    if (!"fleet" %in% names(bio_age_data)) {
      bio_age_data$fleet <- "Commercial" # Fallback if missing
      cat("Warning: 'fleet' column missing in bio_age_data, added default 'Commercial'\n")
    }
    cat("Loaded Biological age data with", nrow(bio_age_data), "rows\n")
  } else {
    cat("No Biological age data detected or age composition not selected\n")
  }
  
  # Combine the data if applicable
  if (!is.null(fis_age_data) && !is.null(bio_age_data)) {
    # Ensure columns match before binding (handle potential missing columns if necessary)
    common_cols <- intersect(names(fis_age_data), names(bio_age_data))
    combined_age_data <- rbind(fis_age_data[, common_cols], bio_age_data[, common_cols])
    cat("Combined FIS and Biological age data. Total rows:", nrow(combined_age_data), "\n")
  } else if (!is.null(fis_age_data)) {
    combined_age_data <- fis_age_data
    cat("Using only FIS age data.\n")
  } else if (!is.null(bio_age_data)) {
    combined_age_data <- bio_age_data
    cat("Using only Biological age data.\n")
  } else {
    # cat("No age data available to format.\n")
    # Assign SS_use_age to FALSE if no combined data, might simplify downstream logic
    SS_use_age <- FALSE
  }
  
  # Access additional parameters
  species <- file_list$species_name %||% "Unknown"
  cat("Detected species:", species, "\n")
  
  length_class <- file_list$length_class %||% 10
  cat("Length bin (cm):", length_class, "\n")
  
  SS_use_age <- file_list$SS_use_age %||% FALSE
  cat("Use age comp data from FIS:", SS_use_age, "\n")
  
  SS_use_length <- file_list$SS_use_length %||% FALSE
  cat("Use length comp data from FIS:", SS_use_length, "\n")
  
  SS_use_bio_length <- file_list$SS_use_bio_length %||% FALSE
  cat("Use length comp from biol databases:", SS_use_bio_length, "\n")
  
  SS_use_cond_age_length = input$conditional_age_select
  SS_use_age = "Age" %in% input$data_include_before
  
  # Generate SS3 files (simplified for debugging)
  outputdir <- paste0("SS3_input_files/", species, "/")
  dir.create(outputdir, recursive = TRUE, showWarnings = FALSE)
  # cat("Output directory:", outputdir, "\n")
  
} else {
  cat("file_list not provided to SS_input.R\n")
}

BiologicalUnitNamesearch <- species #Only uses when testing this script in isolationw
# print(BiologicalUnitNamesearch)
# cat("\noutput dir 1:",outputdir,"\n")
# outputdir <-  paste0(outputdir,BiologicalUnitNamesearch,"/") # Define output directory
# cat("\noutput dir 2:",outputdir,"\n")
dir.create(outputdir, showWarnings = FALSE) # Create subfolder

## Global parameters for SS inputs -----------------------------------------

# Check FIS and Biol databases for years
SS_start_year <- suppressWarnings(pmin(min(merged_kim_pilb$year, na.rm = TRUE), 
                                       min(fixedsiteonly$year, na.rm = TRUE),
                                       min(catch_data$year, na.rm = TRUE)))
# cat("SS_start_year:",SS_start_year,"\n")
SS_end_year <- suppressWarnings(pmax(max(merged_kim_pilb$year, na.rm = TRUE), 
                                     max(fixedsiteonly$year, na.rm = TRUE),
                                     max(catch_data$year, na.rm = TRUE)))
# cat("SS_end_year:",SS_end_year,"\n")

cat("Start-end year:",SS_start_year,"-",SS_end_year,"\n")

# Check for NAs in catch data, and convert to zero
catch_data[is.na(catch_data)] <- 0

# Get fleet info
count_non_zero_data_columns <- function(df) {
  # Exclude the first two columns
  data_cols <- df[, -c(1, 2), drop = FALSE]
  
  # Find columns with at least one non-zero value
  non_zero_cols <- sapply(data_cols, function(col) any(col != 0, na.rm = TRUE))
  
  # Count the number of non-zero columns
  return(sum(non_zero_cols))
}

get_non_zero_data_columns <- function(df) {
  # Exclude the first two columns
  data_cols <- df[, -c(1, 2), drop = FALSE]
  
  # Find columns with at least one non-zero value
  non_zero_cols <- sapply(data_cols, function(col) any(col != 0, na.rm = TRUE))
  
  # Get the names of the non-zero columns
  non_zero_col_names <- names(data_cols)[non_zero_cols]
  
  return(non_zero_col_names)
}

SS_n_fleets <- count_non_zero_data_columns(catch_data)
SS_fleetnames <- get_non_zero_data_columns(catch_data)

### HAVE TO ADD SOMETHING HERE TO HANDLE FLEETS TALKED ABOUT IN INDICES

# cat("\n")
# print(SS_fleetnames)
# cat("\n")


SS_fleetnames_INDICES <- unique(effort_data$fleet)

# cat("\n")
# print(SS_fleetnames_INDICES)
# cat("\n")
# 
# cat("\n")
# print(c(SS_fleetnames_INDICES, SS_fleetnames) )
# cat("\n")

## Make sure always a FIS fleet
SS_fleetnames_INDICES <- c(SS_fleetnames_INDICES,"FIS")


## Sort fleetnames by largest catch.
# Filter to only include names that exist in catch_data
valid_fleetnames <- SS_fleetnames[SS_fleetnames %in% colnames(catch_data)]

# Calculate total catch for each valid fleet
fleet_totals <- colSums(catch_data[, valid_fleetnames, drop = FALSE], na.rm = TRUE)

# Sort fleet names by total catch in descending order
SS_fleetnames <- names(sort(fleet_totals, decreasing = TRUE))

# cat("SS_fleetnames:",SS_fleetnames,"\n")
# Commercial.Monthly Commercial.daily Foreign.Trawl Commercial.Line Recreational Charter Foreign.Line

# ## Default value for 2 sex model 
# SS_Age_Post_Settlement <- 2
# SS_CV_Growth_Pattern <- 1

SS_Est_Retention <- TRUE
SS_Est_Selex_From_Fleets_with_Comps <- FALSE
SS_Mirror_Discards_from_FIS <- TRUE

if(species == "Lutjanus sebae") {
  cat("DEBUG: Manually ordering fleetnames to match Lutjanus sebae Rubie model\n")

  SS_fleetnames <- c("Foreign.Trawl","Foreign.Line","Commercial.Monthly","Commercial.daily","Commercial.Line","Recreational","Charter")

  cat("SS_fleetnames:",SS_fleetnames,"\n")
}

if(species == "Pristipomoides multidens") {
  cat("DEBUG: Manually ordering fleetnames to match Pristipomoides multidens Rubie model\n")
  
  SS_fleetnames <- c("Foreign.Trawl","Foreign.Line","Commercial.Monthly","Commercial.daily","Commercial.Line","Recreational","Charter")
  
  cat("SS_fleetnames:",SS_fleetnames,"\n")
  
  
  
  # cat("DEBUG: Manually removing Sex info to match Rubie Pristipomoides multidens model\n")
  # combined_age_data$Sex <- NA
  
  
  # SS_Age_Post_Settlement <- 3
  # SS_CV_Growth_Pattern <- 0
  
  SS_Est_Retention <- FALSE
  SS_Mirror_Discards_from_FIS = FALSE
  SS_Est_Selex_From_Fleets_with_Comps <- TRUE
}



if(fishery_parameters$use_1_sex_model){
  combined_age_data$Sex <- NA
  cat("Sex in fis_age_data:",unique(combined_age_data$Sex),"\n")
}

if(species == "Epinephelus rankini") {
  cat("DEBUG: Manually renaming fleets in combined_age_data for Epinephelus rankini to match catch data\n")
  
  # SS_fleetnames <- c("Commercial")
  
  # combined_age_data$fleet <- "Commercial"
  # fixedsiteonly$fleet <- "Commercial"
  
  # cat("SS_fleetnames:",SS_fleetnames,"\n")
}


## Conditionally add FIS fleets.
### $$$ !!!! This needs to be moved to the Shiny App
# SurveyFleet = T
# if(SurveyFleet) {
#   SS_n_fleets <- SS_n_fleets+1
#   # SS_fleetnames <- c(SS_fleetnames,"FIS")
#   SS_TEMP_FIS_FLEETS <- "FIS"
#   SS_fleetnames <- c(SS_TEMP_FIS_FLEETS, SS_fleetnames) # Here is where order is fixed
#   
#   SS_fleet_details = data.frame(fleet_type = c(3,rep(1,SS_n_fleets-1)), # Make the FIS a fleet type =3 "SURVEY"
#                                 fishery_timing = c(1,rep(-1,SS_n_fleets-1)), #_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#                                 area = rep(1,SS_n_fleets), 
#                                 catch_units = rep(1,SS_n_fleets), 
#                                 need_catch_mult = rep(0,SS_n_fleets), 
#                                 fleetname = SS_fleetnames)
#   
#   # # Make CPUE data the survey fleet
#   # SS_cpue_data$fleet <- SS_n_fleets
#   
#   # Add a dummy field of zero catch to survey fleet
#   catch_data$FIS <- 0 
#   
# } else {
#   SS_fleet_details = data.frame(fleet_type = rep(1,SS_n_fleets), 
#                                 fishery_timing = rep(-1,SS_n_fleets), 
#                                 area = rep(1,SS_n_fleets), 
#                                 catch_units = rep(1,SS_n_fleets), 
#                                 need_catch_mult = rep(0,SS_n_fleets), 
#                                 fleetname = SS_fleetnames)
#   
# }


# Fleet objects v2, handle FIS fleets dynamically
SurveyFleet = T
if(SurveyFleet) {
  
  ## NEW LINES
  # Check for duplicates between SS_fleetnames_INDICES and SS_fleetnames
  common_names <- intersect(SS_fleetnames_INDICES, SS_fleetnames)
  if(length(common_names) > 0) {
    warning("Duplicate fleet names found between SS_fleetnames_INDICES and SS_fleetnames: ", 
            paste(common_names, collapse = ", "), ". Removing duplicates from SS_fleetnames_INDICES.")
    SS_fleetnames_INDICES <- setdiff(SS_fleetnames_INDICES, common_names)
  }
  
  # Number of survey fleets after removing duplicates
  n_survey_fleets <- length(SS_fleetnames_INDICES)
  SS_n_fleets <- SS_n_fleets + n_survey_fleets
  SS_fleetnames <- c(SS_fleetnames_INDICES, SS_fleetnames) # Add all FIS fleets to fleetnames
  
  # Create fleet details dynamically
  SS_fleet_details = data.frame(
    fleet_type = c(rep(3, n_survey_fleets), rep(1, SS_n_fleets - n_survey_fleets)), # SURVEY type for FIS fleets, 1 for others
    fishery_timing = c(rep(1, n_survey_fleets), rep(-1, SS_n_fleets - n_survey_fleets)), # 1 for surveys, -1 for fishing fleets
    area = rep(1, SS_n_fleets), 
    catch_units = rep(1, SS_n_fleets), 
    need_catch_mult = rep(0, SS_n_fleets), 
    fleetname = SS_fleetnames
  )
  
  # Add dummy catch fields (zeros) for each survey fleet
  for(fis in SS_fleetnames_INDICES) {
    catch_data[[fis]] <- 0
  }
  
} else {
  SS_fleet_details = data.frame(
    fleet_type = rep(1, SS_n_fleets), 
    fishery_timing = rep(-1, SS_n_fleets), 
    area = rep(1, SS_n_fleets), 
    catch_units = rep(1, SS_n_fleets), 
    need_catch_mult = rep(0, SS_n_fleets), 
    fleetname = SS_fleetnames
  )
}


# Chrysophrys auratus 
# cat("\n")
# print(SS_fleet_details)
# cat("\n")

if(species == "Chrysophrys auratus") {
  cat("DEBUG: TESTING MANUAL RENAME FLEET TO COMMERICAL FOR Chrysophrys auratus \n")

  SS_fleet_details <- SS_fleet_details %>%
    mutate(fleetname = recode(fleetname, "Commercial.Line" = "Commercial"))
  
  # cat("\n")
  # print(SS_fleet_details)
  # cat("\n")
}

## Custom user parameters --------------------------------------------------

use_custom_M <- fishery_parameters$use_custom_M
use_custom_h <- fishery_parameters$use_custom_h
use_custom_R <- fishery_parameters$use_custom_R
use_custom_SigmaR <- fishery_parameters$use_custom_sigma_r

use_age_post_settlement <- fishery_parameters$use_age_post_settlement
use_cv_growth_pattern <- fishery_parameters$use_cv_growth_pattern



if(use_age_post_settlement) {
  cat("Custom Age(post-settlement)_for_L1 value of", fishery_parameters$age_post_settlement,"applied.\n")
  SS_Age_Post_Settlement <- fishery_parameters$age_post_settlement
} else {
  cat("Default Age(post-settlement)_for_L1 value of 2 applied.\n")
  SS_Age_Post_Settlement <- 2
}

if(use_cv_growth_pattern) {
  cat("Custom CV_Growth_Pattern value of", fishery_parameters$cv_growth_pattern,"applied.\n")
  SS_CV_Growth_Pattern <- fishery_parameters$cv_growth_pattern
} else {
  cat("Default CV_Growth_Pattern value of 1 applied.\n")
  SS_CV_Growth_Pattern <- 1
}


# cat("use_custom_M: ",fishery_parameters$use_custom_M,"\n")
# cat("use_custom_M: ",fishery_parameters$custom_M,"\n")
# cat("use_custom_h: ",fishery_parameters$use_custom_h,"\n")
# cat("use_custom_R: ",fishery_parameters$use_custom_R,"\n")

if(use_custom_M) {
  cat("Custom M value of", fishery_parameters$custom_M,"applied.\n")
  bio_params$NaturalMortality <- fishery_parameters$custom_M
}

if(use_custom_h) {
  cat("Custom h value of", fishery_parameters$custom_h,"applied.\n")
  SR_BH_steep <- fishery_parameters$custom_h
} else {
  cat("Default h value of 0.75 applied.\n")
  SR_BH_steep <- 0.75
}

if(use_custom_R) {
  cat("Custom R0 value of", fishery_parameters$custom_R,"applied.\n")
  SR_LN_R0 <- fishery_parameters$custom_R
} else {
  cat("Default R0 value of 8.5 applied.\n")
  SR_LN_R0 <- 8.5
}

if(use_custom_SigmaR) {
  cat("Custom sigmaR value of", fishery_parameters$custom_sigma_r,"applied.\n")
  SR_sigmaR <- fishery_parameters$custom_sigma_r
} else {
  cat("Default sigmaR value of 0.6 applied.\n")
  SR_sigmaR <- 0.6
}

# SR_BH_steep
# SR_LN_R0

## Catch data --------------------------------------------------------------

UseInitialCatch <- fishery_parameters$use_initial_catch
cat("Use initial catch in model",UseInitialCatch,"\n")

if(UseInitialCatch){
  cat("Using initial catch for fleet:",fishery_parameters$year0fleet,"\n")
  SS_F_ballpark_value <- 0.3
  SS_Flines <- paste0("   0   3  ",fishery_parameters$initf,"    ",fishery_parameters$initf,"    0.2        6      3  #InitF_seas_1_fleet_1")
  
  Year0catch <- fishery_parameters$year0catch #60
  Year0CatchSE <- fishery_parameters$year0catchse #666
  Year0Fleet <- fishery_parameters$year0fleet
} else {
  
  SS_F_ballpark_value <- "0"
  SS_Flines <- "#   0   3  0.1    0.115    0.2        6      3  #InitF_seas_1_fleet_1"
}



# Function to process catch data, excluding "FIS" but keeping original fleet numbering
process_catch_data <- function(catch_data, SS_fleetnames) {
  # Exclude "FIS" from the fleet names used for selecting and pivoting
  SS_fleetnames_no_FIS <- SS_fleetnames[SS_fleetnames != "FIS"]
  
  # Process catch data without "FIS"
  catch_data_long <- catch_data %>%
    select(year, all_of(SS_fleetnames_no_FIS)) %>%
    pivot_longer(cols = all_of(SS_fleetnames_no_FIS), names_to = "fleet_name", values_to = "catch")
  
  
  # Filter out rows where catch is 0, but only for actual data (not the initial catch row)
  # The initial catch row will be handled separately if UseInitialCatch is TRUE
  catch_data_long <- catch_data_long %>%
    filter(catch != 0) # Keep only non-zero catches
  
  # Create fleet mapping with original SS_fleetnames (including "FIS") to preserve numbering
  fleet_mapping <- data.frame(fleet_name = SS_fleetnames, fleet = 1:length(SS_fleetnames))
  
  # Join with the full fleet mapping to keep original fleet numbers
  catch_data_long <- catch_data_long %>%
    left_join(fleet_mapping, by = "fleet_name") %>%
    select(yr = year, seas = 1, fleet, catch)
  
  # Add catch_se column
  catch_data_long <- catch_data_long %>%
    mutate(catch_se = 0.01)
  
  
  # Create last row
  last_year <- max(catch_data_long$yr)
  last_row <- catch_data_long %>%
    filter(yr == last_year) %>%
    mutate(yr = -9999, catch = 0, catch_se = 0, seas = 0, fleet = 0) %>%
    slice(1)
  
  # Conditional for initial year catch
  
  if(UseInitialCatch) {
    
    # Create first row for initial catch
    first_year <- min(catch_data_long$yr)
    
    first_row <- catch_data_long %>%
      filter(yr == first_year) %>%
      mutate(yr = -999, catch = Year0catch, catch_se = Year0CatchSE, 
             fleet = match(Year0Fleet, SS_fleetnames)) %>%
      slice(1)
    
    # cat("Year0Fleet",Year0Fleet,"\n")
    # cat("SS_fleetnames",SS_fleetnames)
    
    # cat("DEBUG: first_row of catch data:","\n")
    # print(first_row)
    # cat("DEBUG: Check fleet is correct for initial catch\n")
    
    
    # # Combine rows and sort
    # result <- bind_rows(first_row, catch_data_long, last_row) %>%
    #   arrange(fleet, yr)
    
    # Combine rows
    temp_result <- bind_rows(catch_data_long, last_row)
    
    # Sort the temporary result, excluding the first_row which is already handled
    sorted_temp_result <- temp_result %>%
      arrange(fleet, yr)
    
    # Now, prepend the first_row to the already sorted data
    result <- bind_rows(first_row, sorted_temp_result)
    
  } else {
    # Combine rows and sort
    result <- bind_rows(catch_data_long, last_row) %>%
      arrange(fleet, yr)
  }
  
  # Move the last row (-9999) to the end
  result <- bind_rows(result %>% filter(yr != -9999), result %>% filter(yr == -9999))
  
  # Set seas to 1 for all except the last row (already set to 0)
  result$seas <- ifelse(result$yr == -9999, 0, 1)
  
  return(result)
}

# Process the catch data
SS_catch_data <- process_catch_data(catch_data, SS_fleetnames)


## Effort - INDICES SECTION ------------------------------------------------

# Step 1. assign CPUE indices to a fleet that exists
# Step 2. Split the Catch fleets if required, unsure how to best handle this

process_cpue_data <- function(effort_data, SS_fleetnames, SS_fleet_details) {
  # Check for fleets in effort_data not matching SS_fleetnames
  effort_fleets <- unique(effort_data$fleet)
  unmatched_fleets <- effort_fleets[!effort_fleets %in% SS_fleetnames]
  if (length(unmatched_fleets) > 0) {
    warning("The following effort fleets do not match any catch fleets and their data will be omitted: ",
            paste(unmatched_fleets, collapse = ", "))
  }
  
  # Filter for survey fleets only (fleet_type == 3)
  survey_fleets <- SS_fleet_details$fleetname[SS_fleet_details$fleet_type == 3]
  catch_fleets <- SS_fleet_details$fleetname[SS_fleet_details$fleet_type == 1]
  # effort_data <- effort_data %>% filter(fleet %in% survey_fleets)
  
  effort_data <- effort_data %>%
    filter(fleet %in% survey_fleets | fleet %in% catch_fleets)
  
  
  # Check if any survey effort data remains
  if (nrow(effort_data) == 0) {
    warning("No survey effort data available after filtering for survey fleets (fleet_type == 3).")
    return(data.frame(
      yr = -9999,
      month = 0,
      fleet = 0L,
      obs = 0,
      stderr = 0
    ))
  }
  
  # Create fleet mapping
  fleet_mapping <- data.frame(
    fleet_name = SS_fleetnames,
    fleet = 1:length(SS_fleetnames)
  )
  
  # Perform the join and use the correct fleet column
  cpue_df <- effort_data %>%
    rename(yr = year, obs = obs, stderr = se_log) %>%
    left_join(fleet_mapping, by = c("fleet" = "fleet_name")) %>%
    mutate(month = 1, 
           fleet = as.integer(fleet.y)) %>%
    select(yr, month, fleet, obs, stderr) %>%
    filter(!is.na(fleet))
  
  # Add termination row
  last_row <- data.frame(
    yr = -9999,
    month = 0,
    fleet = 0L,
    obs = 0,
    stderr = 0
  )
  
  # Combine and explicitly place -9999 last
  result <- bind_rows(cpue_df %>% arrange(fleet, yr), last_row)
  
  return(result)
}

SS_cpue_data <- process_cpue_data(effort_data, SS_fleetnames, SS_fleet_details)

# Check if user wants use_Q_extraSD
SS_Use_Q_extraSD <- fishery_parameters$use_Q_extraSD 

## actual value Q_extraSD
if(SS_Use_Q_extraSD) { 
  Q_extraSD <- fishery_parameters$Q_extraSD
  cat("Using  Q_extraSD of",Q_extraSD,"\n") 
  
}

# cat("DEBUG: Q_extraSD impact",if(SS_Use_Q_extraSD){1}else{0},"\n")


create_q_objects <- function(SS_cpue_data, SS_fleetnames, SS_Use_Q_extraSD = FALSE) {
  # Get number of rows
  n_rows <- nrow(SS_cpue_data)
  
  # Case when there's only 1 row (termination row only)
  if (n_rows == 1) {
    cat("No Indices detected, disabling Q_setup and Q_params\n")
    Q_setup <- data.frame(
      value = "#",
      stringsAsFactors = FALSE
    )
    Q_params <- data.frame(
      value = "#",
      stringsAsFactors = FALSE
    )
    return(list(Q_setup = Q_setup, Q_params = Q_params))
  }
  
  # Get unique fleets (excluding termination row)
  unique_fleets <- unique(SS_cpue_data$fleet[SS_cpue_data$yr != -9999])
  fleet_names <- SS_fleetnames[unique_fleets]
  
  # Create Q_setup dataframe
  # Q_setup <- data.frame(
  #   fleet = unique_fleets,
  #   value1 = 1,
  #   value2 = 1,
  #   # value3 = 0,
  #   value3 = if(SS_Use_Q_extraSD){1}else{0},
  #   value4 = 0,
  #   value5 = 0,
  #   value6 = 0,
  #   fleet_name = fleet_names,
  #   stringsAsFactors = FALSE
  # )
  
  Q_setup <- data.frame(
    fleet = unique_fleets,
    link = 1,
    link_info = 0,
    # value3 = 0,
    extra_se = if(SS_Use_Q_extraSD){1}else{0},
    biasadj = 0,
    float = 0,
    # value6 = 0,
    fleet_name = fleet_names,
    stringsAsFactors = FALSE
  )
  
  # Initialize Q_params dataframe for LnQ_base parameters
  Q_params <- data.frame(
    fleet = unique_fleets,
    min = -15,
    max = 15,
    init = 1,
    prior = 0,
    prior_sd = 1,
    prior_type = 0,
    phase = 1,
    v8 = 0,
    v9 = 0,
    v10 = 0,
    v11 = 0,
    v12 = 0,
    v13 = 0,
    v14 = 0,
    param_name = paste0("LnQ_base_", fleet_names, "(", unique_fleets, ")"),
    stringsAsFactors = FALSE
  )
  
  # Add Q_extraSD parameters if SS_Use_Q_extraSD is TRUE
  if (SS_Use_Q_extraSD) {
    cat("Using  Q_extraSD of",Q_extraSD,"\n") 
    Q_extraSD <- data.frame(
      fleet = unique_fleets,
      min = 0,
      max = 0.5,
      init = Q_extraSD,
      prior = Q_extraSD,
      prior_sd = 1,
      prior_type = 0,
      phase = -1,
      v8 = 0,
      v9 = 0,
      v10 = 0,
      v11 = 0,
      v12 = 0,
      v13 = 0,
      v14 = 0,
      param_name = paste0("Q_extraSD_", fleet_names, "(", unique_fleets, ")"),
      stringsAsFactors = FALSE
    )
    # # Combine LnQ_base and Q_extraSD parameters
    # Q_params <- rbind(Q_params, Q_extraSD)
    
    # Interleave Q_params and Q_extraSD rows
    combined <- data.frame()
    for (i in 1:nrow(Q_params)) {
      combined <- rbind(combined, Q_params[i, ], Q_extraSD[i, ])
    }
    Q_params <- combined
  
  }
  
  # Return both objects as a list
  return(list(Q_setup = Q_setup, Q_params = Q_params))
}



### Need to comment out the _Q_setup and _Q_parms lines if no INDEX=F
if(nrow(SS_cpue_data)==1) {
  cat("No Indices detected, disabling Q_setup and Q_params\n")
  
  SS_Q_setup <- FALSE
  
} else {
  cat("Indices detected, enabling Q_setup and Q_params\n")
  SS_Q_setup <- TRUE
  
  create_q_objects_result <- create_q_objects(SS_cpue_data, SS_fleetnames, SS_Use_Q_extraSD)
  Q_setup <- create_q_objects_result$Q_setup
  Q_params <- create_q_objects_result$Q_params
  
  cat("Q_setup: \n")
  print(Q_setup)
  cat("\n")
}


## Put extra SD stuff here


## Growth ------------------------------------------------------------------

# cat("Growthcurvetype:",bio_params$GrowthCurve,"\n")

if (startsWith(bio_params$GrowthCurve, "von Bertalanffy")){
  cat("Using von Bertalanffy GrowthModel (1)\n")
  SS_GrowthModel <- 1
}

if (startsWith(bio_params$GrowthCurve, "Schnute")){
  cat("Using Schnute GrowthModel (2: Richards with L1&L2)\n")
  SS_GrowthModel <- 2
}

# Define params for vonBert growth
if (SS_GrowthModel == 1) {
  
  ## Biol params
  if (is.null(bio_params$NaturalMortality)) {  warning("bio_params$NaturalMortality is NULL")}
  if (is.null(bio_params$GrowthParamsFemale_1)) {  warning("bio_params$GrowthParamsFemale_1 is NULL")}
  if (is.null(bio_params$GrowthParamsFemale_2)) {  warning("bio_params$GrowthParamsFemale_2 is NULL")}
  if (is.null(bio_params$GrowthParamsFemale_3)) {  warning("bio_params$GrowthParamsFemale_3 is NULL")}
  if (is.null(bio_params$GrowthParamsMale_1)) {  warning("bio_params$GrowthParamsMale_1 is NULL")}
  if (is.null(bio_params$GrowthParamsMale_2)) {  warning("bio_params$GrowthParamsMale_2 is NULL")}
  if (is.null(bio_params$GrowthParamsMale_3)) {  warning("bio_params$GrowthParamsMale_3 is NULL")}
  
  ## Calculate L_at_Amin_Fem_GP_1 using inverse vb for _Age(post-settlement)_for_L1. which is =  1 all these models
  SS_L_at_Amin_Fem <- bio_params$GrowthParamsFemale_1 * (1 - exp(-bio_params$GrowthParamsFemale_2 * (1 - bio_params$GrowthParamsFemale_3)))/10
  SS_L_at_Amin_Mal <- bio_params$GrowthParamsMale_1 * (1 - exp(-bio_params$GrowthParamsMale_2 * (1 - bio_params$GrowthParamsMale_3)))/10
  
  # age_post_settlement derived from SS_Age_Post_Settlement
  SS_L_at_Amin_Fem <- bio_params$GrowthParamsFemale_1 * (1 - exp(-bio_params$GrowthParamsFemale_2 * (SS_Age_Post_Settlement - bio_params$GrowthParamsFemale_3)))/10
  SS_L_at_Amin_Mal <- bio_params$GrowthParamsMale_1 * (1 - exp(-bio_params$GrowthParamsMale_2 * (SS_Age_Post_Settlement - bio_params$GrowthParamsMale_3)))/10
  
  SS_L_at_Amin_Fem <- round(SS_L_at_Amin_Fem,2)
  SS_L_at_Amin_Mal <- round(SS_L_at_Amin_Mal,2)
  
  SS_L_at_Amin_Fem_LO <- 0.01
  SS_L_at_Amin_Fem_HI<- round(SS_L_at_Amin_Fem+SS_L_at_Amin_Fem/2)
  
  SS_L_at_Amin_Mal_LO <- 0.01
  SS_L_at_Amin_Mal_HI<- round(SS_L_at_Amin_Mal+SS_L_at_Amin_Mal/2)
  
  Growth_Age_for_L2 <- 10
  
  SS_L_at_Amax_Fem <- bio_params$GrowthParamsFemale_1 * (1 - exp(-bio_params$GrowthParamsFemale_2 * (Growth_Age_for_L2 - bio_params$GrowthParamsFemale_3)))/10
  SS_L_at_Amax_Mal <- bio_params$GrowthParamsMale_1 * (1 - exp(-bio_params$GrowthParamsMale_2 * (Growth_Age_for_L2 - bio_params$GrowthParamsMale_3)))/10
  
  #ROunding to 2 decimal places
  SS_L_at_Amax_Fem <- round(SS_L_at_Amax_Fem,2)
  SS_L_at_Amax_Mal <- round(SS_L_at_Amax_Mal,2)
  
  if(species == "Lutjanus sebae") {
    cat("DEBUG: TESTING GROWTH START PARAM HI LIMITS \n")
    SS_L_at_Amin_Fem_HI <- 30
    SS_L_at_Amin_Mal_HI <- 30
    
  }
  
}

# Define params for Schnute growth
if (SS_GrowthModel == 2) {
  

  
  # Females Schnute
  Linf_Fem_GP_1 <- bio_params$GrowthParamsFemale_2/10
  K_Fem_GP_1 <- bio_params$GrowthParamsFemale_3
  b_Fem_GP_1 <- bio_params$GrowthParamsFemale_4
  
  Growth_Age_for_L2  <- bio_params$GrowthParamsFemale_6
  # cat("Growth_Age_for_L2:",Growth_Age_for_L2,"\n")
  
  # Currently set to age 0, need to recalculate to get L1 for age 1, SS cant handle L1 being for age 0
  # Define the Schnute function
  schnute <- function(t, L1, L2, k, b, A1, A2) {
    (L1^b + (L2^b - L1^b) * (1 - exp(-k * (t - A1))) / (1 - exp(-k * (A2 - A1))))^(1/b)
  }
  
  # Set the parameters L1_Fem_GP_1 for age =1
  L1 <- bio_params$GrowthParamsFemale_1
  L2 <- bio_params$GrowthParamsFemale_2
  k <- bio_params$GrowthParamsFemale_3
  b <- bio_params$GrowthParamsFemale_4
  A1 <- bio_params$GrowthParamsFemale_5
  A2 <- bio_params$GrowthParamsFemale_6
  t <- 1
  
  # Calculate the value at t = 1
  L1_Fem_GP_1 <- schnute(t, L1, L2, k, b, A1, A2)/10

  
  # Males Schnute
  Linf_Mal_GP_1 <- bio_params$GrowthParamsMale_2/10
  K_Mal_GP_1 <- bio_params$GrowthParamsMale_3
  b_Mal_GP_1 <- bio_params$GrowthParamsMale_4
  
  # Set the parameters to recalculate L1_Mal_GP_1 for age =1
  L1 <- bio_params$GrowthParamsMale_1
  L2 <- bio_params$GrowthParamsMale_2
  k <- bio_params$GrowthParamsMale_3
  b <- bio_params$GrowthParamsMale_4
  A1 <- bio_params$GrowthParamsMale_5
  A2 <- bio_params$GrowthParamsMale_6
  t <- 1
  
  # Calculate the value at t = 1
  L1_Mal_GP_1 <- schnute(t, L1, L2, k, b, A1, A2)/10

  
  SS_L_at_Amin_Fem_LO <- 0.01
  SS_L_at_Amin_Fem_HI<- round(L1_Fem_GP_1+L1_Fem_GP_1/2)
  
  SS_L_at_Amin_Mal_LO <- 0.01
  SS_L_at_Amin_Mal_HI<- round(Linf_Mal_GP_1+Linf_Mal_GP_1/2)
  

  
}


## PROBABLY NEED MROE CHECKS FOR MISSING PARAMS

if(is.na(bio_params$LengthWeightParamsMale_1)) {
  
  bio_params$LengthWeightParamsMale_1 <- bio_params$LengthWeightParamsFemale_1
  bio_params$LengthWeightParamsMale_2 <- bio_params$LengthWeightParamsFemale_2
  
  cat("Missing length-weight parameters for males, using female for both.\n")
  
}

if(is.na(bio_params$MaturityParamsFemale_3)) {
  
  cat("Not age-based maturity params, using length-converted\n")
  
  Numeric_Mat_A50 <- -1/bio_params$GrowthParamsFemale_2 * log(1 - bio_params$MaturityParamsFemale_1/bio_params$GrowthParamsFemale_1) + bio_params$GrowthParamsFemale_3
  Numeric_Mat_A95 <- -1/bio_params$GrowthParamsFemale_2 * log(1 - bio_params$MaturityParamsFemale_2/bio_params$GrowthParamsFemale_1) + bio_params$GrowthParamsFemale_3
  
  Mat50_Fem_GP_1 <- Numeric_Mat_A50
  Mat_slope_Fem_GP_1 <- log(19)/(Numeric_Mat_A95-Numeric_Mat_A50)
  
} else {
  cat("Using age-based maturity params\n")
  Mat50_Fem_GP_1 <- bio_params$MaturityParamsFemale_3
  Mat_slope_Fem_GP_1 <- log(19)/(bio_params$MaturityParamsFemale_4-bio_params$MaturityParamsFemale_3)
  
}

if(species == "Lutjanus sebae") {
  cat("DEBUG: TESTING different decimals for maturity params \n")
  Mat50_Fem_GP_1 <- 4.8
  Mat_slope_Fem_GP_1 <- 1.18 # Made negative further down
  
}

if(species == "Pristipomoides multidens") {
  cat("DEBUG: TESTING different decimals for maturity params \n")
  Mat50_Fem_GP_1 <- 4.6
  Mat_slope_Fem_GP_1 <- 1.09 # Made negative further down
  
}

if(species == "Lutjanus malabaricus") {
  cat("DEBUG: TESTING different decimals for maturity params for Saddletail\n")
  Mat50_Fem_GP_1 <- 9 #4.8
  Mat_slope_Fem_GP_1 <- 1.18 # Made negative further down
  
}


## Hermaphroditism ---------------------------------------------------------

# Initialize hermaphroditism options with a default
hermaphroditism_option <- 0
Hermaphro_season <- "#"
Hermaphro_maleSPB <- "#"
hermaphroditism_lines <- "# Placeholder for Hermaphroditism lines "


# Check if bio_params exists and handle sex-change parameters
if (exists("bio_params") && !is.null(bio_params)) {
  if (!is.na(bio_params$SexChangeParamsFemale_1) && !is.na(bio_params$SexChangeParamsMale_1)) {
    stop("Error: Both female and male sex-change parameters detected. Specify only one for protogynous (female-to-male) or protandrous (male-to-female).\n")
  } else if (!is.na(bio_params$SexChangeParamsFemale_1)) {
    cat("Female sex-change parameter detected, assuming protogynous, option 1\n")
    hermaphroditism_option <- 1
    Hermaphro_season <- -1
    Hermaphro_maleSPB <- 0.5
    
    hermaphroditism_inflection_age <- bio_params$SexChangeParamsFemale_3
    # if (species == "Epinephelus rankini") {
    #   cat("manually testing age params for sex-change")
    #   bio_params$SexChangeParamsFemale_3 <- 11.28
    #   bio_params$SexChangeParamsFemale_4 <- 17.77
    # }
    
    
    # Age at 50% transition (inflection point / mean)
    mu <- bio_params$SexChangeParamsFemale_3
    
    # Age at 95% transition
    X <- bio_params$SexChangeParamsFemale_4
    
    # The cumulative probability at age X
    P <- 0.95
    
    # --- Calculation ---
    
    # 1. Find the Z-score (number of standard deviations from the mean)
    #    that corresponds to the 95th percentile.
    z <- qnorm(P)
    
    # 2. Rearrange the Z-score formula (z = (X - mu) / sigma)
    #    to solve for sigma.
    hermaphroditism_SD <- (X - mu) / z
    
    # Hermaphroditism_asymptotic_rate <- log(19) / (bio_params$SexChangeParamsFemale_4 - bio_params$SexChangeParamsFemale_3) # I dont think that right, thats the width
    Hermaphroditism_asymptotic_rate <- 1 # 1 assumes all will change sex.
    
    hermaphroditism_lines <- c(paste0("	  1	  20	       ",hermaphroditism_inflection_age,"	       1          99	0		-99		0	0	0	0	0	0	0	#_Hermaphroditism_Inflection_Age"),  
                               paste0("	  0	   1        ",hermaphroditism_SD,"	       0	   0	0        	-99		0	0	0	0	0	0	0	#_Hermaphroditism_SD"),
                               paste0("	  -5	   5	     ",Hermaphroditism_asymptotic_rate,"	     0.5	  99	0		-99		0	0	0	0	0	0	0	#_Hermaphroditism_asymptotic_rate "))
    
  } else if (!is.na(bio_params$SexChangeParamsMale_1)) {
    cat("Male sex-change parameter detected, assuming protandrous, option -1\n")
    # hermaphroditism_option <- -1
    hermaphroditism_option <- 1
    Hermaphro_season <- -1
    Hermaphro_maleSPB <- 0.5
    
    hermaphroditism_inflection_age <- bio_params$SexChangeParamsMale_3
    Hermaphroditism_asymptotic_rate <- log(19) / (bio_params$SexChangeParamsMale_4 - bio_params$SexChangeParamsMale_3)
    
    hermaphroditism_lines <- c(paste0("	  1	  20	       ",hermaphroditism_inflection_age,"	       1          99	0		-99		0	0	0	0	0	0	0	#_Hermaphroditism_Inflection_Age"),  
                               "	  0	   1        0.02	       0	   0	0        	-99		0	0	0	0	0	0	0	#_Hermaphroditism_SD",
                               paste0("	  -5	   5	     ",Hermaphroditism_asymptotic_rate,"	     0.5	  99	0		-99		0	0	0	0	0	0	0	#_Hermaphroditism_asymptotic_rate "))
    
  } else {
    cat("No sex-change parameters detected, hermaphroditism options disabled\n")
    hermaphroditism_option <- 0
  }
} else {
  cat("bio_params not found, defaulting to no hermaphroditism (option 0)\n")
  hermaphroditism_option <- 0
  
  hermaphroditism_lines <- NULL
}


# Age and Size selex initial params ---------------------------------------

# Check if discard data is available
has_discard_info <- "Discarded." %in% names(fixedsiteonly) && !all(is.na(fixedsiteonly$Discarded.))
has_discard_info <- nrow(fixedsiteonly) > 0 && "Discarded." %in% names(fixedsiteonly) && !all(is.na(fixedsiteonly$Discarded.))


cat("Discard length comp detected:", has_discard_info,"\n")
# I dont thnik the calculating is helping, disabled for now

has_discard_info <- F
if (has_discard_info) {
  # Method 1: With discard info - Logistic regression
  # Convert 'Discarded.' to binary (0 = Retained, 1 = Discarded)
  selectivity_data <- fixedsiteonly %>%
    mutate(Retained = ifelse(Discarded. == "No", 1, 0))
  
  # Fit logistic regression
  selectivity_model <- glm(Retained ~ Fork.Length, data = selectivity_data, family = binomial(link = "logit"))
  
  # Extract coefficients
  coefficients <- coef(selectivity_model)
  intercept <- coefficients["(Intercept)"]
  SS_size_selex_slope <- coefficients["Fork.Length"]
  
  # Calculate L50
  SS_size_selex_L50 <- -intercept / SS_size_selex_slope
  # Convert to cm
  SS_size_selex_L50 <- SS_size_selex_L50/10
} else {
  # Method 2: Without discard info - Simple assumption
  # Use the median length as a rough L50 estimate
  SS_size_selex_L50 <- median(fixedsiteonly$Fork.Length, na.rm = TRUE)
  
  # If FIS not being used then get the SS_size_selex_L50 from Biol data
  if(is.na(SS_size_selex_L50)){
    cat("Using Biol length data to get SS_size_selex_L50\n")
    SS_size_selex_L50 <- median(Merged_Kim_Pilb$FL_mm, na.rm = TRUE)
    
  }
  
  # Convert to cm
  SS_size_selex_L50 <- SS_size_selex_L50/10
  
  # Assume a default slope (e.g., 0.1) for a moderately steep logistic curve
  # This corresponds to a reasonable spread in selectivity; adjust as needed
  SS_size_selex_slope <- 0.1
}

cat("SS_size_selex_L50:",SS_size_selex_L50,"\n")

if(is.na(SS_size_selex_L50)) {
  
  cat("No Length comp data, using max size/2 from Biological parameters as initial L50 selectivity \n")
  
  SS_size_selex_L50 <- (bio_params$MaximumLength/2)/10
  
}

SS_size_selex_L50 <- round(SS_size_selex_L50,0)
SS_size_selex_slope <- round(SS_size_selex_slope,1)

if(species == "Lutjanus sebae") {
  cat("DEBUG: TESTING SELECTIVITY STARTING PARAMS \n")
  SS_size_selex_L50 <- 30
  SS_size_selex_slope <- 5
  
}

SS_size_selex_L50_HI <- SS_size_selex_L50 + 50


cat("Intial Age_inflection_Type12_age_logistic_FISHERY1: ", SS_size_selex_L50, "\n")
cat("Initial Age_95%width_Type12_age_logistic_FISHERY1: ", SS_size_selex_slope,"\n")

## Phases ------------------------------------------------------------------

## Growth parameter phase, -99 if not using conditional age-lengths, 3 if you are using
if(SS_use_cond_age_length){
  
  Growth_phase <- 3
  Growth_phase_1 <- 3
  
  
} else {
  Growth_phase <- "-99"
  Growth_phase_1 <- "-99"
}

SS_estimate_growth_parmams <- fishery_parameters$estimate_growth_params

if(SS_estimate_growth_parmams) {
  Growth_phase <- 3
  Growth_phase_1 <- 3
  cat("Estimating growth params\n")
}


# First and Last year of recdevs ------------------------------------------



if(fishery_parameters$use_recdevs_range){
  
  
  SS_first_year_recr_devs <- fishery_parameters$first_year_recr_devs
  SS_last_year_recr_devs <- fishery_parameters$last_year_recr_devs
  
} else {
  
  SS_first_year_recr_devs <- 1950
  SS_last_year_recr_devs <- 2023
}



# Timevarying Parameters --------------------------------------------------

SS_use_tv <- fishery_parameters$use_time_varying_params

SS_Nblock_Patterns <- 0
SS_blocks_per_pattern <- "#"
tv_block_lines <- "#"

#Initialise the values for Block and Blk_Fxn
SS_growth_Block <- 0
SS_growth_Blk_Fxn <- 0

SS_growth_Block_L_at_Amin <- 0
SS_growth_Blk_Fxn_L_at_Amin <- 0
Growth_phase_L_at_Amin <- Growth_phase
Growth_phase_1_L_at_Amin <- Growth_phase_1

SS_growth_Block_L_at_Amax <- 0
SS_growth_Blk_Fxn_L_at_Amax <- 0
Growth_phase_L_at_Amax <- Growth_phase
Growth_phase_1_L_at_Amax <- Growth_phase_1

SS_growth_Block_vbK <- 0
SS_growth_Blk_Fxn_vbK <- 0
Growth_phase_vbK <- Growth_phase
Growth_phase_1_vbK <- Growth_phase_1

SS_sel_Block <- 0
SS_sel_Blk_Fxn <- 0
SS_ret_Block <- 0
SS_ret_Blk_Fxn <- 0



if(SS_use_tv){
  
  cat("Using time varying parameters\n")
  SS_tv_blocks <- fishery_parameters$time_varying_params_text
  cat("tv blocks:")
  print(SS_tv_blocks)
  
  # "2002 2006\n2007 2010"
  
  count_lines <- function(text) {
    if (is.null(text) || is.na(text) || text == "") {
      return(0)
    }
    length(strsplit(text, "\n")[[1]])
  }
  
  SS_blocks_per_pattern <- count_lines(SS_tv_blocks)
  
  ## NOW 
  # use_time_varying_growth_all
  # use_time_varying_growth_L_at_Amin
  # use_time_varying_growth_L_at_Amax
  # use_time_varying_growth_vbK
  
  SS_use_tv_growth <- fishery_parameters$use_time_varying_growth_all
  SS_use_tv_growth_L_at_Amin <- fishery_parameters$use_time_varying_growth_L_at_Amin
  SS_use_tv_growth_L_at_Amax <- fishery_parameters$use_time_varying_growth_L_at_Amax
  SS_use_tv_growth_vbK <- fishery_parameters$use_time_varying_growth_vbK
  
  
  SS_use_tv_sel <- fishery_parameters$use_time_varying_selectivity
  SS_use_tv_ret <- fishery_parameters$use_time_varying_retention
  
  cat("SS_blocks_per_pattern:",SS_blocks_per_pattern,"\n")
  cat("SS_use_tv_growth:",SS_use_tv_growth,"\n")
  cat("SS_use_tv_growth_L_at_Amin:",SS_use_tv_growth_L_at_Amin,"\n")
  cat("SS_use_tv_growth_L_at_Amax:",SS_use_tv_growth_L_at_Amax,"\n")
  cat("SS_use_tv_growth_vbK:",SS_use_tv_growth_vbK,"\n")
  cat("SS_use_tv_sel:",SS_use_tv_sel,"\n")
  cat("SS_use_tv_ret:",SS_use_tv_ret,"\n")
  
  SS_Nblock_Patterns <- 1
  
  # Assuming SS_tv_blocks is a string like "2002 2006\n2007 2010"
  # Split SS_tv_blocks into individual lines
  tv_block_lines <- if (!is.null(SS_tv_blocks) && !is.na(SS_tv_blocks) && SS_tv_blocks != "") {
    strsplit(SS_tv_blocks, "\n")[[1]]
  } else {
    character(0)  # Empty vector if SS_tv_blocks is empty/NULL
  }
  
  # Determine which parameters to enable tv
  #Growth - all params
  if(SS_use_tv_growth){
    SS_growth_Block <- 1
    SS_growth_Blk_Fxn <- 2
    # Enable phases for growth params, not needed for ret and sel as always estimated
    Growth_phase <- 3
    Growth_phase_1 <- 3
    
  }
  ### $$$ HERE ADDED CUSTOM GROWTH OPTIONS
  if(SS_use_tv_growth_L_at_Amin){
    SS_growth_Block_L_at_Amin <- 1
    SS_growth_Blk_Fxn_L_at_Amin <- 2
    Growth_phase_L_at_Amin <- 3
    Growth_phase_1_L_at_Amin <- 3
  }
  
  if(SS_use_tv_growth_L_at_Amax){
    SS_growth_Block_L_at_Amax <- 1
    SS_growth_Blk_Fxn_L_at_Amax <- 2
    Growth_phase_L_at_Amax <- 3
    Growth_phase_1_L_at_Amax <- 3
  }
  
  if(SS_use_tv_growth_vbK){
    SS_growth_Block_vbK <- 1
    SS_growth_Blk_Fxn_vbK <- 2
    Growth_phase_vbK <- 3
    Growth_phase_1_vbK <- 3
  }
  
  #Selectivity
  if(SS_use_tv_sel){
    SS_sel_Block <- 1
    SS_sel_Blk_Fxn <- 2
  }
  
  # Retention
  if(SS_use_tv_ret){
    SS_ret_Block <- 1
    SS_ret_Blk_Fxn <- 2
  }
  
}

## Add Male specific phases
Growth_phase_L_at_Amin_Mal <- Growth_phase_L_at_Amin
Growth_phase_1_L_at_Amin_Mal <- Growth_phase_1_L_at_Amin
Growth_phase_L_at_Amax_Mal <- Growth_phase_L_at_Amax
Growth_phase_1_L_at_Amax_Mal <- Growth_phase_1_L_at_Amax
Growth_phase_vbK_Mal <- Growth_phase_vbK
Growth_phase_1_vbK_Mal <- Growth_phase_1_vbK
Growth_phase_1_CV_Mal <- Growth_phase_1

## Add switch for 1 sex model

SS_1_Sex_Model <- TRUE

if(SS_1_Sex_Model){
  
  # Growth_phase_L_at_Amin_Mal <- 3
  Growth_phase_1_L_at_Amin_Mal <- -99
  # Growth_phase_L_at_Amax_Mal <- 3
  Growth_phase_1_L_at_Amax_Mal <- -99
  # Growth_phase_vbK_Mal <- 3
  Growth_phase_1_vbK_Mal <- -99
  
  Growth_phase_1_CV_Mal <- -99
  
}


# Bias Adjustments --------------------------------------------------------

SS_use_custom_bias_adj <- fishery_parameters$use_custom_bias_adj

if(SS_use_custom_bias_adj) {
  
  cat("Using custom bias adjustments\n")
  
  SS_last_early_yr_nobias_adj <- fishery_parameters$last_early_yr_nobias_adj
  SS_first_yr_fullbias_adj <- fishery_parameters$first_yr_fullbias_adj
  SS_last_yr_fullbias_adj <- fishery_parameters$last_yr_fullbias_adj
  SS_first_recent_yr_nobias_adj <- fishery_parameters$first_recent_yr_nobias_adj
  SS_Use_max_bias_adj_in_MPD <- fishery_parameters$Use_max_bias_adj_in_MPD
  
} else {
  
  cat("Using default bias adjustment values \n")
  
  SS_last_early_yr_nobias_adj <- 1950
  SS_first_yr_fullbias_adj <- 1950
  SS_last_yr_fullbias_adj <- 2023
  SS_first_recent_yr_nobias_adj <- 2023
  SS_Use_max_bias_adj_in_MPD <- 0
  
}


# Vairance Adjustment Factors (FRANCIS) -----------------------------------

SS_Use_francis_weighting <- fishery_parameters$use_francis_weighting

if(SS_Use_francis_weighting) { 
  
  SS_francis_values <- fishery_parameters$francis_weighting_value
  cat("Using Francis Weighting Factors:\n") 
  cat(SS_francis_values)
  cat("\n")
  }

# Dirichlet setup ---------------------------------------------------------

# Initialize a counter outside the modification function to maintain the sequential number. Used in function modify_fleet_lines
sequential_counter_Dirichlet <- 0

SS_Use_ln_dm_theta_lines <- fishery_parameters$estimate_Dirichlet

# if(SS_use_age){
#   
#   SS_Use_ln_dm_theta_lines <- fishery_parameters$estimate_Dirichlet
# } else {
#   SS_Use_ln_dm_theta_lines <- FALSE
# }


# cat("SS_Use_ln_dm_theta_lines:",SS_Use_ln_dm_theta_lines,"\n")
# if(SS_Use_ln_dm_theta_lines) { 
#   cat("Estimating ln_dm_theta\n")
#   
#   SS_user_defined_ln_dm_theta_lines <- (fishery_parameters$dirichlet_textbox_value)
#   
#   if(is.na(SS_user_defined_ln_dm_theta_lines) ) {
#     cat("No User Defined Dirichlet Parameter Lines detected, using defaults \n")
#     
#   } else {
#     cat("User Defined Dirichlet Parameter Lines detected: \n")
#     
#   }
#   
# }

# Generate starter file ---------------------------------------------------------

# cat("\noutput dir used in starter.ss:",outputdir,"\n")

generate_start_ss <- function(datafile, controlfile, comment = "", species = BiologicalUnitNamesearch,
                              use_init_values = 0,
                              run_display_detail = 1, detailed_age_structure = 1, write_1st_iteration = 0,
                              write_parm_values = 0, write_cumreport = 0, include_prior_like = 0,
                              use_soft_boundaries = 1, num_datafiles = 1, phase_turnoff = 10,
                              mcmc_burn_interval = 0, mcmc_thin_interval = 1, jitter_initial_parm = 0,
                              min_yr_sdreport = SS_start_year, max_yr_sdreport = -2, n_individual_std_years = 0,
                              final_convergence_criteria = "0.0001", retrospective_year = 0,
                              min_age_summary_biomass = 1, depletion_basis = 1, depletion_fraction = 1,
                              spr_report_basis = 4, annual_f_units = 3, f_std_basis = 0,
                              mcmc_output_detail = 0, alk_tolerance = 0, random_number_seed = -1,
                              check_value = "3.30", output_file = paste0(outputdir,"starter.ss")) {
  
  lines <- c(
    "#V3.30.19.00;_safe;_compile_date:_Apr 4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3",
    "#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.",
    "#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.",
    "#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov",
    "#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis",
    "#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis",
    "",
    paste0("#_Generated on ", Sys.time(),". ", comment),
    paste0("# ", species),
    datafile,
    controlfile,
    paste0(use_init_values, " # 0=use init values in control file; 1=use ss.par"),
    paste0(run_display_detail, " # run display detail (0,1,2)"),
    paste0(detailed_age_structure, " # detailed age structure output (0=minimal for data-limited, 1=high (w/ wtatage.ss_new), 2=brief, 3=custom) "),
    "# custom report options: -100 to start with minimal; -101 to start with all; -number to remove, +number to add, -999 to end",
    paste0(write_1st_iteration, " # write 1st iteration details to echoinput.sso file (0,1) "),
    paste0(write_parm_values, " # write parm values to ParmTrace.sso (0=no,1=good,active; 2=good,all; 3=every_iter,all_parms; 4=every,active)"),
    paste0(write_cumreport, " # write to cumreport.sso (0=no,1=like&timeseries; 2=add survey fits)"),
    paste0(include_prior_like, " # Include prior_like for non-estimated parameters (0,1) "),
    paste0(use_soft_boundaries, " # Use Soft Boundaries to aid convergence (0,1) (recommended)"),
    "#",
    paste0(num_datafiles, " # Number of datafiles to produce: 0 turns off all *.ss_new; 1st is data_echo.ss_new, 2nd is data_expval.ss, 3rd and higher are data_boot_**N.ss,"),
    paste0(phase_turnoff, " # Turn off estimation for parameters entering after this phase"),
    "#",
    paste0(mcmc_burn_interval, " # MCeval burn interval"),
    paste0(mcmc_thin_interval, " # MCeval thin interval"),
    paste0(jitter_initial_parm, " # jitter initial parm value by this fraction"),
    paste0(min_yr_sdreport, " # min yr for sdreport outputs (-1 for styr); # first obs catch 1973, note 1973 is 1972/73 "),
    paste0(max_yr_sdreport, "  # max yr for sdreport outputs (-1 for endyr+1; -2 for endyr+Nforecastyrs);"),
    paste0(n_individual_std_years, " # N individual STD years "),
    "#vector of year values ",
    "",
    paste0(final_convergence_criteria, " # final convergence criteria (e.g. 1.0e-04) "),
    paste0(retrospective_year, " # retrospective year relative to end year (e.g. -4)"),
    paste0(min_age_summary_biomass, " # min age for calc of summary biomass"),
    paste0(depletion_basis, " # Depletion basis: denom is: 0=skip; 1=rel X*SPB0; 2=rel SPBmsy; 3=rel X*SPB_styr; 4=rel X*SPB_endyr; values; >=11 invoke N multiyr (up to 9!) with 10's digit; >100 invokes log(ratio)"),
    paste0(depletion_fraction, " # Fraction (X) for Depletion denominator (e.g. 0.4)"),
    paste0(spr_report_basis, " # SPR_report_basis: 0=skip; 1=(1-SPR)/(1-SPR_tgt); 2=(1-SPR)/(1-SPR_MSY); 3=(1-SPR)/(1-SPR_Btarget); 4=rawSPR"),
    paste0(annual_f_units, " # Annual_F_units: 0=skip; 1=exploitation(Bio); 2=exploitation(Num); 3=sum(Apical_F's); 4=true F for range of ages; 5=unweighted avg. F for range of ages"),
    "#COND 10 15 #_min and max age over which average F will be calculated with F_reporting=4 or 5",
    paste0(f_std_basis, " # F_std_basis: 0=raw_annual_F; 1=F/Fspr; 2=F/Fmsy; 3=F/Fbtgt; where F means annual_F; values >=11 invoke N multiyr (up to 9!) with 10's digit; >100 invokes log(ratio)"),
    paste0(mcmc_output_detail, " # MCMC output detail: integer part (0=default; 1=adds obj func components; 2= +write_report_for_each_mceval); and decimal part (added to SR_LN(R0) on first call to mcmc)"),
    paste0(alk_tolerance, " # ALK tolerance ***disabled in code (example 0.0001)"),
    paste0(random_number_seed, " # random number seed for bootstrap data (-1 to use long(time) as seed): # 1649770251"),
    paste0(check_value, " # check value for end of file and for version control")
  )
  
  writeLines(lines, output_file)
  # cat(paste("File", output_file, "created successfully.\n"))
  cat("starter.ss created successfully.\n")
}

# Example usage:
generate_start_ss(datafile = "datafile.dat", controlfile = "controlfile.ctl")

# Generate control file -------------------------------------------------------

generate_control_ss <- function(datafile = "data.ss", comment = "control comment here", 
                                species = BiologicalUnitNamesearch, natM_type = 0, growth_model = SS_GrowthModel, 
                                Age_Post_Settlement = SS_Age_Post_Settlement, CV_Growth_Pattern= SS_CV_Growth_Pattern,
                                maturity_option = 2, fecundity_option = 1,  
                                parameter_offset_approach = 1, spawner_recruitment_option = 3, 
                                use_steepness = 1, do_recdev = 2, first_year_recr_devs = SS_first_year_recr_devs ,
                                last_year_recr_devs = SS_last_year_recr_devs , recdev_phase = 2, 
                                recdev_early_start = 0, recdev_early_phase = -4, 
                                forecast_recruitment_phase = 0, lambda_for_fcast_recr_like = 1, 
                                last_early_yr_nobias_adj =   SS_last_early_yr_nobias_adj, first_yr_fullbias_adj = SS_first_yr_fullbias_adj, 
                                last_yr_fullbias_adj = SS_last_yr_fullbias_adj, first_recent_yr_nobias_adj = SS_first_recent_yr_nobias_adj, 
                                max_bias_adj = SS_Use_max_bias_adj_in_MPD, min_rec_dev = -5, max_rec_dev = 5, 
                                F_ballpark_value = SS_F_ballpark_value, F_ballpark_year = -1980, F_method = 3, 
                                max_F = 4, N_iterations_tuning = 4, 
                                output_file = paste0(outputdir,"controlfile.ctl")) {
  
  # Generate lines for _selex_patterns
  generate_age_selex_patterns_lines <- function(SS_n_fleets, SS_fleetnames) {
    if (SS_n_fleets != length(SS_fleetnames)) {
      stop("SS_n_fleets and SS_fleetnames length must match")
    }
    
    fleet_lines <- character(SS_n_fleets)
    for (i in 1:SS_n_fleets) {
      fleet_lines[i] <- paste0("    0    0    0    0 #_fleet:", i, "_", SS_fleetnames[i])
    }
    return(fleet_lines)
  }
  
  
  
  SS_fleets_with_comps <- unique(merged_kim_pilb$fleet)
    
  # A single, unified function to handle all options
  generate_size_selex_patterns_lines <- function(SS_n_fleets,
                                                 SS_fleetnames,
                                                 Mirror_Discards_from_FIS=SS_Mirror_Discards_from_FIS ,
                                                 Est_Selex_From_Fleets_with_Comps=SS_Est_Selex_From_Fleets_with_Comps,
                                                 fleets_with_comps=SS_fleets_with_comps) { # Pass the fleets with comps as an argument
    
    if (SS_n_fleets != length(SS_fleetnames)) {
      stop("SS_n_fleets and SS_fleetnames length must match")
    }
    
    fleet_lines <- character(SS_n_fleets)
    for (i in 1:SS_n_fleets) {
      
      # --- Logic for Fleet 1 (always a special case) ---
      if (i == 1) {
        # Your code had conflicting definitions for fleet 1.
        # If estimating selex, your second block set parameter 2 to 0. We'll honour that.
        # Otherwise, it was 1.
        p1 <- 1
        p2 <- if (Mirror_Discards_from_FIS) { 1 } else { 0 } # Assumes fleet 1 is also mirrored
        p3 <- 0
        p4 <- 0
        
        # --- Logic for Other Fleets (i > 1) ---
      } else {
        is_commercial_fleet <- grepl("Commercial\\.Monthly|Commercial\\.daily", SS_fleetnames[i])
        
        # Note: Corrected `==` to `%in%` for checking against multiple fleets
        has_comps_fleet <- i %in% fleets_with_comps 
        
        # Determine parameter 1 (Selex type: 1 for estimated, 15 for fixed/default)
        # This is based on the logic from your second code block
        p1 <- if (Est_Selex_From_Fleets_with_Comps && has_comps_fleet) { 1 } else { 15 }
        
        # Determine parameter 2 (Mirroring: -1 for mirrored, 0 otherwise)
        # This is based on the logic from your first code block
        p2 <- if (Mirror_Discards_from_FIS && is_commercial_fleet) { -1 } else { 0 }
        
        p3 <- 0
        # p4 <- 1
        p4 <- if (Est_Selex_From_Fleets_with_Comps && has_comps_fleet) { 0 } else { 1 }
      }
      
      # Assemble the final line with clean formatting
      line_text <- sprintf("    %-4d %-4d %-4d %-4d", p1, p2, p3, p4)
      fleet_lines[i] <- paste0(line_text, " #_fleet:", i, "_", SS_fleetnames[i])
    }
    return(fleet_lines)
  }
  
  # unique(merged_kim_pilb$fleet)
  # SS_Est_Selex_From_Fleets_with_Comps
  
  fleet_size_selex_lines <- generate_size_selex_patterns_lines(SS_n_fleets, SS_fleetnames)
  
  cat("\n")
  print(fleet_size_selex_lines)
  cat("\n")
  cat("length(SS_fleets_with_comps): ",length(SS_fleets_with_comps),"\n")
  
  
  fleet_age_selex_lines <- generate_age_selex_patterns_lines(SS_n_fleets, SS_fleetnames)
  fleet_age_selex_lines
  
  # Generate _Dirichlet parameters lines, only needed if you have CompressBins enabled. SO can be a NULL lines.
  generate_ln_dm_theta_lines <- function(SS_n_fleets, SS_fleetnames) {
    if (SS_n_fleets != length(SS_fleetnames)) {
      stop("SS_n_fleets and SS_fleetnames length must match")
    }
    
    fleet_lines <- character(SS_n_fleets)
    for (i in 1:SS_n_fleets) {
      fleet_lines[i] <- paste0("  -5 	    20        0.5        0     1.813           6           4           0          0          0          0          0          0          0  # ln(DM_theta)_", i, ":", SS_fleetnames[i])
    }
    return(fleet_lines)
  }
  
  # SS_Use_ln_dm_theta_lines = FALSE
  # if(SS_Use_ln_dm_theta_lines) {
  #   # ln_dm_theta_lines <- generate_ln_dm_theta_lines(sequential_counter_Dirichlet, SS_fleetnames)
  #   ln_dm_theta_lines <- generate_ln_dm_theta_lines(sequential_counter_Dirichlet, c(1:sequential_counter_Dirichlet)) # doesnt pull the actual fleet name, too hard for now
  # } else {
  #   ln_dm_theta_lines <- NULL
  # }
  
  ## New version with custom lines
  if(SS_Use_ln_dm_theta_lines) {
    cat("Estimating ln_dm_theta\n")
    SS_user_defined_ln_dm_theta_lines <- (fishery_parameters$dirichlet_textbox_value)
    
    if(is.na(SS_user_defined_ln_dm_theta_lines) ) {
      cat("No User Defined Dirichlet Parameter Lines detected, using defaults \n")
      ln_dm_theta_lines <- generate_ln_dm_theta_lines(sequential_counter_Dirichlet, c(1:sequential_counter_Dirichlet)) # doesnt pull the actual fleet name, too hard for now
    } else {
      cat("User Defined Dirichlet Parameter Lines detected: \n")
      ln_dm_theta_lines <- SS_user_defined_ln_dm_theta_lines
    }
    
  } else {
    ln_dm_theta_lines <- NULL
  }
  

  
  
  lines <- c(
    "#V3.30.19.00;_safe;_compile_date:_Apr 4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3",
    "#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.",
    "#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.",
    "#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov",
    "#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis",
    "#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis",
    paste0("#_Generated on ", Sys.time(),". ", comment),
    paste0("# ", species),
    paste0("#_data_and_control_files: ", datafile, " // control.ss"),
    "0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters",
    "1  #_N_Growth_Patterns (Growth Patterns, Morphs, Bio Patterns, GP are terms used interchangeably in SS3)",
    "1 #_N_platoons_Within_GrowthPattern",
    "",
    "4 # recr_dist_method for parameters:  2=main effects for GP, Area, Settle timing; 3=each Settle entity; 4=none (only when N_GP*Nsettle*pop==1)",
    "1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area",
    "1 #  number of recruitment settlement assignments",
    "0 # unused option",
    "#GPattern month  area  age (for each settlement assignment)",
    " 1 1 1 0",
    "",
    "#_Cond 0 # N_movement_definitions goes here if Nareas > 1",
    "",
    # "0 #_Nblock_Patterns",
    paste0(SS_Nblock_Patterns, " #_Nblock_Patterns"),
    # "#_blocks_per_pattern",
    paste0(SS_blocks_per_pattern, " #_blocks_per_pattern"),
    "# begin and end years of blocks",
    tv_block_lines,
    "1 #_time-vary parm bound check (1=warn relative to base parm bounds; 3=no bound check); Also see env (3) and dev (5) options to constrain with base bounds",
    "",
    "# AUTOGEN",
    "0 0 0 0 0 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex",
    "",
    "### !!!!! BIOLOGY SECTION !!!!!",
    paste0(natM_type, " #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate;_5=BETA:_Maunder_link_to_maturity"),
    paste0(growth_model, " # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K_incr; 4=age_specific_K_decr; 5=age_specific_K_each; 6=NA; 7=NA; 8=growth cessation"),
    paste0(Age_Post_Settlement," #_Age(post-settlement)_for_L1;linear growth below this"),
    # "10 #_Growth_Age_for_L2 (999 to use as Linf)",
    paste0(Growth_Age_for_L2," #_Growth_Age_for_L2 (999 to use as Linf)"),
    "-999 #_exponential decay for growth above maxage (value should approx initial Z; -999 replicates 3.24; -998 to not allow growth above maxage)",
    "0  #_placeholder for future growth feature",
    "0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)",
    paste0(CV_Growth_Pattern," #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)"),
    paste0(maturity_option, " #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity"),
    "2 #_First_Mature_Age # Length maturity params",
    paste0(fecundity_option, " #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W"),
    paste0(hermaphroditism_option, " #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn"),
    paste0(Hermaphro_season, " #Hermaphro_season"),
    paste0(Hermaphro_maleSPB, " #Hermaphro_maleSPB from decimal part of seas input; note that firstage can only be a single digit, so 9 is max"),
    paste0(parameter_offset_approach, " #_parameter_offset_approach for M, G, CV_G:  1- direct, no offset**; 2- male=fem_parm*exp(male_parm); 3: male=female*exp(parm) then old=young*exp(parm)"),
    "",
    "#_growth_parms",
    "#_   LO   HI    INIT PRIOR PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn")
  
  # VB growth
  if (growth_model == 1){
    cat("creating vb growth model lines\n")


    # More options for tv paramaters added
    
    if(!fishery_parameters$use_1_sex_model) {
      cat("2 sex params\n")
    lines <- c(lines,
               paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Fem_GP_1"),
               paste0("      ",SS_L_at_Amin_Fem_LO,"   ",SS_L_at_Amin_Fem_HI,"   ",SS_L_at_Amin_Fem,"      1    10     0     " ,Growth_phase_1_L_at_Amin, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amin,"    ",SS_growth_Blk_Fxn_L_at_Amin," # L_at_Amin_Fem_GP_1"),
               paste0("     30  100   ",SS_L_at_Amax_Fem,"      1    10     0     " ,Growth_phase_1_L_at_Amax, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amax,"    ",SS_growth_Blk_Fxn_L_at_Amax," # L_at_Amax_Fem_GP_1"),
               paste0("   0.05 0.60    ",bio_params$GrowthParamsFemale_2,"      1    10     0     " ,Growth_phase_1_vbK, "    0    0    0    0    0    ",SS_growth_Block_vbK,"    ",SS_growth_Blk_Fxn_vbK," # VonBert_K_Fem_GP_1"),
               paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1, "    0    0    0    0    0    0    0 # CV_young_Fem_GP_1 ##"),
               paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1, "    0    0    0    0    0    0    0 # CV_old_Fem_GP_1 ##"),
               paste0("     -3    3 ",bio_params$LengthWeightParamsFemale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Fem_GP_1"),
               paste0("     -3    4 ",bio_params$LengthWeightParamsFemale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Fem_GP_1"),
               paste0("      1   20    ",Mat50_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat50%_Fem_GP_1 ## Age based maturity params"),
               paste0("     -3    3   ",-Mat_slope_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat_slope_Fem_GP_1 ## Age based maturity params"),
               "     -3    3       1      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_inter_Fem_GP_1",
               "     -3    3       0      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_slope_wt_Fem_GP_1",
               paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Mal_GP_1"),
               paste0("      ",SS_L_at_Amin_Mal_LO,"   ",SS_L_at_Amin_Mal_HI,"   ",SS_L_at_Amin_Mal,"      1    10     0     " ,Growth_phase_1_L_at_Amin_Mal, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amin,"    ",SS_growth_Blk_Fxn_L_at_Amin," # L_at_Amin_Mal_GP_1"),
               paste0("     30  100   ",SS_L_at_Amax_Mal,"      1    10     0     " ,Growth_phase_1_L_at_Amax_Mal, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amax,"    ",SS_growth_Blk_Fxn_L_at_Amax," # L_at_Amax_Mal_GP_1"),
               paste0("   0.05 0.60    ",bio_params$GrowthParamsMale_2,"      1    10     0     " ,Growth_phase_1_vbK_Mal, "    0    0    0    0    0    ",SS_growth_Block_vbK,"    ",SS_growth_Blk_Fxn_vbK," # VonBert_K_Mal_GP_1"),
               paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1_CV_Mal, "    0    0    0    0    0    0    0 # CV_young_Mal_GP_1"),
               paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1_CV_Mal, "    0    0    0    0    0    0    0 # CV_old_Mal_GP_1"),
               paste0("     -3    3 ",bio_params$LengthWeightParamsMale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Mal_GP_1"),
               paste0("     -3    4 ",bio_params$LengthWeightParamsMale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Mal_GP_1"))
    
    } else {
      cat("1 sex params\n")
      lines <- c(lines,
                 paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Fem_GP_1"),
                 paste0("      ",SS_L_at_Amin_Fem_LO,"   ",SS_L_at_Amin_Fem_HI,"   ",SS_L_at_Amin_Fem,"      1    10     0     " ,Growth_phase_1_L_at_Amin, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amin,"    ",SS_growth_Blk_Fxn_L_at_Amin," # L_at_Amin_Fem_GP_1"),
                 paste0("     30  100   ",SS_L_at_Amax_Fem,"      1    10     0     " ,Growth_phase_1_L_at_Amax, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amax,"    ",SS_growth_Blk_Fxn_L_at_Amax," # L_at_Amax_Fem_GP_1"),
                 paste0("   0.05 0.60    ",bio_params$GrowthParamsFemale_2,"      1    10     0     " ,Growth_phase_1_vbK, "    0    0    0    0    0    ",SS_growth_Block_vbK,"    ",SS_growth_Blk_Fxn_vbK," # VonBert_K_Fem_GP_1"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1, "    0    0    0    0    0    0    0 # CV_young_Fem_GP_1 ##"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1, "    0    0    0    0    0    0    0 # CV_old_Fem_GP_1 ##"),
                 paste0("     -3    3 ",bio_params$LengthWeightParamsFemale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Fem_GP_1"),
                 paste0("     -3    4 ",bio_params$LengthWeightParamsFemale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Fem_GP_1"),
                 paste0("      1   20    ",Mat50_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat50%_Fem_GP_1 ## Age based maturity params"),
                 paste0("     -3    3   ",-Mat_slope_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat_slope_Fem_GP_1 ## Age based maturity params"),
                 "     -3    3       1      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_inter_Fem_GP_1",
                 "     -3    3       0      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_slope_wt_Fem_GP_1")
                 # paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Mal_GP_1"),
                 # paste0("      ",SS_L_at_Amin_Mal_LO,"   ",SS_L_at_Amin_Mal_HI,"   ",SS_L_at_Amin_Mal,"      1    10     0     " ,Growth_phase_1_L_at_Amin_Mal, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amin,"    ",SS_growth_Blk_Fxn_L_at_Amin," # L_at_Amin_Mal_GP_1"),
                 # paste0("     30  100   ",SS_L_at_Amax_Mal,"      1    10     0     " ,Growth_phase_1_L_at_Amax_Mal, "    0    0    0    0    0    ",SS_growth_Block_L_at_Amax,"    ",SS_growth_Blk_Fxn_L_at_Amax," # L_at_Amax_Mal_GP_1"),
                 # paste0("   0.05 0.60    ",bio_params$GrowthParamsMale_2,"      1    10     0     " ,Growth_phase_1_vbK_Mal, "    0    0    0    0    0    ",SS_growth_Block_vbK,"    ",SS_growth_Blk_Fxn_vbK," # VonBert_K_Mal_GP_1"),
                 # paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1_CV_Mal, "    0    0    0    0    0    0    0 # CV_young_Mal_GP_1"),
                 # paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase_1_CV_Mal, "    0    0    0    0    0    0    0 # CV_old_Mal_GP_1"),
                 # paste0("     -3    3 ",bio_params$LengthWeightParamsMale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Mal_GP_1"),
                 # paste0("     -3    4 ",bio_params$LengthWeightParamsMale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Mal_GP_1"))
      
      
    }
  }
  
  
  
  # Schnute Growth
  if (growth_model == 2){
    cat("creating schnute growth model lines\n")
    # Backup of lines before tv growth
    # lines <- c(lines,
    #            paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Fem_GP_1"),
    #            paste0("      ",SS_L_at_Amin_Fem_LO,"   ",SS_L_at_Amin_Fem_HI,"   ",L1_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # L1_Fem_GP_1"),
    #            paste0("     30  100   ",Linf_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # Linf_Fem_GP_1"),
    #            paste0("   0.05 0.3    ",K_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # K_Fem_GP_1"),
    #            paste0("   0.05 3    ",b_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # b_Fem_GP_1"),
    #            paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_young_Fem_GP_1 ##"),
    #            paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_old_Fem_GP_1 ##"),
    #            paste0("     -3    3 ",bio_params$LengthWeightParamsFemale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Fem_GP_1"),
    #            paste0("     -3    4 ",bio_params$LengthWeightParamsFemale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Fem_GP_1"),
    #            paste0("      1   20    ",Mat50_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat50%_Fem_GP_1 ## Age based maturity params"),
    #            paste0("     -3    3   ",-Mat_slope_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat_slope_Fem_GP_1 ## Age based maturity params"),
    #            "     -3    3       1      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_inter_Fem_GP_1",
    #            "     -3    3       0      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_slope_wt_Fem_GP_1",
    #            paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Mal_GP_1"),
    #            paste0("      ",SS_L_at_Amin_Mal_LO,"   ",SS_L_at_Amin_Mal_HI,"   ",L1_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # L1_Mal_GP_1"),
    #            paste0("     30  100   ",Linf_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # Linf_Mal_GP_1"),
    #            paste0("   0.05 0.3    ",K_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # K_Mal_GP_1"),
    #            paste0("   0.05 3    ",b_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # b_Mal_GP_1"),
    #            paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_young_Mal_GP_1"),
    #            paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_old_Mal_GP_1"),
    #            paste0("     -3    3 ",bio_params$LengthWeightParamsMale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Mal_GP_1"),
    #            paste0("     -3    4 ",bio_params$LengthWeightParamsMale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Mal_GP_1"))
    if(!fishery_parameters$use_1_sex_model) {
      cat("2 sex params\n")
      lines <- c(lines,
                 paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Fem_GP_1"),
                 paste0("      ",SS_L_at_Amin_Fem_LO,"   ",SS_L_at_Amin_Fem_HI,"   ",L1_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # L1_Fem_GP_1"),
                 paste0("     30  100   ",Linf_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # Linf_Fem_GP_1"),
                 paste0("   0.05 0.3    ",K_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # K_Fem_GP_1"),
                 paste0("   0.05 3    ",b_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # b_Fem_GP_1"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_young_Fem_GP_1 ##"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_old_Fem_GP_1 ##"),
                 paste0("     -3    3 ",bio_params$LengthWeightParamsFemale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Fem_GP_1"),
                 paste0("     -3    4 ",bio_params$LengthWeightParamsFemale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Fem_GP_1"),
                 paste0("      1   20    ",Mat50_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat50%_Fem_GP_1 ## Age based maturity params"),
                 paste0("     -3    3   ",-Mat_slope_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat_slope_Fem_GP_1 ## Age based maturity params"),
                 "     -3    3       1      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_inter_Fem_GP_1",
                 "     -3    3       0      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_slope_wt_Fem_GP_1",
                 paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Mal_GP_1"),
                 paste0("      ",SS_L_at_Amin_Mal_LO,"   ",SS_L_at_Amin_Mal_HI,"   ",L1_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # L1_Mal_GP_1"),
                 paste0("     30  100   ",Linf_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # Linf_Mal_GP_1"),
                 paste0("   0.05 0.3    ",K_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # K_Mal_GP_1"),
                 paste0("   0.05 3    ",b_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # b_Mal_GP_1"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_young_Mal_GP_1"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_old_Mal_GP_1"),
                 paste0("     -3    3 ",bio_params$LengthWeightParamsMale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Mal_GP_1"),
                 paste0("     -3    4 ",bio_params$LengthWeightParamsMale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Mal_GP_1"))
      
    } else {
      cat("Single sex params\n")
      # Single sex version
      lines <- c(lines,
                 paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Fem_GP_1"),
                 paste0("      ",SS_L_at_Amin_Fem_LO,"   ",SS_L_at_Amin_Fem_HI,"   ",L1_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # L1_Fem_GP_1"),
                 paste0("     30  100   ",Linf_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # Linf_Fem_GP_1"),
                 paste0("   0.05 0.3    ",K_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # K_Fem_GP_1"),
                 paste0("   0.05 3    ",b_Fem_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # b_Fem_GP_1"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_young_Fem_GP_1 ##"),
                 paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_old_Fem_GP_1 ##"),
                 paste0("     -3    3 ",bio_params$LengthWeightParamsFemale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Fem_GP_1"),
                 paste0("     -3    4 ",bio_params$LengthWeightParamsFemale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Fem_GP_1"),
                 paste0("      1   20    ",Mat50_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat50%_Fem_GP_1 ## Age based maturity params"),
                 paste0("     -3    3   ",-Mat_slope_Fem_GP_1,"      1    10     0     -99    0    0    0    0    0    0    0 # Mat_slope_Fem_GP_1 ## Age based maturity params"),
                 "     -3    3       1      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_inter_Fem_GP_1",
                 "     -3    3       0      1    10     0     -99    0    0    0    0    0    0    0 # Eggs/kg_slope_wt_Fem_GP_1")
                 # paste0("   0.05 0.15   ",bio_params$NaturalMortality,"     1   10     0     -99    0    0    0    0    0    0    0 # NatM_uniform_Mal_GP_1"),
                 # paste0("      ",SS_L_at_Amin_Mal_LO,"   ",SS_L_at_Amin_Mal_HI,"   ",L1_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # L1_Mal_GP_1"),
                 # paste0("     30  100   ",Linf_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # Linf_Mal_GP_1"),
                 # paste0("   0.05 0.3    ",K_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # K_Mal_GP_1"),
                 # paste0("   0.05 3    ",b_Mal_GP_1,"      1    10     0     " ,Growth_phase, "    0    0    0    0    0    ",SS_growth_Block,"    ",SS_growth_Blk_Fxn," # b_Mal_GP_1"),
                 # paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_young_Mal_GP_1"),
                 # paste0(" 0.0001 0.25    0.1      1    10     0     " ,Growth_phase, "    0    0    0    0    0    0    0 # CV_old_Mal_GP_1"),
                 # paste0("     -3    3 ",bio_params$LengthWeightParamsMale_1,"  1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_1_Mal_GP_1"),
                 # paste0("     -3    4 ",bio_params$LengthWeightParamsMale_2,"      1    10     0     -99    0    0    0    0    0    0    0 # Wtlen_2_Mal_GP_1")
      
      
    }
    
  }
  
  # cat("DEBUG: BEFORE hermaphroditism_lines lines \n")
  
  lines <- c(lines,
    hermaphroditism_lines,
    "   0.01   10       1      1     1     0     -99    0    0    0    0    0    0    0 # CohortGrowDev ##",
    "  1e-06 0.99     0.5      1    10     0     -99    0    0    0    0    0    0    0 # FracFemale_GP_1",
    "",
    "#_no timevary MG parameters",
    "",
    "#_seasonal_effects_on_biology_parms",
    "0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K",
    "",
    "### !!!!! SPAWNER - RECRUITMENT SECTION !!!!!",
    paste0(spawner_recruitment_option, " #_Spawner-Recruitment; Options: 1=NA; 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepherd_3Parm; 9=RickerPower_3parm"),
    paste0(use_steepness, " # 0/1 to use steepness in initial equ recruitment calculation"),
    "0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature",
    "#_ LO     HI    INIT   PRIOR  PR_SD  PR_type PHASE  env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn #  parm_name",
    # "    3     31     8.5     8.5     10     0       1     0     0     0     0     0     0     0 # SR_LN(R0)",
    # "  0.2      1    0.75     0.7   0.05     0     -99     0     0     0     0     0     0     0 # SR_BH_steep",
    paste0("    3     31     ",SR_LN_R0,"     8.5     10     0       1     0     0     0     0     0     0     0 # SR_LN(R0)"),
    paste0("  0.2      1    ",SR_BH_steep,"     0.7   0.05     0     -99     0     0     0     0     0     0     0 # SR_BH_steep"),
    paste0("    0      2     ",SR_sigmaR,"     0.8    0.8     0     -99     0     0     0     0     0     0     0 # SR_sigmaR"),
    "   -10    10       0       0      1     0     -99     0     0     0     0     0     0     0 # SR_regime",
    "    0      0       0       0      0     0     -99     0     0     0     0     0     0     0 # SR_autocorr",
    "#_no timevary SR parameters",
    paste0(do_recdev, " #do_recdev:  0=none; 1=devvector (R=F(SSB)+dev); 2=deviations (R=F(SSB)+dev); 3=deviations (R=R0*dev; dev2=R-f(SSB)); 4=like 3 with sum(dev2) adding penalty"),
    # "1950 # first year of main recr_devs; early devs can preceed this era",
    paste0(first_year_recr_devs, " # first year of main recr_devs; early devs can preceed this era"),
    # "2023 # last year of main recr_devs; forecast devs start in following year",
    paste0(last_year_recr_devs, " # last year of main recr_devs; forecast devs start in following year"),
    paste0(recdev_phase, " #_recdev phase"),
    "1 # (0/1) to read 13 advanced options",
    paste0(recdev_early_start, " #_recdev_early_start (0=none; neg value makes relative to recdev_start)"),
    paste0(recdev_early_phase, " #_recdev_early_phase"),
    paste0(forecast_recruitment_phase, " #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)"),
    paste0(lambda_for_fcast_recr_like, " #_lambda for Fcast_recr_like occurring before endyr+1"),
    paste0(last_early_yr_nobias_adj, "   #_last_early_yr_nobias_adj_in_MPD"),
    paste0(first_yr_fullbias_adj, "   #_first_yr_fullbias_adj_in_MPD"),
    paste0(last_yr_fullbias_adj, "   #_last_yr_fullbias_adj_in_MPD"),
    paste0(first_recent_yr_nobias_adj, "   #_first_recent_yr_nobias_adj_in_MPD"),
    paste0(max_bias_adj, "  #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)"),
    "0 #_period of cycles in recruitment (N parms read below)",
    paste0(min_rec_dev, " #_min rec_dev"),
    paste0(max_rec_dev, " #_max rec_dev"),
    "0 #_read_recdevs",
    "#_end of advanced SR options",
    "",
    "## !!!!! FISHING MORTALITY SECTION !!!!!",
    paste0(F_ballpark_value, " # F ballpark value in units of annual_F"),
    paste0(F_ballpark_year, " # F ballpark year (neg value to disable)"),
    paste0(F_method, "  # F_Method:  1=Pope midseason rate; 2=F as parameter; 3=F as hybrid; 4=fleet-specific parm/hybrid (#4 is superset of #2 and #3 and is recommended)"),
    paste0(max_F, "  # max F (methods 2-4) or harvest fraction (method 1)"),
    paste0(N_iterations_tuning, "  # N iterations for tuning in hybrid mode; recommend 3 (faster) to 5 (more precise if many fleets)"),
    "",
    "#_initial_F_parms; for each fleet x season that has init_catch; nest season in fleet; count = 0",
    "#_LO  HI  INIT PRIOR PR_SD  PR_type  PHASE",
    SS_Flines,
    # "   0   3  0.1    0.115    0.2        6      3  #InitF_seas_1_fleet_1",
    "",
    "## !!!!! CATCHABILITY !!!!!",
    "#_Q_setup for fleets with cpue or survey data",
    "#_fleet	link link_info extra_se biasadj float")
  
  # cat("DEBUG: BEFORE SS_Q_setup lines \n")
  
  
  if(SS_Q_setup) {
  for (i in 1:nrow(Q_setup)) {
    lines <- c(lines,
               sprintf("%d\t%d\t%d\t%d\t%d\t%d\t# %s",
                       Q_setup$fleet[i],      # First column
                       Q_setup$link[i],
                       Q_setup$link_info[i],
                       Q_setup$extra_se[i],
                       Q_setup$biasadj[i],
                       Q_setup$float[i],
                       Q_setup$fleet_name[i]))  # Comment field
  }
  } else {
    
    lines <- c(lines,"#")
  }
  

  
  
  lines <- c(lines,
             "-9999 0 0 0 0 0 #  END READ",
             "",
             "#_Q_parms(if_any);Qunits_are_ln(q)",
             "#_LO    HI    INIT    PRIOR    PR_SD    PR_type    PHASE    env-var    use_dev    dev_mnyr    dev_mxyr    dev_PH    Block    Blk_Fxn  #  parm_name")
  

  
  if(SS_Q_setup) {
  for (i in 1:nrow(Q_params)) {
    lines <- c(lines,
               # sprintf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t# %s",
               sprintf("%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t%g\t# %s",        
                       Q_params$min[i], Q_params$max[i],
                       Q_params$init[i], Q_params$prior[i],
                       Q_params$prior_sd[i], Q_params$prior_type[i],
                       Q_params$phase[i], Q_params$v8[i],
                       Q_params$v9[i], Q_params$v10[i],
                       Q_params$v11[i], Q_params$v12[i],
                       Q_params$v13[i], Q_params$v14[i],
                       Q_params$param_name[i]))
  }
  } else {
    
    lines <- c(lines,"#")
  }
  

  
  lines <- c(lines,
             # paste0(SS_Q_setup,"-15     15      1       0       1       0        1      0       0       0       0       0       0       0  #  LnQ_base_FISHERY1(2)"),
             # "  0      1      0.02    0       0       0       -4      0       0       0       0       0       0       0  #  Q_extraSD_FISHERY1(2)",
             "",
             "## !!!!! SELECTIVITY AND DISCARD/RETENTION !!!!!",
             "#_size_selex_patterns",
             # "        0       0    0       0 # 1 FISHERY1", # duplicate for each fisher
             fleet_size_selex_lines,
             "",
             "#_age_selex_patterns",
             # "      12      1    0       0 # 1 FISHERY1", # duplicate for each fishery, make 15 to mirror
             fleet_age_selex_lines,
             "",
             "#_          LO       HI     INIT    PRIOR    PR_SD  PR_type      PHASE    env-var    use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn  #  parm_name",
             paste0("         0.01       ", SS_size_selex_L50_HI ,"       ",SS_size_selex_L50,"       1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_inflection_Type12_age_logistic_FISHERY1(1)"),
             paste0("         0.01       20        ",SS_size_selex_slope,"         1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_95%width_Type12_age_logistic_FISHERY1(1)"))
  
  if(SS_Est_Selex_From_Fleets_with_Comps){
    
    
    if(length(SS_fleets_with_comps) == 1){
    lines <- c(lines, 
               paste0("         0.01       ", SS_size_selex_L50_HI ,"       ",SS_size_selex_L50,"       1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_inflection_Type12_age_logistic_FISHERY1(1)"),
               paste0("         0.01       20        ",SS_size_selex_slope,"         1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_95%width_Type12_age_logistic_FISHERY1(1)"))
    }
    
    if(length(SS_fleets_with_comps) == 2){
      lines <- c(lines, 
                 paste0("         0.01       ", SS_size_selex_L50_HI ,"       ",SS_size_selex_L50,"       1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_inflection_Type12_age_logistic_FISHERY1(1)"),
                 paste0("         0.01       20        ",SS_size_selex_slope,"         1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_95%width_Type12_age_logistic_FISHERY1(1)"), 
                 paste0("         0.01       ", SS_size_selex_L50_HI ,"       ",SS_size_selex_L50,"       1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_inflection_Type12_age_logistic_FISHERY1(1)"),
                 paste0("         0.01       20        ",SS_size_selex_slope,"         1     0.01        0            3          0          0          0          0          0          ",SS_sel_Block,"          ",          SS_sel_Blk_Fxn,"  #  Age_95%width_Type12_age_logistic_FISHERY1(1)"))
    }
    
    if(length(SS_fleets_with_comps) > 1){
      stop("ERROR: TOO MANY FLEETS WITH COMPS, CHECK WHERE SELECTIVITY PATTERNS DEFINED")
      }
  }
  
  
  lines <- c(lines,             
             "# retention - age based")
  
  if(SS_Est_Retention){
  lines <- c(lines,
             # "         0.01       50        38       1     0.01        0           3          0          0          0          0          0          0          0  #  Retention_p1_A50_FISHERY1(1)",
             # "         0.01       20        1        1     0.01        0           3          0          0          0          0          0          0          0  #  Retention_p2_width_FISHERY1(1)",
             paste0("         0.01       50        38       1     0.01        0           3          0          0          0          0          0          ",SS_ret_Block,"          ",SS_ret_Blk_Fxn,"  #  Retention_p1_A50_FISHERY1(1)"),
             paste0("         0.01       20        1        1     0.01        0           3          0          0          0          0          0          ",SS_ret_Block,"          ",SS_ret_Blk_Fxn,"  #  Retention_p2_width_FISHERY1(1)"),
             "          -10     1000      999        1     0.01        0           -3          0          0          0          0          0          0          0  #  Retention_p3_asymptote_FISHERY1(1)",
             "          -10       10        0        1     0.01        0           -3          0          0          0          0          0          0          0  #  Retention_p4_maleoffset_FISHERY1(1)",
             "")
  }
  
  
  lines <- c(lines,
             "#_Dirichlet parameters",
             "#_LO HI INIT PRIOR  PR_SD PR_type PHASE env-var   use_dev   dev_mnyr   dev_mxyr     dev_PH      Block    Blk_Fxn # Parm_name",
             # "  -5 20  0    0     1.813       6    2       0         0          0          0          0          0          0 # ln(DM_theta)_1",
             ln_dm_theta_lines,
             "",
             "0   #  use 2D_AR1 selectivity(0/1)",
             "",
             "## !!!!! TAG-RECAPTURE SECTION !!!!!",
             "0  # TG_custom:  0=no read and autogen if tag data exist; 1=read",
             "",
             "## !!!!! VARIANCE_ADUSTMENT FACTORS !!!!!",
             "# Input variance adjustments factors:",
             "#factor fleet New_Var_adj hash Old_Var_adj New_Francis   New_MI Francis_mult Francis_lo Francis_hi  MI_mult Type    Name Note")
  
  

  
  

  if(SS_Use_francis_weighting) {
    # lines <- c(lines,
    #            # "      4     1    2.944622    #           1    2.944622 0.315638     2.944622   2.002433  30.452289 0.315638  len FISTrap     ", # Testing RED Kim 28 May
    #            # "      5     1    0.258582    #           1    0.258582 0.334947     0.258582   0.147395   2.435387 0.334947  age FISTrap   ", # Testing RED Kim 28 May
    #            "      4     1    5.328853    #           1    5.328853 0.340902     5.328853   3.376112  28.765341 0.340902  len FISTrap     ", # Testing RED Kim 28 May tv sel
    #            "      5     1    0.255742    #           1    0.255742 0.334749     0.255742   0.149936   1.756688 0.334749  age FISTrap   ", # Testing RED Kim 28 May tv sel
    #            "     -9999     0           0    # terminator",
    #            "",
    #            "## !!!!! LAMBDA/EMPHASIS FACTORS !!!!!",
    #            "5 #_maxlambdaphase",
    #            "1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter",
    #            "# read 3 changes to default Lambdas (default value is 1.0)",
    #            "-9999  1  1  1  1  #  terminator",
    #            "",
    #            "## !!!!! CONTROLS FOR VARIANCE OF DERIVED QUANTITIES !!!!!",
    #            "0 # (0/1/2) read specs for more stddev reporting",
    #            "999"
    # )
    
    # SS_francis_values
    
    lines <- c(lines,
               SS_francis_values,
               # "      4     1    5.328853    #           1    5.328853 0.340902     5.328853   3.376112  28.765341 0.340902  len FISTrap     ", # Testing RED Kim 28 May tv sel
               # "      5     1    0.255742    #           1    0.255742 0.334749     0.255742   0.149936   1.756688 0.334749  age FISTrap   ", # Testing RED Kim 28 May tv sel
               "     -9999     0           0    # terminator",
               "",
               "## !!!!! LAMBDA/EMPHASIS FACTORS !!!!!",
               "5 #_maxlambdaphase",
               "1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter",
               "# read 3 changes to default Lambdas (default value is 1.0)",
               "-9999  1  1  1  1  #  terminator",
               "",
               "## !!!!! CONTROLS FOR VARIANCE OF DERIVED QUANTITIES !!!!!",
               "0 # (0/1/2) read specs for more stddev reporting",
               "999"
    )
    
    
  } else {
    lines <- c(lines,
               "     -9999     0           0    # terminator",
               "",
               "## !!!!! LAMBDA/EMPHASIS FACTORS !!!!!",
               "5 #_maxlambdaphase",
               "1 #_sd_offset; must be 1 if any growthCV, sigmaR, or survey extraSD is an estimated parameter",
               "# read 3 changes to default Lambdas (default value is 1.0)",
               "-9999  1  1  1  1  #  terminator",
               "",
               "## !!!!! CONTROLS FOR VARIANCE OF DERIVED QUANTITIES !!!!!",
               "0 # (0/1/2) read specs for more stddev reporting",
               "999"
    )
    
  }
  
  writeLines(lines, output_file)
  # cat(paste("File", output_file, "created successfully.\n"))
  cat("controlfile.ctl created successfully.\n")
}


# generate_control_ss(datafile = "data.ss", output_file = paste0(outputdir,"controlfile.ctl"))

# Generate DAT file -------------------------------------------------------

## Helper function to pad vectors to a specified length with zeros
pad_vector <- function(vec, target_length) {
  if (length(vec) < target_length) {
    vec <- c(vec, rep(0, target_length - length(vec)))
  }
  return(vec[1:target_length])  # Ensure it doesn't exceed target_length
}

SS_length_binwidth <- length_class/10 # convert to cm

# print(sort(unique(Fixedsiteonly$species)))

round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
fixedsiteonly <- fixedsiteonly %>%
  mutate(LengthClass = round_any(Fork.Length, accuracy = SS_length_binwidth*10, f = floor))

# print(is.na(fixedsiteonly$LengthClass[1]))

if(!is.na(fixedsiteonly$LengthClass[1])) {
  
  SS_max_size <- max(fixedsiteonly$LengthClass,na.rm = T)/10

} else {
  cat("No FISLength comp data, using max size from Biological parameters \n")
  SS_max_size <- bio_params$MaximumLength/10
  
  # Need dynamic here for datasource
  SS_max_size_bioparams <- bio_params$MaximumLength/10
  SS_max_size_Biodata <- max(merged_kim_pilb$LengthClass,na.rm = T)/10
  # SS_max_size <- max(SS_max_size_bioparams,SS_max_size_Biodata) # THis caused error because decimals
  SS_max_size <- round(max(SS_max_size_bioparams,SS_max_size_Biodata),0)

}
cat("SS_max_size",SS_max_size, "\n")
# SS_length_bins <-  seq(1, SS_max_size, by = 1)
# SS_length_bins <-  seq(1, SS_max_size, by = SS_length_binwidth)
SS_length_bins <-  seq(0, SS_max_size, by = SS_length_binwidth)
SS_n_length_bins  <- length(SS_length_bins)


## Age data formatting
if(SS_use_age){

  # SS_n_age_bins <- max(age_data$IntAge, na.rm = T)
  SS_n_age_bins <- max(combined_age_data$IntAge, na.rm = TRUE)
  # SS_age_bins <- seq(1, SS_n_age_bins, by = 1)
  SS_age_bins <- seq(0, SS_n_age_bins, by = 1)
} else {

  SS_n_age_bins <- bio_params$MaxObservedAge
  SS_age_bins <- 0
}

cat("SS_max_age (SS_n_age_bins):",SS_n_age_bins, "\n")


# Include Length data from Biol dbases ------------------------------------



if(nrow(fixedsiteonly)>0) {

fixedsiteonly$fleet <- which(SS_fleet_details$fleetname == "FIS")
cat("FIS fleet:", which(SS_fleet_details$fleetname == "FIS"),"\n")
} else{
  cat("no FIS data fleet\n")
}


## $$$ Neeed to make this dynamic, or inherit from the app.R
# SS_bio_length_fleet <- "Commercial"
# SS_bio_length_fleet <- "Commercial.Trap"
SS_bio_length_fleet <- SS_fleet_details$fleetname[2]

# cat("SS_bio_length_fleet:",SS_bio_length_fleet,"\n")

## 

# cat("n(FIS)",nrow(fixedsiteonly),"\n")
# cat("colnames(fixedsiteonly)",colnames(fixedsiteonly),"\n")

# cat("n(Biol lengths)",nrow(merged_kim_pilb),"\n")
# cat("colnames(merged_kim_pilb)",colnames(merged_kim_pilb),"\n")

if(SS_use_bio_length){
  cat("start of SS_use_bio_length\n")
  # Reformat merged_kim_pilb to match fixedsiteonly 
  merged_kim_pilb$Discarded. <- "No"  # Add the 'Discarded.' column to merged_kim_pilb and fill with NA
  
  # Select and reorder the columns in merged_kim_pilb to match fixedsiteonly
  # merged_kim_pilb <- merged_kim_pilb[, c("SpeciesName", "year", "Location", "Discarded.", "FL_mm", "LengthClass")]
  merged_kim_pilb <- merged_kim_pilb[, c("SpeciesName", "year", "Location", "Discarded.", "FL_mm", "LengthClass","fleet","Sex")]
  
  # Rename the 'FL_mm' column to 'Fork.Length' to match fixedsiteonly
  colnames(merged_kim_pilb)[colnames(merged_kim_pilb) == "FL_mm"] <- "Fork.Length"
  
  # cat("colnames(merged_kim_pilb):",colnames(merged_kim_pilb),"\n")
  cat("Unique fleets in catch data:", unique(SS_fleet_details$fleetname),"\n")
  cat("Unique fleets in length data:", unique(merged_kim_pilb$fleet),"\n")
  
  ### Match way
  current_fleet <- unique(merged_kim_pilb$fleet)
  cat("Unique current fleets:", paste(current_fleet, collapse = ", "), "\n")
  
  # # Try the match approach
  # merged_kim_pilb$fleet_index <- match(merged_kim_pilb$fleet, SS_fleet_details$fleetname)
  # cat("Resulting fleet_index (first few rows):\n")
  # cat(paste(head(merged_kim_pilb$fleet_index), collapse = ", "), "\n")
  
  # Try the match approach
  merged_kim_pilb$fleet <- match(merged_kim_pilb$fleet, SS_fleet_details$fleetname)
  cat("Resulting fleet_index (first few rows):\n")
  cat(paste(head(merged_kim_pilb$fleet), collapse = ", "), "\n")
  
  # Add a blank Sex field in, fixedsiteonly has no sex info? Probably should $$$
  

  # Merge extra lengths in, Only if fixedsiteonly has data
  if(nrow(fixedsiteonly)>0) {
    fixedsiteonly$Sex <- NA
  fixedsiteonly <- rbind(fixedsiteonly,merged_kim_pilb)
  } else {
    fixedsiteonly <- merged_kim_pilb
  }
  
}

cat("colnames merged_kim_pilb",colnames(merged_kim_pilb),"\n")
cat("colnames fixedsiteonly",colnames(fixedsiteonly),"\n")


# cat("nrow(length data) after merge:",nrow(fixedsiteonly),"\n")

# cat("FIS fleet:", which(SS_fleet_details$fleetname == "FIS"),"\n")

# cat("fleets in length data:", unique(fixedsiteonly$fleet), "\n")


## format length data  -------------

cat("Start of SS_format_length_data\n")


SS_format_length_data <- function(length_data, max_length = SS_max_size, 
                                  month = 1, 
                                  length_bins = seq(0, max_length, by = SS_length_binwidth), 
                                  Lbin_lo = -1, Lbin_hi = -1) {
  
  # Helper function to pad vectors to the required length for SS3
  pad_vector <- function(vec, target_length = length(length_bins) * 2) {
    if (length(vec) < target_length) {
      vec <- c(vec, rep(0, target_length - length(vec)))
    }
    return(vec[1:target_length])
  }
  
  # Initial checks for valid input data
  if (!"fleet" %in% colnames(length_data)) {
    warning("No 'fleet' column found in length_data. Assigning default fleet = 1.")
    length_data$fleet <- 1
  }
  if (any(is.na(length_data$fleet) | length_data$fleet <= 0)) {
    warning("Invalid fleet values (NA or <= 0) found and replaced with fleet = 1.")
    length_data$fleet[is.na(length_data$fleet) | length_data$fleet <= 0] <- 1
  }
  
  # Convert length from mm to cm for processing
  length_data <- length_data %>%
    mutate(LengthClass_cm = LengthClass / 10)
  
  fleets <- unique(length_data$fleet)
  
  # Initialise an empty data frame to store the results
  result <- data.frame(
    yr = integer(), month = integer(), fleet = integer(), sex = integer(),
    part = integer(), Lbin_lo = integer(), Lbin_hi = integer(),
    Nsamp = integer(), datavector = I(vector("list", 0))
  )
  
  # Process the data for each fleet, year, and discard status
  for (f in fleets) {
    fleet_data <- length_data %>% filter(fleet == f)
    for (y in unique(fleet_data$year)) {
      year_data <- fleet_data %>% filter(year == y)
      for (discard_status in c("Yes", "No")) {
        discard_data <- year_data %>% filter(Discarded. == discard_status)
        
        if (nrow(discard_data) == 0) next
        
        part_value <- ifelse(discard_status == "Yes", 1, 2) # 1=discard, 2=retained
        
        has_males <- any(discard_data$Sex == "M", na.rm = TRUE)
        has_females <- any(discard_data$Sex == "F", na.rm = TRUE)
        
        # If data contains both males and females, process it as sex-specific (sex=3)
        if (has_males && has_females) {
          sex_code <- 3
          sexed_subset <- discard_data %>% filter(Sex %in% c("M", "F"))
          
          female_counts <- sexed_subset %>% 
            filter(Sex == "F") %>%
            group_by(LengthClass_cm) %>%
            summarise(count = n(), .groups = 'drop') %>%
            complete(LengthClass_cm = length_bins, fill = list(count = 0)) %>%
            arrange(LengthClass_cm)
          
          male_counts <- sexed_subset %>% 
            filter(Sex == "M") %>%
            group_by(LengthClass_cm) %>%
            summarise(count = n(), .groups = 'drop') %>%
            complete(LengthClass_cm = length_bins, fill = list(count = 0)) %>%
            arrange(LengthClass_cm)
          
          combined_vector <- pad_vector(c(female_counts$count, male_counts$count))
        } else {
          # Otherwise, treat the entire sample as unsexed (sex=0)
          sex_code <- 0
          
          unsexed_counts <- discard_data %>% 
            group_by(LengthClass_cm) %>%
            summarise(count = n(), .groups = 'drop') %>%
            complete(LengthClass_cm = length_bins, fill = list(count = 0)) %>%
            arrange(LengthClass_cm)
          
          # Unsexed data goes in the first half of the vector (female portion)
          combined_vector <- pad_vector(c(unsexed_counts$count, rep(0, length(length_bins))))
        }
        
        # Finalise the row for the datafile
        new_nsamp <- sum(combined_vector)
        if (new_nsamp > 0) {
          new_row <- data.frame(
            yr = y, month = month, fleet = f, sex = sex_code, part = part_value,
            Lbin_lo = Lbin_lo, Lbin_hi = Lbin_hi, Nsamp = new_nsamp,
            datavector = I(list(combined_vector))
          )
          result <- rbind(result, new_row)
        }
      }
    }
  }
  
  # Add the required terminator row at the end of the data block
  terminal_row <- data.frame(
    yr = -9999, month = 0, fleet = 0, sex = 0, part = 0, Lbin_lo = 0,
    Lbin_hi = 0, Nsamp = 0, datavector = I(list(pad_vector(rep(0, length(length_bins) * 2))))
  )
  
  if (nrow(result) > 0) {
    result <- result[order(result$part, result$fleet, result$yr), ]
    result <- rbind(result, terminal_row)
  } else {
    result <- terminal_row
  }
  
  return(result)
}

# Add a NULL fleet field if missing
if (!("fleet" %in% colnames(fixedsiteonly))) {
  fixedsiteonly$fleet <- character(nrow(fixedsiteonly))
  cat("An empty 'fleet' column has been added to fixedsiteonly.\n")
} else {
  cat("The 'fleet' column already exists in fixedsiteonly.\n")
}

# cat("unique(fixedsiteonly$fleet)",unique(fixedsiteonly$fleet),"\n")

# Run in length data exists
# if(nrow(fixedsiteonly)>1){
SS_length_data_formatted <- SS_format_length_data(fixedsiteonly, 
                                                  max_length = SS_max_size,
                                                  length_bins = seq(0, SS_max_size, by = SS_length_binwidth))



# cat("nfleets in SS_length_data_formatted:", unique(SS_length_data_formatted$fleet), "\n")
cat("fleets in SS_length_data_formatted:", unique(SS_length_data_formatted[-nrow(SS_length_data_formatted), ]$fleet), "\n") # 999 row removed


cat("nrow(SS_length_data_formatted)",nrow(SS_length_data_formatted),"\n")
# cat("after SS_length_data_formatted\n")

cat("unique fleets in SS_length_data_formatted-SHOULD BE NUMBERS: ",unique(SS_length_data_formatted$fleet),"\n")

## format age data  -------------


SS_format_age_data <- function(age_data, max_age = SS_n_age_bins, 
                               month = 1, part = 2, ageerr = 1, Lbin_lo = -1, Lbin_hi = -1) {
  
  # Helper function to pad vectors
  pad_vector <- function(vec, target_length = (max_age + 1) * 2) {
    if (length(vec) < target_length) {
      vec <- c(vec, rep(0, target_length - length(vec)))
    }
    return(vec[1:target_length])
  }
  
  # Check if fleet column exists
  if (!"fleet" %in% colnames(age_data)) {
    stop("No 'fleet' column found in age_data.")
  }
  
  # Get unique fleets
  fleets <- unique(age_data$fleet)
  if (length(fleets) == 0) {
    # Return a valid empty/terminal structure if no fleet data
    terminal_row <- data.frame(
      yr = -9999, month = 0, fleet = 0, sex = 0, part = 0, ageerr = 0,
      Lbin_lo = 0, Lbin_hi = 0, Nsamp = 0,
      datavector = I(list(pad_vector(rep(0, (max_age + 1) * 2))))
    )
    return(terminal_row)
  }
  
  # Initialize result data frame
  result <- data.frame(
    yr = integer(),
    month = integer(),
    fleet = integer(),
    sex = integer(),
    part = integer(),
    ageerr = integer(),
    Lbin_lo = integer(),
    Lbin_hi = integer(),
    Nsamp = integer(),
    datavector = I(vector("list", 0))
  )
  
  # Process each fleet
  for (fleet_name in fleets) {
    # Get fleet index from SS_fleet_details
    fleet_idx <- which(SS_fleet_details$fleetname == fleet_name)
    if (length(fleet_idx) == 0) {
      warning("Fleet name '", fleet_name, "' not found in SS_fleet_details$fleetname. Skipping.")
      next
    }
    
    fleet_data <- age_data %>% filter(fleet == fleet_name)
    years <- unique(fleet_data$year)
    
    # Process each year
    for (y in years) {
      year_data <- fleet_data %>% filter(year == y)
      
      # --- NEW LOGIC START ---
      # Split data into sexed (M/F) and unsexed (NA)
      sexed_subset <- year_data %>% filter(Sex %in% c("M", "F"))
      unsexed_subset <- year_data %>% filter(is.na(Sex))
      
      # cat("hello unsexed_subset:")
      # print(unsexed_subset)
      
      # 1. Process the SEXED subset (sex_code = 3)
      # Process only if both males and females are present in the subset
      if (nrow(sexed_subset) > 0 && any(sexed_subset$Sex == "M") && any(sexed_subset$Sex == "F")) {
        female_data <- sexed_subset %>% 
          filter(Sex == "F") %>%
          group_by(IntAge) %>%
          summarise(count = n(), .groups = 'drop') %>%
          complete(IntAge = 0:max_age, fill = list(count = 0)) %>%
          arrange(IntAge)
        
        male_data <- sexed_subset %>% 
          filter(Sex == "M") %>%
          group_by(IntAge) %>%
          summarise(count = n(), .groups = 'drop') %>%
          complete(IntAge = 0:max_age, fill = list(count = 0)) %>%
          arrange(IntAge)
        
        combined_vector <- pad_vector(c(female_data$count, male_data$count))
        new_nsamp <- sum(combined_vector)
        
        if (new_nsamp > 0) {
          new_row <- data.frame(
            yr = y, month = month, fleet = fleet_idx, sex = 3, part = part,
            ageerr = ageerr, Lbin_lo = Lbin_lo, Lbin_hi = Lbin_hi,
            Nsamp = new_nsamp, datavector = I(list(combined_vector))
          )
          result <- rbind(result, new_row)
        }
      }
      
      # 2. Process the UNSEXED subset (sex_code = 0)
      if (nrow(unsexed_subset) > 0) {
        all_data <- unsexed_subset %>% 
          group_by(IntAge) %>%
          summarise(count = n(), .groups = 'drop') %>%
          complete(IntAge = 0:max_age, fill = list(count = 0)) %>%
          arrange(IntAge)
        
        combined_vector <- pad_vector(c(all_data$count, rep(0, max_age + 1)))
        new_nsamp <- sum(combined_vector)
        
        if (new_nsamp > 0) {
          new_row <- data.frame(
            yr = y, month = month, fleet = fleet_idx, sex = 0, part = part,
            ageerr = ageerr, Lbin_lo = Lbin_lo, Lbin_hi = Lbin_hi,
            Nsamp = new_nsamp, datavector = I(list(combined_vector))
          )
          result <- rbind(result, new_row)
        }
      }
      # --- NEW LOGIC END ---
    }
  }
  
  # Add terminal row required by Stock Synthesis
  if (nrow(result) > 0) {
    terminal_row <- data.frame(
      yr = -9999, month = 0, fleet = 0, sex = 0, part = 0, ageerr = 0,
      Lbin_lo = 0, Lbin_hi = 0, Nsamp = 0,
      datavector = I(list(pad_vector(rep(0, (max_age + 1) * 2))))
    )
    result <- result[order(result$fleet, result$yr), ]
    result <- rbind(result, terminal_row)
  } else {
    # If no data, still need a terminal row
    terminal_row <- data.frame(
      yr = -9999, month = 0, fleet = 0, sex = 0, part = 0, ageerr = 0,
      Lbin_lo = 0, Lbin_hi = 0, Nsamp = 0,
      datavector = I(list(pad_vector(rep(0, (max_age + 1) * 2))))
    )
    result <- terminal_row
  }
  
  return(result)
}

## format conditional age-at-length data  -------------


## updated Nsmap method https://gemini.google.com/app/219360fa0e1840d4
SS_format_cond_age_length <- function(age_data, SS_length_bins, max_age = SS_n_age_bins,
                                      month = 1, part = 0, ageerr = 1, SS_length_binwidth = 1) {
  
  pad_vector <- function(vec, target_length = (max_age + 1) * 2) {
    if (length(vec) < target_length) {
      vec <- c(vec, rep(0, target_length - length(vec)))
    }
    return(vec[1:target_length])
  }
  
  if (!"fleet" %in% colnames(age_data)) {
    stop("No 'fleet' column found in age_data.")
  }
  fleets <- unique(age_data$fleet)
  if (length(fleets) == 0) {
    stop("No valid fleet names found in age_data$fleet.")
  }
  
  length_col_name <- NULL
  if ("FL" %in% names(age_data) && !all(is.na(age_data$FL))) {
    length_col_name <- "FL"
  } else if ("TL" %in% names(age_data) && !all(is.na(age_data$TL))) {
    length_col_name <- "TL"
  } else {
    stop("No suitable length column (FL or TL) found in age_data.")
  }
  
  # Validate age_data: Keep rows with valid length and age, allow any Sex value
  age_data <- age_data %>%
    filter(!is.na(.data[[length_col_name]]), !is.na(IntAge), IntAge >= 0, IntAge <= max_age)
  
  age_data$Length_bin <- floor(age_data[[length_col_name]] / 10)
  
  combined_data <- age_data
  
  result <- data.frame(
    yr = integer(), month = integer(), fleet = integer(), sex = integer(),
    part = integer(), ageerr = integer(), Lbin_lo = integer(),
    Lbin_hi = integer(), Nsamp = integer(), datavector = I(vector("list", 0))
  )
  
  for (fleet_name in fleets) {
    # This part assumes SS_fleet_details is available in the global environment
    # You might want to pass it as an argument for better practice
    fleet_idx <- which(SS_fleet_details$fleetname == fleet_name)
    if (length(fleet_idx) == 0) {
      warning("Fleet name '", fleet_name, "' not found in SS_fleet_details$fleetname. Skipping.")
      next
    }
    
    fleet_data <- combined_data %>% filter(fleet == fleet_name)
    years <- unique(fleet_data$year)
    length_bins <- sort(unique(fleet_data$Length_bin))
    
    for (y in years) {
      year_data <- fleet_data %>% filter(year == y)
      has_males <- any(year_data$Sex == "M", na.rm = TRUE)
      has_females <- any(year_data$Sex == "F", na.rm = TRUE)
      
      # Determine sex code
      sex_code <- if (has_males && has_females) 3 else if (has_females) 1 else if (has_males) 2 else 0
      
      for (lbin in length_bins) {
        bin_data <- year_data %>% filter(Length_bin == lbin)
        if (nrow(bin_data) > 0) {
          
          # data_vector is created first
          if (sex_code == 3) {
            female_data <- bin_data %>% filter(Sex == "F") %>%
              group_by(IntAge) %>% summarise(count = n(), .groups = 'drop') %>%
              complete(IntAge = 0:max_age, fill = list(count = 0)) %>% arrange(IntAge)
            male_data <- bin_data %>% filter(Sex == "M") %>%
              group_by(IntAge) %>% summarise(count = n(), .groups = 'drop') %>%
              complete(IntAge = 0:max_age, fill = list(count = 0)) %>% arrange(IntAge)
            data_vector <- pad_vector(c(female_data$count, male_data$count))
          } else if (sex_code == 1) {
            female_data <- bin_data %>% filter(Sex == "F") %>%
              group_by(IntAge) %>% summarise(count = n(), .groups = 'drop') %>%
              complete(IntAge = 0:max_age, fill = list(count = 0)) %>% arrange(IntAge)
            data_vector <- pad_vector(c(female_data$count, rep(0, max_age + 1)))
          } else if (sex_code == 2) {
            male_data <- bin_data %>% filter(Sex == "M") %>%
              group_by(IntAge) %>% summarise(count = n(), .groups = 'drop') %>%
              complete(IntAge = 0:max_age, fill = list(count = 0)) %>% arrange(IntAge)
            data_vector <- pad_vector(c(rep(0, max_age + 1), male_data$count))
          } else { # sex_code = 0, unsexed or combined
            all_data <- bin_data %>%
              group_by(IntAge) %>% summarise(count = n(), .groups = 'drop') %>%
              complete(IntAge = 0:max_age, fill = list(count = 0)) %>% arrange(IntAge)
            data_vector <- pad_vector(c(all_data$count, rep(0, max_age + 1)))
          }
          
          # --- MODIFICATION START ---
          # Recalculate Nsamp from the generated vector to ensure they match.
          new_nsamp <- sum(data_vector)
          
          # If the new Nsamp is 0, there's no data to add, so skip to the next bin.
          if (new_nsamp == 0) {
            next
          }
          
          new_row <- data.frame(
            yr = y, month = month, fleet = fleet_idx, sex = sex_code,
            part = part, ageerr = ageerr, Lbin_lo = lbin + 1,
            Lbin_hi = lbin + 1, Nsamp = new_nsamp, # Use the corrected Nsamp here
            datavector = I(list(data_vector))
          )
          # --- MODIFICATION END ---
          
          result <- rbind(result, new_row)
        }
      }
    }
  }
  
  if (nrow(result) > 0) {
    terminal_row <- data.frame(
      yr = -9999, month = 0, fleet = 0, sex = 0, part = 0, ageerr = 0,
      Lbin_lo = 0, Lbin_hi = 0, Nsamp = 0,
      datavector = I(list(pad_vector(rep(0, (max_age + 1) * 2))))
    )
    result <- result[order(result$fleet, result$yr, result$Lbin_lo), ]
    result <- rbind(result, terminal_row)
  }
  
  return(result)
}

## faster version
# Helper function to pad vectors (self-contained)
pad_vector_optimized <- function(vec, max_age_val) {
  # Calculate the target length for the data vector (females 0 to max_age, males 0 to max_age)
  target_length <- (as.integer(max_age_val) + 1L) * 2L
  current_length <- length(vec)
  
  if (current_length < target_length) {
    # If current vector is shorter, pad with zeros
    vec_padded <- c(vec, rep(0, target_length - current_length))
  } else {
    # If current vector is not shorter, use it as is (will be truncated if longer)
    vec_padded <- vec
  }
  # Return the vector, ensuring it's exactly target_length (handles padding and truncation)
  return(vec_padded[1:target_length])
}

SS_format_cond_age_length_optimised <- function(age_data, 
                                                SS_length_bins_param, # Kept for signature consistency, not directly used in this optimized logic
                                                max_age,
                                                month_val = 1, 
                                                part_val = 0, 
                                                ageerr_val = 1,
                                                # SS_length_binwidth_val = 1, # Kept for signature consistency
                                                SS_fleet_details_df) { # Pass SS_fleet_details as a data.frame or data.table
  
  # Convert age_data to data.table
  dt <- as.data.table(age_data)
  
  # 1. Initial Checks and Setup
  if (!"fleet" %in% colnames(dt)) {
    stop("No 'fleet' column found in age_data.")
  }
  if (nrow(dt) == 0) { # Handle empty input age_data
    terminal_row <- data.frame(
      yr = -9999L, month = 0L, fleet = 0L, sex = 0L, part = 0L, ageerr = 0L,
      Lbin_lo = 0L, Lbin_hi = 0L, Nsamp = 0L,
      datavector = I(list(pad_vector_optimized(rep(0, (as.integer(max_age) + 1L) * 2L), as.integer(max_age))))
    )
    return(terminal_row)
  }
  
  # Create a mapping for fleet names to fleet indices
  fleet_details_dt <- as.data.table(SS_fleet_details_df)
  # The original code implies fleet_idx is the row number in SS_fleet_details
  # Ensure fleetname in SS_fleet_details_df is unique if using it as a key.
  # Here, we assume the order in SS_fleet_details_df corresponds to desired numeric fleet index.
  fleet_map <- data.table(
    fleet_char_original = fleet_details_dt$fleetname, 
    fleet_idx_numeric = seq_len(nrow(fleet_details_dt))
  )
  
  # Merge to get numeric fleet index, keep only matched fleets
  dt <- merge(dt, fleet_map, by.x = "fleet", by.y = "fleet_char_original", all.x = FALSE) # inner join
  
  if (nrow(dt) == 0) { # Handle case where no fleets in age_data match SS_fleet_details_df
    terminal_row <- data.frame(
      yr = -9999L, month = 0L, fleet = 0L, sex = 0L, part = 0L, ageerr = 0L,
      Lbin_lo = 0L, Lbin_hi = 0L, Nsamp = 0L,
      datavector = I(list(pad_vector_optimized(rep(0, (as.integer(max_age) + 1L) * 2L), as.integer(max_age))))
    )
    return(terminal_row)
  }
  
  
  # Identify the correct length column (FL or TL)
  length_col_name <- NULL
  if ("FL" %in% names(dt) && !all(is.na(dt$FL))) {
    length_col_name <- "FL"
  } else if ("TL" %in% names(dt) && !all(is.na(dt$TL))) {
    length_col_name <- "TL"
  } else {
    stop("No suitable length column (FL or TL) found in age_data or all values are NA.")
  }
  
  # 2. Filter data for valid entries and create Length_bin
  # Ensure IntAge and length_col_name are numeric for comparison
  dt[, (length_col_name) := as.numeric(get(length_col_name))]
  dt[, IntAge := as.integer(IntAge)]
  dt <- dt[!is.na(get(length_col_name)) & !is.na(IntAge) & IntAge >= 0L & IntAge <= as.integer(max_age)]
  
  if (nrow(dt) == 0) { # Early exit if no valid data after filtering
    terminal_row <- data.frame(
      yr = -9999L, month = 0L, fleet = 0L, sex = 0L, part = 0L, ageerr = 0L,
      Lbin_lo = 0L, Lbin_hi = 0L, Nsamp = 0L,
      datavector = I(list(pad_vector_optimized(rep(0, (as.integer(max_age) + 1L) * 2L), as.integer(max_age))))
    )
    return(terminal_row)
  }
  # dt[, Length_bin := floor(get(length_col_name) / 10)] # Assumes length is in mm, Length_bin in cm
  dt[, Length_bin := floor(get(length_col_name) / (SS_length_binwidth*10))] # Assumes length is in mm, Length_bin in cm
  
  # 3. Determine sex_code for each year within each original fleet name
  dt[, `:=`(
    has_males_in_year = any(Sex == "M", na.rm = TRUE),
    has_females_in_year = any(Sex == "F", na.rm = TRUE)
  ), by = .(fleet, year)] # Group by original character fleet name and year
  
  dt[, calculated_sex_code := fcase(
    has_males_in_year & has_females_in_year, 3L,
    has_females_in_year, 1L,
    has_males_in_year, 2L,
    default = 0L
  )]
  
  # Create a data.table of all ages from 0 to max_age for joins (completing data)
  all_ages_dt <- data.table(IntAge = 0L:as.integer(max_age), key = "IntAge")
  
  # 4. Group by relevant columns and calculate data_vector for each group
  # Group by the numeric fleet index, year, calculated sex code, and length bin
  processed_data <- dt[, {
    # .SD contains the subset of data for the current group
    current_group_data <- .SD 
    
    # Initialize counts for female and male portions of the data_vector
    female_age_counts <- rep(0L, as.integer(max_age) + 1L)
    male_age_counts <- rep(0L, as.integer(max_age) + 1L)
    
    # Use the first value of calculated_sex_code for the group (should be unique per group)
    group_sex_code <- .BY$calculated_sex_code 
    
    if (group_sex_code == 3L) { # Both sexes present in the year's data
      # Female data for this bin
      f_data_counts <- current_group_data[Sex == "F", .N, by = IntAge]
      setkey(f_data_counts, IntAge)
      f_data_complete <- f_data_counts[all_ages_dt, roll = FALSE] # Right join
      f_data_complete[is.na(N), N := 0L]
      female_age_counts <- f_data_complete$N
      
      # Male data for this bin
      m_data_counts <- current_group_data[Sex == "M", .N, by = IntAge]
      setkey(m_data_counts, IntAge)
      m_data_complete <- m_data_counts[all_ages_dt, roll = FALSE] # Right join
      m_data_complete[is.na(N), N := 0L]
      male_age_counts <- m_data_complete$N
      
    } else if (group_sex_code == 1L) { # Females only in the year's data
      # Data for this bin (explicitly filter by Sex as per original logic)
      f_data_counts <- current_group_data[Sex == "F", .N, by = IntAge]
      setkey(f_data_counts, IntAge)
      f_data_complete <- f_data_counts[all_ages_dt, roll = FALSE]
      f_data_complete[is.na(N), N := 0L]
      female_age_counts <- f_data_complete$N
      # male_age_counts remains all zeros
      
    } else if (group_sex_code == 2L) { # Males only in the year's data
      # Data for this bin (explicitly filter by Sex as per original logic)
      m_data_counts <- current_group_data[Sex == "M", .N, by = IntAge]
      setkey(m_data_counts, IntAge)
      m_data_complete <- m_data_counts[all_ages_dt, roll = FALSE]
      m_data_complete[is.na(N), N := 0L]
      male_age_counts <- m_data_complete$N
      # female_age_counts remains all zeros
      
    } else { # sex_code == 0L (Unsexed or combined for the year's data)
      # All data in this bin contributes to the 'female' part of the vector
      all_data_counts <- current_group_data[, .N, by = IntAge]
      setkey(all_data_counts, IntAge)
      all_data_complete <- all_data_counts[all_ages_dt, roll = FALSE]
      all_data_complete[is.na(N), N := 0L]
      female_age_counts <- all_data_complete$N
      # male_age_counts remains all zeros
    }
    
    # Combine female and male counts and pad the vector
    combined_age_dist <- c(female_age_counts, male_age_counts)
    data_vec_padded <- pad_vector_optimized(combined_age_dist, as.integer(max_age))
    
    # Return a list containing the datavector and calculated Nsamp
    list(
      datavector_list_col = list(data_vec_padded), 
      Nsamp_calculated = sum(data_vec_padded)
    )
  }, by = .(year, fleet_idx_numeric, calculated_sex_code, Length_bin)] # Grouping variables
  
  # Filter out groups where Nsamp_calculated is 0 (no actual age data for the bin)
  processed_data <- processed_data[Nsamp_calculated > 0]
  
  if (nrow(processed_data) == 0) { # If all bins ended up with Nsamp = 0
    terminal_row <- data.frame(
      yr = -9999L, month = 0L, fleet = 0L, sex = 0L, part = 0L, ageerr = 0L,
      Lbin_lo = 0L, Lbin_hi = 0L, Nsamp = 0L,
      datavector = I(list(pad_vector_optimized(rep(0, (as.integer(max_age) + 1L) * 2L), as.integer(max_age))))
    )
    return(terminal_row)
  }
  
  # Construct the final result table based on processed_data
  final_result_dt <- processed_data[, .(
    yr = year, # Original year from the group
    month = as.integer(month_val),   # From function argument
    fleet = fleet_idx_numeric, # Numeric fleet index from the group
    sex = calculated_sex_code, # Calculated sex code from the group
    part = as.integer(part_val),     # From function argument
    ageerr = as.integer(ageerr_val),   # From function argument
    Lbin_lo = Length_bin + 1L, # Original logic: lbin + 1
    Lbin_hi = Length_bin + 1L, # Original logic: lbin + 1
    Nsamp = Nsamp_calculated,  # Use the sum of the padded data_vector
    datavector = datavector_list_col # The list column containing padded vectors
  )]
  
  # 5. Final processing: order and add terminal row
  setorderv(final_result_dt, c("fleet", "yr", "Lbin_lo")) # Order as per original function
  
  terminal_row_dt <- data.table(
    yr = -9999L, month = 0L, fleet = 0L, sex = 0L, part = 0L, ageerr = 0L,
    Lbin_lo = 0L, Lbin_hi = 0L, Nsamp = 0L,
    datavector = list(pad_vector_optimized(rep(0, (as.integer(max_age) + 1L) * 2L), as.integer(max_age)))
  )
  
  # Combine the main results with the terminal row
  final_result_dt <- rbindlist(list(final_result_dt, terminal_row_dt), use.names = TRUE, fill = TRUE)
  
  # Convert to data.frame and ensure datavector column has I() for list column behavior
  final_df_output <- as.data.frame(final_result_dt)
  final_df_output$datavector <- I(final_df_output$datavector) # Crucial for exact match with original structure
  
  return(final_df_output)
}


cat("SS_use_age:",SS_use_age,"\n")

if (SS_use_age) {
  
  # cat("unique(combined_age_data$fleet):", unique(combined_age_data$fleet), "\n")
  
  # Process age data for all fleets
  SS_age_data_formatted <- SS_format_age_data(combined_age_data)
  
  if (SS_use_cond_age_length) {
    # Process conditional age-at-length data with fleet = -1
    SS_age_data_formatted <- SS_format_age_data(combined_age_data, Lbin_lo = -1, Lbin_hi = -1)
    SS_age_data_formatted$fleet <- -SS_age_data_formatted$fleet #-1
  }
  
}

# cat("DEBUG: Writing combined_age_data to file DELME.csv\n")
# write.csv(combined_age_data,"DELME.csv")


cat("SS_use_cond_age_length:",SS_use_cond_age_length,"\n")

# cat("\n")
# print(SS_age_data_formatted)
# cat("\n")
# 
# cat("before csv\n")
# write.csv(combined_age_data,"combined_age_data.csv")
# write.csv(SS_length_bins,"SS_length_bins.csv")
# write.csv(SS_n_age_bins,"SS_n_age_bins.csv")
# cat("after csv\n")

if (SS_use_cond_age_length) {
  
  ### BENCHMARKING
  
  time.start <- Sys.time()
  SS_cond_age_length <- SS_format_cond_age_length_optimised(
    age_data = combined_age_data,
    SS_length_bins_param = c(0, SS_length_bins), # Argument kept for signature matching
    max_age = SS_n_age_bins,
    part_val = 2, # As in your example call
    SS_fleet_details_df = SS_fleet_details # Pass the actual data frame here
  )
  opt.time <- Sys.time()-time.start
  
  # # You can then compare with your original output:
  # time.start <- Sys.time()
  # original_output <- SS_format_cond_age_length(
  #   age_data = combined_age_data,
  #   SS_length_bins = c(0,SS_length_bins),
  #   max_age = SS_n_age_bins,
  #   part = 2
  # )
  # orig.time <- Sys.time()-time.start
  
  # print("Time for Original Function:")
  # print(orig.time)
  
  print("Time for SS_format_cond_age_length_optimised Function:")
  print(opt.time)
  
  # print(all.equal(original_output, optimised_output, check.attributes = FALSE))
  # print(identical(original_output, optimised_output))
  
  # SS_cond_age_length <- SS_format_cond_age_length(
  #   age_data = combined_age_data,
  #   SS_length_bins = c(0,SS_length_bins), # Added zero here to help with indexing
  #   max_age = SS_n_age_bins,
  #   part = 2  # Set to 2 for retained data
  # )
  
  
  # Combine with age data, removing terminal row first
  SS_age_data_formatted <- rbind(SS_age_data_formatted[-nrow(SS_age_data_formatted), ], SS_cond_age_length)
}

SS_n_genders = 2

if(fishery_parameters$use_1_sex_model) {
  SS_n_genders = -1

  ## Modify part Code if 1 Sex model
  SS_length_data_formatted$part <- 0
  SS_age_data_formatted$part <- 0
  SS_cond_age_length$part <- 0
  
  SS_cond_age_length$datavector
  
  cat("\n")
  print(SS_length_data_formatted)
  cat("\n")
  # cat("length datavector",ncol(SS_cond_age_length$datavector[1]),"\n")
  
  cat("\n")
  str(SS_length_data_formatted)
  cat("\n")
  
  # TRim the extra zeroes off the comps for sinlge sex model
  SS_length_data_formatted$datavector <- lapply(SS_length_data_formatted$datavector, function(vec) {
    # Calculate the halfway point of the vector
    halfway <- floor(length(vec) / 2)
    # Return the first half of the numeric vector
    vec[1:halfway]
  })
  
  SS_age_data_formatted$datavector <- lapply(SS_age_data_formatted$datavector, function(vec) {
    # Calculate the halfway point of the vector
    halfway <- floor(length(vec) / 2)
    # Return the first half of the numeric vector
    vec[1:halfway]
  })
  
  SS_cond_age_length$datavector <- lapply(SS_cond_age_length$datavector, function(vec) {
    # Calculate the halfway point of the vector
    halfway <- floor(length(vec) / 2)
    # Return the first half of the numeric vector
    vec[1:halfway]
  })
  
  
}

if(species == "Pristipomoides multidens") {
  SS_size_selex_L50_HI <- 100
  # SS_size_selex_L50 <- 30
  # SS_last_year_recr_devs <- 2021
}

generate_data_ss <- function(start_year = SS_start_year, end_year = SS_end_year, n_seasons = 1, months_per_season = 12,
                             n_subseasons = 2, spawn_month = 1, n_genders = SS_n_genders, n_ages = SS_n_age_bins-1 , n_areas = 1,
                             n_fleets = SS_n_fleets, fleet_details = SS_fleet_details,
                             # fleet_details = data.frame(fleet_type = rep(1,n_fleets), 
                             #                                          fishery_timing = rep(-1,n_fleets), 
                             #                                          area = rep(1,n_fleets), 
                             #                                          catch_units = rep(1,n_fleets), 
                             #                                          need_catch_mult = rep(0,n_fleets), 
                             #                                          fleetname = SS_fleetnames),
                             catch_data_datfile = SS_catch_data, cpue_data = SS_cpue_data, use_length_data = TRUE, use_age_data = SS_use_age, n_age_bins = SS_n_age_bins, 
                             age_bins = SS_age_bins, age_composition_data = SS_age_data_formatted,
                             comment = "data file comment here", species = BiologicalUnitNamesearch,
                             output_file = paste0(outputdir,"datafile.dat")) {
  
  # Default header for Stock Synthesis data file
  lines <- c(
    "#V3.30.19.00;_safe;_compile_date:_Apr  4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3",
    "#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.",
    "#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.",
    "#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov",
    "#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis",
    "#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis",
    "",
    "#_Start_time: Tue Apr 12 09:30:51 2022",
    paste0("# ", species),
    "#_echo_input_data",
    paste0("#_Generated on ", Sys.time(),". ", comment)
  )
  
  # Basic model structure
  lines <- c(lines,
             paste(start_year, "#_StartYr"),
             paste(end_year, "#_EndYr"),
             paste(n_seasons, "#_Nseas"),
             paste(months_per_season, "#_months/season"),
             paste(n_subseasons, "#_Nsubseasons (even number, minimum is 2)"),
             paste(spawn_month, "#_spawn_month"),
             paste(n_genders, "#_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)"),
             # paste(n_ages, "#_Nages=accumulator age, first age is always age 0"),
             paste(n_ages+1, "#_Nages=accumulator age, first age is always age 0"),
             paste(n_areas, "#_Nareas"),
             paste(n_fleets, "#_Nfleets (including surveys) ## !!! 1=commercial, 2=cpue (comm)"),
             "#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=predator(M2)",
             "#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)",
             "#_fleet_area:  area the fleet/survey operates in",
             "#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)",
             "#_catch_mult: 0=no; 1=yes",
             "#_rows are fleets",
             "#_fleet_type fishery_timing area catch_units need_catch_mult fleetname"
  )
  
  # cat("Debug: before fleet \n")
  
  # Add fleet details
  for (i in 1:nrow(fleet_details)) {
    lines <- c(lines,
               sprintf("%d %d %d %d %d %s  # %d %s",
                       fleet_details$fleet_type[i], fleet_details$fishery_timing[i],
                       fleet_details$area[i], fleet_details$catch_units[i],
                       fleet_details$need_catch_mult[i], fleet_details$fleetname[i],
                       i, fleet_details$fleetname[i]))
  }
  
  # cat("Debug: before Catch \n")
  
  # Catch data section
  lines <- c(lines,
             "#",
             "## !!!!! CATCH DATA !!!!! ##",
             "#_Catch data: yr, seas, fleet, catch, catch_se",
             "#_catch_se:  standard error of log(catch)",
             "#_NOTE:  catch data is ignored for survey fleets",
             "# yr, seas, fleet, catch, catch_se")
  

  
  for (i in 1:nrow(catch_data_datfile)) {
    lines <- c(lines,
               sprintf("%d\t%d\t%d\t%.3f\t%.3f", catch_data_datfile$yr[i], catch_data_datfile$seas[i],
                       catch_data_datfile$fleet[i], catch_data_datfile$catch[i], catch_data_datfile$catch_se[i]))
  }
  
  # Indices (CPUE) section
  lines <- c(lines,
             "#",
             "## !!!!! INDICES SECTION !!!!! ##",
             "# _CPUE_and_surveyabundance_observations",
             "#_Units:  0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)",
             "#_Errtype:  -1=normal; 0=lognormal; >0=T",
             "#_SD_Report: 0=no sdreport; 1=enable sdreport",
             "#_Fleet Units Errtype SD_Report")
  
  # Add fleet-specific CPUE setup lines
  for (i in 1:nrow(fleet_details)) {
    lines <- c(lines,
               sprintf("%d %d %d %d # %s",
                       i, 1, 0, 0, fleet_details$fleetname[i]))
  }
  
  lines <- c(lines,
             "#_yr month fleet obs stderr")
  for (i in 1:nrow(cpue_data)) {
    lines <- c(lines,
               sprintf("%d\t%d\t%d\t%.3f\t%.3f",
                       cpue_data$yr[i], cpue_data$month[i], cpue_data$fleet[i],
                       cpue_data$obs[i], cpue_data$stderr[i]))
  }
  
  # cat("Debug: before generate_fleet_lines \n")
  
  generate_fleet_lines <- function(SS_n_fleets, SS_fleetnames) {
    if (SS_n_fleets != length(SS_fleetnames)) {
      stop("SS_n_fleets and SS_fleetnames length must match")
    }
    
    fleet_lines <- character(SS_n_fleets)
    for (i in 1:SS_n_fleets) {
      fleet_lines[i] <- paste0("-1 0.001 0 0 0 0 0.001 #_fleet:", i, "_", SS_fleetnames[i])
    }
    return(fleet_lines)
  }
  
  fleet_lines <- generate_fleet_lines(SS_n_fleets, SS_fleetnames)
  
  

  
  # # Initialize a counter outside the modification function to maintain the sequential number. Used in function modify_fleet_lines
  # sequential_counter_Dirichlet <- 0
  
  # Function to modify fleet lines for dirichlet
  modify_fleet_lines <- function(fleet_lines, Lencomp_fleets) {
    if (length(Lencomp_fleets) > 0) {
      for (i in 1:length(Lencomp_fleets)) {
        fleet_index <- Lencomp_fleets[i]
        if (fleet_index >= 1 && fleet_index <= length(fleet_lines)) {
          parts <- strsplit(fleet_lines[fleet_index], " ")[[1]]
          if (length(parts) >= 7) {
            parts[5] <- "1"
            sequential_counter_Dirichlet <<- sequential_counter_Dirichlet + 1 # Use <<- to update the global counter
            # cat("sequential_counter_Dirichlet",sequential_counter_Dirichlet,"\n")
            parts[6] <- as.character(sequential_counter_Dirichlet)
            fleet_lines[fleet_index] <- paste(parts, collapse = " ")
          } else {
            warning(paste("Unexpected format in fleet_lines at index:", fleet_index))
          }
        } else {
          warning(paste("Lencomp_fleets index out of bounds:", fleet_index))
        }
      }
    }
    return(fleet_lines)
  }
  
  lines <- c(lines,
             "#",
             "## !!!!! DISCARD SECTION !!!!! ##",
             "0 #_N_fleets_with_discard",
             "#",
             "## !!!!! MEAN BODY WEIGHT OR LENGTH SECTION !!!!! ##",
             "0 #_use meanbodysize_data (0/1)",
             "#",
             "## !!!!! POPULATION LENGTH BINS !!!!! ##",
             "2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector",
             # "1 # binwidth for population size comp", ## !!! Make dynamic
             paste(SS_length_binwidth,"# binwidth for population size comp"),
             "0 # minimum size in the population (lower edge of first bin and size at age 0.00)",
             # "76 # maximum size in the population (lower edge of last bin)",
             paste(SS_max_size, "# maximum size in the population (lower edge of last bin)"),
             "#",
             "## !!!!! LENGTH COMPOSITION SECTION !!!!! ##")
  
  if(use_length_data){
    
    length_composition_data <- SS_length_data_formatted
    
    # cat("Lencomp fleetlines: \n")
    # print(as.data.frame(fleet_lines))
    # cat("\n")
    
    
    # If Use Dirichlet Multinomial, must modify fleetlines objects where there is comp data
    if(SS_Use_ln_dm_theta_lines){
      
      # Get fleets with length comps, excluding zeroes
      Lencomp_fleets <- unique(length_composition_data$fleet[length_composition_data$fleet != "0"])
      cat("unique(length_composition_data$fleet)",Lencomp_fleets,"\n")

      Lencomp_fleet_lines <- modify_fleet_lines(fleet_lines, Lencomp_fleets)
      
    } else {
      
      Lencomp_fleet_lines <- fleet_lines
    }
    
    # cat("Lencomp fleetlines: \n")
    # print(as.data.frame(Lencomp_fleet_lines))
    # cat("\n")
    # 
    # lines <- c(lines,
    #            "1 # use length composition data (0/1)",
    #            "#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize",
    #            # "-1 0.001 0 0 0 0 0.001 #_fleet:1_FISHERY1",
    #            Lencomp_fleet_lines,
    #            "# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution",
    #            "# partition codes:  (0=combined; 1=discard; 2=retained)",
    #            paste(SS_n_length_bins+1, "#_N_LengthBins; then enter lower edge of each length bin"),
    #            # paste(SS_length_bins, collapse = "\t"),
    #            paste(c(0,SS_length_bins), collapse = "\t"),
    #            paste("#_Year	Month	Fleet	Sex	Part	Nsamp	0",paste(SS_length_bins, collapse = "\t"))
    # )
    
    lines <- c(lines,
               "1 # use length composition data (0/1)",
               "#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize",
               Lencomp_fleet_lines,
               "# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution",
               "# partition codes:  (0=combined; 1=discard; 2=retained)",
               paste(SS_n_length_bins, "#_N_LengthBins; then enter lower edge of each length bin"),
               paste(SS_length_bins, collapse = "\t"),
               paste("#_Year	Month	Fleet	Sex	Part	Nsamp",paste(SS_length_bins, collapse = "\t"))
    )
    
    

    
    for (i in 1:nrow(length_composition_data)) {
      lines <- c(lines,
                 sprintf("%d\t%d\t%d\t%d\t%d\t%d\t%s",
                         # sprintf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s",
                         length_composition_data$yr[i], length_composition_data$month[i],
                         length_composition_data$fleet[i], length_composition_data$sex[i],
                         length_composition_data$part[i],
                         length_composition_data$Nsamp[i],
                         paste(length_composition_data$datavector[[i]], collapse = "\t")))
      
    }
    
  } else {
    # Turn off Length data
    lines <- c(lines,
               "0 # use length composition data (0/1)")
    
  }
  
  if (use_age_data) {
    
    # cat("Agecomp fleetlines: \n")
    # print(as.data.frame(fleet_lines))
    # cat("\n")
    
    
    # If Use Dirichlet Multinomial, must modify fleetlines objects where there is comp data
    if(SS_Use_ln_dm_theta_lines){
      
      # Get fleets with length comps, excluding zeroes
      Agecomp_fleets <- unique(age_composition_data$fleet[age_composition_data$fleet != "0"])
      cat("unique(age_composition_data$fleet)",Agecomp_fleets,"\n")
      
      Agecomp_fleet_lines <- modify_fleet_lines(fleet_lines, Agecomp_fleets)
      
    } else {
      
      Agecomp_fleet_lines <- fleet_lines
    }
    
    # cat("Agecomp fleetlines: \n")
    # print(as.data.frame(Agecomp_fleet_lines))
    # cat("\n")
    
    lines <- c(lines,
               "#",
               # "## !!!!! AGE COMPOSITION SECTION !!!!! ##",
               # paste(n_age_bins+1, "#_N_age_bins"),
               # paste(c(0,age_bins), collapse = "\t"),
               # "1 #_N_ageerror_definitions",
               "## !!!!! AGE COMPOSITION SECTION !!!!! ##",
               paste(length(age_bins), "#_N_age_bins"),
               paste(age_bins, collapse = "\t"),
               "1 #_N_ageerror_definitions",
               # paste(rep("-1",n_age_bins), collapse = "\t"),
               # paste(rep("0.001",n_age_bins), collapse = "\t"),
               paste(rep("-1",n_age_bins+1), collapse = "\t"),
               paste(rep("0.001",n_age_bins+1), collapse = "\t"),
               "#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize",
               Agecomp_fleet_lines,
               "1 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths",
               "#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)")
    

    age_composition_data <- SS_age_data_formatted
    
    for (i in 1:nrow(age_composition_data)) {
      lines <- c(lines,
                 # sprintf("%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%s",
                 sprintf("%d\t%d\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%s",        
                         age_composition_data$yr[i], age_composition_data$month[i],
                         age_composition_data$fleet[i], age_composition_data$sex[i],
                         age_composition_data$part[i], age_composition_data$ageerr[i],
                         age_composition_data$Lbin_lo[i], age_composition_data$Lbin_hi[i],
                         age_composition_data$Nsamp[i],
                         paste(age_composition_data$datavector[[i]], collapse = "\t")))
    }
    
  } else {
    
    # Turn off Age data
    lines <- c(lines,
               "## !!!!! AGE COMPOSITION SECTION !!!!! ##",
               "0 #_N_age_bins")
  }
  
  # Additional placeholder sections
  lines <- c(lines,
             "#",
             "## !!!!! MEAN LENGTH OR BODY WEIGHT_AT_AGE !!!!! ##",
             "0 #_Use_MeanSize-at-Age_obs (0/1)",
             "#",
             "## !!!!! ENVIRONMENTAL DATA !!!!! ##",
             "0 #_N_environ_variables",
             "#",
             "## !!!!! GENERALISED SIZE COMPOSITION DATA !!!!! ##",
             "0 # N sizefreq methods to read",
             "#",
             "## !!!!! TAG_RECAPTURE DATA !!!!! ##",
             "0 # do tags (0/1/2); where 2 allows entry of TG_min_recap",
             "#",
             "## !!!!! STOCK COMPOSITION DATA (MORPH) !!!!! ##",
             "0 #    morphcomp data(0/1)",
             "#",
             "## !!!!! SELECTIVITY EMPIRICAL DATA (FUTURE FEATURE) !!!!! ##",
             "0  #  Do dataread for selectivity priors(0/1)",
             "#",
             "## !!!!! END OF DATA FILE MARKER !!!!! ##",
             "999")
  
  # Write the file
  writeLines(lines, output_file)
  # cat(paste("File", output_file, "created successfully.\n"))
  cat("datafile.dat created successfully.\n")
}

# Example usage:
generate_data_ss()

# Generate control file after datafile as some bits are dependdent on whats in the datafile generations

cat("SS_Use_ln_dm_theta_lines:",SS_Use_ln_dm_theta_lines,"\n")
cat("sequential_counter_Dirichlet:",sequential_counter_Dirichlet,"\n")
# cat("Agecomp_fleets",Agecomp_fleets,"\n")

cat("Start of Control File \n")

#$$$$$
cat("SS_Age_Post_Settlement:",SS_Age_Post_Settlement,"\n")
cat("SS_CV_Growth_Pattern:",SS_CV_Growth_Pattern,"\n")

generate_control_ss(datafile = "data.ss", output_file = paste0(outputdir,"controlfile.ctl"))


# Generate forecast file -------------------------------------------------------

generate_forecast_ss <- function(benchmarks = 1, do_msy = 2, spr_target = 0.4, biomass_target = 0.4,
                                 bmark_years = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), bmark_relf_basis = 1,
                                 # forecast_option = 2, n_forecast_years = 10, fmult = 1,
                                 forecast_option = 2, n_forecast_years = 10, fmult = 1,
                                 fcast_years = c(0, 0, 0, 0, 0, 0), forecast_selectivity = 0,
                                 control_rule_method = 0, cr_biomass_constant_f = 0.3, cr_biomass_no_f = 0.2,
                                 buffer = 1, n_forecast_loops = 3, first_loop_stochastic_recr = 1,
                                 forecast_recruitment = 1, recr_multiplier = 1, fcast_loop_control_5 = 0,
                                 first_year_caps_alloc = 2035, stddev_log_catch = 0, do_rebuilder = 0,
                                 rebuilder_ydecl = 1999, rebuilder_yinit = 2002, fleet_rel_f = 1,
                                 basis_fcast_catch = 3, forecast_catch_data = NULL,
                                 comment = "generic forecast file", output_file = paste0(outputdir,"forecast.ss")) {
  
  # Default header for Stock Synthesis forecast file
  lines <- c(
    "#V3.30.19.00;_safe;_compile_date:_Apr  4 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_12.3",
    paste0("#_Generated on ", Sys.time(),". ", comment),
    "# for all year entries except rebuilder; enter either: actual year, -999 for styr, 0 for endyr, neg number for rel. endyr"
  )
  
  # Benchmarks section
  lines <- c(lines,
             paste(benchmarks, "# Benchmarks: 0=skip; 1=calc F_spr,F_btgt,F_msy; 2=calc F_spr,F0.1,F_msy; 3=add F_Blimit;"),
             paste(do_msy, "# Do_MSY: 1= set to F(SPR); 2=calc F(MSY); 3=set to F(Btgt) or F0.1; 4=set to F(endyr); 5=calc F(MEY) with MSY_unit options"),
             "# if Do_MSY=5, enter MSY_Units; then list fleet_ID, cost/F, price/mt, include_in_Fmey_scaling; # -fleet_ID to fill; -9999 to terminate",
             paste(sprintf("%.1f", spr_target), "# SPR target (e.g. 0.40)"),
             paste(sprintf("%.1f", biomass_target), "# Biomass target (e.g. 0.40)"),
             "#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF, beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)",
             paste(bmark_years, collapse = "  "),
             "# value <0 convert to endyr-value; except -999 converts to start_yr; must be >=start_yr and <=endyr",
             paste(bmark_relf_basis, "#Bmark_relF_Basis: 1 = use year range; 2 = set relF same as forecast below"),
             "#")
  
  # Forecast section
  lines <- c(lines,
             paste(forecast_option, "# Forecast: -1=none; 0=simple_1yr; 1=F(SPR); 2=F(MSY) 3=F(Btgt) or F0.1; 4=Ave F (uses first-last relF yrs); 5=input annual F scalar"),
             "# where none and simple require no input after this line; simple sets forecast F same as end year F",
             paste(n_forecast_years, "# N forecast years"),
             paste(fmult, "# Fmult (only used for Do_Forecast==5) such that apical_F(f)=Fmult*relF(f)"),
             "#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF, beg_mean recruits, end_recruits  (enter actual year, or values of 0 or -integer to be rel. endyr)",
             paste(fcast_years, collapse = " "),
             paste(forecast_selectivity, "# Forecast selectivity (0=fcast selex is mean from year range; 1=fcast selectivity from annual time-vary parms)"),
             paste(control_rule_method, "# Control rule method (0: none; 1: ramp does catch=f(SSB), buffer on F; 2: ramp does F=f(SSB), buffer on F; 3: ramp does catch=f(SSB), buffer on catch; 4: ramp does F=f(SSB), buffer on catch)"),
             "# values for top, bottom and buffer exist, but not used when Policy=0",
             paste(cr_biomass_constant_f, "# Control rule Biomass level for constant F (as frac of Bzero, e.g. 0.40); (Must be > the no F level below)"),
             paste(cr_biomass_no_f, "# Control rule Biomass level for no F (as frac of Bzero, e.g. 0.10)"),
             paste(buffer, "# Buffer:  enter Control rule target as fraction of Flimit (e.g. 0.75), negative value invokes list of [year, scalar] with filling from year to YrMax"),
             paste(n_forecast_loops, "#_N forecast loops (1=OFL only; 2=ABC; 3=get F from forecast ABC catch with allocations applied)"),
             paste(first_loop_stochastic_recr, "#_First forecast loop with stochastic recruitment"),
             paste(forecast_recruitment, "#_Forecast recruitment:  0= spawn_recr; 1=value*spawn_recr_fxn; 2=value*VirginRecr; 3=recent mean from yr range above (need to set phase to -1 in control to get constant recruitment in MCMC)"),
             paste(recr_multiplier, "# value is multiplier of SRR"),
             paste(fcast_loop_control_5, "#_Forecast loop control #5 (reserved for future bells&whistles)"),
             paste(first_year_caps_alloc, "#FirstYear for caps and allocations (should be after years with fixed inputs)"),
             paste(stddev_log_catch, "# stddev of log(realized catch/target catch) in forecast (set value>0.0 to cause active impl_error)"),
             paste(do_rebuilder, "# Do West Coast gfish rebuilder output: 0=no; 1=yes"),
             paste(rebuilder_ydecl, "# Rebuilder:  first year catch could have been set to zero (Ydecl)(-1 to set to 1999)"),
             paste(rebuilder_yinit, "# Rebuilder:  year for current age structure (Yinit) (-1 to set to endyear+1)"),
             paste(fleet_rel_f, "# fleet relative F:  1=use first-last alloc year; 2=read seas, fleet, alloc list below"),
             "# Note that fleet allocation is used directly as average F if Do_Forecast=4",
             paste(basis_fcast_catch, "# basis for fcast catch tuning and for fcast catch caps and allocation  (2=deadbio; 3=retainbio; 5=deadnum; 6=retainnum); NOTE: same units for all fleets"),
             "# Conditional input if relative F choice = 2",
             "# enter list of:  season,  fleet, relF; if used, terminate with season=-9999",
             "# -9999 0 0  # terminator for list of relF",
             "# enter list of: fleet number, max annual catch for fleets with a max; terminate with fleet=-9999",
             "-9999 -1",
             "# enter list of area ID and max annual catch; terminate with area=-9999",
             "-9999 -1",
             "# enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999",
             "-9999 -1",
             "#_if N allocation groups >0, list year, allocation fraction for each group",
             "# list sequentially because read values fill to end of N forecast",
             "# terminate with -9999 in year field",
             "# no allocation groups",
             "#")
  
  # Forecast catch section
  lines <- c(lines,
             paste(basis_fcast_catch, "# basis for input Fcast catch: -1=read basis with each obs; 2=dead catch; 3=retained catch; 99=input apical_F; NOTE: bio vs num based on fleet's catchunits"),
             "#enter list of Fcast catches or Fa; terminate with line having year=-9999",
             "#_Yr Seas Fleet Catch(or_F)")
  
  if (is.null(forecast_catch_data)) {
    # Default forecast catch data if none provided
    forecast_catch_data <- data.frame(
      yr = c(seq(2023, 2032, by = 1), -9999),
      seas = c(rep(1, 10), 1),
      fleet = c(rep(1, 10), 1),
      catch_or_f = c(rep(170, 10), 0)
    )
  }
  
  for (i in 1:nrow(forecast_catch_data)) {
    lines <- c(lines,
               sprintf("%d\t%d\t%d\t%.5f",
                       forecast_catch_data$yr[i], forecast_catch_data$seas[i],
                       forecast_catch_data$fleet[i], forecast_catch_data$catch_or_f[i]))
  }
  
  # End of file marker
  lines <- c(lines,
             "#",
             "999 # verify end of input")
  
  # Write the file
  writeLines(lines, output_file)
  # cat(paste("File", output_file, "created successfully.\n"))
  cat("forecast.ss created successfully.\n")
}

# # Example usage:
# # Define sample forecast catch data
# sample_forecast_catch <- data.frame(
#   yr = c(seq(2023, 2032, by = 1), -9999),
#   seas = c(rep(1, 10), 1),
#   fleet = c(rep(1, 10), 1),
#   catch_or_f = c(rep(170, 10), 0)
# )

## Dynamic forecast object
# Define the forecast years
start_year <- 2025
end_year <- 2034
forecast_years <- seq(start_year, end_year, by = 1)

# Define the catch values for each fleet (must be the same length as the number of fleets)
# This vector should correspond to the order of fleets in SS_fleet_details
forecast_catches <- rep(170, nrow(SS_fleet_details))

## DEBUG testing forecast catches for Red Emp Kim
if(species == "Lutjanus sebae") {
  cat("DEBUG: TESTING MANUAL FORECAST CATCHES FOR RED EMP KIM\n")

## Rubie 

## SO hardcoding that should be
  # options(digits = 10)
# forecast_catches <- c(0,150.7,0,0,0,1.209,0.219,0)
forecast_catches <- c(0,0,0,0,150.7,0,1.209,0.219)
}

if(species == "Pristipomoides multidens") {
  cat("DEBUG: TESTING MANUAL FORECAST CATCHES FOR GOLDBAND KIM\n")
  
  ## Rubie 
  
  ## SO hardcoding that should be
  # options(digits = 10)
  # forecast_catches <- c(0,150.7,0,0,0,1.209,0.219,0)
  forecast_catches <- c(0,0,0,0,435.538,0,0.09,0.018)
}

if(species == "Epinephelus rankini") {
  cat("DEBUG: TESTING MANUAL FORECAST CATCHES FOR Rankin Pilbara\n")
  
  forecast_catches <- c(0,159,0,13.16,5.46,0,0,0,0)

}

# Check if the length of forecast_catches matches the number of fleets
if (length(forecast_catches) != nrow(SS_fleet_details)) {
  stop("The length of 'forecast_catches' must match the number of fleets in 'SS_fleet_details'.")
}
# Create a list to store data frames for each fleet
forecast_list <- list()
# Loop through each fleet
for (i in 1:nrow(SS_fleet_details)) {
  fleet_number <- i # Or some other identifier if your fleet numbers are not sequential 1,2,3...
                   # If SS_fleet_details has an actual fleet ID column, use that.
                   # For now, we assume fleet numbers are 1, 2, 3... corresponding to rows.
  fleet_forecast_data <- data.frame(
    yr = forecast_years,
    seas = 1, # Assuming seas is always 1 for forecast data as in your original example
    fleet = fleet_number,
    catch_or_f = forecast_catches[i]
  )
  # Filter out rows where catch_or_f is 0 for this specific fleet
  fleet_forecast_data <- fleet_forecast_data[fleet_forecast_data$catch_or_f != 0, ]
  forecast_list[[i]] <- fleet_forecast_data
}
# Combine all fleet forecast data into one data frame
forecast_catch_data <- do.call(rbind, forecast_list)

# Add the terminator row
terminator_row <- data.frame(
  yr = -9999,
  seas = 0,
  fleet = 0, 
  catch_or_f = 0
)

forecast_catch_data <- rbind(forecast_catch_data, terminator_row)

# Ensure column names are correct
names(forecast_catch_data) <- c("yr", "seas", "fleet", "catch_or_f")

# # Print the resulting data frame (optional)
# cat("\n")
# print(forecast_catch_data)
# cat("\n")

# Generate the forecast file
# generate_forecast_ss(forecast_catch_data = sample_forecast_catch)
generate_forecast_ss(forecast_catch_data = forecast_catch_data)

cat("\n SS_input.R run complete \n")
