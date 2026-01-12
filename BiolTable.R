# Biol table

# Load biological parameters data
# load("interim_data/bupBiologicalUnitParam.RData")
# 
# file_path <- "interim_data/bupBiologicalUnitParam.RData"
# created_time <- file.info(file_path)$mtime
# 
# message("Biological parameters extracted:", format(created_time), "\n")

# --- START: MODIFIED BLOCK ---
file_path <- "interim_data/bupBiologicalUnitParam.RData"

if (file.exists(file_path)) {
  # Load biological parameters data (this will create the object bupBiologicalUnitParam)
  load(file_path)
  created_time <- file.info(file_path)$mtime
  message("Biological parameters extracted:", format(created_time), "\n")
  
  # Check if the load operation was successful and defined the expected object
  if (!exists("bupBiologicalUnitParam") || !is.data.frame(bupBiologicalUnitParam)) {
    warning("bupBiologicalUnitParam.RData found, but did not load the expected 'bupBiologicalUnitParam' object. Falling back to dummy data.")
    # Define fallback data if the file existed but was malformed
    bupBiologicalUnitParam <- data.frame(BiologicalUnitName = character(0), RSSpeciesCode = integer(0), stringsAsFactors = FALSE)
  }
} else {
  # File does not exist: Define fallback data structure
  message("INFO: Biological parameter file not found. Using empty structure for UI logic.")
  bupBiologicalUnitParam <- data.frame(BiologicalUnitName = character(0), RSSpeciesCode = integer(0), stringsAsFactors = FALSE)
}
# --- END: MODIFIED BLOCK ---

bupBiologicalUnitParam$CSIRO <- bupBiologicalUnitParam$RSSpeciesCode

# Function to create a formatted bibliography string
create_bibliography_string <- function(bib_data, cited_keys) {
  if (is.null(bib_data) || length(bib_data) == 0) {
    return("No references available.")
  }
  if (is.null(cited_keys) || length(cited_keys) == 0) {
    return("No references cited in the caption.")
  }
  
  # Remove leading "@" from keys
  cited_keys <- sub("^@", "", cited_keys)
  
  # Filter bib_data to include only cited keys
  bib_data <- bib_data[names(bib_data) %in% cited_keys]
  
  if (length(bib_data) == 0) {
    return("No valid references found for the cited keys.")
  }
  
  tryCatch({
    formatted_bibliography <- RefManageR::PrintBibliography(
      bib_data,
      .opts = list(
        style = "html",
        bib.style = "apa"
      )
    )
    return(formatted_bibliography)
  }, warning = function(w) {
    message("Warning in bibliography formatting: ", w$message)
    return("Some references could not be formatted due to missing fields.")
  }, error = function(e) {
    message("Error in bibliography formatting: ", e$message)
    return("Error formatting bibliography.")
  })
}

if (create_biol_table) {
  # ===================================================================
  # 1. DEFINE MASTER TEMPLATES (Applied to ALL species)
  # ===================================================================
  # This is the desired presentation order for all parameters, based on your 'correct' example.
  master_order <- c("__Growth__", "GrowthParams1", "GrowthParams2", "GrowthParams3", "MaxObservedAge", 
                    "MaximumLength", "NaturalMortality", "__Length-weight (g)__", "LengthWeightParams1", 
                    "LengthWeightParams2", "__Maturity__", "MaturityParams1", "MaturityParams2", 
                    "MaturityParams3", "MaturityParams4", "__Fecundity__", "FecundityParams_1", 
                    "FecundityParams_2", "SpawningFrequency", "__Sex change__", 
                    "SexChangeParams1", "SexChangeParams2", "SexChangeParams3", "SexChangeParams4", 
                    "__Length-length__", "LengthRelationshipParams_1", "LengthRelationshipParams_2")
  
  # This is the master dictionary for converting parameter names to formatted text.
  master_clean_names <- data.frame(
    stringsAsFactors = FALSE,
    Parameter = c("GrowthParams1", "GrowthParams2", "GrowthParams3", "MaturityParams1", "MaturityParams2", 
                  "MaturityParams3", "MaturityParams4", "LengthWeightParams1", "LengthWeightParams2", 
                  "MaxObservedAge", "NaturalMortality", "LengthRelationshipParams_1", "LengthRelationshipParams_2", 
                  "MaximumLength", "SexRatioFemale", "SexChangeParams1", "SexChangeParams2", 
                  "SexChangeParams3", "SexChangeParams4", "FecundityParams_1", "FecundityParams_2",
                  "L1r", "L1K", "L1DepletionParamsInit_1", "L1DepletionParamsInit_2", 
                  "L1DepletionParamsFinal_1", "L1DepletionParamsFinal_2", "SpawningFrequency"),
    Param.clean = c("$L_{\\infty}$ (mm)", "$k$ ($yr^{-1}$)", "$t_0$ (yrs)", "$L_{50}$ (mm)", 
                    "$L_{95}$ (mm)", "$A_{50}$ (yrs)", "$A_{95}$ (yrs)", "<br> $a$", "<br> $b$", 
                    "Max. age (yrs)", "Natural mortality, $M$ ($yr^{-1}$)", "<br> $a$", "<br> $b$", 
                    "Max. length (mm)", "Sex Ratio Female", "$L_{50}$ (mm)", 
                    "$L_{95}$ (mm)", "$A_{50}$ (yrs)", "$A_{95}$ (yrs)", "<br> $a$", "<br> $b$",
                    "$r$ prior", "$K$ prior", "$d_1$ prior", "$d_2$ prior", "$d_1$ final", "$d_2$ final",
                    "Spawning Frequency")
  )
  
  # ===================================================================
  # 2. DATA PROCESSING (Same as your original code)
  # ===================================================================
  parameters <- subset(bupBiologicalUnitParam, BiologicalUnitName == selected_bio_species)
  
  fields_to_split <- c("L1DepletionParamsInit", "L1DepletionParamsFinal", "GrowthUnits", "GrowthParamsFemale",  
                       "GrowthParamsMale", "MaturityParamsFemale", "MaturityParamsMale", "LengthWeightParamsFemale",  
                       "LengthWeightParamsMale", "SexChangeParamsFemale", "SexChangeParamsMale",  
                       "LengthRelationshipParams", "FecundityParams")
  
  parameters_s <- suppressWarnings(splitstackshape::cSplit(parameters, fields_to_split, sep = "|"))
  
  tabdat <- data.frame(Parameter = colnames(parameters_s), Value = t(parameters_s))
  rownames(tabdat) <- NULL
  colnames(tabdat) <- c("Parameter", "Value")
  male_var <- tabdat[tabdat$Parameter %like% "Male", ]
  tabdat <- tabdat[!tabdat$Parameter %like% "Male", ]
  
  comment_var <- tabdat[tabdat$Parameter %like% "Type" | tabdat$Parameter == "GrowthCurve" |  
                          tabdat$Parameter == "LengthWeightFunction" | tabdat$Parameter == "MaturityCurve", ]
  tabdat <- tabdat %>% filter(!grepl('Type', Parameter))
  tabdat <- tabdat[tabdat$Parameter != "GrowthCurve" & tabdat$Parameter != "LengthWeightFunction" &  
                     tabdat$Parameter != "MaturityCurve", ]
  tabdat$Parameter <- gsub("Female_", "", x = tabdat$Parameter)
  male_var$Parameter <- gsub("Male_", "", x = male_var$Parameter)
  tabdat <- left_join(tabdat, male_var, by = "Parameter")
  
  colnames(tabdat) <- c("Parameter", "Female", "Male")
  tabdat <- tabdat[!with(tabdat, is.na(Female) & is.na(Male)), ]
  
  headers <- c("__Catch-MSY__", "__Growth__", "__Maturity__", "__Length-weight (g)__",  
               "__Sex change__", "__Fecundity__", "__Length-length__")
  
  addEmptyRows <- function(D) {
    output <- tabdat
    headerpos <- 0
    i <- 1
    while (i < NROW(tabdat)) {
      if (tabdat$Parameter[i] %in% c("L1DepletionParamsInit_1", "GrowthParams1", "MaturityParams1",  
                                     "LengthWeightParams1", "SexChangeParams1", "FecundityParams_1",  
                                     "LengthRelationshipParams_1")) {
        headerpos <- match(tabdat$Parameter[i], c("L1DepletionParamsInit_1", "GrowthParams1",  
                                                  "MaturityParams1", "LengthWeightParams1",  
                                                  "SexChangeParams1", "FecundityParams_1",  
                                                  "LengthRelationshipParams_1"))
        tabdat <- rbind(tabdat[1:i-1, ], c(headers[headerpos], NA, NA), tabdat[i:NROW(tabdat), ])
        i <- i + 1
      }
      i <- i + 1
    }
    return(rbind(c(headers[headerpos], NA, NA), tabdat))
  }
  
  tabdat <- addEmptyRows(tabdat)
  tabdat <- tabdat[-1, ]
  
  colnames(comment_var)[2] <- "Comments"
  comment_var$Parameter <- gsub("GrowthCurve", "__Growth__", comment_var$Parameter)
  comment_var$Parameter <- gsub("MaturityCurve", "__Maturity__", comment_var$Parameter)
  comment_var$Parameter <- gsub("LengthWeightFunction", "__Length-weight (g)__", comment_var$Parameter)
  comment_var$Parameter <- gsub("SexChangeType", "__Sex change__", comment_var$Parameter)
  comment_var$Parameter <- gsub("FecundityType", "__Fecundity__", comment_var$Parameter)
  comment_var$Parameter <- gsub("LengthRelationshipType", "__Length-length__", comment_var$Parameter)
  comment_var$Parameter <- gsub("NaturalMortalityType", "NaturalMortality", comment_var$Parameter)
  
  tabdat <- left_join(tabdat, comment_var, by = "Parameter")
  
  rm_var_list <- c("CSIRO","BiologicalUnitId", "BiologicalUnitCode", "BiologicalUnitName", "BiologicalUnitIndex",  
                   "StockId", "StockCode", "StockName", "IsSAFSStockFlag", "StockSpeciesIdList",  
                   "StockSpeciesLabel", "StockLabel", "ResourceId", "ResourceName", "RSSpeciesId",  
                   "RSSpeciesCommonName", "RSSpeciesCode", "RSSpeciesScientificName", "Growth_units",  
                   "mat_param_count", "Maturit_.units", "WL_units", "WL_weight", "WL_measure",  
                   "Sex_change_param_count", "CreatedByActiveDirectoryUser", "CreatedDate",  
                   "LastUpdatedDate", "SubpopulationName", "Scientists", "References", "GrowthCurveId",  
                   "MaturityCurveId", "MaturityUnits", "LengthWeightFunctionId", "LengthWeightUnits",  
                   "LengthWeightWeight", "GrowthUnits_1", "GrowthUnits_2", "GrowthUnits_3",  
                   "DPIRDAssessmentLevel", "Comments", "AuditingComments","GrowthUnits_4","GrowthUnits_5",
                   "GrowthUnits_6")
  tabdat <- tabdat[!tabdat$Parameter %in% rm_var_list, ]
  
  # ===================================================================
  # 3. CONDITIONAL TWEAKS (Only if needed)
  # ===================================================================
  # Make a copy of the master names to modify safely
  clean_names <- master_clean_names 
  
  # If the growth curve is Schnute, overwrite the default (vB) growth parameter names
  if (!is.na(parameters$GrowthCurve) && startsWith(parameters$GrowthCurve, "Schnute")) {
    schnute_names <- c("$y_{1}$ (mm)", "$y_{2}$ (mm)", "$a$ ($yr^{-1}$)", 
                       "$b$ ($yr^{-1}$)", "$t_{1}$ (yrs)", "$t_{2}$ (yrs)")
    growth_params <- paste0("GrowthParams", 1:6)
    for (i in seq_along(growth_params)) {
      clean_names$Param.clean[clean_names$Parameter == growth_params[i]] <- schnute_names[i]
    }
  }
  
  # ===================================================================
  # 4. UNCONDITIONAL FORMATTING AND ORDERING
  # ===================================================================
  # Apply the master ordering. `factor` handles missing parameters gracefully.
  tabdat <- tabdat[order(factor(tabdat$Parameter, levels = master_order)), ]
  
  # Apply the clean names using the (potentially tweaked) dictionary
  # This finds which rows in tabdat need their names replaced
  rows_to_replace_idx <- which(tabdat$Parameter %in% clean_names$Parameter)
  
  # This finds the corresponding new names from the dictionary
  new_names <- clean_names$Param.clean[match(tabdat$Parameter[rows_to_replace_idx], clean_names$Parameter)]
  
  # Perform the replacement
  if(length(rows_to_replace_idx) > 0) {
    tabdat$Parameter[rows_to_replace_idx] <- new_names
  }
  
  # Remove Catch-MSY section if required
  remove_catch_msy_section <- TRUE  
  if(remove_catch_msy_section) {
    catch_msy_index <- which(tabdat$Parameter == "__Catch-MSY__")
    if (length(catch_msy_index) > 0) {
      tabdat <- tabdat[1:(catch_msy_index - 1), ]
    }
  }
  
  rownames(tabdat) <- NULL
  
  # ===================================================================
  # 5. FINAL OUTPUT (Bibliography, caption etc.)
  # ===================================================================
  bib_keys <- character(0)
  if (!is.null(parameters_s$References) && !is.na(parameters_s$References) && nzchar(parameters_s$References)) {
    extracted_keys <- str_extract_all(parameters_s$References, "@[A-Za-z0-9_:]+")[[1]]
    bib_keys <- sub("^@", "", extracted_keys)
    if (length(bib_keys) == 0 || all(bib_keys == "")) {
      bib_keys <- str_extract_all(parameters_s$References, "(?<=@)[A-Za-z0-9_:]+(?=[;\\]])")[[1]]
    }
  }
  
  bibliography_string <- create_bibliography_string(bib_data, bib_keys)
  
  caption_text <- paste0(
    "Summary of biological parameters for ",
    parameters_s$RSSpeciesCommonName,
    " (*", parameters_s$RSSpeciesScientificName, "*), in ",
    parameters_s$StockLabel,
    " ", parameters_s$References, "."
  )
  
  result <- list(
    tabdat = tabdat,
    caption_text = caption_text,
    parameters_s = parameters_s,
    bibliography_string = bibliography_string
  )
  
} else {
  # This part remains the same for when create_biol_table is FALSE
  result <- list(
    tabdat = data.frame(Message = "No biological data available"),
    caption_text = "No caption available",
    parameters_s = "No parameters_s available"
  )
}

# The 'result' object is now ready
result