# SS_bias_tuning.R (updated version)

cat("--- Background Bias & Tuning Script Initiated ---\n")

# --- Load required plotting library ---
suppressPackageStartupMessages(library(ggplot2))

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

# --- Option Extraction and Validation ---
model_dir <- bias_tuning_options$model_dir
exe_path <- bias_tuning_options$exe_path
exe_name <- bias_tuning_options$exe_name
tuning_dir <- bias_tuning_options$tuning_dir
run_step <- bias_tuning_options$run_step
weighting_method <- bias_tuning_options$weighting_method # Will be NULL if not set

# model_dir <-  "C:/Users/bxc/OneDrive - Department of Primary Industries And Regional Development/SADA/Shiny-FishAssess/output/ReK1_250807_NoFIS_WtDir"
# exe_path <- "C:/Users/bxc/OneDrive - Department of Primary Industries And Regional Development/SADA/Shiny-FishAssess/Stock_Synthesis_latest/ss.exe "
# tuning_dir <- "C:/Users/bxc/OneDrive - Department of Primary Industries And Regional Development/SADA/Shiny-FishAssess/output/ReK1_250807_NoFIS_WtDir/tuning"
# exe_name <- "ss3.exe"
# run_step <- "full_sequence" #"bias_ramp_only"
# weighting_method <- "francis" # "dirichlet" # "francis"

## MODIFIED: Create a base name from the model directory path for naming output folders.
base_folder_name <- basename(model_dir)

cat(paste("Task:", run_step, "\n"))
cat(paste("Using initial model from:", model_dir, "\n"))
cat(paste("Output will be in:", tuning_dir, "\n"))
cat(paste("Output folder prefix will be:", base_folder_name, "\n\n"))


dir.create(tuning_dir, showWarnings = FALSE)

# Helper function to run SS in a specific directory
run_ss_in_dir <- function(target_dir, exe_file_name) {
  exe_in_target_dir <- file.path(target_dir, exe_file_name)
  if (!file.exists(exe_in_target_dir)) {
    cat(paste("FATAL ERROR in run_ss_in_dir: Executable not found at:", exe_in_target_dir, "\n"))
    return(FALSE) # Return a status
  }
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(target_dir)
  
  cat(paste("  -> Attempting to run", exe_file_name, "in", getwd(), "\n"))
  
  # Use tryCatch to handle potential errors from processx::run
  result <- tryCatch(
    processx::run(
      command = exe_in_target_dir,
      args = "",
      wd = target_dir,
      error_on_status = TRUE
    ),
    error = function(e) e # Return the error object if it occurs
  )
  
  # Check if the run itself produced an error object
  if (inherits(result, "error")) {
    cat("  -> !!! Stock Synthesis execution FAILED. !!!\n")
    cat("  -> R ERROR MESSAGE:", result$message, "\n")
    return(FALSE) # Return failure status
  }
  
  cat("  -> Stock Synthesis execution appears successful.\n")
  return(TRUE) # Return success status
}


# --- Main Logic ---
tryCatch({
  # Step 1: Always ensure the source model has been run and read its files
  cat("Step 1: Reading source model files...\n")
  if (!file.copy(exe_path, file.path(model_dir, exe_name), overwrite = TRUE)) {
    stop("Failed to copy executable to model directory.")
  }
  
  starter_file_orig <- r4ss::SS_readstarter(file.path(model_dir, "starter.ss"), verbose = FALSE)
  # Read dat file first to pass to SS_readctl
  dat_orig_path <- file.path(model_dir, starter_file_orig$datfile)
  dat_orig <- r4ss::SS_readdat(file = dat_orig_path, verbose = FALSE)
  
  ctl_orig <- r4ss::SS_readctl(
    file.path(model_dir, starter_file_orig$ctlfile),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_orig
  )
  cat("-> Successfully read initial starter, data, and control files.\n")
  
  # Helper function for Bias Ramp Adjustment Logic (used by both run types)
  perform_bias_ramp <- function(input_replist, output_dir_name) {
    cat(paste("Performing bias ramp adjustment, output to:", output_dir_name, "...\n"))
    
    bias_adj <- r4ss::SS_fitbiasramp(input_replist, verbose = FALSE, plot = FALSE)
    cat("  -> Suggested bias adjustment parameters:\n")
    print(bias_adj$df)
    cat("\n")
    
    # --- FIX APPLIED HERE ---
    # First, read the data file using SS_readdat to get the correctly formatted datlist
    dat_path <- file.path(input_replist$inputs$dir, starter_file_orig$datfile)
    datlist_for_ctl <- r4ss::SS_readdat(file = dat_path, verbose = FALSE)
    
    # Now, read the control file using the newly created datlist
    ctl_modified <- r4ss::SS_readctl(
      file = file.path(input_replist$inputs$dir, starter_file_orig$ctlfile),
      verbose = FALSE, 
      use_datlist = TRUE, 
      datlist = datlist_for_ctl
    )
    
    ctl_modified$last_early_yr_nobias_adj <- bias_adj$newbias$par[1]
    ctl_modified$first_yr_fullbias_adj <- bias_adj$newbias$par[2]
    ctl_modified$last_yr_fullbias_adj <- bias_adj$newbias$par[3]
    ctl_modified$first_recent_yr_nobias_adj <- bias_adj$newbias$par[4]
    ctl_modified$max_bias_adj <- bias_adj$newbias$par[5]
    
    output_dir_path <- file.path(tuning_dir, output_dir_name)
    r4ss::copy_SS_inputs(dir.old = input_replist$inputs$dir, dir.new = output_dir_path, overwrite = TRUE, copy_exe = FALSE)
    file.copy(exe_path, file.path(output_dir_path, exe_name), overwrite = TRUE)
    r4ss::SS_writectl(ctl_modified, outfile = file.path(output_dir_path, starter_file_orig$ctlfile), overwrite = TRUE, verbose = FALSE)
    
    cat(paste("  -> Running Stock Synthesis in:", output_dir_path, "...\n"))
    run_success <- run_ss_in_dir(output_dir_path, exe_name)
    if (!run_success) stop("Stock Synthesis run failed during bias adjustment.")
    
    cat("-> Bias ramp adjustment model run complete.\n")
    return(r4ss::SS_output(dir = output_dir_path, verbose = FALSE, printstats = FALSE, covar = TRUE))
  }
  
  # --- Execution based on run_step ---
  
  if (run_step == "bias_ramp_only") {
    cat("--- EXECUTING: Single Bias Ramp Adjustment ---\n")
    
    # cat("Running source model to get up-to-date outputs...\n")
    # run_success <- run_ss_in_dir(model_dir, exe_name)
    # if (!run_success) stop("Initial Stock Synthesis run failed.")
    
    replist_initial <- r4ss::SS_output(dir = model_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
    
    ## MODIFIED: Use the base_folder_name for the output directory.
    base_output_dir_name <- paste0(base_folder_name, "_bias_adj")
    output_dir_name <- base_output_dir_name
    counter <- 1
    # Loop to find a folder name that doesn't exist yet
    while (dir.exists(file.path(tuning_dir, output_dir_name))) {
      counter <- counter + 1
      output_dir_name <- paste0(base_output_dir_name, "_", counter)
    }
    cat(paste("Output for this run will be saved to unique folder:", output_dir_name, "\n"))
    
    # Run the bias ramp and save to the uniquely named folder
    replist_after_bias_adj <- perform_bias_ramp(replist_initial, output_dir_name)
    
    cat("\nSingle Bias Ramp Adjustment Complete! ✅\n")
    cat(paste("Adjusted model is ready in:", file.path(tuning_dir, output_dir_name), "\n"))
    
    ## MODIFIED: Corrected the plot directory to use the unique output_dir_name.
    r4ss::SS_plots(replist_after_bias_adj, dir = file.path(tuning_dir, output_dir_name), printfolder = "r4ss_plots", pdf = FALSE, png = TRUE, html = TRUE)
    
    try({
      html_dir <- file.path(tuning_dir, output_dir_name, "r4ss_plots")
      html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
      for (f in html_files) {
        tx <- readLines(f, warn = FALSE)
        tx <- gsub("<title>SS Output</title>", paste0("<title>", output_dir_name, "</title>"), tx)
        writeLines(tx, f)
      }
    })
    
    cat("-> r4ss plots generated in the 'r4ss_plots' subfolder.\n")
    
    
  ##DPIRD PLOTS
    tryCatch({
      cat("Generating custom DPIRD plots for Bias Ramp run...\n")
      generate_DPIRD_plots(replist_after_bias_adj, file.path(tuning_dir, output_dir_name))
    }, error = function(e) {
      cat(paste("Error generating DPIRD plots:", e$message, "\n"))
    })
    
  } else if (run_step == "full_sequence") {
    cat("--- EXECUTING: Full Tuning Sequence ---\n")
    
    # Initialize a list to track folders we want to delete later
    dirs_to_remove <- c()
    
    ## MODIFIED: Define the weighting suffix and combine it with the base name
    weighting_suffix <- if (!is.null(weighting_method)) {
      switch(weighting_method,
             "francis" = "_WtFr",
             "dirichlet" = "_WtDir",
             "") 
    } else {
      ""
    }
    full_prefix <- paste0(base_folder_name, weighting_suffix)
    cat(paste("Full folder prefix for this run:", full_prefix, "\n"))
    
    if (weighting_method == "francis") {
      
      replist1 <- r4ss::SS_output(dir = model_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
      ## Step 2.2: First Bias Adjustment
      first_bias_dir_name <- paste0(full_prefix, "_first_bias_adj")
      
      # Mark this folder for deletion later
      dirs_to_remove <- c(dirs_to_remove, file.path(tuning_dir, first_bias_dir_name))
      
      replist2 <- perform_bias_ramp(replist1, first_bias_dir_name)
      
      cat(paste("Step 2.3: Performing Composition Weighting using", weighting_method, "method...\n"))
      
      replist_before_final_bias_adj <- NULL
      
      cat("    -> Francis Tuning (3 iterations)...\n")
      
      # Iteration 1
      modelrun3_dir_name <- paste0(full_prefix, "_francis1")
      modelrun3_dir <- file.path(tuning_dir, modelrun3_dir_name)
      dirs_to_remove <- c(dirs_to_remove, modelrun3_dir) # Mark for deletion
      
      r4ss::copy_SS_inputs(dir.old = replist2$inputs$dir, dir.new = modelrun3_dir, overwrite = TRUE, copy_exe = TRUE)
      tuning_table_1 <- r4ss::tune_comps(replist2, option = "Francis", write = TRUE, dir = modelrun3_dir, verbose = FALSE, plot = FALSE)
      ctl_2 <- r4ss::SS_readctl(file.path(replist2$inputs$dir, starter_file_orig$ctlfile), verbose = FALSE, use_datlist = TRUE, datlist = file.path(replist2$inputs$dir, starter_file_orig$datfile))
      ctl_2$Variance_adjustment_list <- tuning_table_1[1:3]
      ctl_2$DoVar_adjust <- 1
      ctl_2$dirichlet_parms <- NULL
      r4ss::SS_writectl(ctl_2, file.path(modelrun3_dir, starter_file_orig$ctlfile), overwrite = TRUE, verbose = FALSE)
      dat_2 <- r4ss::SS_readdat(file.path(replist2$inputs$dir, starter_file_orig$datfile), verbose = FALSE)
      dat_2$len_info$CompError <- 0
      dat_2$len_info$ParmSelect <- 0
      dat_2$age_info$CompError <- 0
      dat_2$age_info$ParmSelect <- 0
      r4ss::SS_writedat(dat_2, file.path(modelrun3_dir, starter_file_orig$datfile), overwrite = TRUE, verbose = FALSE)
      run_ss_in_dir(modelrun3_dir, exe_name)
      replist3 <- r4ss::SS_output(dir = modelrun3_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
      # Iteration 2
      modelrun4_dir_name <- paste0(full_prefix, "_francis2")
      modelrun4_dir <- file.path(tuning_dir, modelrun4_dir_name)
      dirs_to_remove <- c(dirs_to_remove, modelrun4_dir) # Mark for deletion
      
      r4ss::copy_SS_inputs(dir.old = modelrun3_dir, dir.new = modelrun4_dir, overwrite = TRUE, copy_exe = TRUE)
      tuning_table_2 <- r4ss::tune_comps(replist3, option = "Francis", write = TRUE, dir = modelrun4_dir, verbose = FALSE, plot = FALSE)
      ctl_3 <- r4ss::SS_readctl(file.path(modelrun3_dir, starter_file_orig$ctlfile), verbose = FALSE, use_datlist = TRUE, datlist = file.path(modelrun3_dir, starter_file_orig$datfile))
      ctl_3$Variance_adjustment_list <- tuning_table_2[1:3]
      ctl_3$DoVar_adjust <- 1
      r4ss::SS_writectl(ctl_3, file.path(modelrun4_dir, starter_file_orig$ctlfile), overwrite = TRUE, verbose = FALSE)
      run_ss_in_dir(modelrun4_dir, exe_name)
      replist4 <- r4ss::SS_output(dir = modelrun4_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
      # Iteration 3
      modelrun5_dir_name <- paste0(full_prefix, "_francis3")
      modelrun5_dir <- file.path(tuning_dir, modelrun5_dir_name)
      dirs_to_remove <- c(dirs_to_remove, modelrun5_dir) # Mark for deletion
      
      r4ss::copy_SS_inputs(dir.old = modelrun4_dir, dir.new = modelrun5_dir, overwrite = TRUE, copy_exe = TRUE)
      tuning_table_3 <- r4ss::tune_comps(replist4, option = "Francis", write = TRUE, dir = modelrun5_dir, verbose = FALSE, plot = FALSE)
      ctl_4 <- r4ss::SS_readctl(file.path(modelrun4_dir, starter_file_orig$ctlfile), verbose = FALSE, use_datlist = TRUE, datlist = file.path(modelrun4_dir, starter_file_orig$datfile))
      ctl_4$Variance_adjustment_list <- tuning_table_3[1:3]
      ctl_4$DoVar_adjust <- 1
      r4ss::SS_writectl(ctl_4, file.path(modelrun5_dir, starter_file_orig$ctlfile), overwrite = TRUE, verbose = FALSE)
      run_ss_in_dir(modelrun5_dir, exe_name)
      replist5 <- r4ss::SS_output(dir = modelrun5_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
      cat("-> Francis tuning complete.\n")
      replist_before_final_bias_adj <- replist5
      
    } else if (weighting_method == "dirichlet") {
      
      replist1 <- r4ss::SS_output(dir = model_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
      cat("    -> Dirichlet Tuning ...\n")
      
      modelrun3_dir_name <- paste0(full_prefix, "_dirichlet1")
      modelrun3_dir <- file.path(tuning_dir, modelrun3_dir_name)
      
      dirs_to_remove <- c(dirs_to_remove, modelrun3_dir) # Mark for deletion
      
      # 1. Copy inputs but NOT the exe
      r4ss::copy_SS_inputs(dir.old = replist1$inputs$dir, dir.new = modelrun3_dir, overwrite = TRUE, copy_exe = FALSE)
      
      # 2. Manually copy the clean executable
      file.copy(exe_path, file.path(modelrun3_dir, exe_name), overwrite = TRUE)
      
      ctl_dirichlet <- r4ss::SS_readctl(file.path(modelrun3_dir, starter_file_orig$ctlfile), verbose = FALSE, use_datlist = TRUE, datlist = file.path(modelrun3_dir, starter_file_orig$datfile))
      dat_dirichlet <- r4ss::SS_readdat(file.path(modelrun3_dir, starter_file_orig$datfile), verbose = FALSE)
      ctl_dirichlet$DoVar_adjust <- 0
      
      len_fleets_with_comps <- sort(unique(dat_dirichlet$lencomp$fleet[dat_dirichlet$lencomp$fleet >= 0]))
      age_fleets_with_comps <- sort(unique(dat_dirichlet$agecomp$fleet[dat_dirichlet$agecomp$fleet >= 0]))
      
      dat_dirichlet$len_info$CompError[len_fleets_with_comps] <- 1
      dat_dirichlet$len_info$ParmSelect[len_fleets_with_comps] <- seq_along(len_fleets_with_comps)
      
      max_len_parm <- max(dat_dirichlet$len_info$ParmSelect)
      
      dat_dirichlet$age_info$CompError[age_fleets_with_comps] <- 1
      dat_dirichlet$age_info$ParmSelect[age_fleets_with_comps] <- max_len_parm + seq_along(age_fleets_with_comps)
      
      max_parm <- max(c(dat_dirichlet$len_info$ParmSelect, dat_dirichlet$age_info$ParmSelect))
      
      ctl_dirichlet$dirichlet_parms <- data.frame(
        LO = rep(-5, max_parm),
        HI = rep(20, max_parm),
        INIT = rep(0.5, max_parm),
        PRIOR = rep(0, max_parm),
        PR_SD = rep(1.813, max_parm),
        PR_type = rep(6, max_parm),
        PHASE = rep(5, max_parm),
        `env_var&link` = rep(0, max_parm),
        dev_link = rep(0, max_parm),
        dev_minyr = rep(0, max_parm),
        dev_maxyr = rep(0, max_parm),
        dev_PH = rep(0, max_parm),
        Block = rep(0, max_parm),
        Block_Fxn = rep(0, max_parm),
        row.names = paste0("ln(DM_theta)_", 1:max_parm)
      )
      
      r4ss::SS_writedat(dat_dirichlet, file.path(modelrun3_dir, starter_file_orig$datfile), overwrite = TRUE, verbose = FALSE)
      r4ss::SS_writectl(ctl_dirichlet, file.path(modelrun3_dir, starter_file_orig$ctlfile), overwrite = TRUE, verbose = FALSE)
      
      run_ss_in_dir(modelrun3_dir, exe_name)
      replist_before_final_bias_adj <- r4ss::SS_output(dir = modelrun3_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
    } else {
      stop(paste("Unknown weighting method provided:", weighting_method))
    }
    
    # Step 2.4: Final Bias Ramp Adjustment (The one we keep!)
    # final_model_dir_name <- paste0(full_prefix, "_final_model")
    final_model_dir_name <- full_prefix
    
    # NOTE: We do NOT add final_model_dir_name to dirs_to_remove because we want to keep it.
    
    replist_final <- perform_bias_ramp(replist_before_final_bias_adj, final_model_dir_name)
    
    # Step 2.5: Generate plots for the final model
    cat("Step 2.5: Generating r4ss plots for the final tuned model...\n")
    final_model_dir <- replist_final$inputs$dir
    r4ss::SS_plots(replist_final, dir = final_model_dir, printfolder = "r4ss_plots", pdf = FALSE, png = TRUE, html = TRUE)
    try({
      html_dir <- file.path(final_model_dir, "r4ss_plots")
      html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
      for (f in html_files) {
        tx <- readLines(f, warn = FALSE)
        tx <- gsub("<title>SS Output</title>", paste0("<title>", final_model_dir_name, "</title>"), tx)
        writeLines(tx, f)
      }
    })
    
    cat("-> r4ss plots generated in the 'r4ss_plots' subfolder.\n")
    
    ## DPIRD plots
    tryCatch({
      cat("Generating custom DPIRD plots for Final Tuned Model...\n")
      generate_DPIRD_plots(replist_final, final_model_dir)
    }, error = function(e) {
      cat(paste("Error generating DPIRD plots:", e$message, "\n"))
    })
    
    # # --- CLEANUP STEP ---
    # cat("\n--- CLEANUP: Removing intermediate model runs ---\n")
    # if (length(dirs_to_remove) > 0) {
    #   for (dir_path in dirs_to_remove) {
    #     if (dir.exists(dir_path)) {
    #       cat(paste("Removing:", dir_path, "\n"))
    #       unlink(dir_path, recursive = TRUE)
    #     } else {
    #       cat(paste("Skipping (not found):", dir_path, "\n"))
    #     }
    #   }
    # } else {
    #   cat("No intermediate folders marked for removal.\n")
    # }
    
    # --- CLEANUP STEP ---
    cat("\n--- CLEANUP: Removing intermediate model runs ---\n")
    
    # 1. Force R to release any file locks
    gc() 
    
    # 2. Wait a moment for the OS to release handles (Windows fix)
    Sys.sleep(2) 
    
    if (length(dirs_to_remove) > 0) {
      for (dir_path in dirs_to_remove) {
        if (dir.exists(dir_path)) {
          cat(paste("Removing:", dir_path, "\n"))
          
          # Attempt deletion
          result <- unlink(dir_path, recursive = TRUE, force = TRUE)
          
          # Retry mechanism if deletion fails (common on Windows)
          if (dir.exists(dir_path)) {
            cat("  -> First delete attempt failed (folder locked?). Retrying in 2 seconds...\n")
            Sys.sleep(2)
            unlink(dir_path, recursive = TRUE, force = TRUE)
          }
          
          # Final check
          if (dir.exists(dir_path)) {
            cat("  -> WARNING: Could not delete folder. You may need to delete it manually.\n")
          } else {
            cat("  -> Deleted successfully.\n")
          }
          
        } else {
          cat(paste("Skipping (not found):", dir_path, "\n"))
        }
      }
    } else {
      cat("No intermediate folders marked for removal.\n")
    }
    
    cat("\nFull Tuning Sequence Complete! ✅\n")
    cat(paste("Final tuned model is ready for inspection in:", final_model_dir, "\n"))
    
  } else {
    stop(paste("Invalid 'run_step' provided to script:", run_step))
  }
  
}, error = function(e) {
  cat("\n--- ERROR IN BACKGROUND SCRIPT ---\n")
  cat("An error occurred and the process was stopped.\n")
  cat("ERROR MESSAGE:", e$message, "\n")
  cat("------------------------------------\n")
})