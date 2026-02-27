# SS3_sensitivities.R

# --- GLOBAL ERROR HANDLER START ---
tryCatch({
  # Force immediate printing of warnings/errors
  options(warn = 1)

if(!all(sensitivity_options$jitter,
        sensitivity_options$profile_r0,
        sensitivity_options$profile_m,
        sensitivity_options$profile_h,
        sensitivity_options$profile_final_depletion,
        sensitivity_options$profile_current_spawning_biomass,
        sensitivity_options$retrospective)){
  
  cat("No sensitivity analyses selected")
}

# cat("Sensitivity Options:\n")
for (name in names(sensitivity_options)) {
  value <- sensitivity_options[[name]]
  
  # Special handling for 'model_folder' to only print the path
  if (name == "model_folder") {
    cat(paste0("Model_folder: ", value, "\n")) # No 'fs_path' chr here
  } else {
    cat(paste0(name, ": ", value, "\n"))
  }
}


# --- Standardise model input filenames ---
# cat("\n--- Checking and Standardising Input Filenames ---\n")
model_dir <- sensitivity_options$model_folder

# Loop through potential files to rename them to the standard
for (files in list(c("data.dat", "datafile.dat"), c("control.ctl", "controlfile.ctl"))) {
  old_path <- file.path(model_dir, files[1])
  new_path <- file.path(model_dir, files[2])
  if (file.exists(old_path) && !file.exists(new_path)) {
    file.rename(from = old_path, to = new_path)
    cat(paste0("Renamed '", files[1], "' to '", files[2], "'.\n"))
  }
}

# Always ensure the starter file points to the standard filenames
starter_path <- file.path(model_dir, "starter.ss")
if (file.exists(starter_path)) {
  starter <- r4ss::SS_readstarter(starter_path, verbose = FALSE)
  if (starter$datfile != "datafile.dat" || starter$ctlfile != "controlfile.ctl") {
    starter$datfile <- "datafile.dat"
    starter$ctlfile <- "controlfile.ctl"
    r4ss::SS_writestarter(starter, dir = model_dir, overwrite = TRUE, verbose = FALSE)
    cat("The 'starter.ss' file has been updated.\n")
  }
}
cat("--- Filename check complete ---\n\n")


# --- Conditionally copy ss.exe to the selected model folder ---
# Define the full path for the destination file
destination_file <- file.path(sensitivity_options$model_folder, "ss.exe")

# Check if ss.exe does NOT exist in the destination folder
if (!file.exists(destination_file)) {
  
  # If it's not there, copy it.
  cat("ss.exe not found. Copying the executable.\n")
  
  file.copy(
    from = file.path("Stock_Synthesis_latest", "ss.exe"),
    to = sensitivity_options$model_folder
    # 'overwrite = FALSE' is the default, so we don't need to specify it.
  )
  
} else {
  
  # If the file already exists, inform the user and do nothing.
  cat("ss.exe is already present. Skipping copy.\n")
  
}

# Quietly load packages, so it does not ugly up the console
suppressPackageStartupMessages({
  library(r4ss)
  library(tibble)
  library(future)
  library(future.apply)
  library(processx)
})


# Ensure parallel packages are loaded
suppressPackageStartupMessages({
  library(doParallel)
  library(foreach)
})

# Jitters in parallel -----------------------------------------------------
if(sensitivity_options$jitter) {
  
  model_name <- sensitivity_options$model_folder
  
  # Number of jitter runs to perform
  Njitter <- sensitivity_options$njitters
  
  # Jitter fraction to apply to the parameters
  JITTER_FRACTION <- sensitivity_options$jitter_fraction
  
  
  ## 2. DIRECTORY AND FILE PREPARATION
  
  message("--- Jitter: Preparing Directories and Files ---")
  
  # Define paths
  # base_model_dir  <- file.path(mydir, "model", model_name)
  base_model_dir  <- sensitivity_options$model_folder
  jitter_main_dir <- file.path(base_model_dir, "jitter_parallel")
  plotdir         <- file.path(jitter_main_dir, "plots")
  
  # Create the main directories
  dir.create(jitter_main_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(plotdir, showWarnings = FALSE)
  
  # --- Prepare Base Model (Run 0) for comparison ---
  # This ensures the final plots have a reference point from the non-jittered model.
  message("Copying base model results to serve as 'Run 0'.")
  
  # Find the parameter file from the base run (ss.par or ss3.par)
  base_par_file <- file.path(base_model_dir, "ss.par")
  if (!file.exists(base_par_file)) {
    base_par_file <- file.path(base_model_dir, "ss3.par")
  }
  if (!file.exists(base_par_file)) {
    stop("Could not find 'ss.par' or 'ss3.par' in the base model directory: ", base_model_dir)
  }
  
  # Copy and rename the base parameter file to be the '0' run.
  file.copy(from = base_par_file, to = file.path(jitter_main_dir, "ss.par_0.sso"), overwrite = TRUE)
  
  # Copy and rename other key output files for the '0' run.
  base_files_to_rename <- c("Report.sso", "CompReport.sso", "covar.sso", "warning.sso")
  for (f in base_files_to_rename) {
    source_path <- file.path(base_model_dir, f)
    if (file.exists(source_path)) {
      dest_path <- file.path(jitter_main_dir, sub("\\.sso$", "0.sso", f))
      file.copy(from = source_path, to = dest_path, overwrite = TRUE)
    }
  }
  

  ## JITTER RUNS (The "starter.ss" Method) -----------------------------------


  
  message("\n--- Jitter: Setting up ", Njitter, " Jitter Subdirectories ---")
  
  # A function to set up a single jitter directory. This is cleaner.
  setup_single_jitter_dir <- function(i, base_dir, main_jitter_dir, jitter_frac) {
    
    # Define the dedicated directory for this run
    run_dir <- file.path(main_jitter_dir, paste0("jitter_", i))
    dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)
    
    # Copy all files from the base model into the run directory
    all_files <- list.files(base_dir, full.names = TRUE)
    file.copy(from = all_files, to = run_dir, overwrite = TRUE)
    
    # --- Modify the starter.ss file ---
    starter_path <- file.path(run_dir, "starter.ss")
    starter <- SS_readstarter(starter_path, verbose = FALSE)
    
    # Tell SS to read initial values from the ss.par file
    starter$init_values_src <- 1
    
    # Set the jitter fraction
    starter$jitter_fraction <- jitter_frac
    
    # Set a unique seed for this run to ensure reproducible, different jitters
    starter$seed <- i
    
    # Write the modified starter file back
    SS_writestarter(starter, dir = run_dir, overwrite = TRUE, verbose = FALSE)
    
    return(TRUE)
  }
  
  # Loop through and create all the jitter directories
  lapply(1:Njitter, FUN = setup_single_jitter_dir,
         base_dir = base_model_dir,
         main_jitter_dir = jitter_main_dir,
         jitter_frac = JITTER_FRACTION)
  
  message("All jitter directories are configured and ready.")
  
  
  ## JITTER PARALLEL EXECUTION ------------------------------------------------
  message("\n--- Jitter: Starting Parallel Model Runs ---")
  
  # Setup parallel processing using as many cores as available
  plan(multisession)
  message(paste("Executing", Njitter, "jitters on", nbrOfWorkers(), "cores."))
  
  # A simple function to execute a run in its directory
  run_ss_in_dir <- function(i, main_jitter_dir) {
    run_dir <- file.path(main_jitter_dir, paste0("jitter_", i))
    ss_exe_name <- ifelse(.Platform$OS.type == "windows", "ss.exe", "ss")
    
    # Use processx::run for robust execution
    processx::run(
      command = file.path(run_dir, ss_exe_name),
      # args = c("-nohess", "-nox"),
      wd = run_dir, # Critical: set the working directory for the process
      error_on_status = FALSE # Don't stop the R script if a model fails
    )
    return(TRUE)
  }
  
  # Use future_lapply for clean and safe parallel execution
  start_time <- Sys.time()
  message(paste("Parallel execution started:", start_time))
  future_lapply(1:Njitter, FUN = run_ss_in_dir, main_jitter_dir = jitter_main_dir)
  end_time <- Sys.time()
  duration_paralell.jitter <- end_time - start_time
  message(paste("Jitter Parallel execution took:", round(duration_paralell.jitter, 2), units(duration_paralell.jitter)))
  
  # Close the parallel workers
  plan(sequential)
  
  ## JITTER  GATHER AND PROCESS RESULTS ------------------------------------------------
  
  message("\n--- Jitter: Consolidating and Processing Results ---")
  
  # Copy results from each run directory back to the main jitter directory and rename them
  for (i in 1:Njitter) {
    run_dir <- file.path(jitter_main_dir, paste0("jitter_", i))
    
    if (file.exists(file.path(run_dir, "Report.sso"))) {
      files_to_collect <- c("Report.sso", "CompReport.sso", "covar.sso", "warning.sso", "ss.par")
      existing_files <- files_to_collect[file.exists(file.path(run_dir, files_to_collect))]
      
      # Create new names with the run number suffix (e.g., Report1.sso)
      new_names <- sub("\\.par$", paste0(".par_", i, ".sso"), sub("\\.sso$", paste0(i, ".sso"), existing_files))
      
      file.copy(
        from = file.path(run_dir, existing_files),
        to = file.path(jitter_main_dir, new_names),
        overwrite = TRUE
      )
    } else {
      message(paste("Run", i, "failed or did not produce Report.sso. Skipping."))
    }
  }
  
  # Read all model outputs (base model '0' + all successful jitters)
  jitmodels <- SSgetoutput(dirvec = jitter_main_dir, keyvec = 0:Njitter, getcovar = FALSE)
  
  # Summarize the outputs
  jitsummary <- SSsummarize(jitmodels)
  message("Model outputs have been summarized.")
  
  ## JITTER  PLOTTING AND ANALYSIS ------------------------------------------------
  
  message("\n--- Jitter: Generating Plots and Final Outputs ---")
  
  
  
  # This section is the same as your well-structured original code.
  likelihoods_df <- jitsummary[["likelihoods"]]
  write.csv(likelihoods_df, file = file.path(plotdir, "Jitter_Likelihoods.csv"), row.names = FALSE)
  
  jitter_likes <- suppressWarnings(as.numeric(likelihoods_df[1, ]))
  names(jitter_likes) <- colnames(likelihoods_df)
  ref_like <- jitter_likes[1]
  
  # png(file.path(plotdir, "Jitter_Comparison_Plot.png"), width = 185, height = 205, units = "mm", res = 300)
  # png(file.path(plotdir, "Jitter_Comparison_Plot.png"), width = 200, height = 200, units = "mm", res = 300)
  
  png(file.path(plotdir, "Jitter_Comparison_Plot.png"), 
      width = 165.1, height = 127, units = "mm", res = 300)
  
  
  # plot(0:(length(jitter_likes) - 1), jitter_likes,
  #      pch = 21, cex = 1.5, col = "black", bg = "grey",
  #      xlab = "Jitter Run", ylab = "-log-likelihood",
  #      xaxt = "n")
  # # axis(1, at = 0:(length(jitter_likes) - 1), labels = names(jitter_likes))
  # axis(1, at = 0:(length(jitter_likes) - 1), labels = 0:(length(jitter_likes) - 1))
  # points(c(0:(length(jitter_likes) - 1))[jitter_likes > ref_like & !is.na(jitter_likes)], jitter_likes[jitter_likes > ref_like & !is.na(jitter_likes)], pch = 21, cex = 1.5, col = "black", bg = "red")
  # points(c(0:(length(jitter_likes) - 1))[jitter_likes < ref_like & !is.na(jitter_likes)], jitter_likes[jitter_likes < ref_like & !is.na(jitter_likes)], pch = 21, cex = 1.5, col = "black", bg = "green")
  # abline(h = ref_like, lty = 2)
  # legend("topright",
  #        legend = c("Higher Likelihood", "Lower Likelihood", "Base Model"),
  #        pch = 21, pt.bg = c("red", "green", "grey"), bty = "n")

  # Define the x-axis values from the dummy data
  x_vals <- 0:(length(jitter_likes) - 1)
  
  # --- NEW: Calculate a buffered axis limit ---
  # Get the min and max of your data (e.g., 0 and 6)
  x_range <- range(x_vals) 
  # Define a small buffer to add to each side
  buffer <- 0.5 
  # Calculate the final limits by adding/subtracting the buffer
  final_xlim <- x_range + c(-buffer, buffer) # e.g., c(-0.5, 6.5)
  
  # Create the plot using the new buffered limits
  plot(x_vals, jitter_likes,
       pch = 21, cex = 1.5, col = "black", bg = "grey",
       xlab = "Jitter Run", ylab = "-log-likelihood",
       xaxt = "n",
       xlim = final_xlim, # Use the new buffered limits
       xaxs = "i"         # Keep this to ensure the limits are respected exactly
  )
  
  # Add the axis, points, and legend
  axis(1)
  
  points(x_vals[jitter_likes > ref_like & !is.na(jitter_likes)], 
         jitter_likes[jitter_likes > ref_like & !is.na(jitter_likes)], 
         pch = 21, cex = 1.5, col = "black", bg = "red")
  points(x_vals[jitter_likes < ref_like & !is.na(jitter_likes)], 
         jitter_likes[jitter_likes < ref_like & !is.na(jitter_likes)], 
         pch = 21, cex = 1.5, col = "black", bg = "green")
  abline(h = ref_like, lty = 2)
  legend("topright",
         legend = c("Higher Likelihood", "Lower Likelihood", "Base Model"),
         pch = 21, pt.bg = c("red", "green", "grey"), bty = "n")
  
  dev.off()
  message(paste("Jitter plot saved to:", plotdir))
  
  
  
  
  SSplotComparisons(jitsummary,
                    plot = FALSE, print = TRUE, plotdir = plotdir,
                    legendlabels = paste("Model", 0:Njitter),
                    btarg = 0.4, minbthresh = 0.2)
  
  message("\nJitter Analysis complete.")
}


# Retrospectives ----------------------------------------------------------
# 
# if(sensitivity_options$retrospective){
# 
#   model_name <- sensitivity_options$model_folder
# 
#   # Number of retro years to include in analysis
#   num_retro <- sensitivity_options$retro_nyears
# 
#   base_model_dir  <- sensitivity_options$model_folder
#   jitter_main_dir <- file.path(base_model_dir, "jitter_parallel")
#   plotdir         <- file.path(jitter_main_dir, "plots")
# 
#   retro(
#     dir = base_model_dir,
#     newsubdir = paste0("retro"),
#     subdirstart = "retro",
#     years = 0:-num_retro,
#     exe = "ss"
#   )
# 
#   retroModels = SSgetoutput(
#     dirvec = file.path(base_model_dir, model_name, "retro", paste("retro", 0:-num_retro, sep = ""))
#   )
# 
#   retroSummary = SSsummarize(retroModels)
# 
#   # plot comparisons
#   (endyrvec = retroSummary[["endyrs"]] + 0:-num_retro)
# 
#   ## create plot directory
#   plot_dir = plot_dir
#   if (!file.exists(plot_dir)) dir.create(plot_dir)
# 
#   SSplotComparisons(retroSummary,
#                     plot = FALSE,
#                     endyrvec = endyrvec,
#                     plotdir = plot_dir,
#                     legendlabels = paste0("Data to ", endyrvec),
#                     xaxs = "r",
#                     yaxs = "r",
#                     btarg = 0.4,
#                     minbthresh = 0.2,
#                     png=TRUE)
# 
#   (mohns <- SSmohnsrho(retroSummary,endyrvec,verbose=T))
# 
#   df <- enframe(mohns)
#   df <- apply(df,2,as.character)
#   write.csv(df, file = paste0(plot_dir,"/MohnsRho.csv"))
# 
# 
# 
# 
# }


# Retro paralell ----------------------------------------------------------

if (sensitivity_options$retrospective) {
  
  
  suppressPackageStartupMessages({
    library(doParallel)
    library(foreach)
  })
  
  model_name <- sensitivity_options$model_folder
  
  # Number of retro years to include in analysis
  num_retro <- sensitivity_options$retro_nyears
  
  base_model_dir  <- sensitivity_options$model_folder
  
  ## Clean up old directory ##
  retro_main_dir <- file.path(base_model_dir, "retro")
  if (dir.exists(retro_main_dir)) {
    message(paste("Removing existing directory:", retro_main_dir))
    unlink(retro_main_dir, recursive = TRUE)
  }
  dir.create(retro_main_dir, showWarnings = FALSE)
  
  ## --- Step 1: Manually set up retrospective model directories --- ##
  # This uses the standard "read -> modify -> write" method with exported r4ss functions.
  
  message("Manually creating retrospective directory structure...")
  years_to_peel <- 0:-num_retro
  
  # 1. READ the starter file from the base model ONCE
  starter <- r4ss::SS_readstarter(file = file.path(base_model_dir, "starter.ss"),
                                  verbose = FALSE)
  
  # Loop through each retrospective year to create its folder and modified starter file
  for (i in years_to_peel) {
    # Define the directory for this specific retrospective year
    retro_sub_dir <- file.path(retro_main_dir, paste0("retro", i))
    dir.create(retro_sub_dir)
    
    # Copy all necessary input files from the base model
    r4ss::copy_SS_inputs(dir.old = base_model_dir, 
                         dir.new = retro_sub_dir, 
                         overwrite = TRUE,
                         copy_exe = TRUE,
                         verbose = FALSE) 
    
    # 2. MODIFY the starter list in R
    starter$retro_yr <- i
    
    # 3. WRITE the modified starter list to the new directory
    r4ss::SS_writestarter(mylist = starter,
                          dir = retro_sub_dir,
                          overwrite = TRUE,
                          verbose = FALSE)
  }
  
  # Define the vector of directories that were just created
  retro_dirs <- file.path(retro_main_dir, paste0("retro", years_to_peel))
  
  
  ## --- Step 2: Run the SS models in parallel --- ##
  
  # Set up the parallel cluster
  n_cores <- parallel::detectCores() - 1
  message(paste("Setting up parallel processing cluster with", n_cores, "cores."))
  my_cluster <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(my_cluster)
  
  start_time <- Sys.time()
  message(paste("Parallel execution started:", start_time))
    
  
  message(paste("Running", length(retro_dirs), "retrospective models in parallel..."))
  foreach(
    dir = retro_dirs,
    .packages = c('r4ss'),
    .final = function(x) NULL
  ) %dopar% {
    r4ss::run(dir = dir, 
              # extras = "-nohess",
              exe = "ss")
  }
  message("Finished running all models.")
  
  end_time <- Sys.time()
  duration_paralell.retro <- end_time - start_time
  message(paste("Retrospective Parallel execution took:", round(duration_paralell.retro, 2), units(duration_paralell.retro)))
  
  
  retroModels = SSgetoutput(
    dirvec = file.path(base_model_dir, "retro", paste("retro", 0:-num_retro, sep = ""))
  )
  
  
  
  ## --- Step 4: Summarise and plot results --- ##
  
  retroSummary <- SSsummarize(retroModels)
  
  endyrvec <- retroSummary[["endyrs"]] + years_to_peel
  
  plot_dir <- file.path(retro_main_dir, "plots")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  SSplotComparisons(retroSummary,
                    plot = FALSE,
                    endyrvec = endyrvec,
                    plotdir = plot_dir,
                    legendlabels = paste0("Data to ", endyrvec),
                    xaxs = "r",
                    yaxs = "r",
                    btarg = 0.4,
                    minbthresh = 0.2,
                    png = TRUE)
  
  # message("Before calulcatuion of SSmohnsrho.")
  # # mohns <- SSmohnsrho(retroSummary, endyrvec, verbose = TRUE)
  # mohns <- SSmohnsrho(retroSummary, endyrvec, startyr = head(retroSummary$SpawnBio$Yr[1]), verbose = TRUE)
  # 
  # library(tibble)
  # message("Before enframe(mohns).")
  # df <- enframe(mohns)
  # message("Before apply(df, 2, as.character)")
  # df <- apply(df, 2, as.character)
  # write.csv(df, file = file.path(plot_dir, "MohnsRho.csv"))
  # 
  # ## Stop the cluster
  # message("Shutting down parallel cluster.")
  # parallel::stopCluster(my_cluster)
  
  message("Attempting calculation of SSmohnsrho...")
  
  tryCatch({
    # Attempt to calculate Mohn's Rho
    # Note: I also tweaked the startyr extraction to be a bit safer: head(..., 1)
    mohns <- SSmohnsrho(retroSummary, endyrvec, startyr = head(retroSummary$SpawnBio$Yr, 1), verbose = TRUE)
    
    library(tibble)
    # message("Before enframe(mohns).")
    df <- enframe(mohns)
    # message("Before apply(df, 2, as.character)")
    df <- apply(df, 2, as.character)
    write.csv(df, file = file.path(plot_dir, "MohnsRho.csv"))
    message("Mohn's Rho calculated and saved successfully.")
    
  }, error = function(e) {
    # This block runs if SSmohnsrho (or the saving steps) fail
    message("\nWARNING: SSmohnsrho crashed. Skipping Mohn's Rho calculation.")
    message("The specific error was: ", conditionMessage(e))
    message("Continuing with the rest of the script...\n")
  })
  
}


# Likelihood Profiling Parallel ---------------------------------------------

# Define the map between the short name and the full parameter name.
param_map <- c(
  profile_r0 = "SR_LN(R0)",
  profile_m  = "NatM_uniform_Fem_GP_1",
  profile_h  = "SR_BH_steep",
  profile_l_amax_fem = "L_at_Amax_Fem_GP_1",
  profile_l_amax_mal = "L_at_Amax_Mal_GP_1",
  profile_k_fem = "VonBert_K_Fem_GP_1",
  profile_k_mal = "VonBert_K_Mal_GP_1",
  profile_final_depletion = "Depl", # Assuming this is the name in SS
  profile_current_spawning_biomass = "CurSB" # Assuming this is the name in SS
)

profile_option_names <- names(param_map)
selected_options <- sensitivity_options[profile_option_names]
params_to_run <- unname(param_map[names(selected_options)[unlist(selected_options)]])

if (length(params_to_run) > 0) {
  
  # Ensure parallel packages are loaded
  suppressPackageStartupMessages({
    library(doParallel)
    library(foreach)
  })
  
  n_cores <- parallel::detectCores() - 1
  message(paste("Likelihood profiling will use up to", n_cores, "cores."))
  
  model_name <- sensitivity_options$model_folder
  
  replist <- SS_output(dir = model_name,
                       verbose = FALSE,
                       printstats = FALSE,
                       covar = TRUE)
  
  # Main loop to process each parameter
  for (param in params_to_run) {
    
    message(paste0("\n### Starting parallel profile for: ", param, " ###\n"))
    
    ## Define Profile Vectors and Labels ---------------------------
    
    if (param == "SR_LN(R0)") {
      est_params <- replist$estimated_non_dev_parameters
      est_params$upper <- est_params$Value + 1.96 * est_params$Parm_StDev
      est_params$lower <- est_params$Value - 1.96 * est_params$Parm_StDev
      x <- est_params["SR_LN(R0)", "Value"]
      upper <- est_params["SR_LN(R0)","upper"]
      lower <- est_params["SR_LN(R0)","lower"]
      (vec <- seq(lower, upper, length.out = 11))
      
      
      if (upper == lower) {
        message(paste0("Parm_StDev for '", param, "' is zero. Reverting to a manual profile range."))
        manual_range <- 0.1
        upper <- x + manual_range
        lower <- x - manual_range
        (vec <- seq(lower, upper, length.out = 11))
        
        ## Testing manual vec to match Matias
        vec <- c(4.13088, 4.532493333, 4.934106667, 5.33572, 5.737333333, 6.138946667, 6.54056, 6.942173333, 7.343786667, 7.7454)
        message(paste("Testing manual vec to match Matias, vec: ", vec, ""))
        
      }
      profile.label <-"SR_LN(R0)"


      
    } else if (param == "SR_BH_steep") {
      # x <- 0.75
      # upper <- x + 0.2
      # lower <- x - 0.2
      # profile.label <- "Steepness (h)"
      # (vec <- seq(lower, upper, ((upper - lower) / 6)))
      
      # Steepness (h) - Beverton-Holt
      est_params <- replist$estimated_non_dev_parameters
      # First, check if 'SR_BH_steep' is in the list of estimated parameters
      if ("SR_BH_steep" %in% rownames(est_params)) {
        
        # --- LOGIC IF STEEPNESS IS ESTIMATED ---
        message("Steepness is an estimated parameter. Creating profile range based on its StDev.")
        
        x <- est_params["SR_BH_steep", "Value"]
        upper <- est_params["SR_BH_steep", "upper"]
        lower <- est_params["SR_BH_steep", "lower"]
        
        # A safety check in case the standard deviation is zero
        if (upper == lower) {
          message("...Parm_StDev for steepness is zero. Using a manual range of +/- 0.2 instead.")
          manual_h_range <- 0.2
          upper <- x + manual_h_range
          lower <- x - manual_h_range
        }
        
      } else {
        
        # --- LOGIC IF STEEPNESS IS FIXED ---
        message("Steepness is a fixed parameter. Creating a manual profile range.")
        
        # Get the fixed value from the full parameter list, which includes fixed pars
        x <- replist$parameters[replist$parameters$Label == "SR_BH_steep", "Value"]
        
        # Add a check to ensure we found the parameter at all
        if (length(x) == 0) {
          stop("Could not find 'SR_BH_steep' in the model parameters at all. Please check the control file.")
        }
        
        # Define a sensible manual range for the profile (you can adjust this)
        manual_h_range <- 0.2
        upper <- x + manual_h_range
        lower <- x - manual_h_range
        message(paste0("...Using fixed value of ", round(x, 2), " and a manual range of +/- ", manual_h_range))
      }
      
      profile.label <- "Steepness (h)"
      # Define the vector of values to profile over, creating 7 points
      (vec <- seq(lower, upper, length.out = 7))
      
    } else if (param == "NatM_uniform_Fem_GP_1") {
      
      # # x <- replist$MGparmAdj$NatM_uniform_Fem_GP_1[1]
      # # upper <- x + 0.06
      # # lower <- x - 0.06
      # # profile.label <- "Natural mortality (M)"
      # # (vec <- seq(lower, upper, ((upper - lower) / 6)))
      # 
      # 
      # # Natural mortality
      # # x <- replist$MGparmAdj$NatM_uniform_Fem_GP_1[1]
      # 
      # ### APPLY FIX HERE. NEED TO HANDLE IF NATM is called
      # ### #_NatM_p_1_Fem_GP_1
      # 
      # # First, get all the names from the list
      # list_names <- names(replist$MGparmAdj)
      # 
      # print(list_names)
      # 
      # # Find the name that matches the pattern
      # # The pattern '^NatM.*_Fem_GP_1$' means:
      # #   ^      - starts with
      # #   NatM   - the literal text "NatM"
      # #   .* - any character, any number of times
      # #   _Fem_GP_1 - the literal text "_Fem_GP_1"
      # #   $      - ends with
      # target_name <- grep("^NatM.*_Fem_GP_1$", list_names, value = TRUE)
      # 
      # # Select the value using the found name
      # # We add a check to ensure a name was actually found
      # if (length(target_name) > 0) {
      #   # Use [[...]] for variable names and take the first match
      #   x <- replist$MGparmAdj[[target_name[1]]][1]
      # } else {
      #   x <- NA # Or stop("Parameter not found!")
      # }
      # 
      # print("x is \n")
      # print(grep("^NatM.*_Fem_GP_1$", list_names, value = TRUE))
      
      ### APPLY FIX HERE. NEED TO HANDLE IF NATM is called
      ### #_NatM_p_1_Fem_GP_1
      
      message("Debug: Searching for NatM parameter...")
      
      # Get all names
      list_names <- names(replist$MGparmAdj)
      
      # Search using regex
      target_name <- grep("^NatM.*_Fem_GP_1$", list_names, value = TRUE)
      
      # if (length(target_name) > 0) {
      #   # Found it
      #   actual_name <- target_name[1]
      #   x <- replist$MGparmAdj[[actual_name]][1]
      #   message(paste("Debug: Found", actual_name, "Value:", x))
      # } else {
      #   # NOT FOUND - PRINT DEBUG INFO BEFORE CRASHING
      #   message("Debug: Match failed. Available MGparmAdj names are:")
      #   print(list_names) # This prints to console log so you can see what it SHOULD be
      #   
      #   x <- NULL # Allow fallback to NatM_vector logic below
      # }
      
      if (length(target_name) > 0) {
        # Found it
        actual_name <- target_name[1]
        x <- replist$MGparmAdj[[actual_name]][1]
        
        # --- FIX: Save the actual name for use in SS_changepars later ---
        found_natm_name <- actual_name 
        # ----------------------------------------------------------------
        
        message(paste("Debug: Found", actual_name, "Value:", x))
      } else {
        # NOT FOUND - PRINT DEBUG INFO BEFORE CRASHING
        message("Debug: Match failed. Available MGparmAdj names are:")
        print(list_names) 
        
        x <- NULL 
        found_natm_name <- NULL # Ensure this is null if not found
      }
      
      ## START FRIDAY TESTING FOR A NATM VECTOR FOR SANDBAR
      
      # if(is.null(x)){
      if(is.null(x) || is.na(x)){
        
        message("Could not find NatM_uniform_Fem_GP_1 in model, trying for M_at_age vector")
        
        message("Changing param from NatM_uniform_Fem_GP_1 to NatM_vector")
        param <- "NatM_vector"
        
        # Read the files from the base model run directory to get the M_at_age vector
        dat_temp <- SS_readdat(file.path(model_name, "datafile.dat"), verbose = FALSE)
        
        ctl_temp <- SS_readctl(file = file.path(model_name, "controlfile.ctl"), 
                               datlist = dat_temp, 
                               verbose = FALSE)
        
        library(dplyr)
        library(purrr)
        
        M_at_age <- ctl_temp$natM
        
        # Extract the last row and select the columns from '0' to '30'
        M_at_age <- ctl_temp$natM %>%
          tail(1) %>%
          # select(as.character(0:30)) %>%
          as.numeric()
        
        # 1. Define the sequence of modifiers (offsets) you want to apply
        offsets <- seq(-0.03, 0.03, by = 0.01)
        
        # This will create a vector of 7 numbers: -0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03
        message("These are the modifiers we'll use:")
        print(offsets)
        
        # 2. Use map() to add each offset to the entire M_at_age vector
        # The result will be a list of 7 vectors, each with 31 values.
        vec <- offsets %>%
          purrr::map(~ M_at_age + .x)

        vec[[1]]
        
        message("These are the modifed NatM vectors we'll use:")
        print(vec)
        
        profile.label <- "Natural mortality (M) vector"
        
      } else {
      
      ## END FRIDAY TESTING FOR A NATM VECTOR FOR SANDBAR
      
        # Default shifts of M for NatM profiling
      upper <- x+0.06
      lower <- x-0.06
      
      profile.label <- "Natural mortality (M)"
      (vec <- seq(lower,upper,((upper-lower)/6)))
      
      }
      

      # --- ADD THE NEW LOGIC HERE ---
    } else if (param %in% c("L_at_Amax_Fem_GP_1", "L_at_Amax_Mal_GP_1", "VonBert_K_Fem_GP_1", "VonBert_K_Mal_GP_1")) {
      
      message(paste("Setting up range for growth parameter:", param))
      est_params <- replist$estimated_non_dev_parameters
      
      # Try to find the parameter in the estimated parameters list
      if (param %in% rownames(est_params)) {
        val <- est_params[param, "Value"]
        sd_val <- est_params[param, "Parm_StDev"]
        
        # Use 1.96 standard deviations to create the profile range
        if (sd_val > 0) {
          lower <- val - (1.96 * sd_val)
          upper <- val + (1.96 * sd_val)
        } else {
          # Fallback if SD is 0 but parameter is estimated
          lower <- val * 0.9
          upper <- val * 1.1
        }
      } else {
        # Fallback if the parameter is currently fixed (not being estimated)
        val <- replist$parameters[replist$parameters$Label == param, "Value"]
        lower <- val * 0.8
        upper <- val * 1.2
      }
      
      # Create a vector of 7 points for the profile
      vec <- seq(lower, upper, length.out = 7)
      profile.label <- param
      # --- END OF NEW LOGIC ---

      # START OF NEWLY ADDED CODE (PART 1 of 2)

    } else if (param %in% c("Depl", "CurSB")) {
      # Depletion profiles
      if (param == "Depl") {
        message("Setting up profile for Final Year Depletion.")
        # Use the vector from the shiny app's selected options

        
        # vec <- sensitivity_options$profile_final_depletion_vec
        # profile.label <- paste0("Depletion B/B0 (", replist$endyr, ")")
        
        
        # Depl
        derived_quants <- replist$derived_quants
        derived_quants$upper <- derived_quants$Value + 1.96 * derived_quants$StdDev
        derived_quants$lower <- pmax(derived_quants$Value - 1.96 * derived_quants$StdDev,0)
        
        # Depl - Current depletion
        x <- derived_quants[paste0("Bratio_",replist$endyr),2]
        upper <- derived_quants[paste0("Bratio_",replist$endyr),6]
        lower <- derived_quants[paste0("Bratio_",replist$endyr),7]
        
        (vec <- seq(lower,upper,((upper-lower)/10)))
        
        profile.label <- "Current depletion"
        
        # message("vec is:",vec)
      } else { # CurSB
        message("Setting up profile for Current Spawning Biomass.")
        # vec <- sensitivity_options$profile_current_spawning_biomass_vec
        # profile.label <- paste0("Spawning Biomass (", replist$endyr, ")")
        
        # CurSB
        derived_quants <- replist$derived_quants
        derived_quants$upper <- derived_quants$Value + 1.96 * derived_quants$StdDev
        derived_quants$lower <- pmax(derived_quants$Value - 1.96 * derived_quants$StdDev,0)
        
        # CurSB - Current spawning biomass
        x<- derived_quants[paste0("SSB_",replist$endyr),2]
        upper<- derived_quants[paste0("SSB_",replist$endyr),6]
        lower<- derived_quants[paste0("SSB_",replist$endyr),7]
        
        (vec <- seq(lower,upper,((upper-lower)/10)))
        
        profile.label <- "Current spawning biomass"
        
        # message("vec is:",vec)
        
      }

      # END OF NEWLY ADDED CODE (PART 1 of 2)

      
    } else {
      warning(paste0("Profile setup for '", param, "' is not defined. Skipping..."))
      next
    }
    
    ## SETUP PROFILING SUBDIRECTORIES (SEQUENTIAL) ----
    
    # Define the main directory for this specific parameter's profile
    dir_prof <- file.path(model_name, "profile", param)
    unlink(dir_prof, recursive = TRUE, force = TRUE) # Clean up old runs
    dir.create(dir_prof, showWarnings = FALSE, recursive = TRUE)
    
    # Create a vector of subdirectory paths for the parallel runs
    prof_run_dirs <- file.path(dir_prof, paste0("run_", 1:length(vec)))
    
    message(paste("Sequentially preparing", length(vec), "subdirectories for parallel runs..."))
    

    # Loop through each profile value to prepare its directory
    for (i in 1:length(vec)) {

      run_dir <- prof_run_dirs[i]
      dir.create(run_dir, showWarnings = FALSE, recursive = TRUE)

      # Copy all necessary SS files into the new run directory
      copy_SS_inputs(
        dir.old = model_name,
        dir.new = run_dir,
        create.dir = TRUE,
        overwrite = TRUE,
        copy_exe = TRUE,
        copy_par = TRUE,
        verbose = FALSE
        # use_ss_new = TRUE
      )
      
      # # --- Dynamically set the string value to avoid clashed with multiple ways of writing natM ---
      # # This ensures the correct parameter is used for the x-axis of the plot
      # if (param == "SR_LN(R0)") {
      #   p.string <- "R0"
      # } else if (param == "SR_BH_steep") {
      #   p.string <- "steep"
      # } else if (param == "NatM_uniform_Fem_GP_1") {
      #   p.string <- "NatM_uniform_Fem_GP_1"
      # } else {
      #   # Fallback for any other parameters
      #   p.string <- param
      # }
      # 
      # 
      # SS_changepars(
      #   dir = run_dir,
      #   ctlfile = "controlfile.ctl",
      #   newctlfile = "control_modified.ss",
      #   strings = p.string,
      #   newvals = vec[i],
      #   verbose = FALSE,
      #   estimate = FALSE) 
      


      # For depletion profiles, we need to modify the data and control files
      # to add a new survey with a fixed index value.
      if (param %in% c("Depl", "CurSB")) {

        # Read the files from the specific run directory
        dat_temp <- SS_readdat(file.path(run_dir, "datafile.dat"), verbose = FALSE)
        # ctl_temp <- SS_readctl(file.path(run_dir, "controlfile.ctl"), verbose = FALSE, use_datlist = FALSE)
        
        ctl_temp <- SS_readctl(file = file.path(run_dir, "controlfile.ctl"), 
                               datlist = dat_temp, 
                               verbose = FALSE)
        
        # --- Modify data file ---
        dat_temp$Nfleets <- dat_temp$Nfleets + 1
        new_fleet_num <- dat_temp$Nfleets
        
        # Add fleet info (duplicating a survey is a safe way)
        dat_temp$fleetinfo <- rbind(dat_temp$fleetinfo, dat_temp$fleetinfo[1, ])
        dat_temp$fleetinfo[new_fleet_num, "fleetname"] <- "Depletion_Survey"
        dat_temp$fleetinfo[new_fleet_num, "type"] <- 3
        
        # Make sure its fishery_timing is 1
        dat_temp$fleetinfo[new_fleet_num, "surveytiming"] <- 1
        
        
        
        # determine which unit for indices, 34 = depletion, 30= CurSB
        indices_units <- ifelse(param == "Depl",34,30)
        
        # dat_temp$CPUEinfo <- rbind(as.data.frame(dat_temp$CPUEinfo), 
        #                            c(new_fleet_num, 34, 0, 0)) # 34 = depletion
        # Add CPUE info
        dat_temp$CPUEinfo <- rbind(as.data.frame(dat_temp$CPUEinfo), 
                                   c(new_fleet_num, indices_units, 0, 0)) # 34 = depletion, 30= CurSB
        
        # Add settings row for the new fleet to lencomp and agecomp info
        new_comp_info_row <- data.frame(
          mintailcomp = -1, addtocomp = 0.001, combine_M_F = 0,
          CompressBins = 0, CompError = 0, ParmSelect = 0, minsamplesize = 0.001
        )
        
        row.names(new_comp_info_row) <- "Depletion_Survey"
        
        dat_temp$len_info <- rbind(dat_temp$len_info, new_comp_info_row)
        dat_temp$age_info <- rbind(dat_temp$age_info, new_comp_info_row)
        
        # Add the actual index data lines, using the value from the profile vector `vec`
        
        if (param %in% c("Depl")) {
        
        new_indices <- data.frame(
          year = c(dat_temp$styr - 1, dat_temp$endyr), month = 1,
          index = new_fleet_num, obs = c(1.0, vec[i]), se_log = 0.0001
        )
        
        }
        
        if (param %in% c("CurSB")) {
          
          new_indices <- data.frame(
            year = dat_temp$endyr, month = 1,
            index = new_fleet_num, obs = vec[i], se_log = 0.0001
          )
          
        }
        
        dat_temp$CPUE <- rbind(dat_temp$CPUE, new_indices)
        
        # --- Modify control file ---
        # ctl_temp$Q_options <- rbind(ctl_temp$Q_options, c(new_fleet_num, 1, 0, 0, 0, 0))
        
        # Create the vector with the correct column names
        new_q_row <- c(new_fleet_num, 1, 0, 0, 0, 0)
        names(new_q_row) <- names(ctl_temp$Q_options)
        
        # Add the row to the data frame and assign the row name in one step
        ctl_temp$Q_options <- rbind(ctl_temp$Q_options, Depletion_Survey = new_q_row)

        
        
        new_q_parm <- c(-15, 15, 0, 0, 1, 0, -1, 0, 0, 0, 0, 0, 0, 0) # Phase -1 makes it non-estimated
        ctl_temp$Q_parms <- rbind(ctl_temp$Q_parms, Depletion_Survey = new_q_parm)
        
        ctl_temp$size_selex_types <- rbind(ctl_temp$size_selex_types, Depletion_Survey = c(0, 0, 0, 0)) # Non-selective
        ctl_temp$age_selex_types <- rbind(ctl_temp$age_selex_types, Depletion_Survey = c(0, 0, 0, 0))   # Non-selective
        
        # Write the modified files back out, overwriting the copied ones
        SS_writedat(dat_temp, file.path(run_dir, "datafile.dat"), overwrite = TRUE, verbose = FALSE)
        SS_writectl(ctl_temp, file.path(run_dir, "control_modified.ss"), overwrite = TRUE, verbose = FALSE)
        
        # --- Set the string for plotting later ---
        p.string <- paste0("Spawning_depletion_", dat_temp$endyr)
        
        
        
        # Use par file options
        

        starter <- SS_readstarter(file.path(run_dir, "starter.ss"), verbose = FALSE)
        starter[["ctlfile"]] <- "control_modified.ss"
        starter[["prior_like"]] <- 1 # include likelihoods of non-estimated params
        
        use_par_file <- sensitivity_options$profile_use_par
        
        if(use_par_file) {
          message("Using par file for model specification")
          starter[["init_values_src"]] <- 1 # use par file as initial values instead of ctl file
          SS_writestarter(starter, dir = run_dir, overwrite = TRUE, verbose = FALSE)
          
          # read the par file

          # message("Trying to open par file")
          # # # read the par file
          # par <- SS_readpar_3.30(parfile = file.path(run_dir, "ss3.par"), datsource = file.path(run_dir,"datafile.dat"),
          #                        ctlsource = file.path(run_dir, "control_modified.ss"), verbose = FALSE)
          # 
          # 
          # # SS_readpar_3.30 doesn't like it. Might have to modify par file manually
          # 
          # ## Add in a Q line for deplt fleet
          # new_row <- c(0, 0.000000)
          # 
          # # Add the new row to the existing object using rbind()
          # par$Q_parms <- rbind(par$Q_parms, "LnQ_base_Depletion_orCursb_Survey" = new_row)
          # 
          # message("Writing par file again to see if it works.")
          # SS_writepar_3.30(par,outfile = file.path(run_dir, "ss3 test.par"), overwrite = TRUE)
          
          # Manuallly edit par file
          
          # 1. Define file paths
          # input_file_path <- "ss3par.txt"
          # output_file_path <- file.path(run_dir, "ss3.par")
          
          # 2. Read all lines from the original file
          lines <- readLines(file.path(run_dir, "ss3.par"))
          
          # 3. Find the indices of all lines containing a Q_parm entry
          q_parm_indices <- grep("# Q_parm\\[", lines)
          
          # Check if any Q_parm lines were found
          if (length(q_parm_indices) > 0) {
            # 4. Get the index and content of the *last* Q_parm line
            last_q_parm_label_index <- tail(q_parm_indices, 1)
            last_q_parm_label_line <- lines[last_q_parm_label_index]
            
            # 5. Extract the number from the last Q_parm line and increment it
            # This regular expression extracts the digits from inside "[ ]"
            last_q_number <- as.numeric(gsub(".*Q_parm\\[(\\d+)\\].*", "\\1", last_q_parm_label_line))
            new_q_number <- last_q_number + 1
            
            # 6. Create the new lines to be added
            new_content <- c(
              paste0("# Q_parm[", new_q_number, "]:"),
              "0"
            )
            
            # 7. Insert the new content right after the value of the last Q_parm
            # The insertion point is after the last Q_parm's value line
            insertion_point <- last_q_parm_label_index + 1
            modified_lines <- append(lines, new_content, after = insertion_point)
            
            # 8. Write the modified lines to a new file
            writeLines(modified_lines, file.path(run_dir, "ss3.par"))
            
            cat("Successfully added '# Q_parm[", new_q_number, "]' to the file '", file.path(run_dir, "ss3.par"), "'.\n", sep = "")
            
          } else {
            cat("No '# Q_parm' entries were found in the file. No changes were made.\n")
          }
          
          
        }
        
      } else if(param == "NatM_vector") {
        
        message("Modifying files for NatM_vector, manually changing input files")
        
        # Read the files from the specific run directory
        dat_temp <- SS_readdat(file.path(run_dir, "datafile.dat"), verbose = FALSE)
        # ctl_temp <- SS_readctl(file.path(run_dir, "controlfile.ctl"), verbose = FALSE, use_datlist = FALSE)
        
        ctl_temp <- SS_readctl(file = file.path(run_dir, "controlfile.ctl"), 
                               datlist = dat_temp, 
                               verbose = FALSE)
        
        ctl_temp$natM["natM1", ] <- vec[[i]]
        ctl_temp$natM["natM2", ] <- vec[[i]]
        
        SS_writectl(ctl_temp, file.path(run_dir, "control_modified.ss"), overwrite = TRUE, verbose = FALSE)
      }
      
      else {
        # --- This is the original logic for standard parameters ---
        # # Dynamically set the string value to avoid clashes
        # if (param == "SR_LN(R0)") {
        #   p.string <- "R0"
        # } else if (param == "SR_BH_steep") {
        #   p.string <- "steep"
        # } else if (param == "NatM_uniform_Fem_GP_1") {
        #   # p.string <- "NatM_uniform_Fem_GP_1"
        #   
        #   # --- FIX: Use the dynamic name found via regex earlier ---
        #   if(exists("found_natm_name") && !is.null(found_natm_name)){
        #     p.string <- found_natm_name
        #     message(paste("Using dynamic parameter string:", p.string))
        #   } else {
        #     p.string <- "NatM_uniform_Fem_GP_1" # Fallback
        #   }
        #   # ---------------------------------------------------------
        #   
        # } else {
        #   p.string <- param
        # }
        
        # --- Dynamically set the string value to avoid clashes with multiple ways of writing natM ---
        if (param == "SR_LN(R0)") {
          p.string <- "R0"
        } else if (param == "SR_BH_steep") {
          p.string <- "steep"
        } else if (param == "L_at_Amax_Fem_GP_1") {
          p.string <- "L_at_Amax_Fem"
        } else if (param == "L_at_Amax_Mal_GP_1") {
          p.string <- "L_at_Amax_Mal"
        } else if (param == "VonBert_K_Fem_GP_1") {
          p.string <- "VonBert_K_Fem"
        } else if (param == "VonBert_K_Mal_GP_1") {
          p.string <- "VonBert_K_Mal"
          
        } else if (param == "NatM_uniform_Fem_GP_1") {
          
          # --- FIX: Search the CONTROL file for the actual parameter label ---
          # The Report file might say 'uniform' while the Control file says 'p_1'
          # SS_changepars needs the exact Control file label.
          
          # Read the control file parameter lines
          ctl_params <- SS_parlines(ctlfile = file.path(run_dir, "controlfile.ctl"), verbose = FALSE)
          
          # Grep for the Female GP 1 NatM parameter
          # matches "NatM" followed by anything (e.g. "_p_1_" or "_uniform_"), then "Fem", then "GP_1"
          natm_label <- grep("NatM.*Fem.*GP_1", ctl_params$Label, value = TRUE)
          
          if(length(natm_label) > 0) {
            p.string <- natm_label[1]
            message(paste("Found NatM label in control file:", p.string))
          } else {
            # Fallback: try the name found in the report, or default
            if(exists("found_natm_name") && !is.null(found_natm_name)){
              p.string <- found_natm_name
            } else {
              p.string <- "NatM_uniform_Fem_GP_1"
            }
            message(paste("Could not find NatM in control file, using fallback:", p.string))
          }
          
        } else {
          p.string <- param
        }
        
        SS_changepars(
          dir = run_dir,
          ctlfile = "controlfile.ctl", # Use the base control file name
          newctlfile = "control_modified.ss",
          strings = p.string,
          newvals = vec[i],
          verbose = FALSE,
          estimate = FALSE)
        
        # --- Modify starter for standard parameters ---
        starter <- SS_readstarter(file.path(run_dir, "starter.ss"), verbose = FALSE)
        starter[["ctlfile"]] <- "control_modified.ss"
        starter[["prior_like"]] <- 1 # include likelihoods of non-estimated params

        use_par_file <- sensitivity_options$profile_use_par

        if(use_par_file) {
        message("Using par file for model specification")
        starter[["init_values_src"]] <- 1 # use par file as initial values instead of ctl file

        
        # Define the file path
        par_file <- file.path(run_dir, "ss3.par")
        # Read the file content
        lines <- readLines(par_file)
        # Replace 'SRparm' with 'SR_parm' globally
        lines <- gsub("SRparm", "SR_parm", lines)
        # Write the modified content back to the file
        writeLines(lines, par_file)
        
        # message(lines)
        
        message("Fixed SRparm labels in ", par_file)
        
        message("Start SS_readpar_3.30")
        
        message("run_dir: ", run_dir )
        
        # --- START FIX: Handle mismatched recdev_early_start ---
        # Problem: If control file has "2" but par file has 75 devs, SS_readpar crashes.
        # Solution: Count devs in par file and auto-correct control file before reading.
        
        local({
          ctl_path <- file.path(run_dir, "control_modified.ss")
          par_path <- file.path(run_dir, "ss3.par")
          
          if(file.exists(ctl_path) && file.exists(par_path)) {
            ctl_lines <- readLines(ctl_path)
            par_lines <- readLines(par_path)
            
            # 1. Find recdev_early_start in Control file
            idx_start <- grep("recdev_early_start", ctl_lines)
            if (length(idx_start) > 0) {
              # Extract the value (assumes format "VALUE #_comment")
              val_str <- strsplit(trimws(ctl_lines[idx_start]), "\\s+")[[1]][1]
              val <- suppressWarnings(as.numeric(val_str))
              
              # Check if it's a suspicious value (e.g., small integer like 2, but not 0 or negative)
              # Standard years are usually > 1900.
              if (!is.na(val) && val > 0 && val < 1800) {
                message("Detected suspicious 'recdev_early_start' value: ", val, ". Attempting to fix...")
                
                # 2. Count actual early devs in Par file
                # Find the line starting with "# recdev_early"
                idx_par_section <- grep("^# recdev_early", par_lines)
                
                if (length(idx_par_section) > 0) {
                  # Find the start of the NEXT section (next line starting with #)
                  all_hash_lines <- grep("^#", par_lines)
                  idx_next_section <- all_hash_lines[all_hash_lines > idx_par_section][1]
                  
                  if(!is.na(idx_next_section)) {
                    # Extract lines between header and next section
                    dev_lines <- par_lines[(idx_par_section + 1):(idx_next_section - 1)]
                    
                    # Convert to one long string and split by whitespace to count numbers
                    dev_vals <- as.numeric(unlist(strsplit(paste(dev_lines, collapse=" "), "\\s+")))
                    dev_vals <- dev_vals[!is.na(dev_vals)] # Remove NAs from empty lines/spaces
                    n_early_devs <- length(dev_vals)
                    
                    message("...Found ", n_early_devs, " early recruitment deviations in ss3.par.")
                    
                    # 3. Find MainRdevYrFirst to calculate the correct Start Year
                    idx_main_yr <- grep("first year of main recr_devs", ctl_lines)
                    if(length(idx_main_yr) > 0) {
                      main_yr_str <- strsplit(trimws(ctl_lines[idx_main_yr]), "\\s+")[[1]][1]
                      main_yr <- as.numeric(main_yr_str)
                      
                      # Calculate correct start year: MainYear - NumberOfDevs
                      correct_start <- main_yr - n_early_devs
                      
                      # 4. Modify Control File
                      ctl_lines[idx_start] <- paste0(correct_start, " #_recdev_early_start (Auto-corrected from ", val, " by SS_sensitivities.R)")
                      writeLines(ctl_lines, ctl_path)
                      message("...Updated control_modified.ss: changed recdev_early_start to ", correct_start)
                    }
                  }
                }
              }
            }
          }
        })
        # --- END FIX ---
        
        # read the par file
        par <- SS_readpar_3.30(parfile = file.path(run_dir, "ss3.par"), datsource = file.path(run_dir,"datafile.dat"),
                               ctlsource = file.path(run_dir, "control_modified.ss"), verbose = T)

        # par <- SS_readpar_3.30(parfile = file.path(model_dir, "ss3.par"), datsource = file.path(model_dir,"datafile.dat"),
                               # ctlsource = file.path(model_dir, "controlfile.ctl"), verbose = T)
        
        message("Finished SS_readpar_3.30")
        
        if (param == "SR_LN(R0)") {
          message("Changing SR_LN(R0) in par file")
          
          ## These are not in par file? how did it work before?
          par$SR_parms["SR_LN(R0)", "INIT"] <-  vec[i] # change initial val in par file
          par$SR_parms["SR_LN(R0)", "ESTIM"] <-  vec[i] # not sure if this is necessary but changed this too, just to be on the safe side
          # SS_writepar_3.30(par,outfile = file.path(run_dir, "ss3.par"), overwrite = TRUE)
        }

        if (param == "SR_BH_steep") {
          message("Changing SR_BH_steep in par file")
          ## These are not in par file?
          par$SR_parms["SR_BH_steep", "INIT"] <-  vec[i] # change initial val in par file
          par$SR_parms["SR_BH_steep", "ESTIM"] <-  vec[i] # not sure if this is necessary but changed this too, just to be on the safe side
          # SS_writepar_3.30(par,outfile = file.path(run_dir, "ss3.par"), overwrite = TRUE)
        }

        if (param == "NatM_uniform_Fem_GP_1") {
          message("Changing NatM_p_1_Fem_GP_1 in par file")
          # Change NatM in par file
          par$MG_parms["NatM_p_1_Fem_GP_1", "INIT"] <-  vec[i] # change initial val in par file
          par$MG_parms["NatM_p_1_Fem_GP_1", "ESTIM"] <-  vec[i] # not sure if this is necessary but changed this too, just to be on the safe side
          # SS_writepar_3.30(par,outfile = file.path(run_dir, "ss3.par"), overwrite = TRUE)
        }
        
        if (param == "L_at_Amax_Fem_GP_1") {
          message("Changing L_at_Amax_Fem_GP_1 in par file")
          # Ensure exact matching of the parameter name in the par file
          par$MG_parms["L_at_Amax_Fem_GP_1", "INIT"] <- vec[i]
          par$MG_parms["L_at_Amax_Fem_GP_1", "ESTIM"] <- vec[i]
        }
        if (param == "L_at_Amax_Mal_GP_1") {
          message("Changing L_at_Amax_Mal_GP_1 in par file")
          par$MG_parms["L_at_Amax_Mal_GP_1", "INIT"] <- vec[i]
          par$MG_parms["L_at_Amax_Mal_GP_1", "ESTIM"] <- vec[i]
        }
        if (param == "VonBert_K_Fem_GP_1") {
          message("Changing VonBert_K_Fem_GP_1 in par file")
          par$MG_parms["VonBert_K_Fem_GP_1", "INIT"] <- vec[i]
          par$MG_parms["VonBert_K_Fem_GP_1", "ESTIM"] <- vec[i]
        }
        if (param == "VonBert_K_Mal_GP_1") {
          message("Changing VonBert_K_Mal_GP_1 in par file")
          par$MG_parms["VonBert_K_Mal_GP_1", "INIT"] <- vec[i]
          par$MG_parms["VonBert_K_Mal_GP_1", "ESTIM"] <- vec[i]
        }
        
        

        SS_writepar_3.30(par,outfile = file.path(run_dir, "ss3.par"), overwrite = TRUE)

        # message("End change of PAR FILES")

        }

        SS_writestarter(starter, dir = run_dir, overwrite = TRUE, verbose = FALSE)
      }
      
      # For depletion runs, the control file isn't renamed, so we must also check phases.
      # For simplicity, this phase-checking logic will now run for all profile types.
      control_to_check <- ifelse(param %in% c("Depl", "CurSB"), "controlfile.ctl", "control_modified.ss")
      ctltable_new <- SS_parlines(ctlfile = file.path(run_dir, control_to_check), verbose = FALSE)
      

      
      # Check if any parameters are in phase 1; if not, move one from phase 2.
      # This prevents the model from failing if the profiled parameter was the only one in phase 1.
      
      # # Read the control file that was just created to check its contents
      # control_mod_path <- file.path(run_dir, "control_modified.ss")
      # ctltable_new <- SS_parlines(ctlfile = control_mod_path, verbose = FALSE)

      

      
      # Check if any parameter is in phase 1
      if (is.data.frame(ctltable_new) && !any(ctltable_new[["PHASE"]] == 1)) {
        
        message(paste0("Run ", i, ": No parameters found in phase 1."))
        
        # Find the lowest positive phase that has parameters available
        positive_phases <- sort(unique(ctltable_new$PHASE[ctltable_new$PHASE > 0]))
        
        if (length(positive_phases) > 0) {
          lowest_available_phase <- positive_phases[1]
          
          message(paste0("...Searching for a parameter to activate from phase ", lowest_available_phase, "."))
          
          # Get all candidate parameters from the lowest available phase
          candidate_pars <- ctltable_new[which(ctltable_new[["PHASE"]] == lowest_available_phase), ]
          
          # Prioritise Size-Selectivity parameters
          selex_labels <- candidate_pars[grepl("SizeSel", candidate_pars$Label, ignore.case = TRUE), "Label"]
          
          if (length(selex_labels) > 0) {
            # If we found at least one selex param, pick the first one alphabetically
            par_label_to_change <- sort(selex_labels)[1]
            message(paste0("...Prioritising and activating selectivity parameter: '", par_label_to_change, "'."))
          } else {
            # FALLBACK: If no selex params, revert to the original logic
            message("...No selectivity parameters found in this phase. Activating the first available parameter alphabetically.")
            par_label_to_change <- sort(candidate_pars$Label)[1]
          }
          
          # --- FINAL FIX: Use the unique line number to avoid ambiguity ---
          # Get the line number associated with the chosen parameter label
          line_num_to_change <- candidate_pars[candidate_pars$Label == par_label_to_change, "Linenum"]
          
          # Double-check we only got one line number (which should always be true here)
          if(length(line_num_to_change) == 1){
            # Modify the control file using the specific and unambiguous line number
            SS_changepars(dir = run_dir, 
                          ctlfile = "control_modified.ss", 
                          newctlfile = "control_modified.ss",
                          linenums = line_num_to_change, # Use linenums argument
                          newphs = 1, 
                          verbose = FALSE)
          } else {
            warning(paste0("Run ", i, ": Could not find a unique line number for parameter '", par_label_to_change, "'."))
          }
        } else {
          warning(paste0("Run ", i, ": No parameters in phase 1 and no other active parameters found to promote."))
        }
      }
        
      # Modify the starter file to use the new control file
      starter <- SS_readstarter(file.path(run_dir, "starter.ss"), verbose = FALSE)
      starter[["ctlfile"]] <- "control_modified.ss"
      starter[["prior_like"]] <- 1
      SS_writestarter(starter, dir = run_dir, overwrite = TRUE, verbose = FALSE)
    }
    
    ## EXECUTE MODEL RUNS IN PARALLEL ----
    
    message("Executing profile runs in parallel...")
    
    start_time <- Sys.time()
    message(paste("Parallel execution started:", start_time))
    
    # Set up the parallel cluster
    my_cluster <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(my_cluster)
    
    # Use foreach to run the model in each subdirectory
    foreach(
      run_dir = prof_run_dirs,
      .packages = 'r4ss'
    ) %dopar% {
      r4ss::run(dir = run_dir, exe = "ss")
    }
    
    # It's crucial to stop the cluster after the parallel loop
    parallel::stopCluster(my_cluster)
    message("Finished parallel execution.")
    
    end_time <- Sys.time()
    duration_paralell.likelihood <- end_time - start_time
    message(paste("Likelihood Parallel execution took:", round(duration_paralell.likelihood, 2), units(duration_paralell.likelihood)))
    

    # <<< START: NEW CODE TO FILTER FAILED RUNS >>>

    
    message("Checking for successful model runs before creating plots...")
    
    # 1. Create a logical vector indicating which runs were successful.
    #    A run is successful if its Report.sso file exists and is not empty.
    successful_runs_check <- sapply(prof_run_dirs, function(dir) {
      report_file <- file.path(dir, "Report.sso")
      return(file.exists(report_file) && file.info(report_file)$size > 0)
    })
    
    # 2. Create new, clean vectors containing only the info for the successful runs.
    successful_run_dirs <- prof_run_dirs[successful_runs_check]
    successful_vec <- vec[successful_runs_check]
    
    # 3. Report the outcome to the console.
    num_successful <- sum(successful_runs_check)
    num_failed <- length(vec) - num_successful
    
    if (num_failed > 0) {
      message(paste("...", num_successful, "runs were successful.", num_failed, "failed and will be excluded from plots."))
    } else {
      message("...All runs were successful.")
    }
    
    # 4. Check if there are enough successful runs to make a plot.
    #    If not, skip to the next parameter in the main loop.
    if (num_successful < 2) {
      warning(paste0("Fewer than 2 successful runs found for '", param, "'. Skipping summary and plotting."), immediate. = TRUE)
      next # This jumps to the next iteration of the main 'for (param in params_to_run)' loop
    }
    

    # <<< END: NEW FILTERING CODE >>>

    
    ## GATHER RESULTS and PLOT (similar to original) ----
    message("Gathering results and generating plots...")
  
    message("Gathering results directly from parallel run directories...")
    
    # Pass the vector of run directories directly to SSgetoutput.
    # This avoids all manual file copying and renaming issues.
    # profilemodels <- SSgetoutput(dirvec = prof_run_dirs, getcovar = TRUE)
    
    profilemodels <- SSgetoutput(dirvec = successful_run_dirs, getcovar = TRUE)
    profilesummary <- SSsummarize(profilemodels, verbose = FALSE)
    
    ## MANUALLY CREATE PLOTS AND CSVs ----
    message("Manually creating profile plots and CSV file with r4ss::profile format...")
    plotdir <- file.path(dir_prof, "plots")
    dir.create(plotdir, showWarnings = FALSE, recursive = TRUE)
    
    # --- Create Likelihoods.csv (to match r4ss::profile output) ---
    
    # 1. Get the transposed likelihoods table (your existing robust method)
    raw_likes <- profilesummary$likelihoods
    label_col_idx <- which(names(raw_likes) == "Label")
    like_labels <- raw_likes[[label_col_idx]]
    like_vals <- raw_likes[, -label_col_idx]
    like_vals[] <- lapply(like_vals, as.numeric)
    likelihoods_t <- as.data.frame(t(like_vals))
    names(likelihoods_t) <- like_labels
    
    # 2. Get the maximum gradient for each model run from the summary object
    max_grads <- profilesummary$maxgrad
    
    # 3. Determine convergence status based on the r4ss::profile default criterion
    conv_criteria <- 0.01
    converged_status <- max_grads <= conv_criteria
    

    
    if (is.list(successful_vec)) {
      # If it's a list, create a sequence from 1 to the number of elements in the list.
      # For your example list of 7 items, length(successful_vec) is 7, so this creates 1:7.
      profile_values_to_use <- 1:length(successful_vec)
    } else {
      # If it's not a list (i.e., it's a simple vector), use the vector itself.
      profile_values_to_use <- successful_vec
    }
    
    # 4. Assemble the final data frame in the correct order to mimic r4ss::profile
    profile_df_final <- cbind(
      # Value = vec,
      # Value = successful_vec,
      Value = profile_values_to_use,
      converged = converged_status,
      likelihoods_t,
      max_grad = max_grads
    )
    
    # 5. Write the new, comprehensive data frame to the CSV file
    write.csv(
      profile_df_final,
      file = file.path(plotdir, "Likelihoods.csv"),
      row.names = FALSE
    )
    
    
    # --- Dynamically set the plotting string based on the parameter ---
    # This ensures the correct parameter is used for the x-axis of the plot
    if (param == "SR_LN(R0)") {
      p.string <- "R0"
    } else if (param == "SR_BH_steep") {
      p.string <- "steep"
    } else if (param == "NatM_uniform_Fem_GP_1") {
      p.string <- "NatM_uniform_Fem_GP_1"
    } else {
      # Fallback for any other parameters
      p.string <- param
    }
    
    message(paste("Plotting profile for", param, "using string:", p.string))
    
    # --- Create ChangeInLikelihoods.csv and the main profile plot ---
    # best_model_idx <- which.min(profilesummary$likelihoods[1,])
    # Exclude the last column (Label) to ensure numeric calculation
    best_model_idx <- which.min(suppressWarnings(as.numeric(profilesummary$likelihoods[1, -ncol(profilesummary$likelihoods)])))
    
    # if(length(best_model_idx) > 0 && !is.na(best_model_idx)) {
    if(!is.null(best_model_idx) && length(best_model_idx) > 0 && !is.na(best_model_idx)) {

      
      if (param %in% c("Depl", "CurSB","NatM_vector")) {
        
        message("SSplotProfile doesnt work for Depl for CurSB so create plot manually")
        
        # Component likelihoods table 
        like.table<- profilesummary[["likelihoods"]]
        
        # Extract the last column to use as the new header
        Label <- like.table[, ncol(like.table)]
        
        # Transpose the dataframe without the last column
        like.table <- as.data.frame(t(like.table[, -ncol(like.table)]))
        
        # Assign the new headers to the transposed dataframe
        colnames(like.table) <- Label
        
        message("colnames(like.table):",colnames(like.table))
        
        # First, create a named vector to map the old column names to the new ones.
        # This makes the renaming process flexible and easy to read.
        name_mapping <- c(
          "TOTAL" = "Total",
          "Catch" = "Catch",
          "Equil_catch" = "Equilibrium catch",
          "Survey" = "Index data",
          "Discard" = "Discard",
          "Mean_body_wt" = "Mean body weight",
          "Length_comp" = "Length data",
          "Age_comp" = "Age data",
          "Size_at_age" = "Size-at-age data",
          "SizeFreq" = "Generalized size data",
          "Morphcomp" = "Morph composition data",
          "Tag_comp" = "Tag recapture distribution",
          "Tag_negbin" = "Tag recapture total",
          "Recruitment" = "Recruitment",
          "InitEQ_Regime" = "Initital equilibrium recruitment",
          "Forecast_Recruitment" = "Forecast recruitment",
          "Parm_priors" = "Priors",
          "Parm_softbounds" = "Soft bounds",
          "Parm_devs" = "Parameter deviations",
          "F_Ballpark" = "F Ballpark",
          "Crash_Pen" = "Crash penalty"
        )
        
        # Get the current column names of your table
        current_names <- colnames(like.table)
        
        # Find the names in your mapping that are also in your table's columns
        names_to_rename <- intersect(names(name_mapping), current_names)
        
        # Now, use the mapping to get the new names for the columns you want to change
        new_names <- name_mapping[names_to_rename]
        
        # Finally, rename the columns in your data table.
        # This uses the `match()` function to find the positions of the columns to be renamed
        # and then assigns the new names.
        colnames(like.table)[match(names_to_rename, current_names)] <- new_names
        
        
        message("colnames(like.table):",colnames(like.table))
        
        restab1<-like.table
        restab1$max_grad <- profilesummary$maxgrad
        restab1$param <- profile_values_to_use #vec
        names(restab1)[ncol(restab1)]<-param
        restab1$converged <- restab1$max_grad < 0.0001
        
        write.csv(restab1, file = paste0(plotdir,"/Likelihoods.csv"))
        
        
        ## Change in likelihoods table 
        prof.table <- like.table
        xlim <- range(vec)
        # subset <- vec >= xlim[1] & vec <= xlim[2]
        
        subset <- unlist(vec) >= xlim[1] & unlist(vec) <= xlim[2] # Handles both lists and vectors
        
        for (icol in 1:ncol(prof.table)) {
          # prof.table[, icol] <- prof.table[, icol] - min(prof.table[subset, icol])
          
          prof.table[, icol] <- prof.table[, icol] - min(prof.table[, icol])
        }
        write.csv(prof.table, file = paste0(plotdir,"/ChangeInLikelihoods.csv"))
        
        ymax <- 1.1 * max(prof.table[subset, ])
        ylim <- c(0, ymax)
        
        
        # remove columns that have change less than minfraction change relative to total
        # column.max <- apply(prof.table[subset, ], 2, max)
        # column.max <- apply(prof.table, 2, max)
        # change.fraction <- column.max / column.max[1]
        # minfraction <- 0.01
        # include <- change.fraction >= minfraction
        # 
        # # prof.table <- prof.table[order(vec), include]
        # prof.table <- prof.table[, include]
        # 
        # nlines <- sum(include)
        # legend <- names(prof.table)
        # remove columns that have change less than minfraction change relative to total
        column.max <- apply(prof.table, 2, max)
        
        # Check for zero total change to avoid error (using index 1 which is Total)
        total_change <- column.max[1] 
        if(total_change == 0) total_change <- 1e-10
        
        change.fraction <- column.max / total_change
        minfraction <- 0.01
        include <- change.fraction >= minfraction
        
        # Filter the table
        prof.table <- prof.table[, include]
        
        # --- SORTING UPDATE: Descending Magnitude, but "Total" First ---
        
        # 1. Calculate max change for the remaining filtered columns
        final_col_max <- apply(prof.table, 2, max)
        
        # 2. Get initial sort order (Largest change first)
        ord <- order(final_col_max, decreasing = TRUE)
        sorted_names <- names(prof.table)[ord]
        
        # 3. Force "Total" to the first position
        # (This keeps Total at the top, followed by the rest sorted by magnitude)
        if ("Total" %in% sorted_names) {
          final_names <- c("Total", setdiff(sorted_names, "Total"))
        } else {
          final_names <- sorted_names
        }
        
        # 4. Apply final order to table
        prof.table <- prof.table[, final_names]
        # ---------------------------------------------------------------
        
        nlines <- ncol(prof.table)
        legend <- names(prof.table)

        
        ymax <- 1.1 * max(prof.table[subset, ])
        ylim <- c(0, ymax)
        
        # Results tab
        # out <- data.frame(vec = vec, prof.table)
        out <- data.frame(vec = profile_values_to_use, prof.table)
        
        names(out)[1] <- profile.label
        out
        
        #Open the PNG device
        png(
          filename = paste0(plotdir, "/profile_plot_likelihood_orig_size",profile.label,".png"),
          width = 200,
          height = 200,
          units = "mm",
          res = 300
        )
        
        # make plot
        matplot(
          # x = vec,
          x = profile_values_to_use,
          y = prof.table,
          type = "o",
          pch = 1:nlines,
          col = rich.colors.short(nlines),
          cex = 1, 
          lty = 1, 
          lwd = 2,
          xlab = profile.label,
          ylab= "Change in-log-likelihood")
        abline(h = 1.96, lty = 2, col = "grey40")
        legend(x="topright",
               bty = "n", 
               legend = legend,
               lwd = 2, 
               pt.cex = 1, lty = 1, pch = 1:nlines, col = rich.colors.short(nlines))
        
        dev.off()
        
        # Same plot but matching size of r4ss likelihood profile plot
        png(filename = paste0(plotdir, "/profile_plot_likelihood_",profile.label,".png"),
            width = 165.1, height = 127, units = "mm", res = 300)
        
        # make plot
        matplot(
          # x = vec,
          x = profile_values_to_use,
          y = prof.table,
          type = "o",
          pch = 1:nlines,
          col = rich.colors.short(nlines),
          cex = 1, 
          lty = 1, 
          lwd = 2,
          xlab = profile.label,
          ylab= "Change in-log-likelihood")
        abline(h = 1.96, lty = 2, col = "grey40")
        legend(x="topright",
               bty = "n", 
               legend = legend,
               lwd = 2, 
               pt.cex = 1, lty = 1, pch = 1:nlines, col = rich.colors.short(nlines))
        
        

        dev.off()
        
        if (param == "NatM_vector") {
          
          # create a plot of the NatM vectors
          # --- 2. Prepare Data for Plotting ---
          
          # Combine the list of vectors into a single matrix.
          # Each vector from the list becomes a column in the matrix.
          # This is the format matplot() needs for the 'y' argument.
          y_data <- do.call(cbind, vec)
          
          # Create the x-axis values. You want "Age 0", "Age 1", etc.
          # R indexing starts at 1, so we create a sequence from 0 to (number of points - 1).
          x_ages <- 0:(nrow(y_data) - 1)
          
          # --- 3. Create the Plot ---
          
          # Define plot parameters
          nlines <- ncol(y_data) # The number of lines to plot (7 in this case)
          legend_text <- paste("Vector", 1:nlines) # Text for the legend
          
          # Use the built-in "Viridis" palette from hcl.colors()
          plot_colours <- hcl.colors(nlines, palette = "viridis")
          
          # Open the PNG device
          png(
            filename = paste0(plotdir, "/NatM_vectors.png"),
            width = 200,
            height = 200,
            units = "mm",
            res = 300
          )
          
          matplot(
            x = x_ages,
            y = y_data,
            type = "o", pch = 16,
            col = plot_colours,
            lty = 1, lwd = 2,
            xlab = "Age (years)",
            ylab = expression(italic(M)^-1)
          )
          
          legend(
            x = "topright",
            legend = legend_text,
            col = plot_colours,
            lty = 1, lwd = 2, pch = 16,
            bty = "n"
          )
          
          dev.off()
          
        }
        

        
        
      } else {
        
        # Check if likelihoods vary before plotting to prevent crashes
        # We check the Total Likelihood (first row) for variance
        total_likes <- as.numeric(profilesummary$likelihoods[1, -ncol(profilesummary$likelihoods)])
        
        if (max(total_likes) - min(total_likes) == 0) {
          message(paste("WARNING: Profile for", param, "is flat (likelihoods did not change). Skipping plot to prevent crash."))
          message("Check that 'init_values_src' is set to 0 in starter.ss so the model reads the modified control file.")
        } else {
          # Use the new dynamic p.string variable here
          tryCatch({
            results <- SSplotProfile(profilesummary,
                                     profile.string = p.string,
                                     profile.label = profile.label,
                                     print = TRUE,
                                     plot = FALSE, # stop the pdf
                                     plotdir = plotdir,
                                     png = TRUE,
                                     add_cutoff = TRUE
            )
            write.csv(results, file = file.path(plotdir, "ChangeInLikelihoods.csv"))
            
            # Rename the default file to include the parameter label
            old_name <- file.path(plotdir, "profile_plot_likelihood.png")
            new_name <- file.path(plotdir, paste0("profile_plot_likelihood - ", profile.label, ".png"))
            if(file.exists(old_name)) {
              file.rename(from = old_name, to = new_name)
            }
          }, error = function(e) {
            message(paste("Error in SSplotProfile for", param, ":", e$message))
          })
        }
        
        # results <- SSplotProfile(profilesummary,
        #                          profile.string = p.string,
        #                          profile.label = profile.label,
        #                          print = TRUE,
        #                          plot = FALSE, # stop the pdf
        #                          plotdir = plotdir,
        #                          png = TRUE,
        #                          add_cutoff = TRUE
        #                          
        #                          
        # )
        # 
        # 
        # write.csv(results, file = file.path(plotdir, "ChangeInLikelihoods.csv"))
        # 
        # # Define the old and new file paths
        # old_name <- file.path(plotdir, "profile_plot_likelihood.png")
        # new_name <- file.path(plotdir, paste0("profile_plot_likelihood - ",profile.label,".png"))
        # 
        # # Rename the file
        # file.rename(from = old_name, to = new_name)
        
        

# PINER plots -------------------------------------------------------------

        
        
        if (param == "SR_LN(R0)") {
          
          message("Trying out a Piner plot for SR_LN(R0).")
          
          # Piner.component <- "Age_like" #"Length_like"
          # Piner.component <- "Length_like"
          # 
          # "Age_comp" %in% profilesummary$likelihoods$Label
          # "Age_comp" %in% profilesummary$likelihoods$Label
          
          # profilesummary$likelihoods$Label
          
          if ("Age_comp" %in% profilesummary$likelihoods$Label) {
            
            Piner.component <- "Age_like" #"Length_like"
            
            PinerPlot(
              profilesummary,
              plot = FALSE,
              print = TRUE,
              component = Piner.component,
              main = "Changes in length-composition likelihoods by fleet",
              models = "all",
              fleets = "all",
              fleetnames = "default",
              profile.string = p.string,
              # profile.label = expression(log(italic(R)[0])),
              profile.label = profile.label,
              plotdir = plotdir,
              verbose = FALSE
            )
            
            # Define the old and new file paths
            old_name <- file.path(plotdir, "profile_plot_likelihood.png")
            new_name <- file.path(plotdir, paste0("profile_plot_likelihood PINER - ",Piner.component,".png"))
            file.rename(from = old_name, to = new_name) # Rename the file
            
          }
          
          if ("Length_comp" %in% profilesummary$likelihoods$Label) {
            
            Piner.component <- "Length_like"
            
            PinerPlot(
              profilesummary,
              plot = FALSE,
              print = TRUE,
              component = Piner.component,
              main = "Changes in length-composition likelihoods by fleet",
              models = "all",
              fleets = "all",
              fleetnames = "default",
              profile.string = p.string,
              # profile.label = expression(log(italic(R)[0])),
              profile.label = profile.label,
              plotdir = plotdir,
              verbose = FALSE
            )
            
            # Define the old and new file paths
            old_name <- file.path(plotdir, "profile_plot_likelihood.png")
            new_name <- file.path(plotdir, paste0("profile_plot_likelihood PINER - ",Piner.component,".png"))
            file.rename(from = old_name, to = new_name) # Rename the file
            
          }
          
        }
        
      }
      
      
    } else {
      message("Could not find a best model to create profile plot or change-in-likelihoods CSV.")
    }
    
    
    # Create comparison plots across the profiled values
    SSplotComparisons(profilesummary,
                      # legendlabels = paste(profile.label," = ", vec),
                      # legendlabels = paste(profile.label," = ", round(successful_vec, 4)),
                      legendlabels = paste(profile.label," = ", round(profile_values_to_use, 4)),
                      print = TRUE,
                      plotdir = plotdir,
                      plot = FALSE,
                      verbose = FALSE,
                      png = TRUE
    )
    

    
    message(paste0("### Finished profile for: ", param, " ###"))
  }
  
  message("\nAll likelihood profiles are complete.\n")
  
} else {
  message("No parameters were selected. Skipping profiling run.")
}



# --- Fixed Parameter Scenarios (M, h, SigmaR, and Data Weighting) --------------------------------
if(any(sensitivity_options$fixed_param_scenarios,
       sensitivity_options$comp_weight_scenario,
       sensitivity_options$index_weight_scenario,
       na.rm = TRUE)) {
  
  message("\n--- Starting Fixed Parameter & Data Weighting Scenarios ---\n")
  
  # Base model directory
  model_name <- sensitivity_options$model_folder
  base_model_dir <- model_name
  
  # Define the main directory for these scenarios
  scenarios_main_dir <- file.path(base_model_dir, "fixed_parameter_scenarios")
  unlink(scenarios_main_dir, recursive = TRUE, force = TRUE) # Clean up old runs
  dir.create(scenarios_main_dir, showWarnings = FALSE, recursive = TRUE)
  plotdir <- file.path(scenarios_main_dir, "plots")
  dir.create(plotdir, showWarnings = FALSE, recursive = TRUE)
  
  # Load the base model output to get initial parameter values
  replist_base <- SS_output(dir = base_model_dir,
                            verbose = FALSE,
                            printstats = FALSE,
                            covar = TRUE)
  
  # Initialize an empty list for scenarios
  scenarios <- list()
  
  # --- Define Scenarios for M, h, SigmaR if selected ---
  if(isTRUE(sensitivity_options$fixed_param_scenarios)) {
    
    # Helper function to safely retrieve parameter value
    get_param_from_replist_parameters_df <- function(replist_obj, param_label) {
      value <- replist_obj$parameters[replist_obj$parameters$Label == param_label, "Value"]
      if (length(value) == 1 && !is.na(value) && is.numeric(value)) {
        return(value)
      } else {
        return(NULL)
      }
    }
    
    # Steepness (h)
    h_base_val <- replist_base$estimated_non_dev_parameters["SR_BH_steep", "Value"]
    if (length(h_base_val) != 1 || is.na(h_base_val) || !is.numeric(h_base_val)) {
      h_base_val <- get_param_from_replist_parameters_df(replist_base, "SR_BH_steep")
    }
    if (is.null(h_base_val)) { 
      warning("Could not retrieve a valid base value for Steepness (SR_BH_steep).") 
    } else {
      message(paste0("Retrieved base value for Steepness (h): ", round(h_base_val, 5)))
    }
    
    # --- Natural Mortality (M) Logic ---
    use_M_vector_strategy <- FALSE
    M_fem_name_ctl <- NA
    M_mal_name_ctl <- NA
    is_one_sex_model <- FALSE
    
    # Try to find SINGLE parameters first
    ctl_params <- r4ss::SS_parlines(ctlfile = file.path(base_model_dir, "controlfile.ctl"), verbose = FALSE)
    M_fem_name_ctl <- grep("NatM.*Fem.*GP_1", ctl_params$Label, value = TRUE)[1]
    
    if (is.na(M_fem_name_ctl)) {
      message("Single NatM parameter not found in Control File. Assuming NatM Vector configuration.")
      use_M_vector_strategy <- TRUE
    } else {
      message(paste0("Identified Control File label for Female M: '", M_fem_name_ctl, "'"))
      
      # Retrieve base value for reporting
      M_fem_base_val <- get_param_from_replist_parameters_df(replist_base, M_fem_name_ctl)
      
      # Fallback for Female M
      if(is.null(M_fem_base_val)) {
        mg_param_names <- names(replist_base$MGparmAdj)
        M_fem_name_report <- grep("NatM.*Fem.*GP_1", mg_param_names, value = TRUE)[1]
        if(!is.na(M_fem_name_report)) M_fem_base_val <- replist_base$MGparmAdj[[M_fem_name_report]][1]
      }
      
      # Check for Male M (if two-sex)
      M_mal_name_ctl <- grep("NatM.*Mal.*GP_1", ctl_params$Label, value = TRUE)[1]
      
      if (is.na(M_mal_name_ctl)) {
        is_one_sex_model <- TRUE
      } else {
        message(paste0("Identified Control File label for Male M: '", M_mal_name_ctl, "'"))
        
        # Retrieve base value for Male M
        M_mal_base_val <- get_param_from_replist_parameters_df(replist_base, M_mal_name_ctl)
        
        # Fallback for Male M if direct lookup fails
        if(is.null(M_mal_base_val)) {
          mg_param_names <- names(replist_base$MGparmAdj)
          M_mal_name_report <- grep("NatM.*Mal.*GP_1", mg_param_names, value = TRUE)[1]
          if(!is.na(M_mal_name_report)) {
            M_mal_base_val <- replist_base$MGparmAdj[[M_mal_name_report]][1]
            message(paste0("Retrieved base value for Male M from MGparmAdj: ", round(M_mal_base_val, 5)))
          }
        }
        
        # Final Safety Check for Male M
        if(is.null(M_mal_base_val)) {
          message("Warning: Could not retrieve Male M value from output. Using Female M value as proxy for offset calculation.")
          M_mal_base_val <- M_fem_base_val
        }
      }
    }
    
    # --- SigmaR ---
    SigmaR_base_val <- get_param_from_replist_parameters_df(replist_base, "SR_sigmaR")
    if (is.null(SigmaR_base_val)) { message("Warning: Could not retrieve a valid base value for SigmaR.") }
    else { message(paste0("Retrieved base value for SigmaR: ", round(SigmaR_base_val, 5))) }
    
    
    # --- Define Scenarios ---
    
    # 1. Steepness
    if (!is.null(h_base_val)) {
      param_scenarios <- list(
        "h_plus_0.1" = list(type = "param_change", param = "SR_BH_steep", value = h_base_val + 0.1, label = "h + 0.1"),
        "h_minus_0.1" = list(type = "param_change", param = "SR_BH_steep", value = h_base_val - 0.1, label = "h - 0.1")
      )
    } else {
      param_scenarios <- list()
    }
    
    # 2. Natural Mortality
    if (use_M_vector_strategy) {
      # VECTOR M STRATEGY
      message("Adding NatM Vector scenarios (M vector +/- 0.02).")
      param_scenarios[["M_plus_0.02"]] <- list(type = "natm_vector_offset", value = 0.02, label = "M vector + 0.02")
      param_scenarios[["M_minus_0.02"]] <- list(type = "natm_vector_offset", value = -0.02, label = "M vector - 0.02")
      
    } else {
      # SINGLE PARAMETER STRATEGY
      if (is_one_sex_model) {
        param_scenarios[["M_plus_0.02"]] <- list(type = "param_change", param = M_fem_name_ctl, value = M_fem_base_val + 0.02, label = "M + 0.02")
        param_scenarios[["M_minus_0.02"]] <- list(type = "param_change", param = M_fem_name_ctl, value = M_fem_base_val - 0.02, label = "M - 0.02")
      } else {
        # Two-sex: Modify both
        param_scenarios[["M_plus_0.02"]] <- list(type = "param_change", param = c(M_fem_name_ctl, M_mal_name_ctl), value = c(M_fem_base_val + 0.02, M_mal_base_val + 0.02), label = "M + 0.02")
        param_scenarios[["M_minus_0.02"]] <- list(type = "param_change", param = c(M_fem_name_ctl, M_mal_name_ctl), value = c(M_fem_base_val - 0.02, M_mal_base_val - 0.02), label = "M - 0.02")
      }
    }
    
    # 3. SigmaR
    if(!is.null(SigmaR_base_val)){
      param_scenarios[["SigmaR_plus_0.1"]] <- list(type = "param_change", param = "SR_sigmaR", value = SigmaR_base_val + 0.1, label = "SigmaR + 0.1")
      param_scenarios[["SigmaR_minus_0.1"]] <- list(type = "param_change", param = "SR_sigmaR", value = SigmaR_base_val - 0.1, label = "SigmaR - 0.1")
    }
    
    scenarios <- c(scenarios, param_scenarios)
  }
  
  # --- Define Scenarios for Composition Data Weighting if selected ---
  if(isTRUE(sensitivity_options$comp_weight_scenario)) {
    comp_scenarios <- list(
      "comp_x2" = list(type = "lambda_change", component = c(4, 5), factor = 2, label = "Comp Weight x2"),
      "comp_x0.5" = list(type = "lambda_change", component = c(4, 5), factor = 0.5, label = "Comp Weight x0.5")
    )
    scenarios <- c(scenarios, comp_scenarios)
  }
  
  # --- Define Scenarios for Index Data Weighting if selected ---
  if(isTRUE(sensitivity_options$index_weight_scenario)) {
    index_scenarios <- list(
      "index_x2" = list(type = "lambda_change", component = 1, factor = 2, label = "Index Weight x2"),
      "index_x0.5" = list(type = "lambda_change", component = 1, factor = 0.5, label = "Index Weight x0.5")
    )
    scenarios <- c(scenarios, index_scenarios)
  }
  
  # --- Proceed only if there are scenarios to run ---
  if (length(scenarios) > 0) {
    scenario_dirs <- character(length(scenarios))
    scenario_labels <- character(length(scenarios))
    names(scenario_dirs) <- names(scenarios)
    names(scenario_labels) <- names(scenarios)
    
    message("Setting up scenario directories...")
    for (i in seq_along(scenarios)) {
      scenario_name <- names(scenarios)[i]
      current_scenario <- scenarios[[i]]
      dir_path <- file.path(scenarios_main_dir, scenario_name)
      dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
      
      # Copy base model files
      r4ss::copy_SS_inputs(
        dir.old = base_model_dir,
        dir.new = dir_path,
        overwrite = TRUE,
        copy_exe = TRUE,
        verbose = FALSE
      )
      
      # Define the new control file name
      new_ctl_file <- "control_scenario.ss"
      
      # --- HANDLE DIFFERENT SCENARIO TYPES ---
      
      if (current_scenario$type == "param_change") {
        
        # Standard Single Parameter Change (M, h, SigmaR)
        SS_changepars(
          dir = dir_path,
          ctlfile = "controlfile.ctl",
          newctlfile = new_ctl_file,
          strings = current_scenario$param,
          newvals = current_scenario$value,
          verbose = FALSE,
          estimate = rep(FALSE, length(current_scenario$param))
        )
        
      } else if (current_scenario$type == "natm_vector_offset") {
        
        # Vector NatM Change
        message(paste0("...Scenario '", scenario_name, "': Applying offset of ", current_scenario$value, " to NatM vector."))
        
        dat_file_path <- file.path(dir_path, "datafile.dat")
        ctl_file_path <- file.path(dir_path, "controlfile.ctl")
        dat <- r4ss::SS_readdat(dat_file_path, verbose = FALSE)
        ctl <- r4ss::SS_readctl(file = ctl_file_path, datlist = dat, verbose = FALSE)
        
        # Modify the NatM vector
        ctl$natM <- ctl$natM + current_scenario$value
        
        r4ss::SS_writectl(ctl, outfile = file.path(dir_path, new_ctl_file), overwrite = TRUE, verbose = FALSE)
        
      } else if (current_scenario$type == "lambda_change") {
        
        # Data Weighting Change
        message(paste0("...Scenario '", scenario_name, "': Changing data weighting."))
        dat_file_path <- file.path(dir_path, "datafile.dat")
        ctl_file_path <- file.path(dir_path, "controlfile.ctl")
        dat <- r4ss::SS_readdat(dat_file_path, verbose = FALSE)
        ctl <- r4ss::SS_readctl(file = ctl_file_path, datlist = dat, verbose = FALSE)
        
        # Populate lambdas if missing
        if (is.null(ctl$lambdas) || nrow(ctl$lambdas) == 0) {
          message("...No lambdas found in control file. Populating defaults.")
          new_lambdas <- data.frame(like_comp = integer(), fleet = integer(), phase = integer(), value = numeric(), sizefreq_method = numeric(), stringsAsFactors = FALSE)
          
          # Survey
          survey_fleets <- which(dat$fleetinfo$type == 3)
          if (length(survey_fleets) > 0) {
            for (f in survey_fleets) new_lambdas <- rbind(new_lambdas, data.frame(like_comp = 1, fleet = f, phase = 1, value = 1, sizefreq_method = 0))
          }
          # Length
          if (dat$N_lbins > 0) {
            len_fleets <- unique(dat$lencomp$fleet)
            for (f in len_fleets) new_lambdas <- rbind(new_lambdas, data.frame(like_comp = 4, fleet = f, phase = 1, value = 1, sizefreq_method = 0))
          }
          # Age
          if (dat$N_agebins > 0) {
            age_fleets <- unique(dat$agecomp$fleet)
            age_fleets <- age_fleets[age_fleets > 0]
            for (f in age_fleets) new_lambdas <- rbind(new_lambdas, data.frame(like_comp = 5, fleet = f, phase = 1, value = 1, sizefreq_method = 0))
          }
          ctl$lambdas <- new_lambdas
          ctl$N_lambdas <- nrow(new_lambdas)
        }
        
        rows_to_modify <- ctl$lambdas$like_comp %in% current_scenario$component & ctl$lambdas$phase > 0
        if (any(rows_to_modify)) {
          ctl$lambdas$value[rows_to_modify] <- ctl$lambdas$value[rows_to_modify] * current_scenario$factor
          message(paste0("...Scenario '", scenario_name, "': Modified lambdas by factor ", current_scenario$factor))
        } else {
          message(paste0("...Warning: No active lambda components found for ", scenario_name))
        }
        
        r4ss::SS_writectl(ctl, outfile = file.path(dir_path, new_ctl_file), overwrite = TRUE, verbose = FALSE)
      }
      
      # Modify the starter file to point to the new control file
      starter_path <- file.path(dir_path, "starter.ss")
      starter <- SS_readstarter(starter_path, verbose = FALSE)
      starter[["ctlfile"]] <- new_ctl_file
      SS_writestarter(starter, dir = dir_path, overwrite = TRUE, verbose = FALSE)
      
      scenario_dirs[i] <- dir_path
      scenario_labels[i] <- current_scenario$label
    }
    
    message("Scenario directories prepared.")
    
    # Run models in parallel
    n_cores <- parallel::detectCores() - 1
    if (n_cores < 1) n_cores <- 1
    
    message(paste("Running", length(scenarios), "fixed parameter scenarios on", n_cores, "cores."))
    my_cluster <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(my_cluster)
    
    start_time <- Sys.time()
    message(paste("Parallel execution started:", start_time))
    
    foreach(
      dir = scenario_dirs,
      .packages = 'r4ss'
    ) %dopar% {
      r4ss::run(dir = dir, exe = "ss")
    }
    
    parallel::stopCluster(my_cluster)
    message("Finished running all fixed parameter scenario models.")
    
    # Gather results and plot comparisons
    message("Gathering results for comparison...")
    
    all_dirs_to_summarize <- c(base_model_dir, scenario_dirs)
    all_labels <- c("Base Model", scenario_labels)
    
    all_models_output <- SSgetoutput(
      dirvec = all_dirs_to_summarize,
      getcovar = FALSE
    )
    
    all_summary <- SSsummarize(all_models_output)
    message("All model outputs summarised.")
    
    message("Generating comparison plots...")
    SSplotComparisons(
      summaryoutput = all_summary,
      legendlabels = all_labels,
      plotdir = plotdir,
      plot = FALSE,
      print = TRUE,
      png = TRUE,
      btarg = 0.4,
      minbthresh = 0.2,
      verbose = FALSE
    )
    
    message("Fixed Parameter Scenarios analysis complete. Plots saved to ", plotdir)
    
    # --- NEW: Generate Sorted Summary CSV Table ---
    message("Generating sorted summary CSV table...")
    
    # Define desired order
    desired_order <- c(
      "Base_Model",
      "M_minus_0.02",
      "M_plus_0.02",
      "h_minus_0.1",
      "h_plus_0.1",
      "SigmaR_minus_0.1",
      "SigmaR_plus_0.1",
      "index_x0.5",
      "index_x2",
      "comp_x0.5",
      "comp_x2"
    )
    
    summary_results <- data.frame(
      Model = character(),
      SSB_Virgin = numeric(),
      SSB_Final = numeric(),
      Bratio_Final = numeric(),
      stringsAsFactors = FALSE
    )
    
    for(curr_dir in all_dirs_to_summarize) {
      if(curr_dir == base_model_dir) {
        m_name <- "Base_Model"
      } else {
        m_name <- basename(curr_dir)
      }
      
      rep_file <- file.path(curr_dir, "Report.sso")
      if(file.exists(rep_file)) {
        tryCatch({
          # Load model output (fast)
          replist_quick <- r4ss::SS_output(dir = curr_dir, verbose = FALSE, printstats = FALSE, covar = FALSE)
          endyr <- replist_quick$endyr
          dq <- replist_quick$derived_quants
          
          # Extract Values
          ssb_v_row <- dq[rownames(dq) %in% c("SSB_Virgin", "SSB_Unfished"), ]
          val_v <- if(nrow(ssb_v_row)>0) ssb_v_row$Value[1] else NA
          
          ssb_f_row <- dq[rownames(dq) == paste0("SSB_", endyr), ]
          val_f <- if(nrow(ssb_f_row)>0) ssb_f_row$Value[1] else NA
          
          br_f_row <- dq[rownames(dq) == paste0("Bratio_", endyr), ]
          val_br <- if(nrow(br_f_row)>0) br_f_row$Value[1] else NA
          
          summary_results <- rbind(summary_results, data.frame(
            Model = m_name,
            SSB_Virgin = val_v,
            SSB_Final = val_f,
            Bratio_Final = val_br,
            stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          message(paste("Error extracting from", m_name))
        })
      }
    }
    
    # Sort Logic
    existing_models <- summary_results$Model
    others <- setdiff(existing_models, desired_order)
    full_sort <- c(intersect(desired_order, existing_models), sort(others))
    
    summary_results$Model <- factor(summary_results$Model, levels = full_sort)
    summary_results <- summary_results[order(summary_results$Model), ]
    
    # Format and Save
    summary_results$SSB_Virgin <- round(summary_results$SSB_Virgin, 2)
    summary_results$SSB_Final <- round(summary_results$SSB_Final, 2)
    summary_results$Bratio_Final <- round(summary_results$Bratio_Final, 3)
    
    out_csv_path <- file.path(scenarios_main_dir, "Scenario_Summary_Table.csv")
    write.csv(summary_results, out_csv_path, row.names = FALSE)
    message("Scenario Summary Table saved to: ", out_csv_path)
    
  } else {
    message("No fixed parameter or data weighting scenarios were defined to run.")
  }
} else {
  message("Fixed parameter scenarios and data weighting are not enabled. Skipping this section.")
}

# --- DIAGNOSTICS: CONSOLIDATE PLOTS ---
message("\n--- Consolidating Key Diagnostic Plots ---")

# 1. Create the diagnostics folder structure
diagnostics_dir <- file.path(sensitivity_options$model_folder, "diagnostics")
diag_retro_dir <- file.path(diagnostics_dir, "retro")
diag_fixed_dir <- file.path(diagnostics_dir, "fixed_parameter_scenarios")

# Create main and sub-directories
for (d in c(diagnostics_dir, diag_retro_dir, diag_fixed_dir)) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE)
  }
}
message(paste("Created diagnostics directory structure in:", diagnostics_dir))

# ---------------------------------------------------------
# GROUP 1: FILES FOR MAIN DIAGNOSTICS FOLDER
# (Jitter, Profiles, CSV Table)
# ---------------------------------------------------------
files_to_root <- c(
  # Jitter
  file.path("jitter_parallel", "plots", "Jitter_Comparison_Plot.png"),
  
  # Scenario Table
  file.path("fixed_parameter_scenarios", "Scenario_Summary_Table.csv"),
  
  # Profiles - Depletion & Biomass
  file.path("profile", "Depl", "plots", "profile_plot_likelihood_Current depletion.png"),
  file.path("profile", "CurSB", "plots", "profile_plot_likelihood_Current spawning biomass.png"),
  
  # Profiles - Standard Parameters
  file.path("profile", "SR_BH_steep", "plots", "profile_plot_likelihood - Steepness (h).png"),
  file.path("profile", "SR_LN(R0)", "plots", "profile_plot_likelihood - SR_LN(R0).png"),
  file.path("profile", "NatM_uniform_Fem_GP_1", "plots", "profile_plot_likelihood - Natural mortality (M).png"),
  
  # Profiles - Growth Parameters (The 4 New Ones)
  file.path("profile", "L_at_Amax_Fem_GP_1", "plots", "profile_plot_likelihood - L_at_Amax_Fem_GP_1.png"),
  file.path("profile", "L_at_Amax_Mal_GP_1", "plots", "profile_plot_likelihood - L_at_Amax_Mal_GP_1.png"),
  file.path("profile", "VonBert_K_Fem_GP_1", "plots", "profile_plot_likelihood - VonBert_K_Fem_GP_1.png"),
  file.path("profile", "VonBert_K_Mal_GP_1", "plots", "profile_plot_likelihood - VonBert_K_Mal_GP_1.png"),
  
  # Fallback for NatM vector if used
  file.path("profile", "NatM_vector", "plots", "profile_plot_likelihood - Natural mortality (M) vector.png")
)

# ---------------------------------------------------------
# GROUP 2: FILES FOR RETRO SUBFOLDER
# ---------------------------------------------------------
files_to_retro <- c(
  file.path("retro", "plots", "compare4_Bratio_uncertainty.png"),
  file.path("retro", "plots", "MohnsRho.csv") # Added Mohn's rho CSV if available
)

# ---------------------------------------------------------
# GROUP 3: FILES FOR FIXED PARAM SCENARIOS SUBFOLDER
# ---------------------------------------------------------
files_to_fixed <- c(
  file.path("fixed_parameter_scenarios", "plots", "compare7_Fvalue.png"),
  file.path("fixed_parameter_scenarios", "plots", "compare3_Bratio.png"),
  file.path("fixed_parameter_scenarios", "plots", "compare11_recdevs.png")
)

# ---------------------------------------------------------
# HELPER FUNCTION TO COPY FILES
# ---------------------------------------------------------
copy_files_safe <- function(file_list, dest_dir) {
  count <- 0
  for (rel_path in file_list) {
    full_source_path <- file.path(sensitivity_options$model_folder, rel_path)
    if (file.exists(full_source_path)) {
      file.copy(from = full_source_path, to = dest_dir, overwrite = TRUE)
      count <- count + 1
    }
  }
  return(count)
}

# Execute Copies
n_root  <- copy_files_safe(files_to_root, diagnostics_dir)
n_retro <- copy_files_safe(files_to_retro, diag_retro_dir)
n_fixed <- copy_files_safe(files_to_fixed, diag_fixed_dir)

message(paste0("Diagnostics Consolidaton Complete:",
               "\n  - Copied ", n_root, " files to root diagnostics folder.",
               "\n  - Copied ", n_retro, " files to 'retro' subfolder.",
               "\n  - Copied ", n_fixed, " files to 'fixed_parameter_scenarios' subfolder."))

# --- CLEANUP SECTION ---
if (isTRUE(sensitivity_options$cleanup_files)) {
  message("\n--- Cleaning up intermediate model run folders and files ---")
  
  # 1. Jitter runs & loose files
  jitter_dir <- file.path(sensitivity_options$model_folder, "jitter_parallel")
  if (dir.exists(jitter_dir)) {
    # Remove the jitter_1, jitter_2, etc. subfolders
    jitter_runs <- list.dirs(jitter_dir, recursive = FALSE)
    jitter_runs <- jitter_runs[grepl("jitter_[0-9]+", basename(jitter_runs))]
    unlink(jitter_runs, recursive = TRUE)
    
    # Remove all loose files in the root of jitter_parallel (keeps the 'plots' directory safe)
    jitter_loose_files <- list.files(jitter_dir, full.names = TRUE, recursive = FALSE)
    jitter_loose_files <- jitter_loose_files[!file.info(jitter_loose_files)$isdir]
    unlink(jitter_loose_files)
  }
  
  # 2. Retro runs
  retro_dir <- file.path(sensitivity_options$model_folder, "retro")
  if (dir.exists(retro_dir)) {
    retro_runs <- list.dirs(retro_dir, recursive = FALSE)
    retro_runs <- retro_runs[grepl("retro-?[0-9]+", basename(retro_runs))]
    unlink(retro_runs, recursive = TRUE)
  }
  
  # 3. Profile runs
  profile_dir <- file.path(sensitivity_options$model_folder, "profile")
  if (dir.exists(profile_dir)) {
    prof_params <- list.dirs(profile_dir, recursive = FALSE)
    for (pd in prof_params) {
      run_dirs <- list.dirs(pd, recursive = FALSE)
      run_dirs <- run_dirs[grepl("run_[0-9]+", basename(run_dirs))]
      unlink(run_dirs, recursive = TRUE)
    }
  }
  
  # 4. Fixed parameter scenario runs
  fixed_dir <- file.path(sensitivity_options$model_folder, "fixed_parameter_scenarios")
  if (dir.exists(fixed_dir)) {
    fixed_runs <- list.dirs(fixed_dir, recursive = FALSE)
    # Safely keep the 'plots' folder, delete the scenario run folders
    fixed_runs <- fixed_runs[!grepl("^plots$", basename(fixed_runs))]
    unlink(fixed_runs, recursive = TRUE)
  }
  
  message("Cleanup complete. Heavy model output files and loose logs removed.")
}
# --- END CLEANUP SECTION ---

# --- GLOBAL ERROR HANDLER END ---
}, error = function(e) {
  # This block runs if ANYTHING in the script crashes
  message("\nCRITICAL ERROR IN SS_SENSITIVITIES.R:")
  message(conditionMessage(e))
  message("\nTraceback:")
  # Attempt to print traceback if available
  try(print(sys.calls())) 
}, finally = {
  message("Sensitivity script execution finished (or terminated).")
})

# # --- DIAGNOSTICS: CONSOLIDATE PLOTS ---
# message("\n--- Consolidating Key Diagnostic Plots ---")
# 
# # 1. Create the diagnostics folder
# diagnostics_dir <- file.path(sensitivity_options$model_folder, "diagnostics")
# if (!dir.exists(diagnostics_dir)) {
#   dir.create(diagnostics_dir, recursive = TRUE)
#   message(paste("Created diagnostics directory:", diagnostics_dir))
# }
# 
# # 2. Define the list of files to look for (Relative paths)
# #    Note: We check multiple potential paths for NatM just in case
# plots_to_copy <- c(
#   # Jitter
#   file.path("jitter_parallel", "plots", "Jitter_Comparison_Plot.png"),
#   
#   # Retrospective
#   file.path("retro", "plots", "compare4_Bratio_uncertainty.png"),
#   
#   # Fixed Parameter Scenarios
#   file.path("fixed_parameter_scenarios", "plots", "compare7_Fvalue.png"),
#   file.path("fixed_parameter_scenarios", "plots", "compare3_Bratio.png"),
#   file.path("fixed_parameter_scenarios", "plots", "compare11_recdevs.png"),
#   
#   # Profiles - Depletion & Biomass (Underscore separator in filename)
#   file.path("profile", "Depl", "plots", "profile_plot_likelihood_Current depletion.png"),
#   file.path("profile", "CurSB", "plots", "profile_plot_likelihood_Current spawning biomass.png"),
#   
#   # Profiles - Parameters (Dash separator in filename)
#   file.path("profile", "SR_BH_steep", "plots", "profile_plot_likelihood - Steepness (h).png"),
#   file.path("profile", "SR_LN(R0)", "plots", "profile_plot_likelihood - SR_LN(R0).png"),
#   file.path("profile", "NatM_uniform_Fem_GP_1", "plots", "profile_plot_likelihood - Natural mortality (M).png"),
#   
#   # Fallback for NatM vector if used
#   file.path("profile", "NatM_vector", "plots", "profile_plot_likelihood - Natural mortality (M) vector.png")
# )
# 
# # 3. Copy the files
# count_copied <- 0
# for (rel_path in plots_to_copy) {
#   full_source_path <- file.path(sensitivity_options$model_folder, rel_path)
#   
#   if (file.exists(full_source_path)) {
#     success <- file.copy(from = full_source_path, to = diagnostics_dir, overwrite = TRUE)
#     if (success) {
#       count_copied <- count_copied + 1
#     }
#   }
# }
# 
# message(paste("Successfully copied", count_copied, "plots to the 'diagnostics' folder."))
# 
# # --- GLOBAL ERROR HANDLER END ---
# }, error = function(e) {
#   # This block runs if ANYTHING in the script crashes
#   message("\nCRITICAL ERROR IN SS_SENSITIVITIES.R:")
#   message(conditionMessage(e))
#   message("\nTraceback:")
#   # Attempt to print traceback if available
#   try(print(sys.calls())) 
# }, finally = {
#   message("Sensitivity script execution finished (or terminated).")
# })