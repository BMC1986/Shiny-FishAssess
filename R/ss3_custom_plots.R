generate_DPIRD_plots <- function(replist, output_dir) {
  
  dpird_dir <- file.path(output_dir, "DPIRD_plots")
  if (!dir.exists(dpird_dir)) dir.create(dpird_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!"package:ggplot2" %in% search()) suppressPackageStartupMessages(library(ggplot2))
  
  end_year <- replist$endyr
  
  target_theme <- theme_bw(base_family = "Arial") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(face = "bold", size = 18),
      axis.text = element_text(size = 16, colour = "black"),
      legend.title = element_text(face = "bold", size = 18),
      legend.text = element_text(size = 16),
      axis.ticks.length = unit(-0.2, "cm"),
      axis.text.x = element_text(margin = margin(t = 10)),
      axis.text.y = element_text(margin = margin(r = 10), hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
      panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
  
  dq <- replist$derived_quants
  bratio_rows <- grep("^Bratio_\\d+$", rownames(dq))
  
  if (length(bratio_rows) > 0) {
    df <- dq[bratio_rows, ]
    df$Label <- rownames(df)
    df$Year <- as.numeric(sub("Bratio_", "", df$Label))
    df <- df[order(df$Year), ]
    
    df$lo_95 <- pmax(0, df$Value - 1.96 * df$StdDev)
    df$hi_95 <- df$Value + 1.96 * df$StdDev
    z_60 <- qnorm(0.8) 
    df$lo_60 <- pmax(0, df$Value - z_60 * df$StdDev)
    df$hi_60 <- df$Value + z_60 * df$StdDev
    
    df_hist <- df[df$Year <= end_year, ]
    df_fore <- df[df$Year >= end_year, ]
    
    p_dep <- ggplot(data = df, aes(x = Year, y = Value)) +
      geom_hline(yintercept = 0.4, linetype = "dashed", colour = "darkgreen", linewidth = 0.7) + 
      annotate("text", x = min(df$Year), y = 0.4, label = "Target (0.4)", hjust = 0, vjust = -0.8, colour = "darkgreen", size = 6, fontface = "bold") +
      geom_hline(yintercept = 0.3, linetype = "dashed", colour = "orange", linewidth = 0.7) + 
      annotate("text", x = min(df$Year), y = 0.3, label = "Threshold (0.3)", hjust = 0, vjust = -0.8, colour = "orange", size = 6, fontface = "bold") +
      geom_hline(yintercept = 0.2, linetype = "dashed", colour = "red", linewidth = 0.7) + 
      annotate("text", x = min(df$Year), y = 0.2, label = "Limit (0.2)", hjust = 0, vjust = -0.8, colour = "red", size = 6, fontface = "bold") +
      
      geom_ribbon(data = df_hist, aes(ymin = lo_95, ymax = hi_95), fill = "grey80", alpha = 0.6) +
      geom_ribbon(data = df_hist, aes(ymin = lo_60, ymax = hi_60), fill = "grey60", alpha = 0.6) +
      
      geom_ribbon(data = df_fore, aes(ymin = lo_95, ymax = hi_95), fill = "grey95", alpha = 0.6) +
      geom_ribbon(data = df_fore, aes(ymin = lo_60, ymax = hi_60), fill = "grey85", alpha = 0.6) +
      
      geom_line(data = df_hist, linewidth = 1.2, colour = "black", linetype = "solid") +
      geom_point(data = df_fore, colour = "black", size = 2) +
      
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA), sec.axis = dup_axis(labels = NULL, name = NULL)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(labels = NULL, name = NULL)) +
      labs(y = "Fraction of unfished spawning biomass", x = "Year") +
      target_theme
    
    ggsave(filename = file.path(dpird_dir, "Depletion_DPIRD_with_intervals.png"), plot = p_dep, width = 9, height = 6, dpi = 300)
  } else {
    warning("DPIRD Plot: No Bratio_YEAR derived quantities found.")
  }
  
  f_rows <- grep("^F_\\d+$", rownames(dq))
  
  if (length(f_rows) > 0) {
    df_f <- dq[f_rows, ]
    df_f$Label <- rownames(df_f)
    df_f$Year <- as.numeric(sub("F_", "", df_f$Label))
    df_f <- df_f[order(df_f$Year), ]
    
    df_f$lo_95 <- pmax(0, df_f$Value - 1.96 * df_f$StdDev)
    df_f$hi_95 <- df_f$Value + 1.96 * df_f$StdDev
    z_60 <- qnorm(0.8) 
    df_f$lo_60 <- pmax(0, df_f$Value - z_60 * df_f$StdDev)
    df_f$hi_60 <- df_f$Value + z_60 * df_f$StdDev
    
    df_f_hist <- df_f[df_f$Year <= end_year, ]
    df_f_fore <- df_f[df_f$Year >= end_year, ]
    
    NatM <- NA
    m_match <- grep("NatM", replist$parameters$Label)
    if (length(m_match) > 0) {
      NatM <- replist$parameters$Value[m_match[1]]
    }
    
    p_f <- ggplot(data = df_f, aes(x = Year, y = Value))
    
    if (!is.na(NatM)) {
      F_targ <- (2/3) * NatM
      F_thresh <- NatM
      F_lim <- 1.5 * NatM
      
      p_f <- p_f +
        geom_hline(yintercept = F_targ, linetype = "dashed", colour = "darkgreen", linewidth = 0.7) + 
        annotate("text", x = min(df_f$Year), y = F_targ, label = paste0("Target (", round(F_targ, 3), ")"), 
                 hjust = 0, vjust = -0.8, colour = "darkgreen", size = 6, fontface = "bold") +
        geom_hline(yintercept = F_thresh, linetype = "dashed", colour = "orange", linewidth = 0.7) + 
        annotate("text", x = min(df_f$Year), y = F_thresh, label = paste0("Threshold (", round(F_thresh, 3), ")"), 
                 hjust = 0, vjust = -0.8, colour = "orange", size = 6, fontface = "bold") +
        geom_hline(yintercept = F_lim, linetype = "dashed", colour = "red", linewidth = 0.7) + 
        annotate("text", x = min(df_f$Year), y = F_lim, label = paste0("Limit (", round(F_lim, 3), ")"), 
                 hjust = 0, vjust = -0.8, colour = "red", size = 6, fontface = "bold")
    } else {
      warning("DPIRD Plot: Could not find Natural Mortality parameter to calculate F reference points.")
    }
    
    p_f <- p_f +
      geom_ribbon(data = df_f_hist, aes(ymin = lo_95, ymax = hi_95), fill = "grey80", alpha = 0.6) +
      geom_ribbon(data = df_f_hist, aes(ymin = lo_60, ymax = hi_60), fill = "grey60", alpha = 0.6) +
      geom_ribbon(data = df_f_fore, aes(ymin = lo_95, ymax = hi_95), fill = "grey95", alpha = 0.6) +
      geom_ribbon(data = df_f_fore, aes(ymin = lo_60, ymax = hi_60), fill = "grey85", alpha = 0.6) +
      geom_line(data = df_f_hist, linewidth = 1.2, colour = "black", linetype = "solid") +
      
      geom_point(data = df_f_fore, colour = "black", size = 2) +
      
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA), sec.axis = dup_axis(labels = NULL, name = NULL)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(labels = NULL, name = NULL)) +
      labs(y = "Summary Fishing mortality", x = "Year") +
      target_theme
    
    ggsave(filename = file.path(dpird_dir, "F_value_DPIRD_with_intervals.png"), plot = p_f, width = 9, height = 6, dpi = 300)
    message(paste("DPIRD Plots generated in:", dpird_dir))
    
  } else {
    warning("DPIRD Plot: No F_YEAR derived quantities found.")
  }
}


# CUSTOM RAR PLOTS: Replot data availability, comp, and residual plots
generate_custom_RAR_plots <- function(replist, output_dir) {
  
  # All custom plots will go into this single subfolder
  custom_dir <- file.path(output_dir, "custom_plots")
  dir.create(custom_dir, showWarnings = FALSE, recursive = TRUE)
  
  # 1. Data Availability Plot
  tryCatch({
    r4ss::SS_plots(replist,
                   plot = 24,
                   dir = output_dir, 
                   printfolder = "custom_plots",
                   png = TRUE,
                   html = FALSE,
                   pwidth = 6.5,
                   pheight = 4,
                   SSplotDatMargin = 12,
                   verbose = FALSE)
  }, error = function(e) {
    warning(paste("Error generating data availability plot:", e$message))
  })
  
  # 2. Composition Plots Function
  plot_standardised_comps <- function(replist, comp_type, out_dir) {
    if (comp_type == "len") { dbase <- replist$lendbase; kind_str <- "LEN" }
    else if (comp_type == "age") { dbase <- replist$agedbase; kind_str <- "AGE" }
    else if (comp_type == "gstage") { dbase <- replist$ghostagedbase; kind_str <- "GSTAGE" }
    else return(NULL)
    
    if (is.null(dbase) || nrow(dbase) == 0) return(NULL)
    
    fleet_parts <- unique(dbase[, c("Fleet", "Part")])
    panel_width <- 3.5; panel_height <- 2.3; margin_width <- 1.2; margin_height <- 1.2
    
    for (i in 1:nrow(fleet_parts)) {
      f <- fleet_parts$Fleet[i]
      p <- fleet_parts$Part[i]
      
      fleet_data <- dbase[dbase$Fleet == f & dbase$Part == p, ]
      years <- unique(fleet_data$Yr)
      n_panels <- length(years)
      
      if (n_panels == 0) next
      
      temp_replist <- replist
      if (comp_type == "len") temp_replist$lendbase <- fleet_data
      else if (comp_type == "age") temp_replist$agedbase <- fleet_data
      else if (comp_type == "gstage") temp_replist$ghostagedbase <- fleet_data
      
      max_cols <- 2
      ncols <- min(n_panels, max_cols)
      nrows <- ceiling(n_panels / ncols)
      
      plot_width <- margin_width + (ncols * panel_width)
      plot_height <- margin_height + (nrows * panel_height)
      
      max_dim <- max(nrows, ncols)
      if (max_dim >= 3) {
        cex_reduction <- 0.66
      } else if (max_dim == 2) {
        cex_reduction <- 0.83
      } else {
        cex_reduction <- 1.0
      }
      dynamic_pointsize <- 12 / cex_reduction
      
      filename <- file.path(out_dir, paste0("comp_", comp_type, "fit_flt", f, "mkt", p, ".png"))
      png(filename, width = plot_width, height = plot_height, units = "in", res = 300, pointsize = dynamic_pointsize)
      
      tryCatch({
        r4ss::SSplotComps(temp_replist, subplots = c(1), kind = kind_str, fleets = f,
                          maxrows = nrows, maxcols = ncols,
                          mainTitle = FALSE, print = FALSE)
      }, finally = { dev.off() })
    }
  }
  
  # 3. Pearson Residuals Function
  plot_standardised_resids <- function(replist, comp_type, out_dir) {
    if (comp_type == "len") { dbase <- replist$lendbase; kind_str <- "LEN" }
    else if (comp_type == "age") { dbase <- replist$agedbase; kind_str <- "AGE" }
    else if (comp_type == "cond") { dbase <- replist$condbase; kind_str <- "cond" }
    else if (comp_type == "gstage") { dbase <- replist$ghostagedbase; kind_str <- "GSTAGE" }
    else return(NULL)
    
    if (is.null(dbase) || nrow(dbase) == 0) return(NULL)
    
    fleet_parts <- unique(dbase[, c("Fleet", "Part")])
    panel_width <- 3.0; panel_height <- 2.8; margin_width <- 1.5; margin_height <- 1.5
    
    for (i in 1:nrow(fleet_parts)) {
      f <- fleet_parts$Fleet[i]
      p <- fleet_parts$Part[i]
      
      fleet_data <- dbase[dbase$Fleet == f & dbase$Part == p, ]
      if (nrow(fleet_data) == 0) next
      
      if (comp_type == "cond") {
        group_cols <- intersect(names(fleet_data), c("Yr", "Seas", "Sex", "Gender", "gender", "sex"))
      } else {
        group_cols <- intersect(names(fleet_data), c("Yr", "Seas"))
      }
      
      n_panels <- nrow(unique(fleet_data[, group_cols, drop = FALSE]))
      if (n_panels == 0) next
      
      temp_replist <- replist
      if (comp_type == "len") temp_replist$lendbase <- fleet_data
      else if (comp_type == "age") temp_replist$agedbase <- fleet_data
      else if (comp_type == "cond") temp_replist$condbase <- fleet_data
      else if (comp_type == "gstage") temp_replist$ghostagedbase <- fleet_data
      
      max_cols <- 3
      ncols <- min(n_panels, max_cols)
      nrows <- ceiling(n_panels / ncols)
      
      plot_width <- margin_width + (ncols * panel_width)
      plot_height <- margin_height + (nrows * panel_height)
      
      max_dim <- max(nrows, ncols)
      if (max_dim >= 3) {
        cex_reduction <- 0.66
      } else if (max_dim == 2) {
        cex_reduction <- 0.83
      } else {
        cex_reduction <- 1.0
      }
      dynamic_pointsize <- 12 / cex_reduction
      
      filename <- file.path(out_dir, paste0("resid_", comp_type, "_flt", f, "mkt", p, ".png"))
      png(filename, width = plot_width, height = plot_height, units = "in", res = 300, pointsize = dynamic_pointsize)
      
      tryCatch({
        par(mar = c(2.0, 2.5, 1.5, 1.0) + 0.1, oma = c(2.0, 2.0, 1.0, 1.0),
            cex = 1.0, cex.axis = 0.9, cex.lab = 1.1)
        
        r4ss::SSplotComps(temp_replist, subplots = c(3), kind = kind_str, fleets = f,
                          maxrows = nrows, maxcols = ncols,
                          maxrows2 = nrows, maxcols2 = ncols,
                          mainTitle = FALSE, print = FALSE)
      }, finally = { dev.off() })
    }
  }
  
  # # 4. Conditional Age-at-Length with Growth Overlay Function
  # plot_caal_growth_overlay <- function(replist, out_dir) {
  #   if (!"package:ggplot2" %in% search()) suppressPackageStartupMessages(library(ggplot2))
  #   if (!"package:dplyr" %in% search()) suppressPackageStartupMessages(library(dplyr))
  #   
  #   condbase <- replist$condbase
  #   growth <- replist$endgrowth
  #   
  #   if (is.null(condbase) || nrow(condbase) == 0 || is.null(growth) || nrow(growth) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # Dynamically find column names
  #   age_col_cond <- if ("Bin" %in% names(condbase)) "Bin" else if ("Age" %in% names(condbase)) "Age" else "Bin"
  #   len_col_cond <- if ("Lbin_lo" %in% names(condbase)) "Lbin_lo" else if ("Lbin_mid" %in% names(condbase)) "Lbin_mid" else "Lbin_lo"
  #   
  #   age_col_gro <- if ("Age_Beg" %in% names(growth)) "Age_Beg" else if ("Age" %in% names(growth)) "Age" else "Age_Beg"
  #   len_col_gro <- if ("Len_Beg" %in% names(growth)) "Len_Beg" else if ("Len_Mid" %in% names(growth)) "Len_Mid" else "Len_Beg"
  #   
  #   # 1. Kill the Ghost Grid
  #   caal_obs <- condbase[!is.na(condbase$Obs) & condbase$Obs > 0.002, ]
  #   
  #   # Strictly drop unsampled ghost bins
  #   n_col <- if("Nsamp_adj" %in% names(caal_obs)) "Nsamp_adj" else if("N" %in% names(caal_obs)) "N" else NULL
  #   if(!is.null(n_col)) {
  #     caal_obs <- caal_obs[!is.na(caal_obs[[n_col]]) & caal_obs[[n_col]] > 0, ]
  #   }
  #   if (nrow(caal_obs) == 0) return(NULL)
  #   
  #   # 2. Assign Base Sex
  #   sex_col_cond <- if ("Gender" %in% names(caal_obs)) "Gender" else if ("Sex" %in% names(caal_obs)) "Sex" else "Sex"
  #   caal_obs$BaseGender <- as.character(caal_obs[[sex_col_cond]])
  #   
  #   caal_obs$PlotGender <- "Unsexed"
  #   caal_obs$PlotGender[caal_obs$BaseGender %in% c("1", "F", "Female", "female")] <- "Female"
  #   caal_obs$PlotGender[caal_obs$BaseGender %in% c("2", "M", "Male", "male")] <- "Male"
  #   
  #   # Override with Pick_gender if it explicitly marks Joint (3) or Unsexed (0)
  #   if ("Pick_gender" %in% names(caal_obs)) {
  #     pg <- as.character(caal_obs$Pick_gender)
  #     caal_obs$PlotGender[pg %in% c("0", "3")] <- "Unsexed"
  #   }
  #   
  #   # 3. Detect Mathematically Split Joint-Sex Records
  #   caal_obs <- caal_obs %>%
  #     dplyr::group_by(Yr, Fleet, dplyr::across(dplyr::all_of(c(age_col_cond, len_col_cond)))) %>%
  #     dplyr::mutate(
  #       has_F = any(PlotGender == "Female"),
  #       has_M = any(PlotGender == "Male"),
  #       is_split = dplyr::n() >= 2 & has_F & has_M
  #     ) %>%
  #     dplyr::ungroup()
  #   
  #   # Re-tag mathematically split records back to Unsexed
  #   caal_obs$PlotGender[caal_obs$is_split] <- "Unsexed"
  #   caal_obs$PlotGender <- factor(caal_obs$PlotGender, levels = c("Female", "Male", "Unsexed"))
  #   
  #   # 4. Recombine and Sum Proportions for Clean Plotting
  #   # caal_obs <- caal_obs %>%
  #   #   dplyr::group_by(Yr, Fleet, PlotGender, dplyr::across(dplyr::all_of(c(age_col_cond, len_col_cond)))) %>%
  #   #   dplyr::summarise(Obs = sum(Obs, na.rm = TRUE), .groups = "drop")
  #   caal_obs <- caal_obs %>%
  #     dplyr::group_by(Yr, Fleet, PlotGender, .data[[age_col_cond]], .data[[len_col_cond]]) %>%
  #     dplyr::summarise(Obs = sum(Obs, na.rm = TRUE), .groups = "drop")
  #   
  #   # 5. Format Growth Curve Sexes and Patterns
  #   growth_curve <- growth[growth$Seas == 1, ]
  #   sex_col_gro <- if ("Sex" %in% names(growth_curve)) "Sex" else if ("Gender" %in% names(growth_curve)) "Gender" else "Sex"
  #   gro_sex <- as.character(growth_curve[[sex_col_gro]])
  #   
  #   growth_curve$PlotGender <- "Unsexed"
  #   growth_curve$PlotGender[gro_sex %in% c("1", "F", "Female", "female")] <- "Female"
  #   growth_curve$PlotGender[gro_sex %in% c("2", "M", "Male", "male")] <- "Male"
  #   growth_curve$PlotGender <- factor(growth_curve$PlotGender, levels = c("Female", "Male", "Unsexed"))
  #   
  #   # Extract pattern to handle multi-pattern models properly
  #   pattern_col_gro <- if ("Pattern" %in% names(growth_curve)) "Pattern" else if ("Morph" %in% names(growth_curve)) "Morph" else NULL
  #   if (!is.null(pattern_col_gro)) {
  #     growth_curve$GrowthPattern <- as.factor(growth_curve[[pattern_col_gro]])
  #   } else {
  #     growth_curve$GrowthPattern <- as.factor("1")
  #   }
  #   
  #   # 6. Explicitly define and lock the colours
  #   sex_colors <- c("Female" = "#F8766D", "Male" = "#00BFC4", "Unsexed" = "#999999")
  #   
  #   fleets <- unique(caal_obs$Fleet)
  #   
  #   for (f in fleets) {
  #     fleet_data <- caal_obs[caal_obs$Fleet == f, ]
  #     if (nrow(fleet_data) == 0) next
  #     
  #     years <- unique(fleet_data$Yr)
  #     n_years <- length(years)
  #     if (n_years == 0) next
  #     
  #     ncols <- min(n_years, 3)
  #     nrows <- ceiling(n_years / ncols)
  #     plot_width <- max(6, 1.5 + (ncols * 3.0))
  #     plot_height <- max(4, 1.5 + (nrows * 2.8))
  #     
  #     
  #     plt <- ggplot() +
  #       # Removed size = Obs from aes() and set a fixed size = 2
  #       geom_point(data = fleet_data, aes(x = .data[[age_col_cond]], y = .data[[len_col_cond]], colour = PlotGender), size = 2, alpha = 0.8, stroke = 0) +
  #       geom_line(data = growth_curve, aes(x = .data[[age_col_gro]], y = .data[[len_col_gro]], colour = PlotGender, group = interaction(PlotGender, GrowthPattern)),
  #                 linetype = "solid", linewidth = 1.2) +
  #       facet_wrap(~ Yr, ncol = ncols) +
  #       scale_colour_manual(values = sex_colors, drop = FALSE) +
  #       # Removed scale_size_area() completely
  #       guides(
  #         colour = guide_legend(override.aes = list(size = 5, alpha = 1)),
  #         # Removed the size guide
  #         linetype = guide_legend(title = "Growth Pattern")
  #       ) +
  #       labs(
  #         title = paste0("Fleet ", f, " Conditional Age-at-Length"),
  #         x = "Age (years)",
  #         y = "Length (mm)",
  #         # Removed the size label
  #         colour = "Sex"
  #       ) +
  #       theme_bw(base_family = "Arial") +
  #       theme(
  #         legend.position = "bottom",
  #         plot.title = element_text(face = "bold", hjust = 0.5),
  #         strip.text = element_text(face = "bold"),
  #         panel.grid.minor = element_blank()
  #       )
  #     
  #     filename <- file.path(out_dir, paste0("caal_growth_overlay_flt", f, "_combined.png"))
  #     ggsave(filename, plot = plt, width = plot_width, height = plot_height, dpi = 300, bg = "white")
  #   }
  # }
  
  # R/ss3_custom_plots.R (Updated inside generate_custom_RAR_plots)
  
  # R/ss3_custom_plots.R (Refactored for strict sex preservation and forecast year mapping)
  
  plot_caal_growth_overlay <- function(replist, out_dir) {
    # Clean namespace checking
    if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("tidyr", quietly = TRUE)) {
      warning("Missing required packages for CAAL overlay plot.")
      return(NULL)
    }
    
    condbase <- replist$condbase
    growth_series <- replist$growthseries
    
    if (is.null(condbase) || nrow(condbase) == 0 || is.null(growth_series) || nrow(growth_series) == 0) {
      return(NULL)
    }
    
    age_col_cond <- if ("Bin" %in% names(condbase)) "Bin" else if ("Age" %in% names(condbase)) "Age" else "Bin"
    len_col_cond <- if ("Lbin_lo" %in% names(condbase)) "Lbin_lo" else if ("Lbin_mid" %in% names(condbase)) "Lbin_mid" else "Lbin_lo"
    
    # 1. Clean Observation Data
    caal_obs <- condbase[!is.na(condbase$Obs) & condbase$Obs > 0.002, ]
    n_col <- if("Nsamp_adj" %in% names(caal_obs)) "Nsamp_adj" else if("N" %in% names(caal_obs)) "N" else NULL
    if(!is.null(n_col)) {
      caal_obs <- caal_obs[!is.na(caal_obs[[n_col]]) & caal_obs[[n_col]] > 0, ]
    }
    if (nrow(caal_obs) == 0) return(NULL)
    
    # 2. Assign Base Sex
    sex_col_cond <- if ("Gender" %in% names(caal_obs)) "Gender" else if ("Sex" %in% names(caal_obs)) "Sex" else "Sex"
    caal_obs$BaseGender <- as.character(caal_obs[[sex_col_cond]])
    
    caal_obs$PlotGender <- "Unsexed"
    caal_obs$PlotGender[caal_obs$BaseGender %in% c("1", "F", "Female", "female")] <- "Female"
    caal_obs$PlotGender[caal_obs$BaseGender %in% c("2", "M", "Male", "male")] <- "Male"
    
    if ("Pick_gender" %in% names(caal_obs)) {
      pg <- as.character(caal_obs$Pick_gender)
      caal_obs$PlotGender[pg %in% c("0", "3")] <- "Unsexed"
    }
    caal_obs$PlotGender <- factor(caal_obs$PlotGender, levels = c("Female", "Male", "Unsexed"))
    
    # 3. Sum Proportions for Clean Plotting
    caal_obs <- caal_obs %>%
      dplyr::group_by(Yr, Fleet, PlotGender, .data[[age_col_cond]], .data[[len_col_cond]]) %>%
      dplyr::summarise(Obs = sum(Obs, na.rm = TRUE), .groups = "drop")
    
    # 4. Format Time-Varying Growth Series with Type-Safe Fallback Year Mapping
    valid_growth_years <- unique(growth_series$Yr)
    obs_years <- unique(caal_obs$Yr)
    
    mapped_years <- data.frame(
      ObsYr = obs_years,
      GrowthYr = vapply(obs_years, function(y) {
        if (y %in% valid_growth_years) return(as.numeric(y))
        as.numeric(valid_growth_years[which.min(abs(valid_growth_years - y))])
      }, numeric(1))
    )
    
    # 5. Robust Regex Pivoting (Eliminates brittle id_cols dependency)
    growth_long <- growth_series %>%
      dplyr::filter(Seas == 1) %>%
      dplyr::inner_join(mapped_years, by = c("Yr" = "GrowthYr"), relationship = "many-to-many") %>%
      dplyr::mutate(Yr = ObsYr) %>% 
      dplyr::select(-ObsYr) %>%
      tidyr::pivot_longer(
        cols = tidyr::matches("^\\d+$"), # Strictly targets only numeric age columns
        names_to = "Age",
        values_to = "Len_Mid",
        names_transform = list(Age = as.numeric)
      ) %>%
      dplyr::filter(!is.na(Age))
    
    # 6. Extract Sex/Morph for Growth Lines
    sex_col_gro <- if ("Sex" %in% names(growth_long)) "Sex" else 
      if ("Gender" %in% names(growth_long)) "Gender" else 
        if ("Morph" %in% names(growth_long)) "Morph" else NULL
    
    growth_long$PlotGender <- "Unsexed"
    if (!is.null(sex_col_gro)) {
      gro_sex <- as.character(growth_long[[sex_col_gro]])
      growth_long$PlotGender[gro_sex %in% c("1", "F", "Female", "female")] <- "Female"
      growth_long$PlotGender[gro_sex %in% c("2", "M", "Male", "male")] <- "Male"
    }
    growth_long$PlotGender <- factor(growth_long$PlotGender, levels = c("Female", "Male", "Unsexed"))
    
    pattern_col_gro <- if ("Pattern" %in% names(growth_long)) "Pattern" else if ("Morph" %in% names(growth_long)) "Morph" else NULL
    if (!is.null(pattern_col_gro)) {
      growth_long$GrowthPattern <- as.factor(growth_long[[pattern_col_gro]])
    } else {
      growth_long$GrowthPattern <- as.factor("1")
    }
    
    # 7. Execute Faceted Plotting
    sex_colors <- c("Female" = "#F8766D", "Male" = "#00BFC4", "Unsexed" = "#999999")
    fleets <- unique(caal_obs$Fleet)
    
    for (f in fleets) {
      fleet_data <- caal_obs[caal_obs$Fleet == f, ]
      if (nrow(fleet_data) == 0) next
      
      years <- sort(unique(fleet_data$Yr))
      n_years <- length(years)
      if (n_years == 0) next
      
      fleet_data$Yr <- factor(fleet_data$Yr, levels = years)
      fleet_growth <- growth_long[growth_long$Yr %in% years, ]
      fleet_growth$Yr <- factor(fleet_growth$Yr, levels = years)
      
      ncols <- min(n_years, 3)
      nrows <- ceiling(n_years / ncols)
      plot_width <- max(6, 1.5 + (ncols * 3.0))
      plot_height <- max(4, 1.5 + (nrows * 2.8))
      
      plt <- ggplot2::ggplot() +
        ggplot2::geom_point(data = fleet_data, ggplot2::aes(x = .data[[age_col_cond]], y = .data[[len_col_cond]], colour = PlotGender), size = 2, alpha = 0.8, stroke = 0) +
        ggplot2::geom_line(data = fleet_growth, ggplot2::aes(x = Age, y = Len_Mid, colour = PlotGender, group = interaction(PlotGender, GrowthPattern)),
                           linetype = "solid", linewidth = 1.2) +
        ggplot2::facet_wrap(~ Yr, ncol = ncols, drop = FALSE) +
        ggplot2::scale_colour_manual(values = sex_colors, drop = FALSE) +
        ggplot2::guides(
          colour = ggplot2::guide_legend(override.aes = list(size = 5, alpha = 1)),
          linetype = ggplot2::guide_legend(title = "Growth Pattern")
        ) +
        ggplot2::labs(
          title = paste0("Fleet ", f, " Conditional Age-at-Length Overlay"),
          x = "Age (years)",
          y = "Length (mm)",
          colour = "Sex"
        ) +
        ggplot2::theme_bw(base_family = "Arial") +
        ggplot2::theme(
          legend.position = "bottom",
          plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
          strip.text = ggplot2::element_text(face = "bold"),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      filename <- file.path(out_dir, paste0("caal_growth_overlay_flt", f, "_combined.png"))
      ggplot2::ggsave(filename, plot = plt, width = plot_width, height = plot_height, dpi = 300, bg = "white")
    }
  }
  
  # Refactored execution commands using standard warning streams instead of print
  tryCatch(plot_standardised_comps(replist, "len", custom_dir), error=function(e) warning(paste("Len Comp Error:", e$message)))
  tryCatch(plot_standardised_comps(replist, "age", custom_dir), error=function(e) warning(paste("Age Comp Error:", e$message)))
  tryCatch(plot_standardised_comps(replist, "gstage", custom_dir), error=function(e) warning(paste("Ghost Age Error:", e$message)))
  tryCatch(plot_standardised_resids(replist, "cond", custom_dir), error=function(e) warning(paste("Resid Error:", e$message)))
  tryCatch(plot_caal_growth_overlay(replist, custom_dir), error=function(e) warning(paste("CAAL Overlay Error:", e$message)))
  
  # # Execute plotting commands, printing errors to console to aid debugging
  # tryCatch(plot_standardised_comps(replist, "len", custom_dir), error=function(e) print(paste("Len Comp Error:", e$message)))
  # tryCatch(plot_standardised_comps(replist, "age", custom_dir), error=function(e) print(paste("Age Comp Error:", e$message)))
  # tryCatch(plot_standardised_comps(replist, "gstage", custom_dir), error=function(e) print(paste("Ghost Age Error:", e$message)))
  # tryCatch(plot_standardised_resids(replist, "cond", custom_dir), error=function(e) print(paste("Resid Error:", e$message)))
  # tryCatch(plot_caal_growth_overlay(replist, custom_dir), error=function(e) print(paste("CAAL Overlay Error:", e$message)))
}

generate_single_REP_plots <- function(replist, output_dir, model_name) {
  
  # Source the script locally so parallel workers can access it
  # source("SSplotComparisonsREP.R", local = TRUE)
  
  # Place the replist in a named list
  models_list <- list(replist)
  names(models_list) <- model_name
  
  # Summarise the single-model list
  mod.sum <- r4ss::SSsummarize(models_list)
  
  # Safely extract Natural Mortality matching the DPIRD plots logic
  NatM <- NA
  m_match <- grep("NatM", replist$parameters$Label)
  if (length(m_match) > 0) {
    NatM <- replist$parameters$Value[m_match[1]]
  } else {
    warning("Could not find Natural Mortality parameter; defaulting to 0.1 for REP plots.")
    NatM <- 0.1 
  }
  
  # Calculate reference points
  Ftarg_val <- (2/3) * NatM
  Fthresh_val <- NatM
  Flim_val <- (3/2) * NatM
  
  # Define your target directory
  target_plotdir <- file.path(output_dir, "REP")
  if (!dir.exists(target_plotdir)) {
    dir.create(target_plotdir, recursive = TRUE)
  }
  
  # Run the plotting function with explicitly defined r4ss defaults
  SSplotComparisons_REP(
    summaryoutput = mod.sum,
    endyrvec = replist$endyr,
    plot = FALSE,
    print = TRUE,                  
    plotdir = target_plotdir,
    btarg = 0.4,
    minbthresh = 0.2,
    filenameprefix = "",
    legend = TRUE,
    Ftarg = Ftarg_val,
    Fthresh = Fthresh_val,
    Flim = Flim_val,
    legendlabels = c(model_name),
    png = TRUE,
    pwidth = 6.5,          # Matches standard SS_plots width
    pheight = 4,         # Matches standard SS_plots height
    punits = "in",       # Standard units
    res = 300,           # Standard resolution
    ptsize = 10          # Matches standard SS_plots font scaling
  )
}