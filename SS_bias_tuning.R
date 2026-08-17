# --- Background Bias & Tuning Script Initiated ---

cat("--- Background Bias & Tuning Script Initiated ---\n")

# --- Load required plotting library ---
suppressPackageStartupMessages(library(ggplot2))

# --- Helper Function: Generate Custom DPIRD Plots ---

# generate_DPIRD_plots <- function(replist, output_dir) {
#   
#   # 1. Setup Directory
#   dpird_dir <- file.path(output_dir, "DPIRD_plots")
#   if (!dir.exists(dpird_dir)) dir.create(dpird_dir, recursive = TRUE, showWarnings = FALSE)
#   
#   # Load ggplot2 if not already loaded
#   if (!"package:ggplot2" %in% search()) suppressPackageStartupMessages(library(ggplot2))
#   
#   # Retrieve the model end year to distinguish history from forecast
#   end_year <- replist$endyr
#   
#   # =========================================================================
#   # CUSTOM THEME: Mimic base R style and improve text legibility
#   # =========================================================================
#   target_theme <- theme_bw(base_family = "Arial") +
#     theme(
#       plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
#       plot.subtitle = element_text(hjust = 0.5, size = 14),
#       axis.title = element_text(face = "bold", size = 16),
#       axis.text = element_text(size = 14, colour = "black"),
#       
#       # Inward pointing ticks
#       axis.ticks.length = unit(-0.2, "cm"),
#       
#       # Add margin so inward ticks don't overlap with the text
#       axis.text.x = element_text(margin = margin(t = 10)),
#       axis.text.y = element_text(margin = margin(r = 10), hjust = 1),
#       
#       # Bounding box
#       panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
#       
#       # Match dotted horizontal gridlines from the target chart
#       panel.grid.major.y = element_line(colour = "grey70", linetype = "dotted", linewidth = 0.5),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor = element_blank(),
#       
#       plot.margin = margin(15, 15, 15, 15)
#     )
#   
#   # =========================================================================
#   # PLOT 1: FRACTION OF UNFISHED SPAWNING BIOMASS (Depletion)
#   # =========================================================================
#   
#   dq <- replist$derived_quants
#   bratio_rows <- grep("^Bratio_\\d+$", rownames(dq))
#   
#   if (length(bratio_rows) > 0) {
#     df <- dq[bratio_rows, ]
#     df$Label <- rownames(df)
#     df$Year <- as.numeric(sub("Bratio_", "", df$Label))
#     df <- df[order(df$Year), ]
#     
#     # Calculate Intervals
#     df$lo_95 <- pmax(0, df$Value - 1.96 * df$StdDev)
#     df$hi_95 <- df$Value + 1.96 * df$StdDev
#     z_60 <- qnorm(0.8) 
#     df$lo_60 <- pmax(0, df$Value - z_60 * df$StdDev)
#     df$hi_60 <- df$Value + z_60 * df$StdDev
#     
#     # Split into Historical and Forecast data frames
#     df_hist <- df[df$Year <= end_year, ]
#     df_fore <- df[df$Year >= end_year, ]
#     
#     p_dep <- ggplot(data = df, aes(x = Year, y = Value)) +
#       # Reference Lines with increased text size (4.5) and adjusted vjust
#       geom_hline(yintercept = 0.4, linetype = "dashed", colour = "darkgreen", linewidth = 0.7) + 
#       annotate("text", x = min(df$Year), y = 0.4, label = "Target (0.4)", hjust = 0, vjust = -0.8, colour = "darkgreen", size = 4.5, fontface = "bold") +
#       geom_hline(yintercept = 0.3, linetype = "dashed", colour = "orange", linewidth = 0.7) + 
#       annotate("text", x = min(df$Year), y = 0.3, label = "Threshold (0.3)", hjust = 0, vjust = -0.8, colour = "orange", size = 4.5, fontface = "bold") +
#       geom_hline(yintercept = 0.2, linetype = "dashed", colour = "red", linewidth = 0.7) + 
#       annotate("text", x = min(df$Year), y = 0.2, label = "Limit (0.2)", hjust = 0, vjust = -0.8, colour = "red", size = 4.5, fontface = "bold") +
#       
#       # Historical Ribbons
#       geom_ribbon(data = df_hist, aes(ymin = lo_95, ymax = hi_95), fill = "grey80", alpha = 0.6) +
#       geom_ribbon(data = df_hist, aes(ymin = lo_60, ymax = hi_60), fill = "grey60", alpha = 0.6) +
#       
#       # Forecast Ribbons
#       geom_ribbon(data = df_fore, aes(ymin = lo_95, ymax = hi_95), fill = "grey95", alpha = 0.6) +
#       geom_ribbon(data = df_fore, aes(ymin = lo_60, ymax = hi_60), fill = "grey85", alpha = 0.6) +
#       
#       # Historical Line
#       geom_line(data = df_hist, linewidth = 1.2, colour = "black", linetype = "solid") +
#       
#       # Forecast Line
#       geom_line(data = df_fore, linewidth = 1.2, colour = "black", linetype = "dotted") +
#       
#       # Formatting: added sec.axis for top and right ticks
#       scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA), sec.axis = dup_axis(labels = NULL, name = NULL)) +
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(labels = NULL, name = NULL)) +
#       labs(y = "Fraction of unfished spawning biomass", x = "Year") +
#       target_theme
#     
#     ggsave(filename = file.path(dpird_dir, "Depletion_DPIRD_with_intervals.png"), plot = p_dep, width = 9, height = 6, dpi = 300)
#   } else {
#     warning("DPIRD Plot: No Bratio_YEAR derived quantities found.")
#   }
#   
#   # =========================================================================
#   # PLOT 2: FISHING MORTALITY (F)
#   # =========================================================================
#   
#   f_rows <- grep("^F_\\d+$", rownames(dq))
#   
#   if (length(f_rows) > 0) {
#     df_f <- dq[f_rows, ]
#     df_f$Label <- rownames(df_f)
#     df_f$Year <- as.numeric(sub("F_", "", df_f$Label))
#     df_f <- df_f[order(df_f$Year), ]
#     
#     # 2. Intervals
#     df_f$lo_95 <- pmax(0, df_f$Value - 1.96 * df_f$StdDev)
#     df_f$hi_95 <- df_f$Value + 1.96 * df_f$StdDev
#     z_60 <- qnorm(0.8) 
#     df_f$lo_60 <- pmax(0, df_f$Value - z_60 * df_f$StdDev)
#     df_f$hi_60 <- df_f$Value + z_60 * df_f$StdDev
#     
#     # Split into Historical and Forecast data frames
#     df_f_hist <- df_f[df_f$Year <= end_year, ]
#     df_f_fore <- df_f[df_f$Year >= end_year, ]
#     
#     # 3. Calculate Reference Points based on Natural Mortality (M)
#     NatM <- NA
#     m_match <- grep("NatM", replist$parameters$Label)
#     if (length(m_match) > 0) {
#       NatM <- replist$parameters$Value[m_match[1]]
#     }
#     
#     p_f <- ggplot(data = df_f, aes(x = Year, y = Value))
#     
#     # Add Reference Lines if M was found
#     if (!is.na(NatM)) {
#       F_targ <- (2/3) * NatM
#       F_thresh <- NatM
#       F_lim <- 1.5 * NatM
#       
#       p_f <- p_f +
#         geom_hline(yintercept = F_targ, linetype = "dashed", colour = "darkgreen", linewidth = 0.7) + 
#         annotate("text", x = min(df_f$Year), y = F_targ, label = paste0("Target (", round(F_targ, 3), ")"), 
#                  hjust = 0, vjust = -0.8, colour = "darkgreen", size = 4.5, fontface = "bold") +
#         geom_hline(yintercept = F_thresh, linetype = "dashed", colour = "orange", linewidth = 0.7) + 
#         annotate("text", x = min(df_f$Year), y = F_thresh, label = paste0("Threshold (", round(F_thresh, 3), ")"), 
#                  hjust = 0, vjust = -0.8, colour = "orange", size = 4.5, fontface = "bold") +
#         geom_hline(yintercept = F_lim, linetype = "dashed", colour = "red", linewidth = 0.7) + 
#         annotate("text", x = min(df_f$Year), y = F_lim, label = paste0("Limit (", round(F_lim, 3), ")"), 
#                  hjust = 0, vjust = -0.8, colour = "red", size = 4.5, fontface = "bold")
#     } else {
#       warning("DPIRD Plot: Could not find Natural Mortality parameter to calculate F reference points.")
#     }
#     
#     # Add Ribbons and Lines
#     p_f <- p_f +
#       geom_ribbon(data = df_f_hist, aes(ymin = lo_95, ymax = hi_95), fill = "grey80", alpha = 0.6) +
#       geom_ribbon(data = df_f_hist, aes(ymin = lo_60, ymax = hi_60), fill = "grey60", alpha = 0.6) +
#       geom_ribbon(data = df_f_fore, aes(ymin = lo_95, ymax = hi_95), fill = "grey95", alpha = 0.6) +
#       geom_ribbon(data = df_f_fore, aes(ymin = lo_60, ymax = hi_60), fill = "grey85", alpha = 0.6) +
#       geom_line(data = df_f_hist, linewidth = 1.2, colour = "black", linetype = "solid") +
#       geom_line(data = df_f_fore, linewidth = 1.2, colour = "black", linetype = "dotted") +
#       
#       # Formatting: added sec.axis for top and right ticks
#       scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA), sec.axis = dup_axis(labels = NULL, name = NULL)) +
#       scale_x_continuous(breaks = scales::pretty_breaks(n = 10), sec.axis = dup_axis(labels = NULL, name = NULL)) +
#       labs(y = "Summary Fishing mortality", x = "Year") +
#       target_theme
#     
#     ggsave(filename = file.path(dpird_dir, "F_value_DPIRD_with_intervals.png"), plot = p_f, width = 9, height = 6, dpi = 300)
#     message(paste("DPIRD Plots generated in:", dpird_dir))
#     
#   } else {
#     warning("DPIRD Plot: No F_YEAR derived quantities found.")
#   }
# }

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

# =========================================================================
# CUSTOM RAR PLOTS: Replot data availability, comp, and residual plots
# =========================================================================
# generate_custom_RAR_plots <- function(replist, output_dir) {
#   
#   # All custom plots will go into this single subfolder
#   custom_dir <- file.path(output_dir, "custom_plots")
#   dir.create(custom_dir, showWarnings = FALSE, recursive = TRUE)
#   
#   # 1. Data Availability Plot
#   tryCatch({
#     r4ss::SS_plots(replist,
#                    plot = 24,
#                    dir = output_dir, 
#                    printfolder = "custom_plots",
#                    png = TRUE,
#                    html = FALSE,
#                    pwidth = 6.5,
#                    pheight = 4,
#                    SSplotDatMargin = 12,
#                    verbose = FALSE)
#   }, error = function(e) {
#     warning(paste("Error generating data availability plot:", e$message))
#   })
#   
#   # 2. Composition Plots Function
#   plot_standardised_comps <- function(replist, comp_type, out_dir) {
#     if (comp_type == "len") { dbase <- replist$lendbase; kind_str <- "LEN" }
#     else if (comp_type == "age") { dbase <- replist$agedbase; kind_str <- "AGE" }
#     else if (comp_type == "gstage") { dbase <- replist$ghostagedbase; kind_str <- "GSTAGE" }
#     else return(NULL)
#     
#     if (is.null(dbase) || nrow(dbase) == 0) return(NULL)
#     
#     fleet_parts <- unique(dbase[, c("Fleet", "Part")])
#     panel_width <- 3.5; panel_height <- 2.3; margin_width <- 1.2; margin_height <- 1.2
#     
#     for (i in 1:nrow(fleet_parts)) {
#       f <- fleet_parts$Fleet[i]
#       p <- fleet_parts$Part[i]
#       
#       fleet_data <- dbase[dbase$Fleet == f & dbase$Part == p, ]
#       years <- unique(fleet_data$Yr)
#       n_panels <- length(years)
#       
#       if (n_panels == 0) next
#       
#       temp_replist <- replist
#       if (comp_type == "len") temp_replist$lendbase <- fleet_data
#       else if (comp_type == "age") temp_replist$agedbase <- fleet_data
#       else if (comp_type == "gstage") temp_replist$ghostagedbase <- fleet_data
#       
#       max_cols <- 2
#       ncols <- min(n_panels, max_cols)
#       nrows <- ceiling(n_panels / ncols)
#       
#       plot_width <- margin_width + (ncols * panel_width)
#       plot_height <- margin_height + (nrows * panel_height)
#       
#       max_dim <- max(nrows, ncols)
#       if (max_dim >= 3) {
#         cex_reduction <- 0.66
#       } else if (max_dim == 2) {
#         cex_reduction <- 0.83
#       } else {
#         cex_reduction <- 1.0
#       }
#       dynamic_pointsize <- 12 / cex_reduction
#       
#       filename <- file.path(out_dir, paste0("comp_", comp_type, "fit_flt", f, "mkt", p, ".png"))
#       png(filename, width = plot_width, height = plot_height, units = "in", res = 300, pointsize = dynamic_pointsize)
#       # tryCatch({
#       #   r4ss::SSplotComps(temp_replist, subplots = c(1), kind = kind_str, fleets = f,
#       #                     nrows = nrows, ncols = ncols, maxrows = nrows, maxcols = ncols,
#       #                     legendcex = 0.8, mainTitle = FALSE, print = FALSE)
#       # }, finally = { dev.off() })
#       tryCatch({
#         r4ss::SSplotComps(temp_replist, subplots = c(1), kind = kind_str, fleets = f,
#                           maxrows = nrows, maxcols = ncols,
#                           mainTitle = FALSE, print = FALSE)
#       }, finally = { dev.off() })
#     }
#   }
#   
#   # 3. Pearson Residuals Function
#   plot_standardised_resids <- function(replist, comp_type, out_dir) {
#     if (comp_type == "len") { dbase <- replist$lendbase; kind_str <- "LEN" }
#     else if (comp_type == "age") { dbase <- replist$agedbase; kind_str <- "AGE" }
#     else if (comp_type == "cond") { dbase <- replist$condbase; kind_str <- "cond" }
#     else if (comp_type == "gstage") { dbase <- replist$ghostagedbase; kind_str <- "GSTAGE" }
#     else return(NULL)
#     
#     if (is.null(dbase) || nrow(dbase) == 0) return(NULL)
#     
#     fleet_parts <- unique(dbase[, c("Fleet", "Part")])
#     panel_width <- 3.0; panel_height <- 2.8; margin_width <- 1.5; margin_height <- 1.5
#     
#     for (i in 1:nrow(fleet_parts)) {
#       f <- fleet_parts$Fleet[i]
#       p <- fleet_parts$Part[i]
#       
#       fleet_data <- dbase[dbase$Fleet == f & dbase$Part == p, ]
#       if (nrow(fleet_data) == 0) next
#       
#       if (comp_type == "cond") {
#         group_cols <- intersect(names(fleet_data), c("Yr", "Seas", "Sex", "Gender", "gender", "sex"))
#       } else {
#         group_cols <- intersect(names(fleet_data), c("Yr", "Seas"))
#       }
#       
#       n_panels <- nrow(unique(fleet_data[, group_cols, drop = FALSE]))
#       if (n_panels == 0) next
#       
#       temp_replist <- replist
#       if (comp_type == "len") temp_replist$lendbase <- fleet_data
#       else if (comp_type == "age") temp_replist$agedbase <- fleet_data
#       else if (comp_type == "cond") temp_replist$condbase <- fleet_data
#       else if (comp_type == "gstage") temp_replist$ghostagedbase <- fleet_data
#       
#       max_cols <- 3
#       ncols <- min(n_panels, max_cols)
#       nrows <- ceiling(n_panels / ncols)
#       
#       plot_width <- margin_width + (ncols * panel_width)
#       plot_height <- margin_height + (nrows * panel_height)
#       
#       max_dim <- max(nrows, ncols)
#       if (max_dim >= 3) {
#         cex_reduction <- 0.66
#       } else if (max_dim == 2) {
#         cex_reduction <- 0.83
#       } else {
#         cex_reduction <- 1.0
#       }
#       dynamic_pointsize <- 12 / cex_reduction
#       
#       filename <- file.path(out_dir, paste0("resid_", comp_type, "_flt", f, "mkt", p, ".png"))
#       png(filename, width = plot_width, height = plot_height, units = "in", res = 300, pointsize = dynamic_pointsize)
#       # tryCatch({
#       #   par(mar = c(2.0, 2.5, 1.5, 1.0) + 0.1, oma = c(2.0, 2.0, 1.0, 1.0),
#       #       cex = 1.0, cex.axis = 0.9, cex.lab = 1.1)
#       #   
#       #   r4ss::SSplotComps(temp_replist, subplots = c(3), kind = kind_str, fleets = f,
#       #                     nrows = nrows, ncols = ncols, maxrows = nrows, maxcols = ncols,
#       #                     maxrows2 = nrows, maxcols2 = ncols, legendcex = 0.8,
#       #                     mainTitle = FALSE, print = FALSE)
#       # }, finally = { dev.off() })
#       tryCatch({
#         par(mar = c(2.0, 2.5, 1.5, 1.0) + 0.1, oma = c(2.0, 2.0, 1.0, 1.0),
#             cex = 1.0, cex.axis = 0.9, cex.lab = 1.1)
#         
#         r4ss::SSplotComps(temp_replist, subplots = c(3), kind = kind_str, fleets = f,
#                           maxrows = nrows, maxcols = ncols,
#                           maxrows2 = nrows, maxcols2 = ncols,
#                           mainTitle = FALSE, print = FALSE)
#       }, finally = { dev.off() })
#     }
#   }
#   
#   # Execute plotting commands, catching errors to avoid stopping the whole pipeline
#   tryCatch(plot_standardised_comps(replist, "len", custom_dir), error=function(e) NULL)
#   tryCatch(plot_standardised_comps(replist, "age", custom_dir), error=function(e) NULL)
#   tryCatch(plot_standardised_comps(replist, "gstage", custom_dir), error=function(e) NULL)
#   tryCatch(plot_standardised_resids(replist, "cond", custom_dir), error=function(e) NULL)
# }

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
  
  # 4. Conditional Age-at-Length with Growth Overlay Function
  plot_caal_growth_overlay <- function(replist, out_dir) {
    if (!"package:ggplot2" %in% search()) suppressPackageStartupMessages(library(ggplot2))
    if (!"package:dplyr" %in% search()) suppressPackageStartupMessages(library(dplyr))
    
    condbase <- replist$condbase
    growth <- replist$endgrowth
    
    if (is.null(condbase) || nrow(condbase) == 0 || is.null(growth) || nrow(growth) == 0) {
      return(NULL)
    }
    
    # Dynamically find column names
    age_col_cond <- if ("Bin" %in% names(condbase)) "Bin" else if ("Age" %in% names(condbase)) "Age" else "Bin"
    len_col_cond <- if ("Lbin_lo" %in% names(condbase)) "Lbin_lo" else if ("Lbin_mid" %in% names(condbase)) "Lbin_mid" else "Lbin_lo"
    
    age_col_gro <- if ("Age_Beg" %in% names(growth)) "Age_Beg" else if ("Age" %in% names(growth)) "Age" else "Age_Beg"
    len_col_gro <- if ("Len_Beg" %in% names(growth)) "Len_Beg" else if ("Len_Mid" %in% names(growth)) "Len_Mid" else "Len_Beg"
    
    # 1. Kill the Ghost Grid
    caal_obs <- condbase[!is.na(condbase$Obs) & condbase$Obs > 0.002, ]
    
    # Strictly drop unsampled ghost bins
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
    
    # Override with Pick_gender if it explicitly marks Joint (3) or Unsexed (0)
    if ("Pick_gender" %in% names(caal_obs)) {
      pg <- as.character(caal_obs$Pick_gender)
      caal_obs$PlotGender[pg %in% c("0", "3")] <- "Unsexed"
    }
    
    # 3. Detect Mathematically Split Joint-Sex Records
    caal_obs <- caal_obs %>%
      dplyr::group_by(Yr, Fleet, dplyr::across(dplyr::all_of(c(age_col_cond, len_col_cond)))) %>%
      dplyr::mutate(
        has_F = any(PlotGender == "Female"),
        has_M = any(PlotGender == "Male"),
        is_split = dplyr::n() >= 2 & has_F & has_M
      ) %>%
      dplyr::ungroup()
    
    # Re-tag mathematically split records back to Unsexed
    caal_obs$PlotGender[caal_obs$is_split] <- "Unsexed"
    caal_obs$PlotGender <- factor(caal_obs$PlotGender, levels = c("Female", "Male", "Unsexed"))
    
    # 4. Recombine and Sum Proportions for Clean Plotting
    caal_obs <- caal_obs %>%
      dplyr::group_by(Yr, Fleet, PlotGender, dplyr::across(dplyr::all_of(c(age_col_cond, len_col_cond)))) %>%
      dplyr::summarise(Obs = sum(Obs, na.rm = TRUE), .groups = "drop")
    
    # 5. Format Growth Curve Sexes and Patterns
    growth_curve <- growth[growth$Seas == 1, ]
    sex_col_gro <- if ("Sex" %in% names(growth_curve)) "Sex" else if ("Gender" %in% names(growth_curve)) "Gender" else "Sex"
    gro_sex <- as.character(growth_curve[[sex_col_gro]])
    
    growth_curve$PlotGender <- "Unsexed"
    growth_curve$PlotGender[gro_sex %in% c("1", "F", "Female", "female")] <- "Female"
    growth_curve$PlotGender[gro_sex %in% c("2", "M", "Male", "male")] <- "Male"
    growth_curve$PlotGender <- factor(growth_curve$PlotGender, levels = c("Female", "Male", "Unsexed"))
    
    # Extract pattern to handle multi-pattern models properly
    pattern_col_gro <- if ("Pattern" %in% names(growth_curve)) "Pattern" else if ("Morph" %in% names(growth_curve)) "Morph" else NULL
    if (!is.null(pattern_col_gro)) {
      growth_curve$GrowthPattern <- as.factor(growth_curve[[pattern_col_gro]])
    } else {
      growth_curve$GrowthPattern <- as.factor("1")
    }
    
    # 6. Explicitly define and lock the colours
    sex_colors <- c("Female" = "#F8766D", "Male" = "#00BFC4", "Unsexed" = "#999999")
    
    fleets <- unique(caal_obs$Fleet)
    
    for (f in fleets) {
      fleet_data <- caal_obs[caal_obs$Fleet == f, ]
      if (nrow(fleet_data) == 0) next
      
      years <- unique(fleet_data$Yr)
      n_years <- length(years)
      if (n_years == 0) next
      
      ncols <- min(n_years, 3)
      nrows <- ceiling(n_years / ncols)
      plot_width <- max(6, 1.5 + (ncols * 3.0))
      plot_height <- max(4, 1.5 + (nrows * 2.8))
      
      plt <- ggplot() +
        geom_point(data = fleet_data, aes(x = .data[[age_col_cond]], y = .data[[len_col_cond]], size = Obs, colour = PlotGender), alpha = 0.8, stroke = 0) +
        # Note the added linetype and group aesthetics to prevent zigzagging lines
        geom_line(data = growth_curve, aes(x = .data[[age_col_gro]], y = .data[[len_col_gro]], colour = PlotGender, linetype = GrowthPattern, group = interaction(PlotGender, GrowthPattern)), linewidth = 1.2) +
        facet_wrap(~ Yr, ncol = ncols) +
        scale_colour_manual(values = sex_colors, drop = FALSE) +
        scale_size_area(max_size = 10) + 
        guides(
          colour = guide_legend(override.aes = list(size = 5, alpha = 1)),
          size = guide_legend(),
          linetype = guide_legend(title = "Growth Pattern")
        ) +
        labs(
          title = paste0("Fleet ", f, " Conditional Age-at-Length"),
          x = "Age (years)",
          y = "Length (mm)",
          size = "Observed Prop.",
          colour = "Sex"
        ) +
        theme_bw(base_family = "Arial") +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = 0.5),
          strip.text = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      filename <- file.path(out_dir, paste0("caal_growth_overlay_flt", f, "_combined.png"))
      ggsave(filename, plot = plt, width = plot_width, height = plot_height, dpi = 300, bg = "white")
    }
  }
  
  # Execute plotting commands, catching errors to avoid stopping the whole pipeline
  tryCatch(plot_standardised_comps(replist, "len", custom_dir), error=function(e) NULL)
  tryCatch(plot_standardised_comps(replist, "age", custom_dir), error=function(e) NULL)
  tryCatch(plot_standardised_comps(replist, "gstage", custom_dir), error=function(e) NULL)
  tryCatch(plot_standardised_resids(replist, "cond", custom_dir), error=function(e) NULL)
  tryCatch(plot_caal_growth_overlay(replist, custom_dir), error=function(e) NULL)
}

# --- Option Extraction and Validation ---
model_dir <- bias_tuning_options$model_dir
exe_path <- bias_tuning_options$exe_path
exe_name <- bias_tuning_options$exe_name
tuning_dir <- bias_tuning_options$tuning_dir
run_step <- bias_tuning_options$run_step
weighting_method <- bias_tuning_options$weighting_method # Will be NULL if not set

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
  
  result <- tryCatch(
    processx::run(
      command = exe_in_target_dir,
      args = "",
      wd = target_dir,
      error_on_status = TRUE
    ),
    error = function(e) e # Return the error object if it occurs
  )
  
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
  cat("Step 1: Reading source model files...\n")
  if (!file.copy(exe_path, file.path(model_dir, exe_name), overwrite = TRUE)) {
    stop("Failed to copy executable to model directory.")
  }
  
  starter_file_orig <- r4ss::SS_readstarter(file.path(model_dir, "starter.ss"), verbose = FALSE)
  dat_orig_path <- file.path(model_dir, starter_file_orig$datfile)
  dat_orig <- r4ss::SS_readdat(file = dat_orig_path, verbose = FALSE)
  
  ctl_orig <- r4ss::SS_readctl(
    file.path(model_dir, starter_file_orig$ctlfile),
    verbose = FALSE,
    use_datlist = TRUE,
    datlist = dat_orig
  )
  cat("-> Successfully read initial starter, data, and control files.\n")
  
  perform_bias_ramp <- function(input_replist, output_dir_name) {
    cat(paste("Performing bias ramp adjustment, output to:", output_dir_name, "...\n"))
    
    bias_adj <- r4ss::SS_fitbiasramp(input_replist, verbose = FALSE, plot = FALSE)
    cat("  -> Suggested bias adjustment parameters:\n")
    print(bias_adj$df)
    cat("\n")
    
    dat_path <- file.path(input_replist$inputs$dir, starter_file_orig$datfile)
    datlist_for_ctl <- r4ss::SS_readdat(file = dat_path, verbose = FALSE)
    
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
  
  if (run_step == "bias_ramp_only") {
    cat("--- EXECUTING: Single Bias Ramp Adjustment ---\n")
    
    replist_initial <- r4ss::SS_output(dir = model_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
    
    base_output_dir_name <- paste0(base_folder_name, "_bias_adj")
    output_dir_name <- base_output_dir_name
    counter <- 1
    while (dir.exists(file.path(tuning_dir, output_dir_name))) {
      counter <- counter + 1
      output_dir_name <- paste0(base_output_dir_name, "_", counter)
    }
    cat(paste("Output for this run will be saved to unique folder:", output_dir_name, "\n"))
    
    replist_after_bias_adj <- perform_bias_ramp(replist_initial, output_dir_name)
    
    cat("\nSingle Bias Ramp Adjustment Complete! ✅\n")
    cat(paste("Adjusted model is ready in:", file.path(tuning_dir, output_dir_name), "\n"))
    
    # r4ss::SS_plots(replist_after_bias_adj, dir = file.path(tuning_dir, output_dir_name), printfolder = "r4ss_plots", pdf = FALSE, png = TRUE, html = TRUE)
    
    r4ss::SS_plots(
      replist_after_bias_adj,
      pdf = FALSE,
      png = TRUE,
      html = TRUE,
      printfolder = "r4ss",
      dir = file.path(tuning_dir, output_dir_name),
      pwidth = 6.5,
      pheight = 4,
      minbthresh = 0.2,
      fitrange = FALSE,
      forecastplot = TRUE
    )
    
    try({
      html_dir <- file.path(tuning_dir, output_dir_name, "r4ss")
      html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
      for (f in html_files) {
        tx <- readLines(f, warn = FALSE)
        tx <- gsub("<title>SS Output</title>", paste0("<title>", output_dir_name, "</title>"), tx)
        writeLines(tx, f)
      }
    })
    
    cat("-> r4ss plots generated in the 'r4ss' subfolder.\n")
    
    tryCatch({
      cat("Generating custom DPIRD plots for Bias Ramp run...\n")
      generate_DPIRD_plots(replist_after_bias_adj, file.path(tuning_dir, output_dir_name))
    }, error = function(e) {
      cat(paste("Error generating DPIRD plots:", e$message, "\n"))
    })
    

    #Custom RAR plots
    tryCatch({
      cat("Generating custom RAR plots for Final Tuned Model...\n")
      generate_custom_RAR_plots(replist_final, final_model_dir)
      cat("Custom RAR plots generated successfully.\n")
    }, error = function(e) {
      cat(paste("Error generating custom RAR plots:", e$message, "\n"))
    })
    
  } else if (run_step == "full_sequence") {
    cat("--- EXECUTING: Full Tuning Sequence ---\n")
    
    dirs_to_remove <- c()
    
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
      
      first_bias_dir_name <- paste0(full_prefix, "_first_bias_adj")
      dirs_to_remove <- c(dirs_to_remove, file.path(tuning_dir, first_bias_dir_name))
      replist2 <- perform_bias_ramp(replist1, first_bias_dir_name)
      
      cat(paste("Step 2.3: Performing Composition Weighting using", weighting_method, "method...\n"))
      
      replist_before_final_bias_adj <- NULL
      cat("    -> Francis Tuning (3 iterations)...\n")
      
      # Iteration 1
      modelrun3_dir_name <- paste0(full_prefix, "_francis1")
      modelrun3_dir <- file.path(tuning_dir, modelrun3_dir_name)
      dirs_to_remove <- c(dirs_to_remove, modelrun3_dir)
      
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
      
      # --- NEW: Cleanup for Generalised Size Composition in Francis method ---
      if (!is.null(dat_2$N_sizefreq_methods) && dat_2$N_sizefreq_methods > 0) {
        # Reset CompError and ParmSelect back to multinomial defaults
        dat_2$Comp_Error_per_method <- rep(0, dat_2$N_sizefreq_methods)
        dat_2$ParmSelect_per_method <- rep(0, dat_2$N_sizefreq_methods)
      }
      # ---------------------------------------------------------------------
      
      r4ss::SS_writedat(dat_2, file.path(modelrun3_dir, starter_file_orig$datfile), overwrite = TRUE, verbose = FALSE)
      run_ss_in_dir(modelrun3_dir, exe_name)
      replist3 <- r4ss::SS_output(dir = modelrun3_dir, verbose = FALSE, printstats = FALSE, covar = TRUE)
      
      # Iteration 2
      modelrun4_dir_name <- paste0(full_prefix, "_francis2")
      modelrun4_dir <- file.path(tuning_dir, modelrun4_dir_name)
      dirs_to_remove <- c(dirs_to_remove, modelrun4_dir)
      
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
      dirs_to_remove <- c(dirs_to_remove, modelrun5_dir)
      
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
      
      first_bias_dir_name <- paste0(full_prefix, "_first_bias_adj")
      dirs_to_remove <- c(dirs_to_remove, file.path(tuning_dir, first_bias_dir_name))
      replist2 <- perform_bias_ramp(replist1, first_bias_dir_name)
      
      modelrun3_dir_name <- paste0(full_prefix, "_dirichlet1")
      modelrun3_dir <- file.path(tuning_dir, modelrun3_dir_name)
      
      dirs_to_remove <- c(dirs_to_remove, modelrun3_dir)
      
      r4ss::copy_SS_inputs(dir.old = replist2$inputs$dir, dir.new = modelrun3_dir, overwrite = TRUE, copy_exe = FALSE)
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
      
      max_parm <- max(c(dat_dirichlet$len_info$ParmSelect, dat_dirichlet$age_info$ParmSelect), na.rm = TRUE)
      
      # --- NEW: Dirichlet support for Generalised Size Composition (Sizefreq) ---
      if (!is.null(dat_dirichlet$N_sizefreq_methods) && dat_dirichlet$N_sizefreq_methods > 0) {
        num_methods <- dat_dirichlet$N_sizefreq_methods
        
        # Set Dirichlet (1) for CompError and increment parameters
        dat_dirichlet$Comp_Error_per_method <- rep(1, num_methods)
        dat_dirichlet$ParmSelect_per_method <- max_parm + seq_len(num_methods)
        
        # Update max parameter tally
        max_parm <- max(c(max_parm, dat_dirichlet$ParmSelect_per_method), na.rm = TRUE)
      }
      # --------------------------------------------------------------------------
      
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
    
    # Step 2.4: Final Bias Ramp Adjustment
    final_model_dir_name <- full_prefix
    
    replist_final <- perform_bias_ramp(replist_before_final_bias_adj, final_model_dir_name)
    
    # Step 2.5: Generate plots for the final model
    cat("Step 2.5: Generating r4ss plots for the final tuned model...\n")
    final_model_dir <- replist_final$inputs$dir
    
    # r4ss::SS_plots(replist_final, dir = final_model_dir, printfolder = "r4ss_plots", pdf = FALSE, png = TRUE, html = TRUE)
    
    r4ss::SS_plots(
      replist_final,
      pdf = FALSE,
      png = TRUE,
      html = TRUE,
      printfolder = "r4ss",
      dir = final_model_dir,
      pwidth = 6.5,
      pheight = 4,
      minbthresh = 0.2,
      fitrange = FALSE,
      forecastplot = TRUE
    )
    
    try({
      html_dir <- file.path(final_model_dir, "r4ss")
      html_files <- list.files(html_dir, pattern = "\\.html$", full.names = TRUE)
      for (f in html_files) {
        tx <- readLines(f, warn = FALSE)
        tx <- gsub("<title>SS Output</title>", paste0("<title>", final_model_dir_name, "</title>"), tx)
        writeLines(tx, f)
      }
    })
    
    cat("-> r4ss plots generated in the 'r4ss' subfolder.\n")
    
    # DPIRD plots
    tryCatch({
      cat("Generating custom DPIRD plots for Final Tuned Model...\n")
      generate_DPIRD_plots(replist_final, final_model_dir)
    }, error = function(e) {
      cat(paste("Error generating DPIRD plots:", e$message, "\n"))
    })
    
    #Custom RAR plots
    tryCatch({
      append_to_log("Generating custom RAR plots...")
      generate_custom_RAR_plots(replist, output_dir)
      append_to_log("Custom RAR plots generated successfully.")
    }, error = function(e) {
      append_to_log(paste("Error generating custom RAR plots:", e$message))
    })
    
    # --- CLEANUP STEP ---
    cat("\n--- CLEANUP: Removing intermediate model runs ---\n")
    
    gc() 
    Sys.sleep(2) 
    
    if (length(dirs_to_remove) > 0) {
      for (dir_path in dirs_to_remove) {
        if (dir.exists(dir_path)) {
          cat(paste("Removing:", dir_path, "\n"))
          
          result <- unlink(dir_path, recursive = TRUE, force = TRUE)
          
          if (dir.exists(dir_path)) {
            cat("  -> First delete attempt failed (folder locked?). Retrying in 2 seconds...\n")
            Sys.sleep(2)
            unlink(dir_path, recursive = TRUE, force = TRUE)
          }
          
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