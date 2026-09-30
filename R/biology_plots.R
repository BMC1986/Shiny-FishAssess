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
