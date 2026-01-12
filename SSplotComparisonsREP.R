## SS model comparison plots in r4ss

SSplotComparisons_REP <- function (summaryoutput, subplots = 1:20, plot = TRUE, print = FALSE, 
                                  png = print, pdf = FALSE, models = "all", endyrvec = NULL, 
                                  indexfleets = NULL, indexUncertainty = TRUE, indexQlabel = TRUE, 
                                  indexQdigits = 4, indexSEvec = NULL, indexPlotEach = FALSE, 
                                  labels = c("Year", "Spawning biomass (t)", "Fraction of unfished", 
                                             "Age-0 recruits (1,000s)", "Recruitment deviations", 
                                             "Index", "Log index", "SPR-related quantity", "Density", 
                                             "Target", "Limit", 
                                             "Spawning output", "Harvest rate", "Summary biomass (t)", 
                                             "Age X+ biomass (t)","Threshold"), col = NULL, shadecol = NULL, 
                                  pch = NULL, lty = 1, lwd = 2, spacepoints = 10, staggerpoints = 1, 
                                  initpoint = 0, tickEndYr = TRUE, shadeForecast = TRUE, xlim = NULL, 
                                  ylimAdj = 1.05, xaxs = "i", yaxs = "i", type = "o", uncertainty = TRUE, 
                                  shadealpha = 0.1, legend = TRUE, legendlabels = NULL, legendloc = "topright", 
                                  legendorder = NULL, legendncol = 1, sprtarg = NULL, btarg = NULL, 
                                  minbthresh = NULL, minblim=0.3, Ftarg = Ftarg,Fthresh=Fthresh,Flim=Flim, pwidth = 6.5, pheight = 5, punits = "in", 
                                  res = 300, ptsize = 10, plotdir = NULL, filenameprefix = "", 
                                  densitynames = c("SSB_Virgin", "R0"), densityxlabs = NULL, 
                                  rescale = TRUE, densityscalex = 1, densityscaley = 1, densityadjust = 1, 
                                  densitysymbols = TRUE, densitytails = TRUE, densitymiddle = FALSE, 
                                  densitylwd = 1, fix0 = TRUE, new = TRUE, add = FALSE, par = list(mar = c(5, 
                                                                                                           4, 1, 1) + 0.1), verbose = TRUE, mcmcVec = FALSE, show_equilibrium = TRUE) 
{
  meanRecWarning <- TRUE
  ymax_vec <- rep(NA, 17)
  save_png_comparisons <- function(file) {
    file <- paste0(filenameprefix, file)
    png(filename = file.path(plotdir, file), width = pwidth, 
        height = pheight, units = punits, res = res, pointsize = ptsize)
    par(par)
  }
  if (png) {
    print <- TRUE
  }
  if (png & is.null(plotdir)) {
    stop("To print PNG files, you must supply a directory as 'plotdir'")
  }
  if (pdf & png) {
    stop("To use 'pdf', set 'print' or 'png' to FALSE.")
  }
  if (pdf) {
    if (is.null(plotdir)) {
      stop("To write to a PDF, you must supply a directory as 'plotdir'")
    }
    pdffile <- file.path(plotdir, paste0(filenameprefix, 
                                         "SSplotComparisons_", format(Sys.time(), "%d-%b-%Y_%H.%M"), 
                                         ".pdf"))
    pdf(file = pdffile, width = pwidth, height = pheight)
    if (verbose) {
      message("PDF file with plots will be:", pdffile)
    }
    par(par)
  }
  n <- summaryoutput[["n"]]
  nsexes <- summaryoutput[["nsexes"]]
  startyrs <- summaryoutput[["startyrs"]]
  endyrs <- summaryoutput[["endyrs"]]
  pars <- summaryoutput[["pars"]]
  parsSD <- summaryoutput[["parsSD"]]
  parphases <- summaryoutput[["parphases"]]
  quants <- summaryoutput[["quants"]]
  quantsSD <- summaryoutput[["quantsSD"]]
  SpawnBio <- summaryoutput[["SpawnBio"]]
  SpawnBioLower <- summaryoutput[["SpawnBioLower"]]
  SpawnBioUpper <- summaryoutput[["SpawnBioUpper"]]
  Bratio <- summaryoutput[["Bratio"]]
  BratioLower <- summaryoutput[["BratioLower"]]
  BratioUpper <- summaryoutput[["BratioUpper"]]
  SmryBio <- summaryoutput[["SmryBio"]]
  SmryBioLower <- summaryoutput[["SmryBioLower"]]
  SmryBioUpper <- summaryoutput[["SmryBioUpper"]]
  if (!is.na(tail(SmryBio[["Label"]], 1)) && tail(SmryBio[["Label"]], 
                                                  1) == "SmryBio_Unfished") {
    SmryBio <- dplyr::filter(SmryBio, Label != "SmryBio_Unfished")
    SmryBioLower <- dplyr::filter(SmryBioLower, Label != 
                                    "SmryBio_Unfished")
    SmryBioUpper <- dplyr::filter(SmryBioUpper, Label != 
                                    "SmryBio_Unfished")
  }
  SPRratio <- summaryoutput[["SPRratio"]]
  SPRratioLower <- summaryoutput[["SPRratioLower"]]
  SPRratioUpper <- summaryoutput[["SPRratioUpper"]]
  Fvalue <- summaryoutput[["Fvalue"]]
  FvalueLower <- summaryoutput[["FvalueLower"]]
  FvalueUpper <- summaryoutput[["FvalueUpper"]]
  recruits <- summaryoutput[["recruits"]]
  recruitsLower <- summaryoutput[["recruitsLower"]]
  recruitsUpper <- summaryoutput[["recruitsUpper"]]
  recdevs <- summaryoutput[["recdevs"]]
  recdevsLower <- summaryoutput[["recdevsLower"]]
  recdevsUpper <- summaryoutput[["recdevsUpper"]]
  indices <- summaryoutput[["indices"]]
  mcmc <- summaryoutput[["mcmc"]]
  lowerCI <- summaryoutput[["lowerCI"]]
  upperCI <- summaryoutput[["upperCI"]]
  SpawnOutputUnits <- summaryoutput[["SpawnOutputUnits"]]
  SpawnOutputLabels <- summaryoutput[["SpawnOutputLabels"]]
  btargs <- summaryoutput[["btargs"]]
  minbthreshs <- summaryoutput[["minbthreshs"]]
  sprtargs <- summaryoutput[["sprtargs"]]
  SPRratioLabels <- summaryoutput[["SPRratioLabels"]]
  FvalueLabels <- summaryoutput[["FvalueLabels"]]
  if (is.null(btarg)) {
    btarg <- unique(btargs)
    if (length(btarg) > 1) {
      warning("setting btarg = -999 because models don't have matching values")
      btarg <- -999
    }
  }
  if (is.null(minbthresh)) {
    minbthresh <- unique(minbthreshs)
    if (length(minbthresh) > 1) {
      warning("setting minbthresh = -999 because models don't have matching values")
      minbthresh <- -999
    }
  }
  if (is.null(sprtarg)) {
    sprtarg <- unique(sprtargs)
    if (length(sprtarg) > 1) {
      warning("setting sprtarg = -999 because models don't have matching values")
      sprtarg <- -999
    }
  }
  SPRratioLabel <- unique(SPRratioLabels)
  if (length(SPRratioLabel) > 1) {
    warning("setting label for SPR plot to 8th element of input 'labels' ", 
            "because the models don't have matching labels")
    SPRratioLabel <- labels[8]
  }
  SpawnOutputLabel <- unique(SpawnOutputLabels)
  if (length(SpawnOutputLabel) > 1) {
    warning("setting label for Spawning Output to 12th element of input 'labels' ", 
            "because the models don't have matching SpawnOutputLabels")
    SpawnOutputLabel <- labels[12]
  }
  FvalueLabel <- unique(FvalueLabels)
  if (length(FvalueLabel) > 1) {
    warning("setting label for F plot to 13th element of input 'labels' ", 
            "because the models don't have matching labels")
    FvalueLabel <- labels[13]
  }
  else {
    FvalueLabel <- gsub("_", " ", FvalueLabel)
  }
  if (!is.logical(uncertainty) & is.numeric(uncertainty)) {
    if (any(!uncertainty %in% 1:n)) {
      stop("'uncertainty' should be a subset of the integers\n", 
           " 1-", n, ", where n=", n, " is the number of models.\n", 
           "  Or it can be a single TRUE/FALSE value.\n", 
           "  Or a vector of TRUE/FALSE, of length n=", 
           n)
    }
    else {
      uncertainty <- 1:n %in% uncertainty
    }
  }
  if (is.logical(uncertainty) & length(uncertainty) == 1) {
    uncertainty <- rep(uncertainty, n)
  }
  if (length(uncertainty) != n) {
    stop("'uncertainty' as TRUE/FALSE should have length 1 or n.\n", 
         "  length(uncertainty) = ", length(uncertainty))
  }
  if (all(uncertainty)) {
    message("showing uncertainty for all models")
  }
  if (!any(uncertainty)) {
    message("not showing uncertainty for any models")
  }
  if (any(uncertainty) & !all(uncertainty)) {
    message("showing uncertainty for model", ifelse(sum(uncertainty) > 
                                                      1, "s: ", " "), paste(which(uncertainty), collapse = ","))
  }
  for (i in 1:n) {
    if (all(is.na(quantsSD[, i]) | quantsSD[, i] == 0)) {
      message("No uncertainty available for model ", i)
      uncertainty[i] <- FALSE
    }
  }
  if (length(unique(nsexes)) > 1) {
    warning("SSplotComparisons no longer divides SpawnBio by 2 for single-sex models\n", 
            "to get female-only spawning biomass output by SS for a single-sex model,\n", 
            "use the new Nsexes = -1 option in the data file.")
  }
  if (models[1] == "all") {
    models <- 1:n
  }
  nlines <- length(models)
  if (any(mcmcVec) & length(mcmc) == 0) {
    mcmcVec <- FALSE
    warning("Setting mcmcVec = FALSE because summaryoutput[['mcmc']] is empty")
  }
  if (nlines > 1 & length(mcmcVec) == 1) {
    mcmcVec <- rep(mcmcVec, nlines)
  }
  if (nlines != length(mcmcVec)) {
    stop("Input 'mcmcVec' must equal 1 or the number of models.\n")
  }
  if (any(subplots %in% 13:14) & !is.null(indices) && nrow(indices) > 
      0) {
    if (is.null(indexfleets)) {
      indexfleets <- list()
      for (imodel in 1:n) {
        indexfleets[[paste0("model", imodel)]] <- sort(unique(indices[["Fleet"]][indices[["imodel"]] == 
                                                                                   imodel]))
      }
    }
    else {
      if (!is.null(indexfleets)) {
        if (is.vector(indexfleets) & length(indexfleets) == 
            1) {
          indexfleets <- rep(indexfleets, n)
        }
        if (length(indexfleets) != n) {
          warning("Skipping index plots: length(indexfleets) should be 1 or n = ", 
                  n, ".")
          indexfleets <- NULL
        }
      }
    }
    if (!length(unique(lapply(indexfleets, FUN = length))) == 
        1) {
      warning("Skipping index plots;\n", "Fleets have different numbers of indices listed in 'indexfleets'.")
      indexfleets <- NULL
    }
    index_plot_suffix <- rep("", length(indexfleets))
    if (length(indexfleets[[1]]) > 1) {
      for (iindex in seq_along(indexfleets[[1]])) {
        fleets <- as.numeric(data.frame(indexfleets)[iindex, 
        ])
        if (length(unique(fleets)) == 1) {
          index_plot_suffix[iindex] <- paste0("_flt", 
                                              fleets[1])
        }
        else {
          index_plot_suffix[iindex] <- paste0("_index", 
                                              iindex)
        }
      }
    }
  }
  if (is.null(col) & nlines > 3) {
    col <- rich.colors.short(nlines + 1)[-1]
  }
  if (is.null(col) & nlines < 3) {
    col <- rich.colors.short(nlines)
  }
  if (is.null(col) & nlines == 3) {
    col <- c("blue", "red", "green3")
  }
  if (is.null(shadecol)) {
    shadecol <- adjustcolor(col, alpha.f = shadealpha)
  }
  if (is.null(pch)) {
    pch <- rep(1:25, 10)[1:nlines]
  }
  if (length(col) < nlines) {
    col <- rep(col, nlines)[1:nlines]
  }
  if (length(pch) < nlines) {
    pch <- rep(pch, nlines)[1:nlines]
  }
  if (length(lty) < nlines) {
    lty <- rep(lty, nlines)[1:nlines]
  }
  if (length(lwd) < nlines) {
    lwd <- rep(lwd, nlines)[1:nlines]
  }
  if (!is.expression(legendlabels[1]) && is.null(legendlabels)) {
    legendlabels <- paste("model", 1:nlines)
  }
  if (plot & new & !pdf) {
    dev.new(width = pwidth, height = pheight, pointsize = ptsize, 
            record = TRUE)
    par(par)
  }
  for (iline in (1:nlines)[mcmcVec]) {
    imodel <- models[iline]
    cols <- imodel
    SpawnBioLower[, cols] <- SpawnBioUpper[, cols] <- SpawnBio[, 
                                                               cols] <- NA
    BratioLower[, cols] <- BratioUpper[, cols] <- Bratio[, 
                                                         cols] <- NA
    SPRratioLower[, cols] <- SPRratioUpper[, cols] <- SPRratio[, 
                                                               cols] <- NA
    recruitsLower[, cols] <- recruitsUpper[, cols] <- recruits[, 
                                                               cols] <- NA
    recdevsLower[, cols] <- recdevsUpper[, cols] <- recdevs[, 
                                                            cols] <- NA
    tmp <- grep("SSB", names(mcmc[[imodel]]))
    tmp2 <- c(grep("SSB_unfished", names(mcmc[[imodel]]), 
                   ignore.case = TRUE), grep("SSB_Btgt", names(mcmc[[imodel]]), 
                                             ignore.case = TRUE), grep("SSB_SPRtgt", names(mcmc[[imodel]]), 
                                                                       ignore.case = TRUE), grep("SSB_MSY", names(mcmc[[imodel]]), 
                                                                                                 ignore.case = TRUE))
    tmp <- setdiff(tmp, tmp2)
    if (length(tmp) > 0) {
      mcmc.tmp <- mcmc[[imodel]][, tmp]
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp, 2, quantile, prob = lowerCI, 
                     na.rm = TRUE)
      med <- apply(mcmc.tmp, 2, quantile, prob = 0.5, 
                   na.rm = TRUE)
      upper <- apply(mcmc.tmp, 2, quantile, prob = upperCI, 
                     na.rm = TRUE)
      SpawnBio[, imodel] <- med[match(SpawnBio[["Label"]], 
                                      mcmclabs)]
      SpawnBioLower[, imodel] <- lower[match(SpawnBioLower[["Label"]], 
                                             mcmclabs)]
      SpawnBioUpper[, imodel] <- upper[match(SpawnBioUpper[["Label"]], 
                                             mcmclabs)]
    }
    tmp <- grep("Bratio", names(mcmc[[imodel]]))
    if (length(tmp) > 0) {
      mcmc.tmp <- mcmc[[imodel]][, tmp]
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp, 2, quantile, prob = lowerCI, 
                     na.rm = TRUE)
      med <- apply(mcmc.tmp, 2, quantile, prob = 0.5, 
                   na.rm = TRUE)
      upper <- apply(mcmc.tmp, 2, quantile, prob = upperCI, 
                     na.rm = TRUE)
      Bratio[, imodel] <- med[match(Bratio[["Label"]], 
                                    mcmclabs)]
      BratioLower[, imodel] <- lower[match(BratioLower[["Label"]], 
                                           mcmclabs)]
      BratioUpper[, imodel] <- upper[match(BratioUpper[["Label"]], 
                                           mcmclabs)]
    }
    tmp <- grep("SmryBio", names(mcmc[[imodel]]))
    tmp2 <- grep("SmryBio_unfished", names(mcmc[[imodel]]), 
                 ignore.case = TRUE)
    tmp <- setdiff(tmp, tmp2)
    if (length(tmp) > 0) {
      mcmc.tmp <- mcmc[[imodel]][, tmp]
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp, 2, quantile, prob = lowerCI, 
                     na.rm = TRUE)
      med <- apply(mcmc.tmp, 2, quantile, prob = 0.5, 
                   na.rm = TRUE)
      upper <- apply(mcmc.tmp, 2, quantile, prob = upperCI, 
                     na.rm = TRUE)
      SmryBio[, imodel] <- med[match(SmryBio[["Label"]], 
                                     mcmclabs)]
      SmryBioLower[, imodel] <- lower[match(SmryBioLower[["Label"]], 
                                            mcmclabs)]
      SmryBioUpper[, imodel] <- upper[match(SmryBioUpper[["Label"]], 
                                            mcmclabs)]
    }
    tmp <- grep("SPRratio", names(mcmc[[imodel]]))
    if (length(tmp) > 0) {
      mcmc.tmp <- mcmc[[imodel]][, tmp]
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp, 2, quantile, prob = lowerCI, 
                     na.rm = TRUE)
      med <- apply(mcmc.tmp, 2, quantile, prob = 0.5, 
                   na.rm = TRUE)
      upper <- apply(mcmc.tmp, 2, quantile, prob = upperCI, 
                     na.rm = TRUE)
      SPRratio[, imodel] <- med[match(SPRratio[["Label"]], 
                                      mcmclabs)]
      SPRratioLower[, imodel] <- lower[match(SPRratioLower[["Label"]], 
                                             mcmclabs)]
      SPRratioUpper[, imodel] <- upper[match(SPRratioUpper[["Label"]], 
                                             mcmclabs)]
    }
    tmp <- grep("^Recr_", names(mcmc[[imodel]]))
    tmp2 <- grep("Recr_unfished", names(mcmc[[imodel]]), 
                 ignore.case = TRUE)
    tmp <- setdiff(tmp, tmp2)
    if (length(tmp) > 0) {
      mcmc.tmp <- mcmc[[imodel]][, tmp]
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp, 2, quantile, prob = lowerCI, 
                     na.rm = TRUE)
      med <- apply(mcmc.tmp, 2, quantile, prob = 0.5, 
                   na.rm = TRUE)
      mean <- apply(mcmc.tmp, 2, mean, na.rm = TRUE)
      upper <- apply(mcmc.tmp, 2, quantile, prob = upperCI, 
                     na.rm = TRUE)
      if (!meanRecWarning) {
        message("note: using mean recruitment from MCMC instead of median,\n", 
                "because it is more comparable to MLE\n")
        meanRecWarning <- TRUE
      }
      recruits[, imodel] <- mean[match(recruits[["Label"]], 
                                       mcmclabs)]
      recruitsLower[, imodel] <- lower[match(recruitsLower[["Label"]], 
                                             mcmclabs)]
      recruitsUpper[, imodel] <- upper[match(recruitsUpper[["Label"]], 
                                             mcmclabs)]
    }
    tmp <- unique(c(grep("_RecrDev_", names(mcmc[[imodel]])), 
                    grep("_InitAge_", names(mcmc[[imodel]])), grep("ForeRecr_", 
                                                                   names(mcmc[[imodel]]))))
    if (length(tmp) > 0) {
      mcmc.tmp <- mcmc[[imodel]][, tmp]
      mcmclabs <- names(mcmc.tmp)
      lower <- apply(mcmc.tmp, 2, quantile, prob = lowerCI, 
                     na.rm = TRUE)
      med <- apply(mcmc.tmp, 2, quantile, prob = 0.5, 
                   na.rm = TRUE)
      upper <- apply(mcmc.tmp, 2, quantile, prob = upperCI, 
                     na.rm = TRUE)
      recdevs[, imodel] <- med[match(recdevs[["Label"]], 
                                     mcmclabs)]
      recdevsLower[, imodel] <- lower[match(recdevsLower[["Label"]], 
                                            mcmclabs)]
      recdevsUpper[, imodel] <- upper[match(recdevsUpper[["Label"]], 
                                            mcmclabs)]
    }
  }
  if (is.null(endyrvec)) {
    endyrvec <- endyrs[models] + 1
  }
  if (length(endyrvec) == 1) {
    endyrvec <- rep(endyrvec, nlines)
  }
  recdevs <- recdevs[!is.na(recdevs[["Yr"]]), ]
  recdevsLower <- recdevsLower[!is.na(recdevsLower[["Yr"]]), 
  ]
  recdevsUpper <- recdevsUpper[!is.na(recdevsUpper[["Yr"]]), 
  ]
  if (!is.null(endyrvec)) {
    for (iline in 1:nlines) {
      endyr <- endyrvec[iline]
      imodel <- models[iline]
      SpawnBio[SpawnBio[["Yr"]] > endyr, imodel] <- NA
      SpawnBioLower[SpawnBio[["Yr"]] > endyr, imodel] <- NA
      SpawnBioUpper[SpawnBio[["Yr"]] > endyr, imodel] <- NA
      Bratio[Bratio[["Yr"]] > endyr, imodel] <- NA
      BratioLower[Bratio[["Yr"]] > endyr, imodel] <- NA
      BratioUpper[Bratio[["Yr"]] > endyr, imodel] <- NA
      SPRratio[SPRratio[["Yr"]] >= endyr, imodel] <- NA
      SPRratioLower[SPRratio[["Yr"]] >= endyr, imodel] <- NA
      SPRratioUpper[SPRratio[["Yr"]] >= endyr, imodel] <- NA
      Fvalue[Fvalue[["Yr"]] >= endyr, imodel] <- NA
      FvalueLower[Fvalue[["Yr"]] >= endyr, imodel] <- NA
      FvalueUpper[Fvalue[["Yr"]] >= endyr, imodel] <- NA
      recruits[recruits[["Yr"]] > endyr, imodel] <- NA
      recruitsLower[recruits[["Yr"]] > endyr, imodel] <- NA
      recruitsUpper[recruits[["Yr"]] > endyr, imodel] <- NA
      if (!is.null(recdevs)) {
        recdevs[recdevs[["Yr"]] > endyr, imodel] <- NA
        recdevsLower[recdevs[["Yr"]] > endyr, imodel] <- NA
        recdevsUpper[recdevs[["Yr"]] > endyr, imodel] <- NA
      }
    }
  }
  addpoly <- function(yrvec, lower, upper) {
    lower[lower < 0] <- 0
    for (iline in (1:nlines)[uncertainty]) {
      imodel <- models[iline]
      good <- !is.na(lower[, imodel]) & !is.na(upper[, 
                                                     imodel])
      polygon(x = c(yrvec[good], rev(yrvec[good])), y = c(lower[good, 
                                                                imodel], rev(upper[good, imodel])), border = NA, 
              col = shadecol[iline])
    }
  }
  plotBio <- function(option, show_uncertainty = TRUE) {
    if (option == 1) {
      Bio <- SpawnBio
      BioUpper <- SpawnBioUpper
      BioLower <- SpawnBioLower
    }
    if (option != 1) {
      Bio <- SmryBio
      BioUpper <- SmryBioUpper
      BioLower <- SmryBioLower
    }
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    if (is.null(xlim)) {
      if (show_equilibrium) {
        xlim <- range(Bio[["Yr"]], na.rm = TRUE) + c(-0.5, 
                                                     0.5)
      }
      else {
        xlim <- range(Bio[["Yr"]][-c(1, 2)], na.rm = TRUE) + 
          c(-0.2, 0.2)
      }
      if (!is.null(endyrvec) & all(endyrvec < max(xlim))) {
        xlim[2] <- max(endyrvec)
      }
    }
    ylim <- ylimAdj * range(0, Bio[Bio[["Yr"]] >= xlim[1] & 
                                     Bio[["Yr"]] <= xlim[2], models], na.rm = TRUE)
    if (show_uncertainty) {
      ylim <- range(ylim, ylimAdj * BioUpper[Bio[["Yr"]] >= 
                                               xlim[1] & Bio[["Yr"]] <= xlim[2], models[uncertainty]], 
                    na.rm = TRUE)
    }
    if (length(unique(SpawnOutputUnits)) != 1) {
      warning("Some models may have different units", 
              " for spawning output than others")
    }
    if (option == 1) {
      if (all(is.na(SpawnOutputUnits)) || any(SpawnOutputUnits == 
                                              "numbers", na.rm = TRUE)) {
        ylab <- SpawnOutputLabel
      }
      else {
        ylab <- labels[2]
      }
    }
    else {
      ylab <- labels[14]
      summary_age <- unique(summaryoutput[["summary_ages"]])
      if (length(summary_age) == 1) {
        ylab <- gsub("X", summary_age, labels[15])
      }
    }
    yunits <- 1
    if (rescale & ylim[2] > 1000 & ylim[2] < 1e+06) {
      yunits <- 1000
      ylab <- gsub("(t)", "(x1000 t)", ylab, fixed = TRUE)
      ylab <- gsub("eggs", "x1000 eggs", ylab, fixed = TRUE)
    }
    if (rescale & ylim[2] > 1e+06) {
      yunits <- 1e+06
      ylab <- gsub("(t)", "(million t)", ylab, fixed = TRUE)
      ylab <- gsub("eggs", "millions of eggs", ylab, fixed = TRUE)
    }
    if (rescale & ylim[2] > 1e+09) {
      yunits <- 1e+09
      ylab <- gsub("million", "billion", ylab, fixed = TRUE)
    }
    if (!add) {
      plot(0, type = "n", xlim = xlim, ylim = ylim, xlab = labels[1], 
           ylab = ylab, xaxs = xaxs, yaxs = yaxs, axes = FALSE)
    }
    if (show_uncertainty) {
      addpoly(yrvec = Bio[["Yr"]][-(1:2)], lower = BioLower[-(1:2), 
      ], upper = BioUpper[-(1:2), ])
      xEqu <- Bio[["Yr"]][2] - (1:nlines)/nlines
    }
    else {
      xEqu <- rep(Bio[["Yr"]][2], nlines)
    }
    if (spacepoints %in% c(0, 1, FALSE)) {
      matplot(Bio[["Yr"]][-(1:2)], Bio[-(1:2), models], 
              col = col, pch = pch, lty = lty, lwd = lwd, 
              type = type, ylim = ylim, add = TRUE)
    }
    else {
      matplot(Bio[["Yr"]][-(1:2)], Bio[-(1:2), models], 
              col = col, lty = lty, lwd = lwd, type = "l", 
              ylim = ylim, add = TRUE)
      Bio2 <- Bio
      for (iline in 1:nlines) {
        imodel <- models[iline]
        Bio2[(Bio2[["Yr"]] - initpoint)%%spacepoints != 
               (staggerpoints * iline)%%spacepoints, imodel] <- NA
      }
      matplot(Bio2[["Yr"]][-(1:2)], Bio2[-(1:2), models], 
              col = col, pch = pch, lwd = lwd, type = "p", 
              ylim = ylim, add = TRUE)
    }
    if (show_equilibrium) {
      old_warn <- options()[["warn"]]
      options(warn = -1)
      if (show_uncertainty) {
        arrows(x0 = xEqu[models[uncertainty]], y0 = as.numeric(BioLower[1, 
                                                                        models[uncertainty]]), x1 = xEqu[models[uncertainty]], 
               y1 = as.numeric(BioUpper[1, models[uncertainty]]), 
               length = 0.01, angle = 90, code = 3, col = col[uncertainty], 
               lwd = 2)
      }
      options(warn = old_warn)
      points(x = xEqu, Bio[1, models], col = col, pch = pch, 
             cex = 1.2, lwd = lwd)
    }
    if (!add) {
      abline(h = 0, col = "grey")
      if (tickEndYr) {
        ticks <- graphics::axTicks(1)
        axis(1, at = c(ticks[ticks < max(endyrvec)], 
                       max(endyrvec)))
      }
      else {
        axis(1)
      }
      if (!is.null(endyrvec) & max(endyrvec) > 1 + max(endyrs) & 
          shadeForecast) {
        rect(xleft = max(endyrs) + 1, ybottom = par()[["usr"]][3], 
             xright = par()[["usr"]][2], ytop = par()[["usr"]][4], 
             col = gray(0, alpha = 0.1), border = NA)
      }
      yticks <- pretty(ylim)
      axis(2, at = yticks, labels = format(yticks/yunits), 
           las = 1)
      box()
    }
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    return(ylim[2])
  }
  plotBratio <- function(show_uncertainty = TRUE) {
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    if (is.null(xlim)) {
      xlim <- range(Bratio[["Yr"]])
      if (!is.null(endyrvec) & all(endyrvec < max(xlim))) {
        xlim[2] <- max(endyrvec)
      }
    }
    ylim <- ylimAdj * range(0, Bratio[Bratio[["Yr"]] >= 
                                        xlim[1] & Bratio[["Yr"]] <= xlim[2], models], na.rm = TRUE)
    if (show_uncertainty) {
      ylim <- ylimAdj * range(ylim/ylimAdj, BratioUpper[Bratio[["Yr"]] >= 
                                                          xlim[1] & Bratio[["Yr"]] <= xlim[2], models[uncertainty]], 
                              na.rm = TRUE)
    }
    if (!add) {
      plot(0, type = "n", xlim = xlim, ylim = ylim, xlab = labels[1], 
           ylab = labels[3], xaxs = xaxs, yaxs = yaxs, 
           axes = FALSE)
    }
    if (show_uncertainty) {
      addpoly(Bratio[["Yr"]], lower = BratioLower, upper = BratioUpper)
    }
    if (spacepoints %in% c(0, 1, FALSE)) {
      matplot(Bratio[["Yr"]], Bratio[, models], col = col, 
              pch = pch, lty = lty, lwd = lwd, type = type, 
              ylim = ylim, add = TRUE)
    }
    else {
      matplot(Bratio[["Yr"]], Bratio[, models], col = col, 
              pch = pch, lty = lty, lwd = lwd, type = "l", 
              ylim = ylim, add = TRUE)
      if (type != "l") {
        Bratio2 <- Bratio
        for (iline in 1:nlines) {
          imodel <- models[iline]
          Bratio2[(Bratio2[["Yr"]] - initpoint)%%spacepoints != 
                    (staggerpoints * iline)%%spacepoints, imodel] <- NA
        }
        matplot(Bratio2[["Yr"]], Bratio2[, models], 
                col = col, pch = pch, lty = lty, lwd = lwd, 
                type = "p", ylim = ylim, add = TRUE)
      }
    }
    yticks <- pretty(par()[["yaxp"]][1:2])
    if (btarg > 0) {
      abline(h = btarg, col = "red", lty = 2)
      text(min(Bratio[["Yr"]]) + 4, btarg + 0.03, labels[10], 
           adj = 0)
      yticks <- sort(c(btarg, yticks))
    }
    if (minbthresh > 0) {
      abline(h = minbthresh, col = "red", lty = 2)
      text(min(Bratio[["Yr"]]) + 4, minbthresh + 0.03, 
           labels[11], adj = 0)
      yticks <- sort(c(minbthresh, yticks))
    } 
    if (minblim > 0) {
      abline(h = minblim, col = "red", lty = 2)
      text(min(Bratio[["Yr"]]) + 4, minblim + 0.03, 
           labels[16], adj = 0)
      yticks <- sort(c(minblim, yticks))
    }
    if (!add) {
      abline(h = 0, col = "grey")
      abline(h = 1, col = "grey", lty = 2)
      if (tickEndYr) {
        ticks <- graphics::axTicks(1)
        axis(1, at = c(ticks[ticks < max(endyrvec)], 
                       max(endyrvec)))
      }
      else {
        axis(1)
      }
      if (!is.null(endyrvec) & max(endyrvec) > 1 + max(endyrs) & 
          shadeForecast) {
        rect(xleft = max(endyrs) + 1, ybottom = par()[["usr"]][3], 
             xright = par()[["usr"]][2], ytop = par()[["usr"]][4], 
             col = gray(0, alpha = 0.1), border = NA)
      }
      axis(2, at = yticks, las = 1)
      box()
    }
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    return(ylim[2])
  }
  plotSPRratio <- function(show_uncertainty = TRUE) {
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    if (is.null(xlim)) {
      xlim <- range(SPRratio[["Yr"]])
      if (!is.null(endyrvec) & all(endyrvec < max(xlim))) {
        xlim[2] <- max(endyrvec)
      }
    }
    ylim <- ylimAdj * range(0, SPRratio[SPRratio[["Yr"]] >= 
                                          xlim[1] & SPRratio[["Yr"]] <= xlim[2], models], 
                            na.rm = TRUE)
    if (show_uncertainty) {
      ylim <- ylimAdj * range(ylim/ylimAdj, SPRratioUpper[SPRratio[["Yr"]] >= 
                                                            xlim[1] & SPRratio[["Yr"]] <= xlim[2], models[uncertainty]], 
                              na.rm = TRUE)
    }
    par(par)
    if (!add) {
      if (isTRUE(!is.na(SPRratioLabel) && SPRratioLabel == 
                 paste0("(1-SPR)/(1-SPR_", floor(100 * sprtarg), 
                        "%)"))) {
        newmar <- oldmar <- par()[["mar"]]
        newmar[4] <- newmar[2]
        par(mar = newmar)
      }
      plot(0, type = "n", xlim = xlim, ylim = ylim, xlab = labels[1], 
           ylab = "", xaxs = xaxs, yaxs = yaxs, las = 1, 
           axes = FALSE)
      axis(2)
    }
    if (show_uncertainty) {
      addpoly(SPRratio[["Yr"]], lower = SPRratioLower, 
              upper = SPRratioUpper)
    }
    if (spacepoints %in% c(0, 1, FALSE)) {
      matplot(SPRratio[["Yr"]], SPRratio[, models], col = col, 
              pch = pch, lty = lty, lwd = lwd, type = type, 
              ylim = ylim, add = TRUE)
    }
    else {
      matplot(SPRratio[["Yr"]], SPRratio[, models], col = col, 
              pch = pch, lty = lty, lwd = lwd, type = "l", 
              ylim = ylim, add = TRUE)
      if (type != "l") {
        SPRratio2 <- SPRratio
        for (iline in 1:nlines) {
          imodel <- models[iline]
          SPRratio2[(SPRratio2[["Yr"]] - initpoint)%%spacepoints != 
                      (staggerpoints * iline)%%spacepoints, imodel] <- NA
        }
        matplot(SPRratio2[["Yr"]], SPRratio2[, models], 
                col = col, pch = pch, lty = lty, lwd = lwd, 
                type = "p", ylim = ylim, add = TRUE)
      }
    }
    abline(h = 0, col = "grey")
    if (sprtarg > 0) {
      if (isTRUE(SPRratioLabel == "1-SPR")) {
        abline(h = sprtarg, col = "red", lty = 2)
        text(SPRratio[["Yr"]][1] + 4, (sprtarg + 0.03), 
             labels[10], adj = 0)
        mtext(side = 2, text = SPRratioLabel, line = par()[["mgp"]][1], 
              col = par()[["col.lab"]], cex = par()[["cex.lab"]])
      }
      else {
        yticks <- pretty(ylim)
        if (isTRUE(!is.na(SPRratioLabel) && SPRratioLabel == 
                   paste0("(1-SPR)/(1-SPR_", floor(100 * sprtarg), 
                          "%)"))) {
          abline(h = 1, col = "red", lty = 2)
          text(SPRratio[["Yr"]][1] + 4, 1 + 0.03, labels[10], 
               adj = 0)
          axis(4, at = yticks, labels = yticks * (1 - 
                                                    sprtarg), las = 1)
          mtext(side = 4, text = "1 - SPR", line = par()[["mgp"]][1], 
                col = par()[["col.lab"]], cex = par()[["cex.lab"]])
          mtext(side = 2, text = paste("(1-SPR)/(1-SPR_", 
                                       100 * sprtarg, "%)", sep = ""), line = par()[["mgp"]][1], 
                col = par()[["col.lab"]], cex = par()[["cex.lab"]])
        }
        else {
          message("No line added to SPR ratio plot, ", 
                  "as the settings used in this model ", "have not yet been configured in SSplotComparisons.")
          mtext(side = 2, text = SPRratioLabel, line = par()[["mgp"]][1], 
                col = par()[["col.lab"]], cex = par()[["cex.lab"]])
        }
      }
    }
    else {
      mtext(side = 2, text = SPRratioLabel, line = par()[["mgp"]][1], 
            col = par()[["col.lab"]], cex = par()[["cex.lab"]])
    }
    if (!add) {
      if (tickEndYr) {
        ticks <- graphics::axTicks(1)
        axis(1, at = c(ticks[ticks < max(endyrvec)], 
                       max(endyrvec)))
      }
      else {
        axis(1)
      }
      if (!is.null(endyrvec) & max(endyrvec) > 1 + max(endyrs) & 
          shadeForecast) {
        rect(xleft = max(endyrs) + 1, ybottom = par()[["usr"]][3], 
             xright = par()[["usr"]][2], ytop = par()[["usr"]][4], 
             col = gray(0, alpha = 0.1), border = NA)
      }
    }
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    box()
    if (exists("oldmar")) {
      par(mar = oldmar)
    }
    return(ylim[2])
  }
  plotF <- function(show_uncertainty = TRUE) {
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    if (is.null(xlim)) {
      xlim <- range(Fvalue[["Yr"]])
      if (!is.null(endyrvec) & all(endyrvec < max(xlim))) {
        xlim[2] <- max(endyrvec)
      }
    }
    ylim <- ylimAdj * range(0, Fvalue[Fvalue[["Yr"]] >= 
                                        xlim[1] & Fvalue[["Yr"]] <= xlim[2], models], na.rm = TRUE)
    if (show_uncertainty) {
      ylim <- ylimAdj * range(ylim/ylimAdj, FvalueUpper[Fvalue[["Yr"]] >= 
                                                          xlim[1] & Fvalue[["Yr"]] <= xlim[2], models[uncertainty]], 
                              na.rm = TRUE)
    }
    par(par)
    if (!add) {
      plot(0, type = "n", xlim = xlim, ylim = ylim, xlab = labels[1], 
           ylab = "", xaxs = xaxs, yaxs = yaxs, las = 1, 
           axes = FALSE)
      if (tickEndYr) {
        ticks <- graphics::axTicks(1)
        axis(1, at = c(ticks[ticks < max(endyrvec)], 
                       max(endyrvec)))
      }
      else {
        axis(1)
      }
      axis(2)
    }
    if (show_uncertainty) {
      addpoly(Fvalue[["Yr"]], lower = FvalueLower, upper = FvalueUpper)
    }
    if (spacepoints %in% c(0, 1, FALSE)) {
      matplot(Fvalue[["Yr"]], Fvalue[, models], col = col, 
              pch = pch, lty = lty, lwd = lwd, type = type, 
              ylim = ylim, add = TRUE)
    }
    else {
      matplot(Fvalue[["Yr"]], Fvalue[, models], col = col, 
              pch = pch, lty = lty, lwd = lwd, type = "l", 
              ylim = ylim, add = TRUE)
      if (type != "l") {
        Fvalue2 <- Fvalue
        for (iline in 1:nlines) {
          imodel <- models[iline]
          Fvalue2[Fvalue2[["Yr"]]%%spacepoints != (staggerpoints * 
                                                     iline)%%spacepoints, imodel] <- NA
        }
        matplot(Fvalue2[["Yr"]], Fvalue2[, models], 
                col = col, pch = pch, lty = lty, lwd = lwd, 
                type = "p", ylim = ylim, add = TRUE)
      }
    }
    yticks <- pretty(par()[["yaxp"]][1:2])
    if (Ftarg > 0) {
      abline(h = Ftarg, col = "red", lty = 2)
      text(min(Fvalue[["Yr"]]) + 4, Ftarg + 0.015, labels[10],
           adj = 0)
      yticks <- sort(c(Ftarg, yticks))
    }
    if (Fthresh > 0) {
      abline(h = Fthresh, col = "red", lty = 2)
      text(min(Fvalue[["Yr"]]) + 4, Fthresh + 0.015,
           labels[16], adj = 0)
      yticks <- sort(c(Fthresh, yticks))
    }
    if (Flim > 0) {
      abline(h = Flim, col = "red", lty = 2)
      text(min(Fvalue[["Yr"]]) + 4, Flim + 0.015,
           labels[11], adj = 0)
      yticks <- sort(c(Flim, yticks))
    }
    abline(h = 0, col = "grey")
    mtext(side = 2, text = FvalueLabel, line = par()[["mgp"]][1], 
          col = par()[["col.lab"]], cex = par()[["cex.lab"]])
    box()
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    return(ylim[2])
  }
  plotRecruits <- function(show_uncertainty = TRUE, recruit_lines = TRUE) {
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    if (is.null(xlim)) {
      if (show_equilibrium) {
        xlim <- range(recruits[["Yr"]])
      }
      else {
        xlim <- range(recruits[["Yr"]][-c(1, 2)])
      }
      if (!is.null(endyrvec) & all(endyrvec < max(xlim))) {
        xlim[2] <- max(endyrvec)
      }
    }
    ylim <- ylimAdj * range(0, recruits[recruits[["Yr"]] >= 
                                          xlim[1] & recruits[["Yr"]] <= xlim[2], models], 
                            na.rm = TRUE)
    if (show_uncertainty) {
      ylim <- ylimAdj * range(ylim/ylimAdj, recruitsUpper[recruits[["Yr"]] >= 
                                                            xlim[1] & recruits[["Yr"]] <= xlim[2], models[uncertainty]], 
                              na.rm = TRUE)
    }
    ylab <- labels[4]
    yunits <- 1
    if (ylim[2] > 1000 & ylim[2] < 1e+06) {
      yunits <- 1000
      ylab <- gsub("1,000s", "millions", ylab)
    }
    if (ylim[2] > 1e+06) {
      yunits <- 1e+06
      ylab <- gsub("1,000s", "billions", ylab)
    }
    if (spacepoints %in% c(0, 1, FALSE)) {
      matplot(recruits[["Yr"]][-(1:2)], recruits[-(1:2), 
                                                 models], col = col, pch = pch, lty = lty, lwd = lwd, 
              type = type, xlim = xlim, ylim = ylim, xlab = labels[1], 
              ylab = ylab, xaxs = xaxs, yaxs = yaxs, axes = FALSE, 
              add = add)
    }
    else {
      matplot(recruits[["Yr"]][-(1:2)], recruits[-(1:2), 
                                                 models], col = col, pch = pch, lty = lty, lwd = lwd, 
              type = "l", xlim = xlim, ylim = ylim, xlab = labels[1], 
              ylab = ylab, xaxs = xaxs, yaxs = yaxs, axes = FALSE, 
              add = add)
      if (type != "l") {
        recruits2 <- recruits
        for (iline in 1:nlines) {
          imodel <- models[iline]
          recruits2[(recruits2[["Yr"]]%%spacepoints - 
                       initpoint) != (staggerpoints * iline)%%spacepoints, 
                    imodel] <- NA
        }
        matplot(recruits2[["Yr"]][-(1:2)], recruits2[-(1:2), 
                                                     models], col = col, pch = pch, lty = lty, 
                lwd = lwd, type = "p", xlab = labels[1], ylab = ylab, 
                xaxs = xaxs, yaxs = yaxs, axes = FALSE, ylim = ylim, 
                add = TRUE)
      }
    }
    if (show_uncertainty) {
      xEqu <- recruits[["Yr"]][2] - (1:nlines)/nlines
    }
    else {
      xEqu <- rep(recruits[["Yr"]][1], nlines)
    }
    if (show_equilibrium) {
      points(x = xEqu, y = recruits[1, models], col = col, 
             pch = pch, cex = 1.2, lwd = lwd)
    }
    if (show_uncertainty) {
      for (iline in 1:nlines) {
        imodel <- models[iline]
        if (uncertainty[imodel]) {
          xvec <- recruits[["Yr"]]
          if (nlines > 1) {
            xvec <- xvec + 0.4 * iline/nlines - 0.2
          }
          old_warn <- options()[["warn"]]
          options(warn = -1)
          arrows(x0 = xvec[-c(1, 2)], y0 = pmax(as.numeric(recruitsLower[-c(1, 
                                                                            2), imodel]), 0), x1 = xvec[-c(1, 2)], y1 = as.numeric(recruitsUpper[-c(1, 
                                                                                                                                                    2), imodel]), length = 0.01, angle = 90, 
                 code = 3, col = col[imodel])
          options(warn = old_warn)
          if (show_equilibrium) {
            arrows(x0 = xEqu[imodel], y0 = pmax(as.numeric(recruitsLower[1, 
                                                                         imodel]), 0), x1 = xEqu[imodel], y1 = as.numeric(recruitsUpper[1, 
                                                                                                                                        imodel]), length = 0.01, angle = 90, code = 3, 
                   col = col[imodel])
          }
        }
      }
    }
    abline(h = 0, col = "grey")
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    if (!add) {
      if (tickEndYr) {
        ticks <- graphics::axTicks(1)
        axis(1, at = c(ticks[ticks < max(endyrvec)], 
                       max(endyrvec)))
      }
      else {
        axis(1)
      }
      if (!is.null(endyrvec) & max(endyrvec) > 1 + max(endyrs) & 
          shadeForecast) {
        rect(xleft = max(endyrs) + 1, ybottom = par()[["usr"]][3], 
             xright = par()[["usr"]][2], ytop = par()[["usr"]][4], 
             col = gray(0, alpha = 0.1), border = NA)
      }
      yticks <- pretty(ylim)
      axis(2, at = yticks, labels = format(yticks/yunits), 
           las = 1)
      box()
    }
    return(ylim[2])
  }
  plotRecDevs <- function(show_uncertainty = TRUE) {
    if (any(is.na(recdevs[["Yr"]]))) {
      warning("Recdevs associated with initial age structure may not be shown")
    }
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    if (is.null(xlim)) {
      xlim <- range(recdevs[["Yr"]], na.rm = TRUE)
      if (!is.null(endyrvec) & all(endyrvec < max(xlim))) {
        xlim[2] <- max(endyrvec)
      }
    }
    ylim <- ylimAdj * range(recdevs[recdevs[["Yr"]] >= xlim[1] & 
                                      recdevs[["Yr"]] <= xlim[2], models], na.rm = TRUE)
    if (any(is.infinite(ylim))) {
      warning("Skipping recdev plots. Infinite ylim may indicate ", 
              "all values are NA in summaryoutput[[\"recdevs\"]]")
      return(ylim[2])
    }
    if (show_uncertainty) {
      if (all(is.na(recdevsLower[, models]))) {
        return(invisible(NA))
      }
      ylim <- ylimAdj * range(recdevsLower[recdevs[["Yr"]] >= 
                                             xlim[1] & recdevs[["Yr"]] <= xlim[2], models], 
                              recdevsUpper[recdevs[["Yr"]] >= xlim[1] & recdevs[["Yr"]] <= 
                                             xlim[2], models], na.rm = TRUE)
    }
    ylim <- range(-ylim, ylim)
    if (!add) {
      plot(0, xlim = xlim, ylim = ylim, axes = FALSE, 
           type = "n", xlab = labels[1], ylab = labels[5], 
           xaxs = xaxs, yaxs = yaxs, las = 1)
      axis(2, las = 1)
      abline(h = 0, col = "grey")
    }
    if (show_uncertainty) {
      for (iline in 1:nlines) {
        imodel <- models[iline]
        if (uncertainty[imodel]) {
          xvec <- recdevs[["Yr"]]
          if (nlines > 1) {
            xvec <- xvec + 0.4 * iline/nlines - 0.2
          }
          arrows(x0 = xvec, y0 = as.numeric(recdevsLower[, 
                                                         imodel]), x1 = xvec, y1 = as.numeric(recdevsUpper[, 
                                                                                                           imodel]), length = 0.01, angle = 90, code = 3, 
                 col = col[iline])
        }
      }
    }
    for (iline in 1:nlines) {
      imodel <- models[iline]
      yvec <- recdevs[, imodel]
      xvec <- recdevs[["Yr"]]
      points(xvec, yvec, pch = pch[iline], lwd = lwd[iline], 
             col = col[iline])
    }
    if (!add) {
      if (tickEndYr) {
        ticks <- graphics::axTicks(1)
        axis(1, at = c(ticks[ticks < max(endyrvec)], 
                       max(endyrvec)))
      }
      else {
        axis(1)
      }
      if (!is.null(endyrvec) & max(endyrvec) > 1 + max(endyrs) & 
          shadeForecast) {
        rect(xleft = max(endyrs) + 1, ybottom = par()[["usr"]][3], 
             xright = par()[["usr"]][2], ytop = par()[["usr"]][4], 
             col = gray(0, alpha = 0.1), border = NA)
      }
      box()
    }
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    return(ylim[2])
  }
  plotPhase <- function(show_uncertainty = TRUE) {
    if (!any(uncertainty)) {
      show_uncertainty <- FALSE
    }
    xlim <- range(0, ylimAdj * Bratio[, models], na.rm = TRUE)
    ylim <- range(0, ylimAdj * SPRratio[, models], na.rm = TRUE)
    if (!add) {
      plot(0, type = "n", xlim = xlim, ylim = ylim, xlab = labels[3], 
           ylab = SPRratioLabel, xaxs = xaxs, yaxs = yaxs, 
           las = 1)
    }
    goodyrs <- intersect(Bratio[["Yr"]], SPRratio[["Yr"]])
    lastyr <- max(goodyrs)
    for (iline in 1:nlines) {
      imodel <- models[iline]
      xvals <- Bratio[Bratio[["Yr"]] %in% goodyrs, imodel]
      yvals <- SPRratio[SPRratio[["Yr"]] %in% goodyrs, 
                        imodel]
      lines(xvals, yvals, col = col[iline], lty = lty[iline], 
            lwd = lwd[iline], type = "l")
      points(tail(xvals, 1), tail(yvals, 1), col = col[iline], 
             pch = pch[iline], lwd = lwd[iline])
    }
    abline(h = 1, v = 1, col = "grey", lty = 2)
    if (btarg > 0) {
      abline(v = btarg, col = "red", lty = 2)
    }
    if (sprtarg > 0) {
      abline(h = sprtarg, col = "red", lty = 2)
    }
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    return(ylim[2])
  }
  plotIndices <- function(log = FALSE, iindex) {
    indices2 <- NULL
    for (iline in 1:nlines) {
      imodel <- models[iline]
      subset2 <- indices[["imodel"]] == imodel & indices[["Yr"]] <= 
        endyrvec[iline] & indices[["Fleet"]] == indexfleets[[imodel]][iindex]
      indices2 <- rbind(indices2, indices[subset2, ])
    }
    yr <- indices2[["Yr"]]
    obs <- indices2[["Obs"]]
    exp <- indices2[["Exp"]]
    imodel <- indices2[["imodel"]]
    Q <- indices2[["Calc_Q"]]
    if (log) {
      obs <- log(obs)
      exp <- log(exp)
      ylab <- labels[7]
    }
    else {
      ylab <- labels[6]
    }
    if (indexUncertainty) {
      if (indexPlotEach) {
        if (is.null(indexSEvec)) {
          indexSEvec <- indices2[["SE"]]
        }
        y <- obs
        if (log) {
          upper <- qnorm(0.975, mean = y, sd = indexSEvec)
          lower <- qnorm(0.025, mean = y, sd = indexSEvec)
        }
        else {
          upper <- qlnorm(0.975, meanlog = log(y), sdlog = indexSEvec)
          lower <- qlnorm(0.025, meanlog = log(y), sdlog = indexSEvec)
        }
      }
      else {
        subset <- indices2[["imodel"]] == models[1]
        if (is.null(indexSEvec)) {
          indexSEvec <- indices2[["SE"]][subset]
        }
        y <- obs
        if (log) {
          upper <- qnorm(0.975, mean = y, sd = indexSEvec)
          lower <- qnorm(0.025, mean = y, sd = indexSEvec)
        }
        else {
          upper <- qlnorm(0.975, meanlog = log(y), sdlog = indexSEvec)
          lower <- qlnorm(0.025, meanlog = log(y), sdlog = indexSEvec)
        }
      }
    }
    else {
      upper <- NULL
      lower <- NULL
    }
    sub <- !is.na(indices2[["Like"]])
    ylim <- range(exp, obs[sub], lower[sub], upper[sub], 
                  na.rm = TRUE)
    if (!any(sub)) {
      ylim <- range(exp, obs, lower, upper, na.rm = TRUE)
    }
    if (!log) {
      ylim <- c(0, ylimAdj * ylim[2])
    }
    else {
      ylim <- ylim + c(-1, 1) * (ylimAdj - 1) * diff(ylim)
    }
    meanQ <- rep(NA, nlines)
    if (!add) {
      if (!is.null(endyrvec)) {
        xlim <- c(min(yr), max(endyrvec))
      }
      else {
        xlim <- range(yr)
      }
      plot(0, type = "n", xlim = xlim, yaxs = yaxs, ylim = ylim, 
           xlab = "Year", ylab = ylab, axes = FALSE)
    }
    if (!log & yaxs != "i") {
      abline(h = 0, col = "grey")
    }
    Qtext <- rep("(Q =", nlines)
    for (iline in (1:nlines)[!mcmcVec]) {
      imodel <- models[iline]
      subset <- indices2[["imodel"]] == imodel
      meanQ[iline] <- mean(Q[subset])
      if (indexQlabel && any(Q[subset] != mean(Q[subset]))) {
        Qtext[iline] <- "(mean Q ="
      }
      x <- yr[subset]
      y <- exp[subset]
      lines(x, y, pch = pch[iline], lwd = lwd[iline], 
            lty = lty[iline], col = col[iline], type = type)
    }
    legendlabels2 <- legendlabels
    if (indexQlabel) {
      legendlabels2 <- paste(legendlabels, Qtext, format(meanQ, 
                                                         digits = indexQdigits), ")")
    }
    if (legend) {
      add_legend(legendlabels, legendloc = legendloc, 
                 legendorder = legendorder, legendncol = legendncol, 
                 col = col, pch = pch, lwd = lwd, lty = lty)
    }
    if (indexPlotEach) {
      for (iline in (1:nlines)[!mcmcVec]) {
        adj <- 0.2 * iline/nlines - 0.1
        imodel <- models[iline]
        if (any(is.na(indices2[["like"]]))) {
          warning("NA's found in likelihood, may cause issues with index plots")
        }
        subset <- indices2[["imodel"]] == imodel & !is.na(indices2[["Like"]])
        if (indexUncertainty) {
          arrows(x0 = yr[subset] + adj, y0 = lower[subset], 
                 x1 = yr[subset] + adj, y1 = upper[subset], 
                 length = 0.01, angle = 90, code = 3, col = adjustcolor(col, 
                                                                        alpha.f = 0.7)[iline])
        }
        points(yr[subset] + adj, obs[subset], pch = 21, 
               cex = 1.5, col = 1, bg = adjustcolor(col, 
                                                    alpha.f = 0.7)[iline])
      }
    }
    else {
      imodel <- models[which(endyrvec == max(endyrvec))[1]]
      subset <- indices2[["imodel"]] == imodel & !is.na(indices2[["Like"]])
      if (indexUncertainty) {
        arrows(x0 = yr[subset], y0 = lower[subset], 
               x1 = yr[subset], y1 = upper[subset], length = 0.01, 
               angle = 90, code = 3, col = 1)
      }
      points(yr[subset], obs[subset], pch = 16, cex = 1.5)
    }
    if (!add) {
      xticks <- pretty(xlim)
      axis(1, at = xticks, labels = format(xticks))
      if (tickEndYr) {
        axis(1, at = max(endyrvec))
      }
      axis(2)
      box()
    }
    return(ylim[2])
  }
  plotDensities <- function(parname, xlab, denslwd, limit0 = TRUE, 
                            cumulative = FALSE) {
    if (any(!mcmcVec)) {
      vals <- rbind(pars[pars[["Label"]] == parname, names(pars) != 
                           "recdev"], quants[quants[["Label"]] == parname, 
                           ])
      if (nrow(vals) != 1) {
        warn <- paste("problem getting values for parameter:", 
                      parname, "")
        if (nrow(vals) == 0) {
          warn <- paste(warn, "no Labels match in either parameters or derived quantities")
        }
        if (nrow(vals) > 0) {
          warn <- paste(warn, "Too many matching Labels:", 
                        pars[["Label"]][pars[["Label"]] == parname], 
                        quants[["Label"]][quants[["Label"]] == parname])
        }
        warning(warn)
        return(NULL)
      }
      valSDs <- rbind(parsSD[pars[["Label"]] == parname, 
      ], quantsSD[quants[["Label"]] == parname, ])
    }
    xmax <- xmin <- ymax <- NULL
    mcmcDens <- vector(mode = "list", length = nlines)
    good <- rep(TRUE, nlines)
    for (iline in 1:nlines) {
      imodel <- models[iline]
      if (mcmcVec[iline]) {
        mcmcColumn <- grep(parname, colnames(mcmc[[imodel]]), 
                           fixed = TRUE)
        if (length(mcmcColumn) == 0) {
          message("No columns selected from MCMC for '", 
                  parname, "' in model ", imodel)
          good[iline] <- FALSE
        }
        if (length(mcmcColumn) > 1) {
          warning("Too many columns selected from MCMC for model ", 
                  imodel, ":", paste0(names(mcmc[[imodel]])[mcmcColumn], 
                                      collapse = ", "), ". Please specify a unique label in the mcmc dataframe", 
                  "or specify mcmcVec = FALSE for model ", 
                  imodel, " (or mcmcVec = FALSE applying to all models). ")
          good[iline] <- FALSE
        }
        if (good[iline]) {
          mcmcVals <- mcmc[[imodel]][, mcmcColumn]
          xmin <- min(xmin, quantile(mcmcVals, 0.005, 
                                     na.rm = TRUE))
          if (limit0) {
            xmin <- max(0, xmin)
          }
          if (fix0 & !grepl("R0", parname)) {
            xmin <- 0
          }
          xmax <- max(xmax, quantile(mcmcVals, 0.995, 
                                     na.rm = TRUE))
          z <- density(mcmcVals, cut = 0, adjust = densityadjust)
          z[["x"]] <- z[["x"]][c(1, seq_along(z[["x"]]), 
                                 length(z[["x"]]))]
          z[["y"]] <- c(0, z[["y"]], 0)
          ymax <- max(ymax, max(z[["y"]]))
          mcmcDens[[iline]] <- z
        }
      }
      else {
        parval <- vals[1, imodel]
        parSD <- valSDs[1, imodel]
        if (!is.numeric(parval)) {
          parval <- -1
        }
        if (!is.na(parSD) && parSD > 0) {
          xmin <- min(xmin, qnorm(0.005, parval, parSD))
          if (limit0) {
            xmin <- max(0, xmin)
          }
          if (fix0 & !grepl("R0", parname)) {
            xmin <- 0
          }
          xmax <- max(xmax, qnorm(0.995, parval, parSD))
          x <- seq(xmin, xmax, length = 500)
          mle <- dnorm(x, parval, parSD)
          mlescale <- 1/(sum(mle) * mean(diff(x)))
          mle <- mle * mlescale
          ymax <- max(ymax, max(mle))
        }
        else {
          xmin <- min(xmin, parval)
          xmax <- max(xmax, parval)
        }
      }
    }
    if (grepl("Bratio", parname)) {
      xmin <- 0
    }
    if (limit0) {
      xmin <- max(0, xmin)
    }
    if (fix0 & !grepl("R0", parname)) {
      xmin <- 0
    }
    xlim <- c(xmin, xmin + (xmax - xmin) * densityscalex)
    x <- seq(xmin, xmax, length = 500)
    xunits <- 1
    if (rescale & xmax > 1000 & xmax < 3e+06) {
      xunits <- 1000
      xlab2 <- "'1000 t"
    }
    if (rescale & xmax > 3e+06) {
      xunits <- 1e+06
      xlab2 <- "million t"
    }
    if (is.null(ymax)) {
      message("  skipping plot of ", parname, " because it seems to not be estimated in any model")
    }
    else {
      par(par)
      if (!add) {
        if (cumulative) {
          plot(0, type = "n", xlim = xlim, axes = FALSE, 
               xaxs = "i", yaxs = yaxs, ylim = c(0, 1), 
               xlab = xlab, ylab = "")
        }
        else {
          plot(0, type = "n", xlim = xlim, axes = FALSE, 
               xaxs = "i", yaxs = yaxs, ylim = c(0, 1.1 * 
                                                   ymax * densityscaley), xlab = xlab, ylab = "")
        }
      }
      if (grepl("Bratio", parname)) {
        if (btarg > 0) {
          abline(v = btarg, col = "red", lty = 2)
          text(btarg + 0.03, par()[["usr"]][4], labels[10], 
               adj = 1.05, srt = 90)
        }
        if (minbthresh > 0) {
          abline(v = minbthresh, col = "red", lty = 2)
          text(minbthresh + 0.03, par()[["usr"]][4], 
               labels[11], adj = 1.05, srt = 90)
        }
      }
      symbolsQuants <- c(0.025, 0.125, 0.25, 0.5, 0.75, 
                         0.875, 0.975)
      for (iline in (1:nlines)[good]) {
        imodel <- models[iline]
        if (mcmcVec[iline]) {
          mcmcColumn <- grep(parname, colnames(mcmc[[imodel]]), 
                             fixed = TRUE)
          mcmcVals <- mcmc[[imodel]][, mcmcColumn]
          x2 <- quantile(mcmcVals, symbolsQuants, na.rm = TRUE)
          x <- mcmcDens[[iline]][["x"]]
          if (!cumulative) {
            y <- mcmcDens[[iline]][["y"]]
            yscale <- 1/(sum(y) * mean(diff(x)))
            y <- y * yscale
          }
          else {
            y <- cumsum(mcmcDens[[iline]][["y"]])/sum(mcmcDens[[iline]][["y"]])
          }
          y2 <- NULL
          for (ii in x2) {
            y2 <- c(y2, min(y[abs(x - ii) == min(abs(x - 
                                                       ii))]))
          }
          if (!cumulative) {
            polygon(c(x[1], x, rev(x)[1]), c(0, y, 0), 
                    col = shadecol[iline], border = NA)
          }
          else {
            polygon(c(x[1], x, rev(x)[c(1, 1)]), c(0, 
                                                   y, 1, 0), col = shadecol[iline], border = NA)
          }
          lines(x, y, col = col[iline], lwd = 2)
          if (!cumulative) {
            if (densitysymbols) {
              points(x2, y2, col = col[iline], pch = pch[iline])
            }
            lines(rep(x2[median(seq_along(x2))], 2), 
                  c(0, y2[median(seq_along(x2))]), col = col[iline])
          }
          else {
            if (densitysymbols) {
              points(x2, symbolsQuants, col = col[iline], 
                     pch = pch[iline])
            }
            lines(rep(median(mcmcVals), 2), c(0, 0.5), 
                  col = col[iline])
          }
        }
        else {
          parval <- vals[1, imodel]
          parSD <- valSDs[1, imodel]
          if (!is.na(parSD) && parSD > 0) {
            xmin <- min(xmin, qnorm(0.005, parval, parSD))
            if (limit0) {
              xmin <- max(0, xmin)
            }
            if (fix0 & !grepl("R0", parname)) {
              xmin <- 0
            }
            x <- seq(xmin, max(xmax, xlim), length = 500)
            x2 <- qnorm(symbolsQuants, parval, parSD)
            if (cumulative) {
              y <- mle <- pnorm(x, parval, parSD)
              y2 <- mle2 <- pnorm(x2, parval, parSD)
            }
            else {
              mle <- dnorm(x, parval, parSD)
              mle2 <- dnorm(x2, parval, parSD)
              mlescale <- 1/(sum(mle) * mean(diff(x)))
              y <- mle <- mle * mlescale
              y2 <- mle2 <- mle2 * mlescale
            }
            polygon(c(x[1], x, rev(x)[1]), c(0, mle, 
                                             0), col = shadecol[iline], border = NA)
            lines(x, mle, col = col[iline], lwd = 2)
            if (!cumulative) {
              if (densitysymbols) {
                points(x2, mle2, col = col[iline], pch = pch[iline])
              }
              lines(rep(parval, 2), c(0, dnorm(parval, 
                                               parval, parSD) * mlescale), col = col[iline], 
                    lwd = denslwd)
            }
            else {
              if (densitysymbols) {
                points(x2, symbolsQuants, col = col[iline], 
                       pch = pch[iline])
              }
              lines(rep(parval, 2), c(0, 0.5), col = col[iline], 
                    lwd = denslwd)
            }
          }
          else {
            abline(v = parval, col = col[iline], lwd = denslwd)
          }
        }
        if (densitytails & densitymiddle) {
          warning("You are shading both tails and central 95% of density plots", 
                  "which is illogical")
        }
        doShade <- FALSE
        if (mcmcVec[iline]) {
          doShade <- TRUE
        }
        else {
          if (!is.na(parSD) && parSD > 0) {
            doShade <- TRUE
          }
        }
        if (densitytails & doShade) {
          x.lower <- x[x <= x2[1]]
          y.lower <- y[x <= x2[1]]
          x.upper <- x[x >= rev(x2)[1]]
          y.upper <- y[x >= rev(x2)[1]]
          polygon(c(x.lower[1], x.lower, rev(x.lower)[1]), 
                  c(0, y.lower, 0), col = shadecol[iline], 
                  border = NA)
          polygon(c(x.upper[1], x.upper, rev(x.upper)[1]), 
                  c(0, y.upper, 0), col = shadecol[iline], 
                  border = NA)
        }
        if (densitymiddle & doShade) {
          x.middle <- x[x >= x2[1] & x <= rev(x2)[1]]
          y.middle <- y[x >= x2[1] & x <= rev(x2)[1]]
          polygon(c(x.middle[1], x.middle, rev(x.middle)[1]), 
                  c(0, y.middle, 0), col = shadecol[iline], 
                  border = NA)
        }
      }
      if (!add) {
        abline(h = 0, col = "grey")
        xticks <- pretty(xlim)
        axis(1, at = xticks, labels = format(xticks/xunits))
        theLine <- par()[["mgp"]][1]
        if (cumulative) {
          axis(2, at = symbolsQuants, labels = format(symbolsQuants), 
               cex.axis = 0.9)
          mtext(side = 2, line = theLine, text = "Cumulative Probability", 
                col = par()[["col.lab"]], cex = par()[["cex.lab"]])
        }
        else {
          mtext(side = 2, line = theLine, text = labels[9], 
                col = par()[["col.lab"]], cex = par()[["cex.lab"]])
        }
        box()
      }
      if (xunits != 1) {
        message("x-axis for ", parname, " in density plot has been divided by ", 
                xunits, " (so may be in units of ", xlab2, 
                ")")
      }
      if (legend) {
        add_legend(legendlabels, legendloc = ifelse(cumulative, 
                                                    "topleft", legendloc), legendorder = legendorder, 
                   legendncol = legendncol, col = col, pch = pch, 
                   lwd = lwd, lty = lty)
      }
    }
    return(NA)
  }
  uncertaintyplots <- intersect(c(2, 4, 6, 8, 10, 12), subplots)
  if (!any(uncertainty) & length(uncertaintyplots) > 0) {
    message("skipping plots with uncertainty:", paste(uncertaintyplots, 
                                                      collapse = ","))
  }
  if (1 %in% subplots) {
    if (verbose) {
      message("subplot 1: spawning biomass")
    }
    if (plot) {
      ymax_vec[1] <- plotBio(option = 1, show_uncertainty = FALSE)
    }
    if (print) {
      save_png_comparisons("compare1_spawnbio.png")
      ymax_vec[1] <- plotBio(option = 1, show_uncertainty = FALSE)
      dev.off()
    }
  }
  if (2 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplot 2: spawning biomass with uncertainty intervals")
      }
      if (plot) {
        ymax_vec[2] <- plotBio(option = 1, show_uncertainty = TRUE)
      }
      if (print) {
        save_png_comparisons("compare2_spawnbio_uncertainty.png")
        ymax_vec[2] <- plotBio(option = 1, show_uncertainty = TRUE)
        dev.off()
      }
    }
  }
  if (3 %in% subplots) {
    if (verbose) {
      message("subplot 3: biomass ratio (hopefully equal to fraction of unfished)")
    }
    if (plot) {
      ymax_vec[3] <- plotBratio(show_uncertainty = FALSE)
    }
    if (print) {
      save_png_comparisons("compare3_Bratio.png")
      ymax_vec[3] <- plotBratio(show_uncertainty = FALSE)
      dev.off()
    }
  }
  if (4 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplot 4: biomass ratio with uncertainty")
      }
      if (plot) {
        ymax_vec[4] <- plotBratio(show_uncertainty = TRUE)
      }
      if (print) {
        save_png_comparisons("compare4_Bratio_uncertainty.png")
        ymax_vec[4] <- plotBratio(show_uncertainty = TRUE)
        dev.off()
      }
    }
  }
  if (18 %in% subplots) {
    if (verbose) {
      message("subplot 18: summary biomass")
    }
    if (plot) {
      ymax_vec[18] <- plotBio(option = 2, show_uncertainty = FALSE)
    }
    if (print) {
      save_png_comparisons("compare18_smrybio.png")
      ymax_vec[18] <- plotBio(option = 2, show_uncertainty = FALSE)
      dev.off()
    }
  }
  if (19 %in% subplots) {
    if (any(uncertainty)) {
      if (all(is.na(summaryoutput[["SmryBioSD"]][["Label"]]))) {
        if (verbose) {
          message("skipping subplot 19 summary biomass with uncertainty ", 
                  "because no models include summary biomass as a derived quantity")
        }
      }
      else {
        if (verbose) {
          message("subplot 19: summary biomass with uncertainty intervals")
        }
        if (plot) {
          ymax_vec[19] <- plotBio(option = 2, show_uncertainty = TRUE)
        }
        if (print) {
          save_png_comparisons("compare19_smrybio_uncertainty.png")
          ymax_vec[19] <- plotBio(option = 2, show_uncertainty = TRUE)
          dev.off()
        }
      }
    }
  }
  if (5 %in% subplots) {
    if (verbose) {
      message("subplot 5: SPR ratio")
    }
    if (plot) {
      ymax_vec[5] <- plotSPRratio(show_uncertainty = FALSE)
    }
    if (print) {
      save_png_comparisons("compare5_SPRratio.png")
      ymax_vec[5] <- plotSPRratio(show_uncertainty = FALSE)
      dev.off()
    }
  }
  if (6 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplot 6: SPR ratio with uncertainty")
      }
      if (plot) {
        ymax_vec[6] <- plotSPRratio(show_uncertainty = TRUE)
      }
      if (print) {
        save_png_comparisons("compare6_SPRratio_uncertainty.png")
        ymax_vec[6] <- plotSPRratio(show_uncertainty = TRUE)
        dev.off()
      }
    }
  }
  if (7 %in% subplots) {
    if (verbose) {
      message("subplot 7: F value")
    }
    if (plot) {
      ymax_vec[7] <- plotF(show_uncertainty = FALSE)
    }
    if (print) {
      save_png_comparisons("compare7_Fvalue.png")
      ymax_vec[7] <- plotF(show_uncertainty = FALSE)
      dev.off()
    }
  }
  if (8 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplot 8: F value with uncertainty")
      }
      if (plot) {
        ymax_vec[8] <- plotF(show_uncertainty = TRUE)
      }
      if (print) {
        save_png_comparisons("compare8_Fvalue_uncertainty.png")
        ymax_vec[8] <- plotF(show_uncertainty = TRUE)
        dev.off()
      }
    }
  }
  if (9 %in% subplots) {
    if (verbose) {
      message("subplot 9: recruits")
    }
    if (plot) {
      ymax_vec[9] <- plotRecruits(show_uncertainty = FALSE)
    }
    if (print) {
      save_png_comparisons("compare9_recruits.png")
      ymax_vec[9] <- plotRecruits(show_uncertainty = FALSE)
      dev.off()
    }
  }
  if (10 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplot 10: recruits with uncertainty")
      }
      if (plot) {
        ymax_vec[10] <- plotRecruits()
      }
      if (print) {
        save_png_comparisons("compare10_recruits_uncertainty.png")
        ymax_vec[10] <- plotRecruits()
        dev.off()
      }
    }
  }
  if (11 %in% subplots) {
    if (verbose) {
      message("subplot 11: recruit devs")
    }
    if (is.null(recdevs)) {
      message("No recdevs present in the model summary, skipping plot.")
    }
    else {
      if (plot) {
        ymax_vec[11] <- plotRecDevs(show_uncertainty = FALSE)
      }
      if (print) {
        save_png_comparisons("compare11_recdevs.png")
        ymax_vec[11] <- plotRecDevs(show_uncertainty = FALSE)
        dev.off()
      }
    }
  }
  if (12 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplot 12: recruit devs with uncertainty")
      }
      if (plot) {
        ymax_vec[12] <- plotRecDevs()
      }
      if (print) {
        save_png_comparisons("compare12_recdevs_uncertainty.png")
        ymax_vec[12] <- plotRecDevs()
        dev.off()
      }
    }
  }
  if (13 %in% subplots & !is.null(indices) && nrow(indices) > 
      0) {
    if (verbose) {
      message("subplot 13: index fits")
    }
    for (iindex in seq_along(indexfleets[[1]])) {
      if (plot) {
        ymax_vec[13] <- plotIndices(log = FALSE, iindex = iindex)
      }
      if (print) {
        save_png_comparisons(paste0("compare13_indices", 
                                    index_plot_suffix[iindex], ".png"))
        ymax_vec[13] <- plotIndices(log = FALSE, iindex = iindex)
        dev.off()
      }
    }
  }
  if (14 %in% subplots & !is.null(indices) && nrow(indices) > 
      0) {
    if (verbose) {
      message("subplot 14: index fits on a log scale")
    }
    for (iindex in seq_along(indexfleets[[1]])) {
      if (plot) {
        ymax_vec[14] <- plotIndices(log = TRUE, iindex = iindex)
      }
      if (print) {
        save_png_comparisons(paste0("compare14_indices_log", 
                                    index_plot_suffix[iindex], ".png"))
        ymax_vec[14] <- plotIndices(log = TRUE, iindex = iindex)
        dev.off()
      }
    }
  }
  if (15 %in% subplots) {
    if (verbose) {
      message("subplot 15: phase plot")
    }
    if (plot) {
      ymax_vec[15] <- plotPhase()
    }
    if (print) {
      save_png_comparisons("compare15_phase_plot.png")
      ymax_vec[15] <- plotPhase()
      dev.off()
    }
  }
  if (16 %in% subplots | 17 %in% subplots) {
    if (any(uncertainty)) {
      if (verbose) {
        message("subplots 16 and 17: densities")
      }
      expandednames <- NULL
      for (i in seq_along(densitynames)) {
        matchingnames <- c(pars[["Label"]], quants[["Label"]])[grep(densitynames[i], 
                                                                    c(pars[["Label"]], quants[["Label"]]), fixed = TRUE)]
        expandednames <- c(expandednames, matchingnames)
      }
      if (length(expandednames) == 0) {
        warning("No parameter/quantity names matching 'densitynames' input.")
      }
      else {
        message("Parameter/quantity names matching 'densitynames' input:\n", 
                paste0(expandednames, collapse = ", "))
        ndensities <- length(expandednames)
        densitytable <- data.frame(name = expandednames, 
                                   label = expandednames, stringsAsFactors = FALSE)
        if (!is.null(densityxlabs) && length(densityxlabs) == 
            ndensities) {
          densitytable[["label"]] <- densityxlabs
          message("  table of parameter/quantity labels with associated", 
                  " x-axis label:")
          print(densitytable)
        }
        else {
          if (!is.null(densityxlabs)) {
            warning("length of 'densityxlabs' doesn't match the number of values ", 
                    "matching 'densitynames' so parameter labels will be used instead")
          }
        }
        if (16 %in% subplots) {
          for (iplot in 1:ndensities) {
            name <- densitytable[iplot, 1]
            xlab <- densitytable[iplot, 2]
            if (plot) {
              ymax_vec[16] <- plotDensities(parname = name, 
                                            xlab = xlab, denslwd = densitylwd)
            }
            if (print) {
              save_png_comparisons(paste("compare16_densities_", 
                                         name, ".png", sep = ""))
              ymax_vec[16] <- plotDensities(parname = name, 
                                            xlab = xlab, denslwd = densitylwd)
              dev.off()
            }
          }
        }
        if (17 %in% subplots) {
          for (iplot in 1:ndensities) {
            name <- densitytable[iplot, 1]
            xlab <- densitytable[iplot, 2]
            if (plot) {
              ymax_vec[17] <- plotDensities(parname = name, 
                                            xlab = xlab, denslwd = densitylwd, cumulative = TRUE)
            }
            if (print) {
              save_png_comparisons(paste("compare17_densities_", 
                                         name, ".png", sep = ""))
              ymax_vec[17] <- plotDensities(parname = name, 
                                            xlab = xlab, denslwd = densitylwd, cumulative = TRUE)
              dev.off()
            }
          }
        }
      }
    }
  }
  if (pdf) {
    dev.off()
  }
  return(invisible(ymax_vec))
}

# NatMort = 0.115 #models_list$test_AgeErr0.5$MGparmAdj$NatM_uniform_Fem_GP_1[1]
# Ftarg = 2/3*NatMort
# Fthresh = NatMort
# Flim = 3/2*NatMort

# plot comparisons
# SSplotComparisonsREP(mod.sum, print=T,plotdir = plotdir,btarg = 0.4,
#                      minbthresh = 0.2,filenameprefix = filenameprefix,legend=T, 
#                      Ftarg = Ftarg,Fthresh=Fthresh,Flim=Flim,
#                      legendlabels=models)


# SSplotComparisonsREP(summaryoutput = mod.sum, 
#                      print=T,
#                      plotdir = plotdir, 
#                      btarg = 0.4,
#                      minbthresh = 0.2,
#                      filenameprefix = paste0(filename_prefix, "_REP_"),
#                      legend=T,
#                      Ftarg = 2/3*NatMort,
#                      Fthresh=NatMort,
#                      Flim=3/2*NatMort,
#                      legendlabels=model_names)