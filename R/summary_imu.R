#' Summarize and plot an IMU vector
#'
#' @description
#' Provides a diagnostic overview of an IMU vector (`acc`, `mag`, or `gyro`) —
#' axis combinations, frequencies, burst sizes, timing, and a coarse quantile
#' summary of the burst sample values. Calling [plot()] on the result draws a
#' multi-panel histogram of those same distributions.
#'
#' Intervals between burst start times are computed in vector order. All
#' bursts are considered together; thus, if bursts come from different
#' sources (e.g. different tags), there may be large interval artifacts
#' between some bursts.
#'
#' Note that the distribution of sample values considers all axes and units 
#' simultaneously.
#'
#' @param object An `imu` object.
#' @param ... For `plot()`, passed to [graphics::hist()].
#'
#' @returns
#' `summary()` returns an `imu_summary` object. `plot()` invisibly returns
#' its input.
#'
#' @name imu_summary
#' @examples
#' a <- acc_example()
#' s <- summary(a)
#' s
#' plot(s)
#'
#' # Focus on a single panel, with custom binning/xlim
#' plot(s, panel = "Values", breaks = 50, xlim = c(0, 1))
NULL

#' @rdname imu_summary
#' @export
summary.imu <- function(object, ...) {
  x <- object[!is.na(object)]

  out <- list(
    sensor = class(object)[1],
    n = length(object),
    n_na = sum(is.na(object))
  )

  if (length(x) == 0) {
    return(new_imu_summary(out))
  }

  br <- bursts(x)

  # Axis combos (e.g. "XYZ", "XY")
  axis_combos <- vapply(br, function(b) paste(colnames(b), collapse = ""), "")
  out$axes <- sort(table(axis_combos), decreasing = TRUE)

  # Frequencies
  f <- freqs(x)
  out$freq_unit <- units::deparse_unit(f)
  out$freqs <- as.numeric(f)

  # Samples per burst
  out$samples <- n_samples(x)

  # Durations
  dur <- burst_dur(x)
  out$durations <- as.numeric(dur)
  out$dur_unit <- units::deparse_unit(dur)

  # Use vector order to calculate inter-burst intervals. If bursts come from
  # different sources some intervals will be artifacts, but if generated
  # from a move2, these should be limited as the tracks should already be
  # ordered.
  st <- starts(x)
  st <- st[!is.na(st)]

  if (length(st) > 0) {
    out$start_range <- range(st)
  } else {
    out$start_range <- NULL
  }

  out$start_tz <- attr(st, "tzone") %||% "UTC"

  out$intervals <- if (length(st) > 1) {
    as.numeric(diff(st), units = "secs")
  } else {
    numeric(0)
  }

  # Units
  unit_strs <- imu_units(x)
  out$imu_units <- unique(stats::na.omit(unit_strs))
  out$has_unitless <- any(is.na(unit_strs))

  # Sample values (units stripped; mixed-unit case flagged via footer)
  out$values <- as.numeric(
    unlist(
      lapply(br, function(b) if (length(b) == 0) NULL else as.numeric(b)),
      use.names = FALSE
    )
  )

  new_imu_summary(out)
}

#' @export
print.imu_summary <- function(x, ...) {
  # Header
  if (x$n_na > 0) {
    na_note <- paste0(" (", format_count(x$n_na), " NA)") 
  } else {
    na_note <- "" 
  }
  
  cat(format_count(x$n), " ", x$sensor, " bursts", na_note, "\n", sep = "")
  
  if (is.null(x$axes)) {
    return(invisible(x))
  }
  
  # Time range
  if (!is.null(x$start_range)) {
    if (nzchar(x$start_tz)) { 
      tz_label <- paste0(" ", x$start_tz)
    } else {
      tz_label <- "" 
    }
    
    cat(paste0("from ", format(x$start_range[1]), " to ", format(x$start_range[2]), tz_label), "\n")
  }
  
  cat("\n")
  
  # Axes
  axis_parts <- paste0(
    names(x$axes), " (", format_count(as.integer(x$axes)), ")"
  )
  cat("Axes:", paste(axis_parts, collapse = ", "), "\n")
  
  # Frequency
  cat("Frequencies:", format_range(x$freqs, x$freq_unit), "\n")
  
  # Samples
  cat("Samples per burst:", format_range(x$samples), "\n")
  
  # Duration
  cat("Durations:", format_range(x$durations, x$dur_unit), "\n")

  # Intervals
  cat("Intervals:", format_quantiles(x$intervals, "s"), "\n")

  # Values + Units
  cat("\n")
  cat("Values: ", format_quantiles(x$values), "\n")
  labels <- character(0)
  if (length(x$imu_units) > 0) labels <- paste0("[", x$imu_units, "]")
  if (isTRUE(x$has_unitless)) labels <- c(labels, "[no units]")
  if (length(labels) == 0) labels <- "[no units]"
  cat("Units:  ", paste(labels, collapse = ", "), "\n")
  
  invisible(x)
}

#' @param x An `imu_summary` object (returned by `summary()`).
#' @param panel Optional character vector of panel names or integer
#'   vector of panel positions, restricting which panels are drawn. Valid
#'   names: `"Frequency"`, `"Samples per burst"`, `"Duration"`, `"Intervals"`,
#'   and/or `"Values"`. By default, all panels are drawn.
#'
#' @rdname imu_summary
#' @export
plot.imu_summary <- function(x, panel = NULL, ...) {
  if (is.null(x$axes)) {
    message("Nothing to plot (no non-NA bursts).")
    return(invisible(x))
  }

  panels <- list(
    Frequency = list(
      data = x$freqs,
      xlab = paste0("Frequency [", x$freq_unit, "]")
    ),
    `Samples per burst` = list(
      data = x$samples,
      xlab = "Samples per burst"
    ),
    Duration = list(
      data = x$durations,
      xlab = paste0("Duration [", x$dur_unit, "]")
    ),
    Intervals = list(
      data = x$intervals,
      xlab = "Interval [s]"
    ),
    Values = list(
      data = x$values,
      xlab = "Value"
    )
  )

  if (!is.null(panel)) {
    panels <- select_panels(panels, panel)
  }

  if (length(panels) == 0) {
    message("Nothing to plot (no panels selected).")
    return(invisible(x))
  }

  np <- length(panels)
  nc <- min(np, 2)
  nr <- ceiling(np / nc)
  
  oldpar <- graphics::par(mfrow = c(nr, nc), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(oldpar))
  
  for (nm in names(panels)) {
    p <- panels[[nm]]
    if (length(p$data) == 0) {
      graphics::plot.new()
      graphics::title(main = nm, xlab = p$xlab)
      graphics::text(0.5, 0.5, "No data", col = "grey50")
      next
    }
    graphics::hist(
      p$data,
      main = nm,
      xlab = p$xlab,
      col = "grey80",
      border = "white",
      ...
    )
  }
  
  invisible(x)
}

# Subset the panel list by name or integer index, validating against the
# panels that are actually available
select_panels <- function(panels, which_panel) {
  available <- names(panels)

  if (is.numeric(which_panel)) {
    if (any(which_panel > length(available)) || any(which_panel <= 0)) {
      cli::cli_abort("{.arg which_panel} must be between 1 and {length(available)}.")
    }

    sel <- available[which_panel]
  } else {
    sel <- which_panel
  }

  sel <- rlang::arg_match(sel, available, multiple = TRUE)
  panels[sel]
}

new_imu_summary <- function(x) {
  structure(x, class = c("imu_summary", class(x)))
}

format_count <- function(x) {
  format(x, big.mark = ",", trim = TRUE)
}

format_num <- function(x) {
  format(round(x, 2), trim = TRUE, nsmall = 0)
}

format_range <- function(x, unit = NULL) {
  mn <- format_num(min(x))
  mx <- format_num(max(x))
  if (!is.null(unit)) {
    paste0(mn, " -- ", mx, " [", unit, "]")
  } else {
    paste0(mn, " -- ", mx)
  }
}

format_quantiles <- function(x, unit = NULL) {
  if (length(x) == 0) {
    return("[ no data ]")
  }
  q <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  parts <- paste(vapply(q, format_num, ""), collapse = " / ")
  out <- paste0("[ ", parts, " ]")
  if (!is.null(unit)) {
    out <- paste0(out, " [", unit, "]")
  }
  paste0(out, "  (min/Q1/med/Q3/max)")
}
