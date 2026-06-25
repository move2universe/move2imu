#' Plot sampling activity over time
#'
#' Bin each IMU vector's samples over time and draw them as colored bands.
#' Sensors stack as lanes within a panel, and `group` IDs (e.g. tags) become
#' facet rows on a shared time axis — making it easy to see *when* each sensor
#' was active, spot sparse or dead periods, and compare tags.
#'
#' Each tile spans one bin-width window, and its color encodes **how many
#' samples** fell in that window (the data volume), ramping from white (none)
#' to the sensor's base color (its busiest bin). The default bin is chosen fine
#' enough that *burstiness reads as texture*: a continuously sampling sensor
#' fills its row as a solid band, while a duty-cycled or bursty one breaks into
#' regularly spaced marks separated by white gaps. At this fine resolution a
#' tile's shade also tracks the instantaneous sampling rate, since a busier
#' (higher-frequency) burst packs more samples into the bin.
#'
#' Two limits bound how fine binning can usefully go: tiles narrower than a
#' screen pixel cannot be drawn distinctly (they alias), and very fine bins over
#' long spans produce many tiles (slower, larger output). The automatic default
#' aims near that sweet spot; to inspect patterns finer than the plot can
#' resolve, subset to a shorter time range (zoom).
#'
#' @param ... One or more IMU vectors (`acc`, `mag`, `gyro`). Argument names
#'   label the sensor lanes; unnamed arguments fall back to the sensor's class.
#'   The first argument is drawn on the top lane.
#' @param group Optional grouping IDs (e.g. track or tag IDs) that split the
#'   plot into one facet row per ID. Either a single vector whose length matches
#'   every IMU vector in `...`, or a named list keyed by the IMU argument names
#'   with one vector per IMU. Pass a factor to control row order and to keep
#'   tags that have no data visible as empty rows.
#' @param bin Bin width controlling the *time resolution* of the plot: a numeric
#'   (seconds), a [base::difftime], or a string like `"1 hour"`, `"30 min"`,
#'   `"1 day"`. Defaults to `NULL`, which chooses a width automatically so the
#'   data span is divided into roughly 1200 tiles (rounded to a round unit) —
#'   fine enough that bursty sampling shows up as gaps. Pass a smaller value to
#'   resolve finer gaps, or a larger one to summarize.
#' @param colors Optional named character vector of base colors keyed by sensor
#'   name. Overrides the built-in defaults (`acc` blue, `mag` magenta, `gyro`
#'   purple); other sensors fall back to the ggplot2 discrete palette.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examplesIf rlang::is_installed(c("ggplot2", "dplyr", "move2"))
#' alb <- albatrosses()
#' a <- as_acc(alb)
#'
#' plot_sampling_raster(acc = a)
#'
#' # One facet row per track/tag
#' plot_sampling_raster(acc = a, group = move2::mt_track_id(alb))
plot_sampling_raster <- function(..., group = NULL, bin = NULL, colors = NULL) {
  rlang::check_installed(c("ggplot2", "dplyr"))

  imus <- list(...)
  if (length(imus) == 0) {
    rlang::abort("Provide at least one IMU vector via `...`.")
  }

  nms <- names(imus) %||% rep("", length(imus))
  for (i in seq_along(imus)) {
    assert_imu(imus[[i]])
    if (!nzchar(nms[i])) nms[i] <- class(imus[[i]])[1]
  }
  names(imus) <- make.unique(nms)

  groups <- resolve_group(group, imus)
  has_group <- !is.null(group)
  bin_sec <- if (is.null(bin)) auto_bin_seconds(imus) else as_bin_seconds(bin)
  tz <- starts_tz(imus)

  # Count samples per bin directly from burst metadata (no per-sample blow-up).
  per_sensor <- mapply(
    function(im, g) bin_counts(im, g, bin_sec),
    imus, groups, SIMPLIFY = FALSE
  )
  df <- dplyr::bind_rows(per_sensor, .id = "sensor")

  if (nrow(df) == 0) {
    rlang::abort(c(
      "No bursts with both data and start timestamps to plot.",
      i = "Use `starts(x) <- ...` to assign timestamps."
    ))
  }

  # Reverse the sensor order so the first `...` argument lands on the top lane
  # (largest y) while the factor's numeric codes run bottom-to-top.
  df$sensor <- factor(df$sensor, levels = rev(names(imus)))
  if (has_group) {
    df$group <- factor(as.character(df$group), levels = group_levels_from(groups))
  }

  palette <- resolve_palette(levels(df$sensor), colors)
  sampling_raster_bins(df, bin_sec, tz, palette, has_group)
}

# Normalize the `group` argument into a per-IMU list of vectors, validating
# that each entry's length matches its IMU vector.
resolve_group <- function(group, imus) {
  if (is.null(group)) {
    return(rep(list(NULL), length(imus)))
  }

  imu_lens <- vapply(imus, length, integer(1))

  # Atomic vector / factor: apply same group to every IMU
  if (!is.list(group) || is.factor(group)) {
    if (!all(imu_lens == length(group))) {
      rlang::abort(
        "`group` length must match every IMU vector; pass a named list to use different IDs per sensor."
      )
    }
    return(rep(list(group), length(imus)))
  }

  # Named list: one entry per IMU
  if (is.null(names(group))) {
    rlang::abort("`group` list must be named to match the IMU arguments in `...`.")
  }
  missing <- setdiff(names(imus), names(group))
  if (length(missing) > 0) {
    rlang::abort(paste0(
      "`group` is missing entries for: ", paste(missing, collapse = ", ")
    ))
  }
  out <- group[names(imus)]
  glens <- vapply(out, length, integer(1))
  if (!all(imu_lens == glens)) {
    rlang::abort("Each `group` entry must match its IMU vector's length.")
  }
  out
}

# Collect every group level in display order: a factor's own level order, else
# first appearance for plain vectors. Keeps tags with no data in the picture.
group_levels_from <- function(groups) {
  unique(unlist(lapply(groups, function(g) {
    if (is.factor(g)) levels(g) else unique(as.character(g))
  })))
}

# Time zone of the first burst with timestamps; falls back to "UTC".
starts_tz <- function(imus) {
  for (im in imus) {
    s <- starts(im)
    if (length(s)) {
      tzo <- attr(s, "tzone")
      if (!is.null(tzo) && nzchar(tzo)) return(tzo)
      break
    }
  }
  "UTC"
}

# Count samples per bin for one IMU vector, working from burst metadata instead
# of expanding every sample. Samples in a burst are evenly spaced
# (start + i/freq), so the count landing in a bin is a difference of cumulative
# sample counts at the bin edges -- a burst costs O(bins it spans), and the
# overwhelmingly common single-bin case is fully vectorized. Returns integer bin
# indices (bin * bin_sec = bin start, epoch seconds), counts `n`, and an
# optional `group`. Bursts without data or a start time are dropped.
bin_counts <- function(x, group, bin_sec) {
  keep <- !is.na(x) & !is.na(starts(x))
  out <- data.frame(bin = numeric(), n = numeric())
  if (!is.null(group)) out$group <- character()
  if (!any(keep)) return(out)

  t0 <- as.numeric(starts(x)[keep])
  # Seconds between samples; force to "s" so non-Hz frequencies stay correct.
  dt <- units::drop_units(units::set_units(1 / freqs(x)[keep], "s"))
  n <- as.numeric(n_samples(x)[keep])
  g <- if (!is.null(group)) as.character(group)[keep] else NULL

  b_first <- floor(t0 / bin_sec)
  b_last <- floor((t0 + (n - 1) * dt) / bin_sec)
  nb <- b_last - b_first + 1L # bins each burst spans

  # Single-bin bursts: the whole burst lands in one bin.
  one <- nb == 1
  bin_idx <- b_first[one]
  cnt <- n[one]
  grp <- if (!is.null(group)) g[one] else NULL

  # Multi-bin bursts (rare): expand to (burst, bin) cells and split exactly.
  if (any(!one)) {
    mi <- which(!one)
    reps <- as.integer(nb[mi])
    cell <- rep(mi, reps)
    b <- sequence(reps, from = b_first[mi]) # bin index per cell
    t0c <- t0[cell]
    dtc <- dt[cell]
    nc <- n[cell]
    # Samples strictly before edge E (clamped to [0, n]); tiny tolerance guards
    # against floating-point off-by-one at exact bin edges.
    cum <- function(e) pmin(pmax(ceiling((e - t0c) / dtc - 1e-9), 0), nc)
    edge_lo <- b * bin_sec
    bin_idx <- c(bin_idx, b)
    cnt <- c(cnt, cum(edge_lo + bin_sec) - cum(edge_lo))
    if (!is.null(group)) grp <- c(grp, g[cell])
  }

  out <- data.frame(bin = bin_idx, n = cnt)
  if (!is.null(group)) out$group <- grp
  out
}

# Choose a bin width (seconds) so the data's time span is divided into roughly
# `target` tiles, rounded up to a round unit. Targets a fine resolution (near
# one tile per screen pixel) so bursty sampling reads as gaps; falls back to one
# hour when the span is empty/degenerate.
auto_bin_seconds <- function(imus, target = 1200) {
  ts <- unlist(lapply(imus, function(im) {
    s <- starts(im)
    as.numeric(s[!is.na(s)])
  }))
  if (length(ts) < 2) return(3600)
  span <- diff(range(ts))
  if (!is.finite(span) || span <= 0) return(3600)

  nice <- c(
    1, 2, 5, 10, 15, 30, # seconds
    60, 120, 300, 600, 900, 1800, # minutes
    3600, 7200, 10800, 21600, 43200, # hours: 1, 2, 3, 6, 12
    86400, 172800, 604800, 1209600, 2592000 # days: 1, 2, 7, 14, 30
  )
  pick <- nice[nice >= span / target][1]
  if (is.na(pick)) nice[length(nice)] else pick
}

# Build a named base-color vector covering every sensor. User-supplied `colors`
# overrides the defaults; anything still uncovered draws from the ggplot2
# discrete palette so unknown sensors still render.
resolve_palette <- function(sensors, colors) {
  defaults <- c(acc = "#2b8cbe", mag = "#d01c8b", gyro = "#7570b3")
  pal <- defaults
  if (!is.null(colors)) {
    if (is.null(names(colors))) {
      rlang::abort("`colors` must be a named character vector.")
    }
    pal[names(colors)] <- colors
  }

  missing <- setdiff(sensors, names(pal))
  if (length(missing) > 0) {
    extra <- scales::hue_pal()(length(missing))
    names(extra) <- missing
    pal <- c(pal, extra)
  }
  pal[sensors]
}

sampling_raster_bins <- function(df, bin_sec, tz, palette, has_group) {
  # Several bursts can fall in the same bin; sum their sample counts. (Zero-data
  # bins are simply absent, so the white panel shows through them.)
  group_cols <- if (has_group) c("sensor", "group", "bin") else c("sensor", "bin")
  agg <- dplyr::summarise(
    dplyr::group_by(df, !!!rlang::syms(group_cols)),
    n = sum(.data$n), .groups = "drop"
  )

  # Intensity encodes sample count (data volume), white -> base color. sqrt
  # compresses the wide dynamic range of counts; a floor keeps any non-empty
  # bin visible. Normalized per sensor (to its busiest bin across all tags) so
  # a high-rate sensor doesn't wash out a low-rate one.
  floor_t <- 0.28
  agg <- dplyr::ungroup(dplyr::mutate(
    dplyr::group_by(agg, .data$sensor),
    smax = max(.data$n)
  ))
  agg$t <- floor_t + (1 - floor_t) * sqrt(pmin(agg$n, agg$smax) / agg$smax)

  # Map intensity to a color via each sensor's white -> base ramp. Vectorized
  # per sensor (a handful) rather than row-by-row.
  ramps <- lapply(palette, function(base) {
    grDevices::colorRampPalette(c("#ffffff", base))(101)
  })
  idx <- 1L + floor(100 * agg$t)
  agg$fill_col <- NA_character_
  for (s in names(ramps)) {
    sel <- agg$sensor == s
    agg$fill_col[sel] <- ramps[[s]][idx[sel]]
  }

  # Bin index -> POSIXct bin start.
  agg$bin <- as.POSIXct(agg$bin * bin_sec, origin = "1970-01-01", tz = tz)

  k <- nlevels(df$sensor)
  wk <- week_breaks(agg$bin)

  p <- ggplot2::ggplot(agg) +
    ggplot2::geom_tile(
      ggplot2::aes(
        x = .data$bin, y = as.numeric(.data$sensor),
        fill = .data$fill_col
      ),
      width = bin_sec, height = 0.92
    ) +
    ggplot2::scale_fill_identity(guide = "none")

  if (!is.null(wk)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = wk, colour = "grey30",
        linetype = "dashed", linewidth = 0.45
      ) +
      ggplot2::scale_x_datetime(
        breaks = wk, date_labels = "%d-%b", expand = c(0, 0)
      )
  } else {
    p <- p + ggplot2::scale_x_datetime(expand = c(0, 0))
  }

  p +
    raster_facets(has_group) +
    ggplot2::scale_y_continuous(
      limits = c(0.5, k + 0.5), expand = c(0, 0),
      breaks = seq_len(k), labels = levels(df$sensor)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    sampling_raster_theme()
}

# One panel per ID (when grouped); sensors live as lanes inside the panel.
# drop = FALSE keeps tags with no data visible as empty rows.
raster_facets <- function(has_group) {
  if (has_group) {
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$group), switch = "y", drop = FALSE
    )
  } else {
    NULL
  }
}

# Weekly dashed-guide / axis-break positions, anchored to midnight. Returns
# NULL for spans under two weeks so the default datetime breaks take over.
week_breaks <- function(times) {
  rng <- range(times, na.rm = TRUE)
  if (!all(is.finite(rng)) || as.numeric(diff(rng), units = "days") < 14) {
    return(NULL)
  }
  tz <- attr(times, "tzone") %||% "UTC"
  start <- as.POSIXct(format(rng[1], "%Y-%m-%d"), tz = tz)
  seq(from = start, to = rng[2], by = "1 week")
}

sampling_raster_theme <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = "grey80", fill = NA, linewidth = 0.3
      ),
      panel.spacing.y = grid::unit(0.05, "lines"),
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 0)
    )
}

# Coerce a bin-width spec to seconds. Accepts numeric (already seconds),
# difftime, or a "<number> <unit>" string.
as_bin_seconds <- function(bin) {
  if (is.numeric(bin)) return(as.numeric(bin))
  if (inherits(bin, "difftime")) return(as.numeric(bin, units = "secs"))
  if (is.character(bin)) {
    parts <- strsplit(trimws(bin), "\\s+")[[1]]
    if (length(parts) != 2) {
      rlang::abort("`bin` string must look like \"1 hour\" or \"30 min\".")
    }
    val <- suppressWarnings(as.numeric(parts[1]))
    if (is.na(val)) rlang::abort("`bin` value must be numeric.")
    mult <- c(
      sec = 1, secs = 1, second = 1, seconds = 1,
      min = 60, mins = 60, minute = 60, minutes = 60,
      hour = 3600, hours = 3600, hr = 3600, hrs = 3600,
      day = 86400, days = 86400
    )[parts[2]]
    if (is.na(mult)) {
      rlang::abort(paste0("Unrecognized `bin` unit: \"", parts[2], "\""))
    }
    return(val * mult)
  }
  rlang::abort("`bin` must be numeric, difftime, or a character string.")
}
