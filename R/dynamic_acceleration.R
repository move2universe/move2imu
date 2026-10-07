#' Compute per-burst dynamic body acceleration
#'
#' @description
#' Calculate vectorial dynamic body acceleration (VeDBA) or overall dynamic
#' body acceleration (ODBA) for each burst in an `acc` vector. Dynamic body
#' acceleration (DBA) is a widely used proxy for movement-related activity.
#'
#' `vedba()` and `odba()` extract the dynamic component of the input
#' acceleration values (see details) and summarize these samples across axes as
#' follows:
#'
#' - VeDBA: \eqn{\sqrt{x^2 + y^2 + z^2}}
#' - ODBA: \eqn{|x| + |y| + |z|}
#'
#' These values are then averaged to give a single value per burst.
#'
#' These functions are intended to provide burst-level DBA
#' summaries. For analyses that need per-sample dynamic and/or static
#' acceleration values, use [dynamic_acc()] and [static_acc()] instead.
#'
#' @param x An `acc` vector.
#' @param window Length of the running mean used to estimate the static
#'   acceleration component. Either a [`units`][units::units] object convertible
#'   to seconds, a [`difftime`][base::difftime] object, or `"burst"` to use the
#'   mean of the entire burst as the static component. Bare numeric values are
#'   assumed to be in seconds. See details.
#'
#' @details
#' ## Static and dynamic acceleration
#'
#' Acceleration recorded on each axis is treated as the sum of a *static*
#' component (the share of gravity on that axis, which reflects the
#' orientation of the tag and thus the animal's posture) and a *dynamic*
#' component (acceleration typically caused by the animal's own movement).
#' For each sample, the static component on each axis is estimated as the
#' mean of the surrounding samples within a centered window of length `window`
#' (or of the whole burst, if `window = "burst"`). Subtracting it from the
#' recorded values leaves the dynamic component for each sample.
#'
#' ## Choosing a window
#'
#' The running mean separates the static from the dynamic component by
#' averaging over complete movement cycles (e.g. wingbeats or strides): within
#' a full cycle, the dynamic component averages to approximately zero, leaving
#' the slowly varying static component. The window therefore defines what
#' counts as "movement" (anything faster than the window) and what counts as
#' "posture" (anything slower).
#'
#' - If the window is shorter than a movement cycle, part of the movement is
#'   treated as posture and DBA is **underestimated**.
#' - If the window is much longer than the timescale of posture changes, those
#'   changes are treated as movement and DBA is **overestimated**.
#'
#' Shepard et al. (2008) recommend a window of at least 3 seconds for animals
#' with a stroke period up to 3 seconds, and, for slower strokes, at least one
#' (and preferably two) stroke cycles. See Shepard et al. (2008) for more
#' details and `vignette("programming", package = "move2imu")` for a
#' worked example.
#'
#' `window = "burst"` uses the mean of each whole burst as the static
#' component, so the effective window is each burst's duration. This can be
#' useful for short bursts, where a running mean would leave few or no
#' samples, but is unlikely to be useful for long recordings of
#' continuously-sampled data.
#'
#' Note that this means that bursts that differ in duration will have DBA
#' calculated with different window sizes, impacting comparability. Further,
#' bursts less than a second or two may be too
#' short to reliably separate posture from movement. This method can also
#' count posture changes as movement for long bursts, in which case a
#' numeric window should be provided.
#'
#' ## Applying the smoothing window
#'
#' `window` is converted to the nearest odd number of samples (so that it
#' can be centered on each sample) using each burst's sampling frequency. An
#' even number of samples is rounded up. Samples within half a window of either
#' end of a burst have no static estimate. Likewise, a missing sample leaves
#' the static component undefined for every sample whose window includes it.
#'
#' The components cannot be calculated (and return missing values) for
#' bursts that are shorter than `window`, for windows of fewer than 3
#' samples, and for bursts with fewer than 2 samples when `window = "burst"`.
#'
#' @returns A vector of VeDBA or ODBA values with the same length and units as
#'   `x`. Mixed units are converted to the units of the first non-missing burst
#'   in `x`.
#'
#' @references
#' Shepard, E. L. C., Wilson, R. P., Halsey, L. G., Quintana, F.,
#' Gómez Laich, A., Gleiss, A. C., Liebsch, N., Myers, A. E., & Norman, B.
#' (2008). Derivation of body motion via appropriate smoothing of acceleration
#' data. *Aquatic Biology*, 4, 235–241. \doi{10.3354/ab00104}
#'
#' Wilson, R. P., Börger, L., Holton, M. D., Scantlebury, D. M.,
#' Gómez-Laich, A., Quintana, F., et al. (2020). Estimates for energy
#' expenditure in free-living animals using acceleration proxies: A
#' reappraisal. *Journal of Animal Ecology*, 89, 161–172.
#' \doi{10.1111/1365-2656.13040}
#'
#' @name dba
#'
#' @examples
#' a <- as_acc(albatrosses())
#'
#' a <- transform_imu(
#'   a,
#'   acc_calibration("eobs", 1000, units = "standard_free_fall")
#' )
#'
#' # Static component estimated with a 3 second running mean
#' vedba(a, window = units::set_units(3, "s"))
#'
#' # Bare numbers are interpreted as seconds
#' odba(a, window = 3)
#'
#' # Burst mean is used as the static acceleration for each burst
#' vedba(a, window = "burst")
NULL

#' @rdname dba
#' @export
vedba <- function(x, window) {
  dba_(x, window, .norm = function(d) sqrt(rowSums(d^2)))
}

#' @rdname dba
#' @export
odba <- function(x, window) {
  dba_(x, window, .norm = function(d) rowSums(abs(d)))
}

#' Decompose acceleration into static and dynamic components
#'
#' @description
#' Separate each sample in an `acc` vector into its static component
#' (`static_acc()`) and its dynamic component (`dynamic_acc()`).
#'
#' These are intended for analyses that need dynamic and/or static acceleration
#' values for individual samples. For burst-level dynamic body acceleration
#' summaries, use [vedba()] and [odba()] instead.
#'
#' @inheritParams dba
#'
#' @inherit dba details
#'
#' @returns An `acc` vector with the same length and burst structure as `x`:
#'   each burst keeps its sample count, start time and units. Samples without
#'   a static estimate (see details) are `NA`.
#'
#' @inherit dba references
#'
#' @export
#'
#' @examples
#' # Two minutes of simulated continuous 20 Hz data from a bird alternating
#' # between 30 seconds of gliding and 30 seconds of flapping (4 Hz wingbeats)
#' set.seed(1)
#' secs <- seq(0, 120 - 1 / 20, by = 1 / 20)
#' flapping <- (secs %/% 30) %% 2 == 1
#' wingbeats <- ifelse(flapping, 0.4 * sin(2 * pi * 4 * secs), 0)
#'
#' a <- acc(
#'   list(cbind(
#'     X = rnorm(length(secs), sd = 0.05),
#'     Y = rnorm(length(secs), sd = 0.05),
#'     Z = 1 + wingbeats + rnorm(length(secs), sd = 0.05)
#'   )),
#'   frequency = units::set_units(20, "Hz"),
#'   start = as.POSIXct("2024-01-01", tz = "UTC")
#' )
#'
#' a <- set_imu_units(a, "standard_free_fall")
#'
#' # The dynamic acc component with a 1 second smoothing window
#' d_acc <- dynamic_acc(a, window = 1)
#' d_acc
#'
#' if (rlang::is_installed("dygraphs")) {
#'   plot_imu_trace(d_acc)
#' }
#'
#' # Static component with same window
#' s_acc <- static_acc(a, window = 1)
#' s_acc
#'
#' if (rlang::is_installed("dygraphs")) {
#'   plot_imu_trace(s_acc)
#' }
#'
#' # Split into 10 second chunks to calculate periodic VeDBA
#' d_split <- split_imu(
#'   drop_imu_units(d_acc),
#'   units::set_units(10, "s"),
#'   flatten = TRUE
#' )
#'
#' data.frame(
#'   start = starts(d_split),
#'   vedba = purrr::map_dbl(
#'     bursts(d_split),
#'     function(b) mean(sqrt(rowSums(b^2)), na.rm = TRUE)
#'   )
#' )
static_acc <- function(x, window) {
  decompose_acc(x, window, component = "static")
}

#' @rdname static_acc
#' @export
dynamic_acc <- function(x, window) {
  decompose_acc(x, window, component = "dynamic")
}

# Shared implementation for vedba() and odba(). Separates the dynamic
# component of each burst, then `.norm` combines its axes (samples in rows)
# into one value per sample, which are averaged to give one value per burst.
dba_ <- function(x, window, .norm, call = rlang::caller_env()) {
  d <- bursts(decompose_acc(x, window, component = "dynamic", call = call))

  out <- rep(NA_real_, length(x))
  present <- !purrr::map_lgl(d, is.null)

  if (!any(present)) {
    return(out)
  }

  d <- d[present]
  has_units <- purrr::map_lgl(d, inherits, what = "units")

  vals <- purrr::map_dbl(
    d,
    function(b) {
      if (inherits(b, "units")) {
        b <- units::drop_units(b)
      }

      dba <- mean(.norm(b), na.rm = TRUE)

      # If DBA can't be calculated (e.g. bursts too short for the window, or
      # missing values), we get NaN. Convert to NA for consistency.
      if (is.nan(dba)) NA_real_ else dba
    }
  )

  if (!any(has_units)) {
    out[present] <- vals
    return(out)
  }

  if (!all(has_units)) {
    cli::cli_abort(
      "Can't combine bursts with and without units in {.arg x}.",
      call = call
    )
  }

  # For each unique unit present, identify the entries that have it and convert
  # to the standard output unit (from the first burst)
  burst_units <- purrr::map_chr(d, units::deparse_unit)
  out_units <- burst_units[1]

  for (u in unique(burst_units)) {
    idx <- burst_units == u

    # Attach original units to each burst (stripped for arithmetic above)
    v <- units::set_units(vals[idx], u, mode = "standard")

    # Convert to consistent output units across all bursts
    vals[idx] <- as.numeric(units::set_units(v, out_units, mode = "standard"))
  }

  out[present] <- vals
  units::set_units(out, out_units, mode = "standard")
}

# Shared implementation for static_acc() and dynamic_acc(). Splits each burst
# into its static and dynamic components and returns the requested one as an
# `acc` vector. Bursts too short for the window are filled with NA, so that
# the output keeps the same bursts, start times and sample counts as `x`.
decompose_acc <- function(x,
                          window,
                          component = c("static", "dynamic"),
                          call = rlang::caller_env()) {
  component <- rlang::arg_match(component)

  if (!is_acc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls acc} vector.", call = call)
  }

  window <- window_to_sec(window, call = call)

  # Window length in samples for each burst. NULL means use the whole burst.
  n <- n_samples(x)

  if (is.null(window)) {
    window_samples <- rep(list(NULL), length(x))
    too_short <- !is.na(x) & n < 2
  } else {
    window_samples <- samples_in_window(window, as.numeric(freqs(x)))
    too_short <- !is.na(x) &
      (is.na(window_samples) | window_samples < 3 | window_samples > n)
  }

  if (any(too_short)) {
    cli::cli_warn(c(
      "Returning NA for {sum(too_short)} {cli::qty(sum(too_short))}burst{?s} too short for the requested {.arg window}.",
      "i" = "A window must span at least 3 samples and no more than the burst."
    ))
  }

  out <- purrr::pmap(
    list(bursts(x), window_samples, too_short),
    function(b, ws, ts) {
      if (is.null(b)) {
        return(NULL)
      }

      u <- if (inherits(b, "units")) units(b) else NULL

      if (!is.null(u)) {
        b <- units::drop_units(b)
      }

      if (ts) {
        b[] <- NA_real_
      } else if (component == "static") {
        b <- static_acc_(b, ws)
      } else {
        b <- b - static_acc_(b, ws)
      }

      if (!is.null(u)) {
        b <- units::set_units(b, u, mode = "standard")
      }

      b
    }
  )

  bursts(x) <- new_burst_list(out, sensor = "acc")

  x
}

# Static component of a unitless burst matrix. `window_samples` is the running
# mean length in samples, or NULL to use the burst mean for every sample.
static_acc_ <- function(b, window_samples) {
  if (is.null(window_samples)) {
    return(
      matrix(
        colMeans(b),
        nrow(b),
        ncol(b),
        byrow = TRUE,
        dimnames = dimnames(b)
      )
    )
  }

  # Running mean from `window_samples` samples centered on each sample in the
  # axis. Samples within `window_samples %/% 2` of either end get NA.
  weights <- rep(1 / window_samples, window_samples)

  static <- apply(
    b,
    2,
    function(axis) as.numeric(stats::filter(axis, weights, sides = 2))
  )

  # apply() drops the matrix shape for single-sample bursts
  matrix(static, nrow(b), ncol(b), dimnames = dimnames(b))
}

# Validate `window` and convert to a bare number of seconds. Returns NULL for
# `window = "burst"`.
window_to_sec <- function(window, call = rlang::caller_env()) {
  if (is.character(window)) {
    if (identical(window, "burst")) {
      return(NULL)
    }

    cli::cli_abort(
      "{.arg window} must be a duration or {.val burst}, not {.val {window}}.",
      call = call
    )
  }

  if (inherits(window, "difftime")) {
    window <- as.numeric(window, units = "secs")
  }

  if (!is.numeric(window) || length(window) != 1 || is.na(window)) {
    cli::cli_abort(
      "{.arg window} must be a single duration or {.val burst}.",
      call = call
    )
  }

  if (inherits(window, "units")) {
    window <- units::set_units(window, "s", mode = "standard")
  }

  window <- as.numeric(window)

  if (!(window > 0)) {
    cli::cli_abort("{.arg window} must be greater than 0.", call = call)
  }

  if (!is.finite(window)) {
    cli::cli_abort("{.arg window} must be finite.", call = call)
  }

  window
}

# Convert a window in seconds to the nearest odd number of samples for each
# frequency (in Hz), so that the window can be centered on each sample. An
# exact even number of samples is rounded up. The product is rounded first so
# that floating point noise just below an even number doesn't change the
# result (e.g. 0.58 * 100 = 57.9999... would otherwise give 57
# samples instead of 59).
samples_in_window <- function(window, freq) {
  2L * as.integer(floor(round(window * freq, 6) / 2)) + 1L
}
