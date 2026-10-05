#' Dynamic body acceleration (VeDBA and ODBA)
#'
#' @description
#' Calculate vectorial dynamic body acceleration (VeDBA) or overall dynamic
#' body acceleration (ODBA) for each burst in an `acc` vector. Dynamic body
#' acceleration (DBA) is a widely used proxy for movement-related activity.
#'
#' Acceleration recorded on each axis is treated as the sum of a *static*
#' component (the share of gravity on that axis, which reflects the
#' orientation of the tag and thus the animal's posture) and a *dynamic*
#' component (acceleration caused by the animal's own movement). For each
#' sample, the static component on each axis is estimated as the mean of the
#' surrounding samples within a centered window of length `window` (or of the
#' whole burst, if `window = "burst"`). Subtracting it from the recorded values
#' leaves the dynamic component for each sample. The axes are then combined
#' for each sample as follows:
#'
#' - VeDBA: \eqn{\sqrt{x^2 + y^2 + z^2}}
#' - ODBA: \eqn{|x| + |y| + |z|}
#'
#' at which point the results are averaged to give a single
#' value per burst.
#'
#' @param x An `acc` vector.
#' @param window Length of the running mean used to estimate the static
#'   acceleration component. Either a [`units`][units::units] object convertible
#'   to seconds or `"burst"` to use the mean of the entire burst as the static
#'   component. Bare numeric values are assumed to be in seconds. See details.
#'
#' @details
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
#' samples.
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
#' end of a burst have no static estimate and are excluded from that burst's
#' mean. Likewise, a missing sample leaves the static component undefined for
#' every sample whose window includes it, and those samples are also excluded.
#'
#' DBA cannot be calculated (and `NA` is returned) for bursts that are shorter
#' than `window`, for windows of fewer than 3 samples, and for bursts with fewer
#' than 2 samples when `window = "burst"`.
#'
#' @returns A vector the same length as `x` containing one value per burst,
#'   in the units of `x` (unitless if `x` has no units).
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
#' a <- set_imu_units(acc_example(), "standard_free_fall")
#'
#' # Static component estimated with a 0.5 second running mean
#' vedba(a, window = units::set_units(0.5, "s"))
#'
#' # Bare numbers are interpreted as seconds
#' odba(a, window = 0.5)
#'
#' # Static component estimated as the mean of each burst
#' vedba(a, window = "burst")
#'
#' # One minute of example 20Hz data with 4Hz "wingbeats".
#' t <- seq(0, 60 - 1 / 20, by = 1 / 20)
#' strength <- rep(c(0.6, 0.4, 0.1, 0.05, 0.2, 0.5), each = 200)
#'
#' raw <- acc(
#'   list(cbind(X = 2048, Y = 2048, Z = 2048 + 512 * (1 + strength * sin(2 * pi * 4 * t)))),
#'   frequency = units::set_units(20, "Hz"),
#'   start = as.POSIXct("2024-01-01", tz = "UTC")
#' )
#'
#' # Calibrate to a standard physical unit before computing DBA
#' cal <- transform_imu(
#'   raw,
#'   acc_calibration(offset = 2048, slope = 1 / 512, units = "standard_free_fall")
#' )
#'
#' # DBA produces a single estimate per burst. For long-running continuous data,
#' # you likely want to split data into intervals for which you want a DBA
#' # estimate. To avoid data loss, intervals should be appreciably larger than
#' # the window.
#' pieces <- purrr::list_c(split_imu(cal, units::set_units(10, "s")))
#'
#' data.frame(start = starts(pieces), vedba = vedba(pieces, window = 1))
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

# Shared implementation for vedba() and odba(). `.norm` combines a matrix of
# per-axis dynamic acceleration (samples in rows) into one value per sample.
dba_ <- function(x, window, .norm, call = rlang::caller_env()) {
  if (!is_acc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls acc} vector.", call = call)
  }

  window <- window_to_sec(window, call = call)

  if (length(x) == 0) {
    return(numeric(0))
  }

  x_na <- is.na(x)
  out <- rep(NA_real_, length(x))

  if (all(x_na)) {
    return(out)
  }

  # Number of samples in each burst's window. NULL means use the whole burst.
  n <- n_samples(x)

  if (is.null(window)) {
    k <- NULL
    too_short <- !x_na & n < 2
  } else {
    k <- dba_window_samples(window, as.numeric(freqs(x)))
    too_short <- !x_na & (is.na(k) | k < 3 | k > n)
  }

  if (any(too_short)) {
    cli::cli_warn(c(
      "Returning NA for {sum(too_short)} {cli::qty(sum(too_short))}burst{?s} too short for the requested {.arg window}.",
      "i" = "A window must span at least 3 samples and no more than the burst."
    ))
  }

  keep <- !x_na & !too_short

  if (!any(keep)) {
    return(out)
  }

  dba_keep <- purrr::list_simplify(
    purrr::map2(
      bursts(x)[keep],
      if (is.null(k)) list(NULL) else k[keep],
      function(.br, .k) dba_burst_(.br, .k, .norm)
    )
  )

  if (inherits(dba_keep, "units")) {
    out <- units::set_units(out, units(dba_keep), mode = "standard")
  }

  out[keep] <- dba_keep

  out
}

# DBA for a single burst. `k` is the running mean length in samples, or NULL
# to use the burst mean as the static component.
dba_burst_ <- function(b, k, .norm) {
  u <- if (inherits(b, "units")) units(b) else NULL

  if (!is.null(u)) {
    b <- units::drop_units(b)
  }

  if (is.null(k)) {
    static <- matrix(colMeans(b), nrow(b), ncol(b), byrow = TRUE)
  } else {
    # Running mean from k samples centered on each sample in the axis.
    # Samples within k %/% 2 of either end get NA.
    static <- apply(
      b,
      2,
      function(axis) stats::filter(axis, rep(1 / k, k), sides = 2)
    )
  }

  dba <- mean(.norm(b - static), na.rm = TRUE)

  # All samples excluded (e.g. due to missing values)
  if (is.nan(dba)) {
    dba <- NA_real_
  }

  if (!is.null(u)) {
    dba <- units::set_units(dba, u, mode = "standard")
  }

  dba
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

  window
}

# Convert a window in seconds to the nearest odd number of samples for each
# frequency (in Hz), so that the window can be centered on each sample. An
# exact even number of samples is rounded up. The product is rounded first so
# that floating point noise just below an even number doesn't change the
# result (e.g. 0.58 * 100 = 57.9999... would otherwise give 57
# samples instead of 59).
dba_window_samples <- function(window, freq) {
  2L * as.integer(floor(round(window * freq, 6) / 2)) + 1L
}
