#' Example `acc` vector
#'
#' A simple `acc` vector for use in examples.
#'
#' @returns An `acc` vector with two bursts.
#'
#' @keywords internal
#' @export
acc_example <- function() {
  acc(
    list(
      cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
      cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3), Z = 1)
    ),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
}

# Helper to snap a computed frequency to a stable precision.
#
# Both parsing (expanded-format data) and merging derive a burst's frequency
# from its timestamp span: (n_samples - 1) / (last_time - first_time). POSIXct
# stores time as seconds since 1970, but floating point representation
# can only resolve so much precision for these large numbers. This leads to
# small irregularities in frequencies after derivation.
#
# This noise also scales with the sampling frequency. We use signif() to
# avoid applying the uniform correction of round(), which would not account
# for this fact. 6 significant figures clears the noise floor for bursts in
# normal frequency ranges (up to a few hundred Hz). Users can otherwise
# do their own normalization post-hoc if this is not sufficient to make
# their bursts uniform in frequency due to noise.
snap_freq <- function(x, digits = 6) {
  signif(x, digits = digits)
}

# Absolute floating-point noise floor for POSIXct-derived time differences, in
# seconds. POSIXct is a double count of seconds since 1970; one ULP at a
# contemporary epoch (~1.77e9 s) is ~4e-7 s, so a difference of two timestamps
# carries ~sub-microsecond noise regardless of the sampling rate. Relative
# frequency comparisons (which divide by the sample period) inflate this noise
# at high sampling rates, so those comparisons are backstopped with this floor:
# a deviation must exceed both the relative `rate_tol` AND this absolute
# floor to count as a real rate change. This keeps sub-microsecond timestamp
# jitter from being mistaken for a rate change on fast (e.g. >1 kHz) data, while
# only ever declining to resolve rate differences too small for POSIXct to
# represent anyway.
fp_time_floor <- 1e-6

# From dplyr
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# Check if a scalar value is NULL or NA
null_or_na <- function(x) {
  is.null(x) || rlang::is_na(x)
}

# Return the first scalar value in `...` that is not NULL or NA.
# Need to handle NA because NA values may be passed via data.frame col vals in
# as_acc_calibration()
first_valid <- function(...) {
  for (v in list(...)) {
    if (!is.null(v) && !rlang::is_na(v)) {
      return(v)
    }
  }
  NULL
}
