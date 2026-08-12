#' Apply a sensor calibration to an IMU vector
#'
#' @description
#' Transforms raw values from an IMU sensor to physical units (e.g., meters
#' per second squared) using a specified calibration.
#'
#' Use [acc_calibration()] to create a calibration for `acc` vectors.
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`).
#' @param calibration An `imu_calibration` object whose subclass matches the
#'   sensor type of `x`. Must be the same length as `x` or length 1, in which
#'   case the calibration is recycled to all elements of `x`.
#'
#'   Currently, only [acc_calibration()] is supported.
#'
#' @details
#' An `acc_calibration` object may contain missing (`NA`) elements (e.g. if
#' produced by [as_acc_calibration()]). `transform_imu()` returns `NA` in such
#' cases and emits a warning if any bursts are lost because of missing
#' calibration specifications.
#'
#' If an `acc_calibration` only has calibration parameters for certain
#' axes (e.g. `offset_x = 2048` and `slope_x = 0.001`), then only those axes
#' will be transformed by [transform_imu()]. Values for other axes will be
#' converted to `NA`. The dimension of the input burst matrices therefore
#' remains the same.
#'
#' @return An IMU vector of the same length as `x`, with each burst transformed
#'   by the corresponding calibration.
#'
#' @seealso [acc_calibration()] to construct an accelerometer calibration.
#'
#' @export
#'
#' @examples
#' a <- acc_example()
#'
#' # Transform values using the standard Ornitela calibration formula
#' transform_imu(a, acc_calibration("ornitela"))
#'
#' # Transform values using a set of custom acc calibrations.
#' # Calibrations will be mapped to the input IMU vector by index.
#' transform_imu(
#'   a,
#'   acc_calibration(offset = c(2048, 2046), slope = c(0.001, 0.002))
#' )
transform_imu <- function(x, calibration) {
  assert_imu(x)

  if (!inherits(calibration, "imu_calibration")) {
    cli::cli_abort(c(
      "{.arg calibration} must be an {.cls imu_calibration} object.",
      "i" = "Use e.g. {.help [{.fn acc_calibration}](move2imu::acc_calibration)} to create one."
    ))
  }

  sensor <- class(x)[1]
  expected <- paste0(sensor, "_calibration")

  if (!inherits(calibration, expected)) {
    cli::cli_abort(c(
      "Cannot apply {.cls {class(calibration)[1]}} to an {.cls {sensor}} vector.",
      "i" = "Expected an {.cls {expected}} object."
    ))
  }

  calibration <- vctrs::vec_recycle(calibration, length(x))
  missing_cal <- vctrs::vec_detect_missing(calibration)

  # Burst with data but no calibration becomes NA. Warn to avoid this
  # going unnoticed.
  uncalibrated <- missing_cal & !is.na(x)

  if (any(uncalibrated)) {
    cli::cli_warn(
      paste0(
        "Returning NA for {sum(uncalibrated)} {cli::qty(sum(uncalibrated))}burst{?s} ",
        "with data but no calibration."
      )
    )
  }

  br <- bursts(x)
  out <- vector("list", length(br))
  n_w_units <- 0L
  n_lost <- 0L

  # Build burst transformer once for each unique calibration. This avoids
  # cost of setting up duplicate transformation functions for each burst.
  # NB: highest cost is parsing the calibration's units string.
  unique_cal <- vctrs::vec_group_loc(calibration)

  # Bursts with no calibration are left NULL, and so become NA.
  unique_cal <- vctrs::vec_slice(
    unique_cal,
    !vctrs::vec_detect_missing(unique_cal$key)
  )

  # Iterate over each unique calibration function
  for (cal in seq_len(nrow(unique_cal))) {
    transform <- burst_transformer(unique_cal$key[cal])

    # Iterate over bursts that belong to this calibration function
    for (i in unique_cal$loc[[cal]]) {
      burst <- br[[i]]

      if (rlang::is_empty(burst) || rlang::is_na(burst)) {
        # Nothing to calibrate (empty/NA burst): pass through unchanged.
        out[i] <- list(burst)
      } else if (inherits(burst, "units")) {
        # Refuse to recalibrate values that already carry units.
        n_w_units <- n_w_units + 1L
        out[i] <- list(burst)
      } else {
        burst_tfrm <- transform(burst)

        # If calibration has introduced NAs (usually because of missing
        # calibration params for a certain axis), keep a record and warn later
        if (anyNA(burst_tfrm) && !anyNA(burst)) {
          n_lost <- n_lost + 1L
        }

        out[[i]] <- burst_tfrm
      }
    }
  }

  if (n_w_units > 0L) {
    cli::cli_warn(
      paste0(
        "Cannot calibrate {n_w_units} {cli::qty(n_w_units)}burst{?s} with ",
        "pre-existing units. Returning input."
      )
    )
  }

  if (n_lost > 0L) {
    cli::cli_warn(c(
      "Calibration produced NA values in {n_lost} {cli::qty(n_lost)}burst{?s}.",
      "i" = "Did you specify calibration parameters for all recorded axes?"
    ))
  }

  bursts(x) <- new_burst_list(out, sensor = sensor)

  # Sync metadata so uncalibrated elements are fully missing (`is.na()` agrees)
  if (any(missing_cal)) {
    x <- vctrs::vec_assign(x, which(missing_cal), vctrs::vec_init(x, 1L))
  }

  x
}

# Build a function that applies a single calibration to a burst.
#
# `burst_transformer()` is the intermediary that handles heterogeneity in the
# way parameters stored in a calibration object are converted into a
# function that maps raw values to physical units. Different sensors can
# implement different methods to convert calibrations for those sensors into
# functions that will be applied to an individual burst to convert values.
#
# `transform_imu()` facilitates the dispatch of these transformations across all
# bursts in an imu object.
#
# `calibration` is a length-1 calibration record. The returned function takes a
# numeric matrix of raw values with axis columns (e.g. "X", "Y", "Z").
burst_transformer <- function(calibration, ...) {
  UseMethod("burst_transformer")
}

# Acc calibrations are linear transformations of the form
# (raw - offset) * slope * orientation. The output preserves the burst's
# columns: only values and units change. Columns the calibration has no
# parameters for become NA.
#' @export
burst_transformer.acc_calibration <- function(calibration, ...) {
  f <- vctrs::vec_data(calibration)

  offsets <- c(X = f$offset_x, Y = f$offset_y, Z = f$offset_z)
  scales <- c(X = f$slope_x, Y = f$slope_y, Z = f$slope_z) *
    c(X = f$orientation_x, Y = f$orientation_y, Z = f$orientation_z)

  unit <- units(units::as_units(f$units))

  function(burst) {
    # Preserve burst columns and align calibration params to them by axis name
    axes <- colnames(burst)
    offset <- offsets[axes]
    scale <- scales[axes]

    # Apply calibration
    n <- nrow(burst)
    tfrm <- (burst - rep(offset, each = n)) * rep(scale, each = n)

    if (f$units == "m/s^2") {
      tfrm <- tfrm * GRAV_CONST
    }

    units(tfrm) <- unit
    tfrm
  }
}
