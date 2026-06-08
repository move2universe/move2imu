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
      "Returning NA for {sum(uncalibrated)} {cli::qty(sum(uncalibrated))}burst{?s} with data but no calibration."
    )
  }
  
  bursts(x) <- new_burst_list(
    purrr::map2(
      bursts(x),
      vctrs::vec_chop(calibration),
      function(.br, .cal) {
        # No calibration for this element: drop the burst (becomes NA).
        if (vctrs::vec_detect_missing(.cal)) {
          return(NULL)
        }
        # Nothing to calibrate (empty/NA burst): pass through unchanged.
        if (rlang::is_empty(.br) || rlang::is_na(.br)) {
          return(.br)
        }
        # Refuse to recalibrate values that already carry units.
        if (inherits(.br, "units")) {
          cli::cli_warn(
            "Cannot calibrate values that already contain units. Returning input."
          )
          return(.br)
        }
        transform_burst(.cal, .br)
      }
    ),
    sensor = sensor
  )
  
  # Sync metadata so uncalibrated elements are fully missing (`is.na()` agrees)
  if (any(missing_cal)) {
    x <- vctrs::vec_assign(x, which(missing_cal), vctrs::vec_init(x, 1L))
  }
  
  x
}

# Apply a single calibration to a single burst.
#
# `transform_burst()` is the intermediary that handles heterogeneity in the
# way parameters stored in a calibration object are converted into a
# function that maps raw values to physical units. Different sensors can
# implement different methods to convert calibrations for those sensors into
# functions that will be applied to an individual burst to convert values.
#
# transform_imu() facilitates the dispatch of these transformations across all
# bursts in an imu object.
#
# `calibration` is a length-1 calibration record and `burst` is a numeric matrix
# of raw values with axis columns (e.g. "X", "Y", "Z").
transform_burst <- function(calibration, burst, ...) {
  UseMethod("transform_burst")
}

# Apply accelerometer calibration to a burst. Acc calibrations are linear
# transformations of the form (raw - offset) * slope * orientation
# `calibration` is a length-1 `acc_calibration`; `burst` is a raw numeric matrix
# with axis columns. The output preserves the burst's columns: only values and
# units change. Columns the calibration has no parameters for become NA.
#' @export
transform_burst.acc_calibration <- function(calibration, burst, ...) {
  f <- vctrs::vec_data(calibration)

  offset <- c(X = f$offset_x, Y = f$offset_y, Z = f$offset_z)
  scale <- c(X = f$slope_x, Y = f$slope_y, Z = f$slope_z) *
    c(X = f$orientation_x, Y = f$orientation_y, Z = f$orientation_z)

  # Preserve the burst's columns; align calibration params to them by axis name
  active_axes <- colnames(burst)
  offset <- offset[active_axes]
  scale <- scale[active_axes]

  # Warn if any of the burst's axes have no calibration parameters
  na_axes <- active_axes[is.na(offset) | is.na(scale)]
  if (length(na_axes) > 0) {
    cli::cli_warn(c(
      "Missing calibration parameters for {cli::qty(na_axes)}{?axis/axes} {.val {na_axes}}.",
      "!" = "{cli::qty(na_axes)}{?This axis/These axes} will produce NA values."
    ))
  }

  # Apply calibration
  xt <- sweep(burst[, active_axes, drop = FALSE], 2, offset, `-`)
  xt <- sweep(xt, 2, scale, `*`)
  if (f$units == "m/s^2") {
    xt <- xt * GRAV_CONST
  }

  colnames(xt) <- active_axes
  units::set_units(xt, f$units, mode = "standard")
}
