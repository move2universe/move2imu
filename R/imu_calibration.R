# Construct a calibration object for a given sensor type. The returned object
# is a list of per-burst transformation functions with a sensor-specific
# class (e.g. `acc_calibration`) and the shared `imu_calibration` parent class.
# The shared parent lets `transform_imu()` validate and dispatch uniformly
# across sensor types as more calibration constructors are added.
new_imu_calibration <- function(x, sensor) {
  rlang::arg_match(sensor, valid_imu_types())

  structure(
    x,
    class = c(paste0(sensor, "_calibration"), "imu_calibration", class(x))
  )
}

#' @export
print.imu_calibration <- function(x, ...) {
  cat(paste0("<", class(x)[1], "[", length(x), "]>\n"))
  invisible(x)
}

# Helper to collect assertions that apply to all calibration functions. These
# must pass empty/NA bursts through unchanged and refuse to re-calibrate bursts
# that already carry units. To be called in `*_calibration()` functions.
imu_calibration_fn <- function(fn) {
  function(x) {
    if (rlang::is_empty(x) || rlang::is_na(x)) {
      return(x)
    }

    if (inherits(x, "units")) {
      rlang::warn(
        "Cannot calibrate values that already contain units. Returning input."
      )
      return(x)
    }

    fn(x)
  }
}
