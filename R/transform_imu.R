#' Apply sensor calibration functions to an IMU vector
#'
#' @description
#' Transforms raw values from an IMU sensor to physical units (e.g., meters
#' per second squared) using a specified calibration function or set of 
#' calibration functions.
#' 
#' Use [acc_calibration()] to specify calibration functions for `acc` vectors.
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`).
#' @param calibration An `imu_calibration` object whose subclass matches the
#'   sensor type of `x`. Must be the same length as `x` or length 1, in which
#'   case the calibration function is recycled to all elements of `x`.
#'   
#'   Currently, only [acc_calibration()] is supported.
#'
#' @return An IMU vector of the same length as `x`, with each burst
#'   transformed by the corresponding calibration function.
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
#' # Transform values using a set of custom acc calibration functions.
#' # Calibrations will be mapped to the input IMU vector by index.
#' transform_imu(
#'   a,
#'   acc_calibration(offset = c(2048, 2046), slope = c(0.001, 0.002))
#' )
transform_imu <- function(x, calibration) {
  assert_imu(x)

  if (!inherits(calibration, "imu_calibration")) {
    rlang::abort(c(
      "`calibration` must be an `imu_calibration` object.",
      i = "Use e.g. `acc_calibration()` to create one."
    ))
  }

  sensor <- class(x)[1]
  expected <- paste0(sensor, "_calibration")

  if (!inherits(calibration, expected)) {
    rlang::abort(c(
      paste0(
        "Cannot apply `", class(calibration)[1],
        "` to an `", sensor, "` vector."
      ),
      i = paste0("Expected an `", expected, "` object.")
    ))
  }

  calibration <- vctrs::vec_recycle(calibration, length(x))

  bursts(x) <- new_burst_list(
    purrr::map2(
      bursts(x),
      calibration,
      function(.br, .calibrate) {
        .calibrate(.br)
      }
    ),
    sensor = sensor
  )

  x
}
