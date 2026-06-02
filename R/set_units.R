#' Set or convert units on IMU burst data
#'
#' @description
#' Methods to attach or convert units on the
#' burst matrices of `acc`, `mag`, and `gyro` vectors. Each method validates
#' that the target unit is dimensionally compatible with its IMU class:
#'
#' - `acc`: acceleration units (e.g., `"m/s^2"`, `"standard_free_fall"`)
#' - `mag`: magnetic flux density units (e.g., `"tesla"`, `"uT"`, `"gauss"`)
#' - `gyro`: angular velocity units (e.g., `"rad/s"`, `"degree/s"`)
#'
#' To transform raw values to physical units rather than simply attaching or
#' converting units, use [transform_imu()].
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`)
#' @param value Character specifying the target units (e.g., `"m/s^2"`). For
#'   units in terms of gravitational acceleration, use `"standard_free_fall"`.
#' @param ... Unused.
#'
#' @returns The input vector with units attached to each burst matrix.
#'
#' @seealso [transform_imu()] to transform raw IMU values
#'
#' @export
#'
#' @examples
#' a <- acc_example()
#'
#' # Attach units to unitless bursts
#' set_imu_units(a, "m/s^2")
#'
#' # Convert between units
#' a_ms2 <- set_imu_units(a, "m/s^2")
#' set_imu_units(a_ms2, "standard_free_fall")
#'
#' # Units must be appropriate for the sensor type of the input
#' try(set_imu_units(a, "kg"))
set_imu_units <- function(x, value, ...) {
  UseMethod("set_imu_units")
}

#' @export
set_imu_units.acc <- function(x, value, ...) {
  set_imu_units_(x, value, reference = "m/s^2", sensor = "acc")
}

#' @export
set_imu_units.mag <- function(x, value, ...) {
  set_imu_units_(x, value, reference = "tesla", sensor = "mag")
}

#' @export
set_imu_units.gyro <- function(x, value, ...) {
  set_imu_units_(x, value, reference = "degree/s", sensor = "gyro")
}

# Also include methods for units::set_units, which may be some users' intuition:

#' @exportS3Method units::set_units
set_units.acc <- function(x, value, ...) {
  set_imu_units(x, value)
}

#' @exportS3Method units::set_units
set_units.mag <- function(x, value, ...) {
  set_imu_units(x, value)
}

#' @exportS3Method units::set_units
set_units.gyro <- function(x, value, ...) {
  set_imu_units(x, value)
}

set_imu_units_ <- function(x, value, reference, sensor) {
  assertthat::assert_that(is.character(value), length(value) == 1)
  
  can_convert <- units::ud_are_convertible(reference, value)
  
  if (!can_convert) {
    rlang::abort(c(
      paste0(value, " units not valid for `", sensor, "` vector."),
      i = paste0("Units must be convertible to ", reference)
    ))
  }
  
  bursts_converted <- purrr::map(
    bursts(x), 
    function(b) {
      if (is.null(b)) {
        return(NULL)
      }
      
      nms <- colnames(b)
      b <- units::set_units(b, value, mode = "standard")
      colnames(b) <- nms
      b
    }
  )
  
  bursts(x) <- new_burst_list(bursts_converted, sensor = sensor)
  
  x
}
