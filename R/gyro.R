# Concrete `gyro` subclass of `imu`. All shared record-level logic
# lives in R/imu.R; this file only holds the gyro-specific constructor,
# predicate, and S3 dispatch wrappers.

#' @rdname imu_constructors
#' @export
gyro <- function(bursts = list(),
                 frequency = units::set_units(double(), "Hz"),
                 start = NULL) {
  imu("gyro", bursts = bursts, frequency = frequency, start = start)
}

#' @export
#' @rdname explore-functions
is_gyro <- function(x) {
  inherits(x, "gyro")
}

#' @export
vec_ptype2.gyro.gyro <- function(x, y, ...) imu_ptype2(x, y, ...)

#' @export
vec_cast.gyro.gyro <- function(x, to, ...) imu_cast(x, to, ...)
