#' Manage units in IMU burst data
#'
#' @description
#' Set, convert, or drop units in burst matrices of `acc`, `mag`, and `gyro`
#' vectors.
#'
#' `set_imu_units()` attaches units to unitless bursts or converts between
#' compatible units. The target unit must be dimensionally compatible with
#' the IMU class:
#'
#'   - `acc`: acceleration units (e.g., `"m/s^2"`, `"standard_free_fall"`)
#'   - `mag`: magnetic flux density units (e.g., `"tesla"`, `"uT"`, `"gauss"`)
#'   - `gyro`: angular velocity units (e.g., `"rad/s"`, `"degree/s"`)
#'
#' `drop_imu_units()` strips units from each burst, leaving the underlying
#' numeric values unchanged. Bursts that do not carry units are returned
#' as-is.
#'
#' To transform raw values to physical units rather than simply attaching or
#' converting units, use [transform_imu()].
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`)
#' @param value Character specifying the target units (e.g., `"m/s^2"`). For
#'   units in terms of gravitational acceleration, use `"standard_free_fall"`.
#' @param ... Unused.
#'
#' @returns The input vector with units attached to, converted on, or removed
#'   from each burst matrix.
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
#'
#' # Strip units back off
#' drop_imu_units(a_ms2)
set_imu_units <- function(x, value, ...) {
  UseMethod("set_imu_units")
}

#' @export
set_imu_units.default <- function(x, value, ...) {
  assert_imu(x)
  set_imu_units_(x, value)
}

#' @export
set_imu_units.acc <- function(x, value, ...) {
  set_imu_units_(x, value, reference = "m/s^2")
}

#' @export
set_imu_units.mag <- function(x, value, ...) {
  set_imu_units_(x, value, reference = "tesla")
}

#' @export
set_imu_units.gyro <- function(x, value, ...) {
  set_imu_units_(x, value, reference = "degree/s")
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

#' @rdname set_imu_units
#' @export
drop_imu_units <- function(x, ...) {
  assert_imu(x)
  sensor <- imu_sensor(x)

  bursts_dropped <- purrr::map(
    bursts(x),
    function(b) {
      if (is.null(b) || !inherits(b, "units")) {
        return(b)
      }
      units::drop_units(b)
    }
  )

  field(x, "bursts") <- new_burst_list(bursts_dropped, sensor = sensor)

  x
}

#' @exportS3Method units::drop_units
drop_units.acc <- function(x, ...) {
  drop_imu_units(x)
}

#' @exportS3Method units::drop_units
drop_units.mag <- function(x, ...) {
  drop_imu_units(x)
}

#' @exportS3Method units::drop_units
drop_units.gyro <- function(x, ...) {
  drop_imu_units(x)
}

# Attach or convert units on every burst. If `reference` is given, `value` must
# be convertible to it (e.g. an acceleration unit for `acc`). Without a
# reference, any unit is accepted.
set_imu_units_ <- function(x, value, reference = NULL) {
  sensor <- imu_sensor(x)

  if (!is.character(value) || length(value) != 1) {
    abort_move2imu("{.arg value} must be a character string of length 1.")
  }

  if (!is.null(reference) && !units::ud_are_convertible(reference, value)) {
    abort_move2imu(
      c(
        "{value} units not valid for {.cls {sensor}} vector.",
        "i" = "Units must be convertible to {reference}."
      ),
      class = c("units_incompatible", "units")
    )
  }

  value <- units(units::as_units(value))

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

  field(x, "bursts") <- new_burst_list(bursts_converted, sensor = sensor)

  x
}
