#' @rdname imu_constructors
#' @order 2
#' @export
mag <- function(bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  imu("mag", bursts = bursts, frequency = frequency, start = start)
}

#' @export
#' @rdname imu-predicates
#' @order 2
is_mag <- function(x) {
  inherits(x, "mag")
}

#' @export
vec_ptype2.mag.mag <- function(x, y, ...) imu_ptype2(x, y, ...)

#' @export
vec_cast.mag.mag <- function(x, to, ...) imu_cast(x, to, ...)
