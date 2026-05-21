
#' Functions to explore an IMU vector
#'
#' @param x An IMU vector (e.g. `acc`, `mag`, `gyro`).
#' @param value Replacement value.
#'
#' @rdname explore-functions
#' @aliases explore-functions
#' @export
#' @examples
#' x <- acc(
#'   bursts = list(
#'     cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
#'     cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
#'   ),
#'   frequency = units::as_units(c(20, 30), "Hz")
#' )
#' x <- c(x, NA)
#' n_axis(x)
#' n_samples(x)
#' is_uniform(x)
#' length(x)
#' is.na(x)
#' na.omit(x)
#'  y <- acc(
#'   bursts = list(
#'     cbind(X = sin(1:20 / 10), Y = cos(1:20 / 10)),
#'     cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
#'   ),
#'   frequency = units::as_units(c(20, 20), "Hz")
#' )
#' is_uniform(y)
n_axis <- function(x) {
  r <- rep(NA_integer_, vec_size(x))
  r[!is.na(x)] <- purrr::map_int(bursts(x[!is.na(x)]), ncol)
  r
}

#' @export
#' @importFrom stats na.omit
#' @rdname explore-functions
is_uniform<-function(x){
  # TODO check units are same?
  all(duplicated(na.omit(n_samples(x)))[-1])&&
    all(duplicated(na.omit(n_axis(x)))[-1]) &&
    all(duplicated(na.omit(freqs(x)))[-1]) &&
    all(duplicated(purrr::map(bursts(x[!is.na(x)]), colnames))[-1]) &&
    all(duplicated(purrr::map_lgl(bursts(x[!is.na(x)]), inherits, "units"))[-1])
}

#' @export
#' @rdname explore-functions
bursts <- function(x) {
  field(x, "bursts")
}

#' @rdname explore-functions
#' @export
`bursts<-` <- function(x, value) {
  field(x, "bursts") <- value
  x
}

#' @export
#' @rdname explore-functions
freqs <- function(x) {
  field(x, "frequency")
}

#' @rdname explore-functions
#' @export
`freqs<-` <- function(x, value) {
  field(x, "frequency") <- value
  x
}

#' @export
#' @rdname explore-functions
starts <- function(x) {
  field(x, "start")
}

#' @rdname explore-functions
#' @export
`starts<-` <- function(x, value) {
  field(x, "start") <- value
  x
}

#' @export
#' @rdname explore-functions
burst_dur <- function(x) {
  units::set_units(n_samples(x) / freqs(x), "s")
}

#' @export
#' @rdname explore-functions
n_samples <- function(x) {
  purrr::map_int(bursts(x), function(b) nrow(b) %||% NA_integer_)
}

#' @export
#' @rdname explore-functions
imu_units <- function(x) {
  purrr::map_chr(
    bursts(x), 
    function(b) {
      tryCatch(as.character(units(b)), error = function(cnd) NA_character_)
    }
  )
}

# TODO finish function and export?
static_acc <- function(x) {
  # should this return a list or a dataframe
  # TODO fix NA
  lapply(bursts(x)[!is.na(x)], colMeans)
}

