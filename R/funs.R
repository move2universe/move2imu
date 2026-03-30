
#' Functions to explore an `acc` vector
#'
#' @param x an acc vector
#'
#' @rdname explore-functions
#' @aliases explore-functions
#' @export
#' @examples
#' x <- acc(
#'   bursts = list(
#'     cbind(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
#'     cbind(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
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
#'     cbind(x = sin(1:20 / 10), y = cos(1:20 / 10)),
#'     cbind(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
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
#' @rdname explore-functions
n_samples <- function(x) {
  r <- rep(NA_integer_, vec_size(x))
  r[!is.na(x)] <- purrr::map_int(bursts(x[!is.na(x)]), nrow)
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

#' @rdname explore-functions
#' @export
is.na.acc <- function(x) {
  vctrs::vec_detect_missing(x)
}

#' @export
#' @rdname explore-functions
bursts <- function(x) {
  field(x, "bursts")
}

#' @export
#' @rdname explore-functions
freqs <- function(x) {
  field(x, "frequency")
}

#' @export
#' @rdname explore-functions
starts <- function(x) {
  field(x, "start")
}

#' @export
#' @rdname explore-functions
burst_dur <- function(x) {
  units::set_units(as.numeric(burst_n(x) / freqs(x)), "s")
}

#' @export
#' @rdname explore-functions
burst_n <- function(x) {
  purrr::map_int(bursts(x), function(b) nrow(b) %||% NA_integer_)
}

#' Filter an acc vector by burst frequency
#'
#' @param x An `acc` vector
#' @param min_freq,max_freq Numeric or units values indicating the minimum
#'   and/or maximum frequency thresholds to use when determining the records in
#'   `x` to retain. Elements in `x` whose frequency falls within these limits
#'   are kept in the output. If no units are provided, values are considered to
#'   be in hertz.
#' @param keep_na Logical indicating whether elements of `x` with a missing
#'   frequency should be retained in the output. By default, these elements
#'   are removed.
#'
#' @returns An `acc` vector
#' @export
#'
#' @examples
#' a <- acc_example()
#' 
#' freqs(a)
#'
#' filter_freq(a, 2.5)
filter_freq <- function(x, min_freq = 0, max_freq = Inf, keep_na = FALSE) {
  min_freq <- units::set_units(min_freq, units(freqs(x)), mode = "standard")
  max_freq <- units::set_units(max_freq, units(freqs(x)), mode = "standard")
  
  if (!keep_na) {
    x <- x[!is.na(freqs(x))]
  }
  
  x[freqs(x) <= max_freq & freqs(x) >= min_freq | is.na(freqs(x))]
}

#' @export
#' @rdname explore-functions
is_acc <- function(x) {
  inherits(x, "acc")
}

# TODO finish function and export?
static_acc <- function(x) {
  # should this return a list or a dataframe
  # TODO fix NA
  lapply(bursts(x)[!is.na(x)], colMeans)
}

#' @export
vec_cast.acc.acc <- function(x, to, ...) {
  x
}
