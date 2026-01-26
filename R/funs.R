
#' Functions to explore an `acc` vector
#'
#' @param x an acc vector
#'
#' @rdname explore-functions
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
  r[!is.na(x)] <- purrr::map_int(field(x[!is.na(x)], "bursts"), ncol)
  r
}
#' @export
#' @rdname explore-functions
n_samples <- function(x) {
  r <- rep(NA_integer_, vec_size(x))
  r[!is.na(x)] <- purrr::map_int(field(x[!is.na(x)], "bursts"), nrow)
  r
}
#' @export
#' @importFrom stats na.omit
#' @rdname explore-functions
is_uniform<-function(x){
  # TODO check units are same?
  all(duplicated(na.omit(n_samples(x)))[-1])&&
  all(duplicated(na.omit(n_axis(x)))[-1]) &&
  all(duplicated(na.omit(field(x,"frequency")))[-1]) &&
  all(duplicated(  purrr::map(field(x[!is.na(x)], "bursts"), colnames))[-1]) &&
  all(duplicated(purrr::map_lgl(field(x[!is.na(x)], "bursts"), inherits, "units"))[-1])
}

burst_dur <- function(x) {
  UseMethod("burst_dur")
}

#' @export
burst_dur.acc <- function(x) {
  bursts <- field(x, "bursts")
  frq <- field(x, "frequency")
  
  dur <- purrr::map2_dbl(
    bursts,
    frq,
    function(x, y) (nrow(x) %||% NA) / y
  )
  
  units::set_units(dur, "s") # not sure if this is guaranteed...but units gives 1/Hz by default which is annoying.
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
  lapply(field(x, "bursts")[!is.na(x)], colMeans)
}


#' @export
vec_cast.acc.acc <- function(x, to, ...) {
  x
}



if (F) {
  static_acc(x) |> bind_rows() -> df

  optim(c(1000, 1000, 1000), \(x, df){
    sum((1 - sqrt(rowSums(t(t(df) - x)^2)))^2)
  }, df = df)
  r <- sqrt(colSums((t(df) - c(2048, 2048, 2048))^2))
  rgl::plot3d(df$X, df$Y, df$Z, col = leaflet::colorNumeric("RdYlBu", range(r))(r), size = 10)
}
