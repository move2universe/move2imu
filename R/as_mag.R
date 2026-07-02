#' Convert an object to a `mag` vector
#'
#' @description
#' Extract `mag` data from a `move2` or convert an object to a `mag` vector.
#'
#' For a `move2`, `mag` data are extracted from the object's
#' [active_mag_colsets()].
#'
#' @inheritParams as_acc
#' @param x A `move2` containing magnetometer data. Most of the time this will be
#'   either loaded from disk using [move2::mt_read] or downloaded using
#'   [move2::movebank_download_study].
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns of `x` that contain magnetometer data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_mag_colsets()]).
#'
#'   Several common colsets are listed under [movebank_mag_colsets()]. To
#'   specify a custom set of columns, use [imu_colset()].
#'
#' @details The resulting vector will be as long as the input. This means it
#' can, for example, be added as a column to a `data.frame`. For some tags
#' this means `NA` values are inserted when one burst is stored over multiple
#' rows of a `data.frame`.
#'
#' @seealso [movebank_mag_colsets()] for supported magnetometer column sets
#'   in Movebank.
#'
#' @export
as_mag <- function(x, ...) {
  UseMethod("as_mag")
}

#' @rdname as_mag
#' @export
as_mag.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("mag"))
}

#' @rdname as_mag
#' @export
as_mag.move2 <- function(x, colset = NULL, min_freq = 0, tolerance = 1e-6, merge_continuous = TRUE, drop = FALSE, ...) {
  as_imu(
    x,
    sensor = "mag",
    colset = colset,
    min_freq = min_freq,
    tolerance = tolerance,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
