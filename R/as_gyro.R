#' Convert an object to a `gyro` vector
#'
#' @description
#' Extract `gyro` data from a `move2` or convert an object to a `gyro` vector.
#'
#' For a `move2`, `gyro` data are extracted from the object's
#' [active_gyro_colsets()].
#'
#' @inheritParams as_acc
#' @param x A `move2` containing gyroscope data. Most of the time this will be
#'   either loaded from disk using [move2::mt_read] or downloaded using
#'   [move2::movebank_download_study].
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns of `x` that contain gyroscope data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_gyro_colsets()]).
#'
#'   Several common colsets are listed under [movebank_gyro_colsets()]. To
#'   specify a custom set of columns, use [imu_colset()].
#'
#' @details The resulting vector will be as long as the input. This means it
#' can, for example, be added as a column to a `data.frame`. For some tags
#' this means `NA` values are inserted when one burst is stored over multiple
#' rows of a `data.frame`.
#'
#' @seealso [movebank_gyro_colsets()] for supported gyroscope column sets
#'   in Movebank.
#'
#' @export
as_gyro <- function(x, ...) {
  UseMethod("as_gyro")
}

#' @rdname as_gyro
#' @export
as_gyro.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("gyro"))
}

#' @rdname as_gyro
#' @export
as_gyro.move2 <- function(x, colset = NULL, min_freq = 1, merge_continuous = TRUE, drop = FALSE, ...) {
  as_imu(
    x,
    sensor = "gyro",
    colset = colset,
    min_freq = min_freq,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
