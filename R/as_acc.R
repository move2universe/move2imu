#' Convert an object to an `acc` vector
#'
#' @description
#' Extract `acc` data from a `move2` or convert an object to an `acc` vector.
#' 
#' For a `move2`, `acc` data are extracted from the object's 
#' [active_acc_colsets()].
#'
#' @param x A `move2` containing acceleration data as collected by EOBS,
#'   Ornitela, or similar tracking devices. Most of the time this will be
#'   either loaded from disk using [move2::mt_read] or downloaded using
#'   [move2::movebank_download_study].
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns of `x` that contain acceleration data. By default,
#'   constructs bursts for all column sets that are detected in `x` that also
#'   contain data (see [active_acc_colsets()]).
#'
#'   Several common colsets are listed under [movebank_acc_colsets()]. To
#'   specify a custom set of columns, use [imu_colset()].
#' @param min_freq Numeric value indicating the
#'   minimum allowable within-burst data collection frequency when identifying
#'   bursts in expanded-format data. Any two adjacent timestamps
#'   that fall outside of the period defined by this frequency will be split
#'   into separate bursts. If no units are provided, this value is assumed to
#'   be in Hz.
#'
#'   Ignored if data are already in predefined bursts.
#' @param merge_continuous Logical value indicating whether to merge
#'   adjacent bursts. Two adjacent bursts can be merged if the
#'   first burst ends at the same time that the second starts and the
#'   burst frequency is identical between the two. This is useful for
#'   processing continuous data that have been stored in chunks
#'   split at regular intervals.
#' @param drop Logical indicating whether empty bursts should
#'   be dropped from the output. If `drop = FALSE`, then the length of the
#'   output will match the number of rows in the input data `x` and
#'   bursts will be stored at the index location corresponding to the start time
#'   of the burst.
#' @param ... currently not used
#'
#' @details The resulting vector will be as long as the input. This means it
#' can, for example, be added as a column to a `data.frame`. For some tags
#' this means `NA` values are inserted when one burst is stored over multiple
#' rows of a `data.frame`.
#'
#' @seealso [movebank_acc_colsets()] for supported acceleration column sets
#'   in Movebank.
#'
#' @export
#' 
#' @examplesIf rlang::is_installed("move2")
#' # Example compact-format data: acc bursts stored in strings in individual rows
#' alb <- albatrosses()
#' 
#' as_acc(alb)
#'
#' # Expanded-format data: bursts are constructed from samples stored across rows
#' g <- gulls()
#' 
#' head(as_acc(g))
#'
#' # Specify the columns to extract explicitly with a colset, e.g. to
#' # pull a single axis from the gulls data:
#' as_acc(g, colset = imu_colset(x = "acceleration_raw_x")) |> 
#'   head()
#' 
#' # Output is index-matched to the input move2, so the result can be
#' # easily attached:
#' g$a <- as_acc(g)
#'
#' # To instead drop missing bursts, set `drop = TRUE`:
#' as_acc(g, drop = TRUE)
as_acc <- function(x, ...) {
  UseMethod("as_acc")
}

#' @rdname as_acc
#' @export
as_acc.default <- function(x, ...) {
  vctrs::vec_cast(x, new_imu("acc"))
}

#' @rdname as_acc
#' @export
as_acc.move2 <- function(x, colset = NULL, min_freq = 1, merge_continuous = TRUE, drop = FALSE, ...) {
  as_imu(
    x,
    sensor = "acc",
    colset = colset,
    min_freq = min_freq,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
