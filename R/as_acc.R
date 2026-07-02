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
#' @param min_freq Numeric value indicating the minimum sampling rate to use
#'   when combining samples into a single burst. Samples recorded at a rate
#'   slower than this value will instead be split into individual (length-1) 
#'   "bursts". Increase this value to avoid producing slow-frequency bursts.
#'   By default, all samples recorded at consistent
#'   intervals will be combined into bursts, regardless of their sampling rate.
#'
#'   Ignored for compact-format data, where values are already in predefined
#'   bursts.
#' @param tolerance Tolerance (in seconds) to use when identifying timestamp
#'   irregularities that should be treated as noise when constructing bursts.
#'   This is the largest amount by which a sample's timestamp may deviate
#'   from the value suggested by the adjacent samples, assuming samples are
#'   collected at a consistent rate. For example, for 1 Hz data with a tolerance
#'   of 0.001, a timestamp recorded 1.001 seconds after another would still
#'   be considered to belong to the same burst.
#'   
#'   Increase this value to avoid splitting samples into separate IMU bursts
#'   because of small timestamp irregularities. See details.
#' @param merge_continuous Logical value indicating whether to merge
#'   adjacent bursts. Two adjacent bursts can be merged if the end of the first
#'   burst coincides with the start of the second burst (within `tolerance`)
#'   and the burst frequency is consistent between the two. This is useful for
#'   processing continuous data that have been stored in chunks
#'   split at regular intervals (e.g. e-obs data).
#' @param drop Logical indicating whether empty bursts should
#'   be dropped from the output. If `drop = FALSE`, then the length of the
#'   output will match the number of rows in the input data `x` and
#'   bursts will be stored at the index location corresponding to the start time
#'   of the burst.
#' @param ... currently not used
#'
#' @details
#' For data stored in expanded format, `as_*()` must derive the implied sampling
#' frequency from the individual timestamps recorded in the data. Within each
#' burst, all samples must be collected at a fixed frequency. However,
#' timestamps may also contain occasional imprecisions. These deviations
#' then violate the requirement that a burst have consistently-sampled records,
#' forcing the creation of a new, separate burst at the point where the
#' deviation occurs.
#' 
#' To avoid this behavior, you can set the `tolerance` parameter to ignore
#' a certain amount of noise in the recorded timestamps. Timestamps can vary 
#' from the value implied by the local sampling frequency
#' (which is indicated by the gaps between adjacent sample timestamps) up to the
#' value of `tolerance` while still being considered as part of the same 
#' burst. This prevents the partitioning of bursts at artificial boundaries 
#' where timestamps contain small noise errors.
#'
#' The `tolerance` also governs the largest amount that the end of one burst
#' and the start of the next burst can vary while still being considered
#' equal for the purposes of merging bursts (see the `merge_continuous`
#' argument.)
#' 
#' Note that increasing tolerance comes at the cost of reducing timestamp 
#' precision; once bursts are constructed, recovering timestamps for 
#' individual samples within a burst is only accurate to roughly the 
#' size of `tolerance`. At large tolerance values, you may also mask true
#' frequency changes in the data.
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
as_acc.move2 <- function(x, colset = NULL, min_freq = 0, tolerance = 1e-6, merge_continuous = TRUE, drop = FALSE, ...) {
  as_imu(
    x,
    sensor = "acc",
    colset = colset,
    min_freq = min_freq,
    tolerance = tolerance,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
