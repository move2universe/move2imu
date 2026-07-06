#' Convert an object to an `acc` vector
#'
#' @description
#' Extract `acc` data from a `move2` or convert an object to an `acc` vector.
#'
#' For a `move2`, `acc` data are extracted from the object's
#' [active_acc_colsets()].
#'
#' @inheritParams merge_imu
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
#' @param rate_tol Relative tolerance to use when detecting differences in
#'   sampling rates when building or merging bursts. This determines how much
#'   two sampling rates may differ before they're treated as belonging to
#'   separate sampling regimes. For example, `rate_tol = 0.01` would allow
#'   deviations up to 1% of the earlier sampling rate to belong to the same
#'   burst.
#'
#'   Increase this value to prevent small deviations in sample timing
#'   from initiating the creation of new bursts. See details.
#' @param merge_continuous Logical value indicating whether to merge
#'   adjacent bursts. Two adjacent bursts can be merged if the end of the first
#'   burst coincides with the start of the second burst (within `gap_tol`)
#'   and their frequencies agree (within `rate_tol`). This is useful for
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
#' ## Dealing with noise in recorded timestamps
#'
#' Noise in the recorded timestamps of an input `move2` object can disrupt the
#' correct identification of the IMU bursts identified by `as_*()`.
#'
#' - For data stored in expanded format, `as_*()` must derive the implied sampling
#'  frequency from the individual timestamps recorded in the data. Within each
#'  burst, all samples must be collected at a fixed frequency. However, timestamp
#'  errors may make it appear as if the sampling rate
#'  has changed, artificially splitting a run of samples into multiple bursts.
#'
#' - For data stored in compact format, sampling frequencies are recorded
#'  explicitly. However, when data are collected continuously, adjacent bursts
#'  need to be merged together. Here again, timestamp noise can prevent bursts
#'  from merging properly if gaps between bursts differ from the sampling
#'  rate of those two bursts.
#'
#' You can fine-tune the burst parsing and merging process with the `rate_tol`
#' and `gap_tol` arguments.
#'
#' - `rate_tol` determines how much sampling rate noise is tolerated when
#'   identifying changes in sampling rate over the course of a series of recorded
#'   samples, as a proportion of the current sampling rate. That is,
#'   at `rate_tol = 0.01`, a new burst will be initiated only when the sampling
#'   rate between concurrent samples differs by more than 1% of the running rate.
#'
#'   Thus, at low values of `rate_tol`, small deviations in the sampling
#'   rate will trigger a new burst. Larger `rate_tol` values will smooth these
#'   inconsistencies, combining samples into single bursts. However, at high
#'   values, `rate_tol` may mask true changes in the sampling rate, producing
#'   bursts with spurious sampling frequencies (e.g. `rate_tol = 0.5`
#'   risks combining samples from a 40Hz signal with those from a 20Hz signal).
#'
#'   `rate_tol` also governs the similarity tolerance for two burst sampling
#'   frequencies when merging bursts (see below).
#'
#' - `gap_tol` determines how much deviation in the time gap between bursts
#'   is tolerated when merging two bursts together, in seconds
#'   (if `merge_continuous = TRUE`). Two adjacent
#'   bursts can be merged when the gap between the two is the same as the
#'   frequency of each burst, and each burst has the same sampling frequency
#'   (within `rate_tol`). This implies that the two bursts represent one
#'   continuous stream of data. Small values of `gap_tol` require that the gap
#'   be a near-exact match to the period implied by the sampling frequency of the
#'   bursts. Larger values of `gap_tol` will ignore larger deviations in gap
#'   timing.
#'
#'   Note that a burst's frequency is recalculated after merging using the number
#'   of samples and the recorded start and end of the burst. Thus, setting
#'   a large `gap_tol` may produce bursts that have non-standard frequencies,
#'   as the gap between the bursts (which deviates from the expected sampling
#'   rate) will be incorporated into the samples of
#'   a single burst.
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
as_acc.move2 <- function(x,
                         colset = NULL,
                         min_freq = 0,
                         gap_tol = 1e-6,
                         rate_tol = 1e-2,
                         merge_continuous = TRUE,
                         drop = FALSE,
                         ...) {
  as_imu(
    x,
    sensor = "acc",
    colset = colset,
    min_freq = min_freq,
    gap_tol = gap_tol,
    rate_tol = rate_tol,
    merge_continuous = merge_continuous,
    drop = drop,
    ...
  )
}
