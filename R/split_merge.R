#' Merge adjacent bursts in an IMU vector
#'
#' For a given IMU vector, identify temporally adjacent bursts and
#' merge them into a single burst. Bursts that end at the same time as the
#' start time of the next burst are considered adjacent. Bursts with different
#' frequencies or axes will not be merged.
#'
#' @inheritParams n_axis
#' @param ids Vector indicating groups to which the elements in `x` belong.
#'   If provided, bursts in `x` will not be merged across different values of
#'   this vector, even if their timestamps and frequencies align.
#' @param drop Logical indicating whether to drop entries that have been merged
#'   into other bursts. If `drop = FALSE` (default), the output will have the
#'   same length as the input `x`, with `NA` values at positions where bursts
#'   were merged into a preceding burst. This is useful for retaining index
#'   matching between the input and output vectors.
#'
#' @returns A vector of the same class as `x`.
#' @export
#'
#' @examples
#' a <- acc(
#'   list(cbind(X = 1:60, Y = 1:60), cbind(X = 61:100, Y = 61:100), cbind(X = 101:140)),
#'   frequency = units::set_units(20, "Hz"),
#'   start = as.POSIXct(c(0, 3, 5), tz = "UTC")
#' )
#'
#' merge_imu(a)
merge_imu <- function(x, ids = NULL, drop = FALSE) {
  n <- vec_size(x)

  # Work only with non-NA entries; track their original positions
  valid <- which(!is.na(x))

  if (length(valid) <= 1) {
    if (drop) return(x[valid])
    return(x)
  }

  burst_starts <- starts(x)

  xv <- x[valid]
  sv <- burst_starts[valid]
  nv <- length(valid)

  # Collapsible bursts must end at the start time of the subsequent burst
  # TODO: add a tolerance parameter here to account for small deviations?
  timediff <- sv + units::as_difftime(burst_dur(xv))
  is_adjacent_burst <- sv[-1] == timediff[-nv]

  # If no adjacent bursts, no need to proceed
  if (!any(is_adjacent_burst, na.rm = TRUE)) {
    if (drop) return(xv)
    return(x)
  }

  # Collapsible bursts must have the same frequency
  fq <- freqs(xv)
  is_same_freq <- fq[-1] == fq[-nv]

  # Collapsible bursts must have axis structure
  # Check both axis names and length to disambiguate possible name duplication
  # after collapsing to single string
  axes <- purrr::map_chr(
    bursts(xv),
    function(b) paste0(colnames(b), collapse = "_")
  )
  is_same_n_axis <- (axes[-1] == axes[-nv]) & (n_axis(xv)[-1] == n_axis(xv)[-nv])

  if (rlang::is_null(ids)) {
    is_same_id <- vctrs::vec_recycle(TRUE, nv - 1)
  } else {
    # Don't collapse bursts across different sources, if IDs provided
    ids_v <- ids[valid]
    is_same_id <- (ids_v[-1] == ids_v[-nv]) | (is.na(ids_v[-1]) & is.na(ids_v[-nv]))
  }

  to_bind <- c(FALSE, is_adjacent_burst & is_same_freq & is_same_n_axis & is_same_id)
  to_bind[is.na(to_bind)] <- FALSE

  # Split entries in the vector into groups that should be collapsed and
  # rbind burst matrices
  idx <- unname(split(seq_along(to_bind), cumsum(!to_bind)))

  bursts_comb <- purrr::map(
    idx,
    function(i) {
      purrr::reduce(bursts(xv)[i], function(x, y) rbind(x, y))
    }
  )

  # Get first entry in each group. This defines the burst freq and start time.
  merged_i <- purrr::map_int(idx, function(x) x[1])

  merged <- imu(
    sensor = class(x)[1],
    bursts = bursts_comb,
    frequency = units::set_units(fq[merged_i], "Hz"),
    start = sv[merged_i]
  )

  # If retaining index matching, fill merged idx with NA entries
  if (!drop) {
    out <- vec_rep(imu(sensor = class(x)[1], bursts = list(NULL), frequency = units::set_units(NA, "Hz")), n)
    out[valid[merged_i]] <- merged
    merged <- out
  }

  merged
}

#' Split an IMU vector at regular intervals
#'
#' Split the bursts in an IMU vector into bursts of a given time
#' duration. The result is a list of vectors of the same length as the input,
#' with the same class as `x`.
#'
#' @inheritParams merge_imu
#' @param interval Numeric or units object defining the time intervals at which
#'   `x` will be split. If no units are provided, the interval is assumed to
#'   be in period units of `x` (i.e., 1 divided by the frequency units).
#'
#' @returns A list of vectors (same class as `x`), the same length as `x`.
#'   Each element contains the split pieces of the corresponding input burst.
#' @export
#'
#' @examples
#' a <- acc(
#'   list(cbind(X = 1:60, Y = 1:60), cbind(X = 101:140)),
#'   frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
#'   start = as.POSIXct(c(0, 10), tz = "UTC")
#' )
#'
#' x <- split_imu(a, units::set_units(1, "s"))
#' x
#'
#' # Flatten to a single vector
#' flat <- purrr::reduce(x, c)
#' flat
#'
#' # Start times are updated to match the start of each split component
#' starts(flat)
#'
#' # Use merge_imu() on flat
#' identical(merge_imu(flat, drop = TRUE), a)
#'
#' \dontrun{
#' # In a dataframe, split and unnest to retain index matching
#' library(dplyr)
#' library(tidyr)
#'
#' tbl <- tibble::tibble(id = c("a", "b"), burst = a)
#'
#' tbl <- tbl |>
#'   mutate(burst = split_imu(burst, units::set_units(1, "s"))) |>
#'   unnest(burst) |>
#'   mutate(timestamp = starts(burst))
#'
#' tbl
#'
#' # Use merge_imu() to recover original bursts
#' tbl |>
#'   mutate(burst = merge_imu(burst, ids = id, drop = FALSE))
#' }
split_imu <- function(x, interval) {
  assertthat::assert_that(
    as.numeric(interval) > 0,
    msg = "`interval` must be a positive number"
  )

  sensor <- class(x)[1]

  x <- purrr::pmap(
    list(bursts(x), freqs(x), starts(x)),
    function(.br, .fq, .st) {
      if (rlang::is_empty(.br) || nrow(.br) < 1) {
        return(imu(sensor, list(NULL), .fq, .st))
      }

      # coerce user interval into units of (1 / frequency) which is what
      # is implied when we split burst records by index
      freq_units <- units::as_units(units(.fq), mode = "standard")
      period_units <- 1 / freq_units

      interval <- units::set_units(
        interval,
        units(period_units),
        mode = "standard"
      )

      # number of rows per chunk
      i <- units::drop_units((interval / period_units) * .fq)

      idx <- unname(split(seq_len(nrow(.br)), ceiling(seq_len(nrow(.br)) / i)))
      b_split <- lapply(idx, function(j) .br[j, , drop = FALSE])

      imu(
        sensor = sensor,
        bursts = b_split,
        frequency = .fq,
        start = .st + cumsum(c(0, rep(interval, length(b_split) - 1)))
      )
    }
  )

  x
}
