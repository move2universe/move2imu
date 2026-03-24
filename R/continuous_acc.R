#' Merge adjacent bursts in an `acc` vector
#' 
#' For a given `acc` vector, identify temporally adjacent bursts and merge
#' them into a single burst. Bursts that end at the same time as the start
#' time of the next burst are considered adjacent. Bursts with different
#' frequencies or acceleration axes will not be merged.
#'
#' @param x An `acc` vector
#' @param acc_ids Vector indicating groups to which the elements in `x` belong.
#'   If provided, bursts in `x` will not be merged across different values of
#'   this vector, even if their timestamps and frequencies align.
#' @param drop Logical indicating whether to drop entries that have been merged
#'   into other bursts. If `drop = FALSE`, the output will have the same length
#'   as the input `x`, with `NA` values at positions where bursts were merged
#'   into a preceding burst. This is useful for retaining index matching between
#'   the input and output vectors. Default is `TRUE`.
#' 
#' @returns An `acc` vector
#' @export
#'
#' @examples
#' a <- acc(
#'   c(acc_burst_example(1:60, 1:60), acc_burst_example(61:100, 61:100), acc_burst_example(101:140)),
#'   frequency = units::set_units(20, "Hz"),
#'   start = as.POSIXct(c(0, 3, 5), tz = "UTC")
#' )
#' 
#' merge_continuous_acc(a)
merge_continuous_acc <- function(x, acc_ids = NULL, drop = TRUE) {
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
  is_same_frq <- fq[-1] == fq[-nv]
  
  # Collapsible bursts must have axis structure
  # Check both axis names and length to disambiguate possible name duplication
  # after collapsing to single string
  axes <- purrr::map_chr(
    bursts(xv),
    function(b) paste0(colnames(b), collapse = "_")
  )
  is_same_n_axis <- (axes[-1] == axes[-nv]) & (n_axis(xv)[-1] == n_axis(xv)[-nv])
  
  if (rlang::is_null(acc_ids)) {
    is_same_id <- vctrs::vec_recycle(TRUE, nv - 1)
  } else {
    # Don't collapse bursts across different sources, if IDs provided
    acc_ids_v <- acc_ids[valid]
    is_same_id <- (acc_ids_v[-1] == acc_ids_v[-nv]) | (is.na(acc_ids_v[-1]) & is.na(acc_ids_v[-nv]))
  }
  
  to_bind <- c(FALSE, is_adjacent_burst & is_same_frq & is_same_n_axis & is_same_id)
  to_bind[is.na(to_bind)] <- FALSE
  
  # Split entries in the acc vector into groups that should be collapsed and
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
  
  merged <- acc(
    bursts_comb,
    frequency = units::set_units(fq[merged_i], "Hz"),
    start = sv[merged_i]
  )

  # If retaining index matching, fill merged idx with NA acc
  if (!drop) {
    out <- vec_rep(acc(list(NULL), units::set_units(NA, "Hz")), n)
    out[valid[merged_i]] <- merged
    merged <- out
  }

  merged
}

#' Split an `acc` object at regular intervals
#' 
#' Split the bursts in an `acc` object into bursts of a given time duration.
#'
#' @inheritParams merge_continuous_acc
#' @param interval Numeric or units object defining the time intervals at which
#'   `x` will be split. If no units are provided, the interval is assumed to
#'   be in period units of `x` (i.e., 1 divided by the frequency units).
#'
#' @returns An `acc` vector
#' @export
#'
#' @examples
#' a <- acc(
#'   c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
#'   frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
#'   start = as.POSIXct(c(0, 10), tz = "UTC")
#' )
#' 
#' x <- split_continuous_acc(a, units::set_units(1, "s"))
#' x
#' 
#' # Start times handled automatically 
#' starts(x)
#' 
#' # Records are not guaranteed to have same duration depending on interval
#' # and burst size:
#' x <- split_continuous_acc(a, 0.7)
#' 
#' burst_dur(x)
split_continuous_acc <- function(x, interval) {
  assertthat::assert_that(as.numeric(interval) > 0)
  
  x <- map_acc(
    x,
    function(.br, .fq, .st) {
      if (rlang::is_empty(.br) || nrow(.br) <= 1) {
        return(
          acc(list(NULL), .fq, .st)
        )
      }
      
      # coerce user interval into units of (1 / frequency) which is what
      # is implied when we split burst records by index
      frq_units <- units::as_units(units(.fq), mode = "standard")
      period_units <- 1 / frq_units
      
      interval <- units::set_units(
        interval, 
        units(period_units), 
        mode = "standard"
      )
      
      # number of rows per chunk
      i <- units::drop_units((interval / period_units) * .fq)
      
      idx <- unname(split(seq_len(nrow(.br)), ceiling(seq_len(nrow(.br)) / i)))
      b_split <- lapply(idx, function(j) .br[j, , drop = FALSE])
      
      a <- acc(
        b_split, 
        .fq, 
        .st + cumsum(c(0, rep(interval, length(b_split) - 1)))
      )
    }
  )
  
  purrr::reduce(x, function(x, y) c(x, y))
}
