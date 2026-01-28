#' Merge adjacent bursts in an `acc` vector
#' 
#' For a given `acc` vector, identify temporally adjacent bursts and merge
#' them into a single burst. Bursts that end at the same time as the start
#' time of the next burst are considered adjacent. Bursts with different
#' frequencies or acceleration axes will not be merged.
#'
#' @param x An `acc` vector
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
merge_continuous_acc <- function(x) {
  n <- vec_size(x)
  
  # Collapsible bursts must end at the start time of the subsequent burst
  # TODO: add a tolerance parameter here to account for small deviations?
  burst_starts <- field(x, "start")
  timediff <- burst_starts + units::as_difftime(burst_dur(x))
  is_adjacent_burst <- burst_starts[-1] == timediff[-n]
  
  # If no adjacent bursts, no need to proceed
  if (!any(is_adjacent_burst, na.rm = TRUE)) {
    return(x)
  }
  
  # Collapsible bursts must have the same frequency
  fq <- field(x, "frequency")
  is_same_frq <- fq[-1] == fq[-n]
  
  # Collapsible bursts must have axis structure
  # Check both axis names and length to disambiguate possible name duplication
  # after collapsing to single string
  axes <- purrr::map_chr(
    field(x, "bursts"), 
    function(b) paste0(colnames(b), collapse = "_")
  )
  is_same_n_axis <- (axes[-1] == axes[-n]) & (n_axis(x)[-1] == n_axis(x)[-n])
  
  to_bind <- c(FALSE, is_adjacent_burst & is_same_frq & is_same_n_axis)
  to_bind[is.na(to_bind)] <- FALSE
  
  # Split entries in the acc vector into groups that should be collapsed and
  # rbind burst matrices
  idx <- unname(split(seq_along(to_bind), cumsum(!to_bind)))
  
  bursts_comb <- purrr::map(
    idx,
    function(i) {
      purrr::reduce(field(x, "bursts")[i], function(x, y) rbind(x, y))
    }
  )
  
  # Get first entry in each group. This defines the burst freq and start time.
  i <- purrr::map_int(idx, function(x) x[1])
  
  acc(
    bursts_comb, 
    frequency = units::set_units(fq[i], "Hz"),
    start = burst_starts[i]
  )
}
