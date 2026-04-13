#'  Create a `acc` vector
#'
#' @param bursts a list of matrices
#' @param frequency The frequency of the acceleration recordings. Either the 
#'   same length of `bursts` or it will be recycled. If no units are specified,
#'   the frequency is assumed to be in Hz.
#' @param start Start time of the burst, in POSIXct format
#'
#' @export
acc <- function(bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  bursts <- acc_list(bursts)
  n <- vec_size(bursts)
  
  if (!inherits(frequency, "units")) {
    frequency <- units::set_units(frequency, "Hz")
  } else if (!units::ud_are_convertible(units::deparse_unit(frequency), "Hz")) {
    rlang::abort("`frequency` must be convertible to a frequency unit.")
  }
  
  start <- start %||% NA_real_
  
  if (inherits(start, "POSIXt")) {
    tz <- attr(start, "tzone")
  } else {
    tz <- "UTC"
  }
  
  start <- as.POSIXct(as.double(start), tz = tz)
  
  frequency <- vec_recycle(frequency, n)
  start <- vec_recycle(start, n)
  
  # Ensure metadata is NA when bursts are missing, so that the record is
  # consistently all-NA and vec_detect_missing() agrees with is.na()
  na_burst <- vec_detect_missing(bursts)
  
  if (any(na_burst)) {
    frequency[na_burst] <- units::set_units(NA, "Hz")
    start[na_burst] <- as.POSIXct(NA, tz = tz)
  }
  
  new_acc(
    bursts = bursts,
    frequency = frequency,
    start = start
  )
}

new_acc <- function(bursts = new_acc_list(list()),
                    frequency = units::set_units(double(), "Hz"),
                    start = as.POSIXct(double(), tz = "UTC")) {
  new_rcrd(
    list(bursts = bursts, frequency = frequency, start = start),
    class = "acc"
  )
}

acc_list <- function(x) {
  valid_axes <- c("X", "Y", "Z")

  is_valid <- purrr::map_lgl(
    x,
    function(b) {
      if (is.null(b)) return(TRUE)
      nms <- colnames(b)
      !is.null(nms) && length(nms) > 0 && all(nms %in% valid_axes)
    }
  )

  if (any(!is_valid)) {
    rlang::abort("Burst matrix columns must be named \"X\", \"Y\", or \"Z\".")
  }

  new_acc_list(x)
}

new_acc_list <- function(x) {
  new_list_of(x, ptype = matrix(numeric()), class = "acc_list")
}