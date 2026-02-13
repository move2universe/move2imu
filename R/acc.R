#'  Create a `acc` vector
#'
#' @param bursts a list of matrices
#' @param frequency The frequency of the acceleration recordings. Either the same length of `bursts` or it will be recycled
#' @param start Start time of the burst, in POSIXct format
#' @param id Group identifier for this burst (for instance, to identify 
#'   bursts that come from the same source)
#'
#' @export
acc <- function(bursts = list(), 
                frequency = units::set_units(double(), "Hz"),
                start = NULL,
                id = NULL) {
  bursts <- new_acc_list(bursts)
  n <- vec_size(bursts)
  
  id <- id %||% NA_character_
  start <- start %||% NA_real_
  
  if (inherits(start, "POSIXt")) {
    tz <- attr(start, "tzone")
  } else {
    tz <- "UTC"
  }
  
  start <- as.POSIXct(start, tz = tz)
  
  new_acc(
    bursts = bursts, 
    frequency = vec_recycle(frequency, n), 
    start = vec_recycle(start, n), 
    id = vec_recycle(id, n)
  )
}

new_acc <- function(bursts = new_acc_list(list()), 
                    frequency = units::set_units(double(), "Hz"),
                    start = as.POSIXct(double(), tz = "UTC"),
                    id = character()) {
  new_rcrd(
    list(bursts = bursts, frequency = frequency, start = start, id = id),
    class = "acc"
  )
}

acc_list <- function(x) {
  new_acc_list(x)
}

new_acc_list <- function(x) {
  assertthat::assert_that(all(unlist(lapply(x, \(y) is.null(y) || !is.null(colnames(y))))))
  new_list_of(x, ptype = matrix(numeric()), class = "acc_list")
}