#' Example `acc` vector
#'
#' A simple `acc` vector for use in examples.
#'
#' @returns An `acc` vector with two bursts.
#'
#' @keywords internal
#' @export
acc_example <- function() {
  acc(
    list(
      cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
      cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3), Z = 1)
    ),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
}

# From dplyr
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  abs(x - y) < tol
}

# Check if a scalar value is NULL or NA
null_or_na <- function(x) {
  is.null(x) || rlang::is_na(x)
}

# Return the first scalar value in `...` that is not NULL or NA.
# Need to handle NA because NA values may be passed via data.frame col vals in
# as_acc_calibration()
first_valid <- function(...) {
  for (v in list(...)) {
    if (!is.null(v) && !rlang::is_na(v)) {
      return(v)
    }
  }
  NULL
}
