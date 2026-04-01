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
    c(acc_burst_example(1:4, 5:8, 9:12), acc_burst_example(1:4, 5:8)),
    frequency = units::set_units(2:3, "Hz"),
    start = as.POSIXct(c(1, 10), tz = "UTC")
  )
}

#' @export
#' @keywords internal
#' @rdname acc_example
acc_burst_example <- function(x = NULL, y = NULL, z = NULL) {
  vctrs::vec_size_common(x, y, z)
  new_acc_list(list(do.call(cbind, list(X = x, Y = y, Z = z))))
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
    if (!is.null(v) && !rlang::is_na(v)) return(v)
  }
  NULL
}