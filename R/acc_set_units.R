#' Set or convert units on burst data in an `acc` vector
#' 
#' @description
#' Attach new units or convert existing units to all bursts in an input `acc`
#' vector. See [units::set_units()].
#' 
#' Note that this function simply attaches or converts directly between unit
#' definitions. To calibrate the raw accelerometer measurements, use 
#' [acc_calibrate()].
#'
#' @param acc An `acc` vector.
#' @param units Character specifying the target units (e.g., `"m/s^2"`). For
#'   units in terms of gravitational acceleration, use `"standard_free_fall"`.
#'
#' @returns An `acc` vector with units attached to its burst matrices.
#'
#' @seealso [acc_calibrate()] to calibrate raw acceleration values.
#'
#' @export
#'
#' @examples
#' a <- acc_example()
#'
#' # Attach units to unitless bursts
#' acc_set_units(a, "m/s^2")
#'
#' # Convert between units
#' a_ms2 <- acc_set_units(a, "m/s^2")
#' acc_set_units(a_ms2, "standard_free_fall")
acc_set_units <- function(acc, units) {
  assertthat::assert_that(inherits(acc, "acc"))
  assertthat::assert_that(is.character(units), length(units) == 1)

  field(acc, "bursts") <- new_acc_list(
    map_acc(acc, function(.br) {
      if (is.null(.br)) return(NULL)
      nms <- colnames(.br)
      .br <- units::set_units(.br, units, mode = "standard")
      colnames(.br) <- nms
      .br
    })
  )

  acc
}
