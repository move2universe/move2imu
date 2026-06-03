#' Identify rows in a `move2` that contain IMU data
#'
#' @description
#' These functions return logical vectors indicating the rows of an input
#' `move2` object that contain IMU data for the specified sensor.
#' 
#' @details
#' If `x` has data in more than one active IMU column set, `has_*()` will return
#' `TRUE`. However, these rows cannot be parsed by [as_acc()], [as_mag()], etc.
#' as they contain duplicated IMU data. To ensure that `has_*()` only considers
#' certain column sets, use the `colset` argument.
#'
#' If no active colset is detected (e.g. a `move2` with only GPS data),
#' `has_*()` returns `FALSE` for all rows.
#'
#' For expanded-format data (where multiple rows compose a single burst)
#' all rows that contain IMU data are flagged `TRUE`. However, the output
#' of `as_*()` will not necessarily return bursts at each of these locations,
#' as multiple of these rows will be combined into a single burst.
#'
#' @param x A `move2` object.
#' @param colset An `imu_colset` object or list of `imu_colset` objects
#'   specifying the columns to check for IMU data. By default, all active 
#'   colsets detected in `x` are considered (see [active_acc_colsets()]).
#'
#' @returns A logical vector the same length as `nrow(x)`. `TRUE` values
#'   indicate rows where IMU data is present under at least one active colset.
#'
#' @seealso [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   `move2` object.
#'   
#'   [active_acc_colsets()], [active_mag_colsets()], [active_gyro_colsets()] 
#'   to inspect which colsets are detected in `x`.
#'
#' @name has_imu
#'
#' @examples
#' alb <- albatrosses()
#'
#' head(has_acc(alb))
#'
#' # Filter to rows with acc data without building bursts
#' nrow(alb[has_acc(alb), ])
NULL

#' @rdname has_imu
#' @export
has_acc <- function(x, colset = NULL) {
  has_imu_(x, sensor = "acc", colset = colset)
}

#' @rdname has_imu
#' @export
has_mag <- function(x, colset = NULL) {
  has_imu_(x, sensor = "mag", colset = colset)
}

#' @rdname has_imu
#' @export
has_gyro <- function(x, colset = NULL) {
  has_imu_(x, sensor = "gyro", colset = colset)
}

# Check whether each row has IMU data for the given sensor and colset.
# Returns TRUE if any valid colset has data. Does not check for duplicate
# IMU data in a given row. Returns FALSE if no valid colsets are found.
has_imu_ <- function(x, sensor, colset = NULL) {
  colsets <- tryCatch(
    parse_colsets(x, colset, sensor, quiet = TRUE),
    move2imu_no_active_colset = function(e) NULL
  )
  
  out <- logical(nrow(x))

  if (is.null(colsets)) {
    return(out)
  }

  vals <- unique(unlist(
    lapply(colsets, function(cs) which_imu_vals(x, colset = cs))
  ))

  out[vals] <- TRUE
  out
}
