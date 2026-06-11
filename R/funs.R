#' Check sensor type of an IMU vector
#' 
#' Determine if an IMU vector inherits from a particular class. These functions
#' return `TRUE` for `imu` vectors of the given subclass and `FALSE` for all
#' other objects.
#'
#' @param x An object
#' 
#' @return `TRUE` if the object inherits from the indicated subclass. `FALSE`
#'   otherwise.
#' 
#' @name imu-predicates
#' 
#' @examples
#' x <- acc(
#'   bursts = list(cbind(X = 1:5, Y = 1:5, Z = 1:5)),
#'   frequency = units::as_units(20, "Hz")
#' )
#' 
#' is_acc(x)
#'
#' is_mag(x)
#'
#' is_gyro(x)
NULL

#' Burst properties of an IMU vector
#'
#' @description
#' These functions describe characteristics of the bursts in an IMU vector.
#' 
#' - `n_axis()` — number of axes (columns) in each burst
#' - `n_samples()` — number of samples (rows) in each burst.
#' - `burst_dur()` — duration of each burst, in seconds.
#' - `burst_intervals()` — interval between each burst and its preceding burst, 
#'   in seconds.
#' - `imu_units()` — units for each burst's data values
#' - `is_uniform()` — logical indicating whether every burst in a vector shares
#'   a consistent structure (axes, frequency, sample count, and units)
#'
#' @details
#' `burst_intervals()` measures intervals between consecutive bursts in vector
#' order. Element `i` is the interval preceding burst `i` (measured against
#' burst `i - 1`), so the first element is always `NA`. An interval is also 
#' `NA` wherever either burst lacks a start time, meaning that missing bursts
#' may mask the true intervals between bursts that do exist. To avoid this, 
#' remove missing bursts from your IMU vector prior to running 
#' `burst_intervals()`.
#'
#' Pass `ids` to measure intervals within groups (e.g. per animal). Intervals
#' are not measured across group boundaries. Intervals 
#' are taken in vector order, so a vector mixing sources should be ordered by
#' group.
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`)
#' @param from For `burst_intervals()`, where to measure each interval from:
#'   `"end"` (default) gives the gap between the end of the previous burst and
#'   the start of the current one, while `"start"` gives the time between
#'   consecutive burst starts.
#' @param ids For `burst_intervals()`, an optional sorted vector the same 
#'   length as `x` giving the group (e.g. animal ID) of each burst. Intervals
#'   are not measured across changes in `ids`.
#'
#' @return `is_uniform()` returns a length-1 logical. All others return a
#'   vector of `length(x)`
#'
#' @name imu-properties
#'
#' @examples
#' x <- acc(
#'   bursts = list(
#'     cbind(X = sin(1:30 / 10), Y = cos(1:30 / 10), Z = 1),
#'     cbind(X = sin(1:20 / 10 + 2), Y = cos(1:20 / 10 + 3))
#'   ),
#'   frequency = units::as_units(c(20, 30), "Hz"),
#'   start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC") + c(0, 60)
#' )
#'
#' # Number of axes for which data was collected
#' n_axis(x)
#'
#' # Number of samples in the burst
#' n_samples(x)
#'
#' # Time duration of the burst
#' burst_dur(x)
#'
#' # Gap from the end of each burst to the start of the next
#' burst_intervals(x)
#'
#' # Or measure between consecutive burst starts
#' burst_intervals(x, from = "start")
#'
#' # Units for the burst data
#' imu_units(x)
#'
#' # Check if all bursts have uniform structure
#' is_uniform(x)
NULL

#' Access and modify fields of an IMU vector
#'
#' Access or update the underlying burst matrices, sampling frequencies, or 
#' start times for each burst in an IMU vector. 
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`)
#' @param value Replacement value.
#' 
#' @return For accessors, the corresponding field of `x`. For setters, `x` 
#'   with the updated value in the indicated field.
#' 
#' @name imu-fields
#' 
#' @examples
#' x <- acc(
#'   bursts = list(
#'     cbind(X = sin(1:20 / 10), Y = cos(1:20 / 10), Z = 1)
#'   ),
#'   frequency = units::as_units(20, "Hz"),
#'   start = as.POSIXct("2020-01-01 00:00:00", tz = "UTC")
#' )
#' 
#' bursts(x)
#' 
#' freqs(x)
#' 
#' starts(x)
#' 
#' freqs(x) <- units::as_units(25, "Hz")
#' freqs(x)
NULL

#' @export
#' @rdname imu-properties
n_axis <- function(x) {
  r <- rep(NA_integer_, vec_size(x))
  r[!is.na(x)] <- purrr::map_int(bursts(x[!is.na(x)]), ncol)
  r
}

#' @export
#' @rdname imu-properties
n_samples <- function(x) {
  purrr::map_int(bursts(x), function(b) nrow(b) %||% NA_integer_)
}

#' @export
#' @rdname imu-properties
burst_dur <- function(x) {
  units::set_units(n_samples(x) / freqs(x), "s")
}

#' @export
#' @rdname imu-properties
burst_intervals <- function(x, ids = NULL, from = "end") {
  from <- rlang::arg_match(from, c("end", "start"))

  n <- vec_size(x)
  
  if (n == 0) {
    return(units::set_units(numeric(0), "s"))
  }
  
  st <- as.numeric(starts(x))
  gap <- st - c(NA_real_, utils::head(st, -1L))

  if (from == "end") {
    dur <- as.numeric(burst_dur(x))
    gap <- gap - c(NA_real_, utils::head(dur, -1L))
  }
  
  # Don't get interval across an ID boundary
  if (!is.null(ids)) {
    if (length(ids) != n) {
      cli::cli_abort("{.arg ids} must be the same length as {.arg x}.")
    }
    same_group <- c(
      FALSE,
      (ids[-1] == ids[-n]) | (is.na(ids[-1]) & is.na(ids[-n]))
    )
    same_group[is.na(same_group)] <- FALSE
    gap[!same_group] <- NA_real_
  }

  # POSIXct as.numeric is seconds since epoch, so differences are seconds.
  units::set_units(gap, "s")
}

#' @export
#' @rdname imu-properties
imu_units <- function(x) {
  purrr::map_chr(
    bursts(x),
    function(b) {
      tryCatch(as.character(units(b)), error = function(cnd) NA_character_)
    }
  )
}

#' @export
#' @importFrom stats na.omit
#' @rdname imu-properties
is_uniform <- function(x) {
  unit_str <- function(b) {
    if (inherits(b, "units")) units::deparse_unit(b) else NA_character_
  }
  all(duplicated(na.omit(n_samples(x)))[-1]) &&
    all(duplicated(na.omit(n_axis(x)))[-1]) &&
    all(duplicated(na.omit(freqs(x)))[-1]) &&
    all(duplicated(purrr::map(bursts(x[!is.na(x)]), colnames))[-1]) &&
    all(duplicated(purrr::map_chr(bursts(x[!is.na(x)]), unit_str))[-1])
}

#' @export
#' @rdname imu-fields
bursts <- function(x) {
  field(x, "bursts")
}

#' @rdname imu-fields
#' @export
`bursts<-` <- function(x, value) {
  field(x, "bursts") <- value
  x
}

#' @export
#' @rdname imu-fields
freqs <- function(x) {
  field(x, "frequency")
}

#' @rdname imu-fields
#' @export
`freqs<-` <- function(x, value) {
  field(x, "frequency") <- value
  x
}

#' @export
#' @rdname imu-fields
starts <- function(x) {
  field(x, "start")
}

#' @rdname imu-fields
#' @export
`starts<-` <- function(x, value) {
  field(x, "start") <- value
  x
}

# TODO finish function and export?
static_acc <- function(x) {
  # should this return a list or a dataframe
  # TODO fix NA
  lapply(bursts(x)[!is.na(x)], colMeans)
}
