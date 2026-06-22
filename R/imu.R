# Registry of supported IMU subclasses. Extend this when adding a new
# concrete IMU type. Helpers like `assert_imu()` and
# `parse_colsets()` read from it.
valid_imu_types <- function() {
  c("acc", "mag", "gyro")
}

# Parent class `imu` defines the shared (bursts, frequency, start)
# record shape for various IMU types that vary by subclass (e.g. `acc`)
imu <- function(sensor,
                bursts = list(),
                frequency = units::set_units(double(), "Hz"),
                start = NULL) {
  bursts <- burst_list(bursts, sensor)
  n <- vec_size(bursts)

  if (!inherits(frequency, "units")) {
    frequency <- units::set_units(frequency, "Hz")
  } else if (!units::ud_are_convertible(units::deparse_unit(frequency), "Hz")) {
    cli::cli_abort("{.arg frequency} must be convertible to a frequency unit.")
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

  new_imu(
    sensor,
    bursts = bursts,
    frequency = frequency,
    start = start
  )
}

new_imu <- function(sensor,
                    bursts = new_burst_list(list(), sensor),
                    frequency = units::set_units(double(), "Hz"),
                    start = as.POSIXct(double(), tz = "UTC")) {
  new_rcrd(
    list(bursts = bursts, frequency = frequency, start = start),
    class = c(sensor, "imu")
  )
}

burst_list <- function(x, sensor) {
  valid_axes <- c("X", "Y", "Z")

  is_valid <- purrr::map_lgl(
    x,
    function(b) {
      if (is.null(b)) {
        return(TRUE)
      }
      nms <- colnames(b)
      !is.null(nms) && length(nms) > 0 && all(nms %in% valid_axes)
    }
  )

  if (any(!is_valid)) {
    cli::cli_abort("Burst matrix columns must be named {.val X}, {.val Y}, or {.val Z}.")
  }

  new_burst_list(x, sensor)
}

new_burst_list <- function(x, sensor) {
  new_list_of(
    x,
    ptype = matrix(numeric()),
    class = c(paste0(sensor, "_list"), "burst_list")
  )
}

# Assert that x is one of the supported IMU vector classes. Centralizes the
# check so the error message lists concrete subclasses (e.g. `acc`, `mag`)
# without exposing the internal `imu` parent class.
assert_imu <- function(x,
                       arg = rlang::caller_arg(x),
                       call = rlang::caller_env()) {
  if (inherits(x, "imu")) {
    return(invisible(x))
  }

  types <- valid_imu_types()
  types_fmt <- paste0(
    vapply(types, function(t) cli::format_inline("{.cls {t}}"), character(1)),
    collapse = "/"
  )

  cli::cli_abort(
    "{.arg {arg}} must be an IMU vector ({types_fmt}).",
    call = call
  )
}

#' @export
is.na.imu <- function(x) {
  vctrs::vec_detect_missing(x)
}

# Shared implementations of vctrs type-combination logic. Subclasses need
# their own methods, but can simply call these methods
imu_ptype2 <- function(x, y, ..., x_arg = "", y_arg = "") {
  freq_common <- vctrs::vec_ptype2(freqs(x), freqs(y))
  start_common <- vctrs::vec_ptype2(starts(x), starts(y))

  new_imu(
    class(x)[1],
    frequency = freq_common,
    start = start_common
  )
}

imu_cast <- function(x, to, ..., x_arg = "", to_arg = "") {
  freqs(x) <- units::set_units(freqs(x), units::deparse_unit(freqs(to)), mode = "standard")
  x
}
