# Condition helpers ------------------------------------------------------------
#
# move2imu reports classed errors and warnings so that users and internal
# package code can reliably catch specific conditions, groups of conditions, or
# any move2imu condition.
#
# Errors/warnings explicitly raised by move2imu receive the base
# `move2imu_error` or `move2imu_warning` class. More specific classes can be
# added in cases where users or internal code may want to catch or act on a
# particular condition type.
#
# An arbitrary number of classes can be added in order of most specific
# to most general. The move2imu_error or move2imu_warning prefix is added
# automatically. All conditions also inherit from the standard
# classes reported by cli (or base R, for `stop_move2imu()`). For example, an
# error signaled with `class = c("colset_not_detected", "colset")` has classes
# `c("move2imu_error_colset_not_detected", "move2imu_error_colset", "move2imu_error")`
#
# Currently, we report the following condition groups:
#
#   move2imu_error_calibration: Calibration object construction and application
#   move2imu_error_colset: IMU column set specification and presence in a data source
#   move2imu_error_imu: Input that is not an IMU vector, or not for the expected sensor
#   move2imu_error_imu_column: Designated IMU column in a move2 missing or invalid
#   move2imu_error_timestamp: Missing or invalid timestamps
#   move2imu_error_units: Inconsistent or invalid units
#
# We also report several ungrouped individual condition classes.

# Signal a classed error. Wraps `cli::cli_abort()` with move2imu-specific
# condition classes.
abort_move2imu <- function(message,
                           class = NULL,
                           ...,
                           call = rlang::caller_env(),
                           .envir = parent.frame()) {
  cli::cli_abort(
    message,
    class = move2imu_condition_class("error", class),
    ...,
    call = call,
    .envir = .envir
  )
}

# Signal a classed warning. Wraps `cli::cli_warn()` with move2imu-specific
# condition classes.
warn_move2imu <- function(message,
                          class = NULL,
                          ...,
                          .envir = parent.frame()) {
  cli::cli_warn(
    message,
    class = move2imu_condition_class("warning", class),
    ...,
    .envir = .envir
  )
}

# Signal a classed error (base R). Wraps `stop()` with move2imu-specific
# condition classes. This is used in code that runs in a loop and whose
# errors are caught internally (cli's formatting is unnecessary in these cases
# and adds processing time).
stop_move2imu <- function(message, class) {
  stop(
    errorCondition(
      message,
      class = move2imu_condition_class("error", class),
      call = NULL
    )
  )
}

# Build the class vector for a condition `type` ("error" or "warning") by
# prefixing each entry of `class` and appending the package-wide class. With
# no `class`, only the package-wide class is returned.
move2imu_condition_class <- function(type, class = NULL) {
  prefix <- paste("move2imu", type, sep = "_")

  if (length(class) == 0) {
    return(prefix)
  }

  c(paste(prefix, class, sep = "_"), prefix)
}
