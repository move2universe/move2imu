# Colset constructor -----------------------------------------------------------

#' Specify IMU data columns present in a `move2` object
#'
#' @description
#' Define which columns in a `move2` object contain IMU data. Pass the
#' result as the `colset` argument of [as_acc()], [as_mag()], or [as_gyro()]
#' to convert those columns into an IMU vector.
#'
#' `move2` objects store IMU data in two ways:
#'
#' - **Long-format** columns store one measurement (possibly for multiple axes)
#'   in a single row.
#'
#' - **Burst-format** columns store a burst of measurements as a space-delimited
#'   string. This string must be segmented into axis-specific measurements using
#'   an associated column that indicates the axes present for the bursted data.
#'   A further column provides the sampling frequency of the burst. All three
#'   of these columns must be present to form a valid burst-format column set.
#'   
#' @param x,y,z (Long-format) Column name(s) for the X, Y, and/or Z axes.
#' @param bursts (Burst-format) Column name containing the raw burst strings.
#' @param axes (Burst-format) Column name containing the axis labels for
#'   each burst.
#' @param frequency (Burst-format) Column name containing the sampling
#'   frequency for each burst.
#'
#' @returns An `imu_colset` object of type `"long"` or `"burst"`.
#'
#' @seealso [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a move2
#'   object.
#'   
#'   [active_acc_colsets()], [active_mag_colsets()], [active_gyro_colsets()] to
#'   identify IMU colsets present in a move2 object.
#'   
#'   [movebank_acc_colsets()], [movebank_mag_colsets()], [movebank_gyro_colsets()]
#'   to see column sets provided by Movebank.
#'
#' @export
#'
#' @examples
#' # Long-format: one or more axes
#' imu_colset(x = "my_x", y = "my_y", z = "my_z")
#' imu_colset(x = "my_x", y = "my_y")
#'
#' # Burst-format: all three columns required
#' imu_colset(bursts = "my_raw", axes = "my_axes", frequency = "my_freq")
#' 
#' # Use a colset to extract IMU data from those columns in a move2 object
#' as_acc(gulls(), colset = imu_colset(x = "acceleration_raw_x"))
imu_colset <- function(x = NULL,
                       y = NULL,
                       z = NULL,
                       bursts = NULL,
                       axes = NULL,
                       frequency = NULL) {
  long_args <- purrr::compact(list(X = x, Y = y, Z = z))
  burst_args <- purrr::compact(list(bursts = bursts, axes = axes, frequency = frequency))
  
  has_long <- length(long_args) > 0
  has_burst <- length(burst_args) > 0
  
  if (has_long && has_burst) {
    rlang::abort(c(
      "Cannot mix long-format and burst-format columns in a single imu_colset.",
      i = "Use either `x`/`y`/`z` (long-format) or `bursts`/`axes`/`frequency` (burst-format)."
    ))
  }
  
  if (!has_long && !has_burst) {
    rlang::abort("No IMU data columns specified.")
  }
  
  if (has_burst) {
    if (length(burst_args) != 3) {
      rlang::abort(
        "Burst format requires `bursts`, `axes`, and `frequency` columns."
      )
    }
    
    cols <- unlist(burst_args)
    type <- "burst"
  } else {
    cols <- unlist(long_args)
    type <- "long"
  }
  
  new_imu_colset(cols = cols, type = type)
}

#' @export
print.imu_colset <- function(x, ...) {
  type <- attr(x, "type")

  cat(paste0(
    type, "-format [",
    paste0(names(x), "=\"", unclass(x), "\"", collapse = ", "),
    "]\n"
  ))

  invisible(x)
}

# Default supported colsets ----------------------------------------------------

#' View standard Movebank IMU data column sets
#'
#' @description
#' Movebank has several standard ways to store data for each IMU sensor. These
#' functions show the recognized columns for each sensor that can be extracted
#' from a `move2` object by default.
#'
#' - `movebank_acc_colsets()` — standard column sets for [as_acc()].
#' - `movebank_mag_colsets()` — standard column sets for [as_mag()].
#' - `movebank_gyro_colsets()` — standard column sets for [as_gyro()].
#'
#' To extract IMU data from a `move2` with column names that don't correspond to
#' Movebank's conventions, provide a custom set of IMU columns with 
#' [imu_colset()].
#'
#' @details
#' `move2` objects store IMU data in two ways:
#'
#' - **Long-format** columns store one measurement (possibly for multiple axes)
#'   in a single row.
#'
#' - **Burst-format** columns store a burst of measurements as a space-delimited
#'   string. This string must be segmented into axis-specific measurements using
#'   an associated column that indicates the axes present for the bursted data.
#'   A further column provides the sampling frequency of the burst. All three
#'   of these columns must be present to form a valid burst-format column set.
#'   
#' @returns A named list of `imu_colset` objects.
#' 
#' @seealso [active_acc_colsets()], [active_mag_colsets()], [active_gyro_colsets()]
#'   to identify column sets present in a given `move2` object.
#'
#' @name movebank_colsets
#'
#' @examples
#' movebank_acc_colsets()
#' 
#' movebank_mag_colsets()
#' 
#' movebank_gyro_colsets()
NULL

#' @export
#' @rdname movebank_colsets
movebank_acc_colsets <- function() {
  purrr::map(acc_colset_config(), function(colset) colset$cols)
}

#' @export
#' @rdname movebank_colsets
movebank_mag_colsets <- function() {
  purrr::map(mag_colset_config(), function(colset) colset$cols)
}

#' @export
#' @rdname movebank_colsets
movebank_gyro_colsets <- function() {
  purrr::map(gyro_colset_config(), function(colset) colset$cols)
}

# Active colsets in a move2 object ---------------------------------------------

#' Identify IMU columns present in a `move2` object
#'
#' @description
#' Determine the column sets that will be used by default when extracting IMU
#' data from a `move2` object. Column sets are processed independently, but a
#' single `move2` may contain multiple active column sets for one IMU sensor.
#'
#' - `active_acc_colsets()` — column sets used by [as_acc()].
#' - `active_mag_colsets()` — column sets used by [as_mag()].
#' - `active_gyro_colsets()` — column sets used by [as_gyro()].
#' 
#' If no active colsets are found, you can use [imu_colset()] to specify 
#' a custom set of columns that contain IMU data.
#'
#' @param x A `move2` object.
#'
#' @returns A list of `imu_colset` objects.
#'
#' @name active_colsets
#' 
#' @inherit movebank_colsets details
#'
#' @seealso [movebank_acc_colsets()], [movebank_mag_colsets()], 
#'   [movebank_gyro_colsets()] for the supported default colsets.
#'   
#'   [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   `move2` object.
#'
#' @examples
#' active_acc_colsets(albatrosses())
#'
#' # Multiple colsets may be available
#' active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
#'
#' # Missing long-format axes are not included in the set
#' g <- gulls()
#' g$acceleration_raw_x <- NULL
#' active_acc_colsets(g)
#'
#' # Columns with no data are also removed
#' g$acceleration_raw_y <- NA
#' active_acc_colsets(g)
#'
#' # Some column sets must be present in their entirety
#' alb <- albatrosses()
#' alb$eobs_acceleration_axes <- NULL
#'
#' \dontrun{
#'   active_acc_colsets(alb)
#' }
NULL

#' @export
#' @rdname active_colsets
active_acc_colsets <- function(x) {
  active_colsets_(x, "acc")
}

#' @export
#' @rdname active_colsets
active_mag_colsets <- function(x) {
  active_colsets_(x, "mag")
}

#' @export
#' @rdname active_colsets
active_gyro_colsets <- function(x) {
  active_colsets_(x, "gyro")
}

# Apply active colset logic in a move2 for a given IMU class. Active colsets
# are fully present (if burst-format) and contain data.
active_colsets_ <- function(x, sensor) {
  force(x)
  config <- switch(
    sensor,
    acc = acc_colset_config(),
    mag = mag_colset_config(),
    gyro = gyro_colset_config()
  )
  i <- which(purrr::map_lgl(config, function(colset) colset$is_in_(x)))
  
  if (length(i) == 0) {
    abort_missing_colset(sensor)
  }
  
  poss_colsets <- config[i]
  
  colsets <- purrr::compact(
    purrr::map(
      poss_colsets,
      function(colset_config) {
        colset <- colset_config$cols
        cols_in_x <- colset[colset %in% colnames(x)]
        cols_present <- cols_in_x[!cols_empty(x, cols_in_x)]

        if (!setequal(cols_present, colset)) {
          if (attr(colset, "type") == "burst") {
            # Remove entire colset for types that require all cols present
            return(NULL)
          }

          # Rebuild long-format colset with only present columns
          return(new_imu_colset(cols = cols_present, type = attr(colset, "type")))
        }

        colset
      }
    )
  )
  
  if (length(colsets) == 0) {
    abort_missing_colset(sensor)
  }
  
  colsets
}

#' Identify rows of a `move2` object with duplicated IMU data
#'
#' @description
#' Return the row indices of a `move2` object where more than one column set
#' for a given sensor contains data. Functions that extract IMU data 
#' will error if a single timestamp contains multiple sources of IMU data
#' for the same sensor.
#' 
#' To resolve duplicated rows, pass a specific set of IMU columns to the
#' `colset` argument of `as_*()` or remove the duplicated data.
#'
#' - `duplicated_acc_rows()` — checks acceleration column sets used by [as_acc()].
#' - `duplicated_mag_rows()` — checks magnetometer column sets used by [as_mag()].
#' - `duplicated_gyro_rows()` — checks gyroscope column sets used by [as_gyro()].
#'
#' @param x A `move2` object.
#' @param colsets List of `imu_colset` objects to check for overlap. Defaults
#'   to the column sets detected by the corresponding `active_*_colsets()`.
#'
#' @returns An integer vector of row indices with duplicated data across
#'   column sets.
#'
#' @name duplicated_rows
#' 
#' @keywords internal
#'
#' @seealso [active_acc_colsets()], [active_mag_colsets()],
#'   [active_gyro_colsets()] to identify available column sets in a `move2`
#'   object. 
#'   
#'   [as_acc()], [as_mag()], [as_gyro()] to extract IMU data from a
#'   `move2` object.
NULL

#' @export
#' @rdname duplicated_rows
duplicated_acc_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_acc_colsets(x))
}

#' @export
#' @rdname duplicated_rows
duplicated_mag_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_mag_colsets(x))
}

#' @export
#' @rdname duplicated_rows
duplicated_gyro_rows <- function(x, colsets = NULL) {
  duplicated_imu_rows(x, colsets %||% active_gyro_colsets(x))
}

duplicated_imu_rows <- function(x, colsets = NULL) {
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }
  
  rows <- unlist(
    purrr::map(
      colsets,
      function(cols) which_imu_vals(x, colset = cols)
    )
  )
  
  # Would be nice to return duplicated groups too so user knows what the issue is...
  sort(unique(rows[duplicated(rows) | duplicated(rows, fromLast = TRUE)]))
}

# Colset constructor helpers ---------------------------------------------------

# Internal constructor for `imu_colset` objects. Colsets are IMU-class-agnostic:
# the same colset can be passed to `as_acc()`, `as_mag()`, or `as_gyro()` -
# the IMU class is determined by which converter you call, not by the colset.
new_imu_colset <- function(cols, type) {
  type <- rlang::arg_match(type, c("long", "burst"))
  
  structure(
    cols,
    type = type,
    class = c("imu_colset", class(cols))
  )
}

is_imu_colset <- function(x) {
  inherits(x, "imu_colset")
}

# Colset config ----------------------------------------------------------------

# Registry of supported default colsets. Each entry is built
# via `register_colset()`, which derives the appropriate is_/is_in_ checks from
# the colset's own `type` attribute.
acc_colset_config <- function() {
  list(
    eobs = register_colset(acc_colset_eobs()),
    burst = register_colset(acc_colset_burst()),
    xyz = register_colset(acc_colset_xyz()),
    raw_xyz = register_colset(acc_colset_raw_xyz())
  )
}

mag_colset_config <- function() {
  list(
    burst = register_colset(mag_colset_burst()),
    xyz = register_colset(mag_colset_xyz()),
    raw_xyz = register_colset(mag_colset_raw_xyz())
  )
}

gyro_colset_config <- function() {
  list(
    burst = register_colset(gyro_colset_burst()),
    xyz = register_colset(gyro_colset_xyz())
  )
}


acc_colset_eobs <- function() {
  new_imu_colset(
    cols = c(
      axes = "eobs_acceleration_axes",
      frequency = "eobs_acceleration_sampling_frequency_per_axis",
      bursts = "eobs_accelerations_raw"
    ),
    type = "burst"
  )
}

acc_colset_burst <- function() {
  new_imu_colset(
    cols = c(
      axes = "acceleration_axes",
      frequency = "acceleration_sampling_frequency_per_axis",
      bursts = "accelerations_raw"
    ),
    type = "burst"
  )
}

acc_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "acceleration_x",
      Y = "acceleration_y",
      Z = "acceleration_z"
    ),
    type = "long"
  )
}

acc_colset_raw_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "acceleration_raw_x",
      Y = "acceleration_raw_y",
      Z = "acceleration_raw_z"
    ),
    type = "long"
  )
}

mag_colset_burst <- function() {
  new_imu_colset(
    cols = c(
      bursts = "magnetic_fields_raw",
      axes = "magnetic_field_axes",
      frequency = "magnetic_field_sampling_frequency_per_axis"
    ),
    type = "burst"
  )
}

mag_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "magnetic_field_x",
      Y = "magnetic_field_y",
      Z = "magnetic_field_z"
    ),
    type = "long"
  )
}

mag_colset_raw_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "magnetic_field_raw_x",
      Y = "magnetic_field_raw_y",
      Z = "magnetic_field_raw_z"
    ),
    type = "long"
  )
}

gyro_colset_burst <- function() {
  new_imu_colset(
    cols = c(
      bursts = "angular_velocities_raw",
      axes = "gyroscope_axes",
      frequency = "gyroscope_sampling_frequency_per_axis"
    ),
    type = "burst"
  )
}

gyro_colset_xyz <- function() {
  new_imu_colset(
    cols = c(
      X = "angular_velocity_x",
      Y = "angular_velocity_y",
      Z = "angular_velocity_z"
    ),
    type = "long"
  )
}

# Build a single config entry from a colset.
#
# - "burst" type colsets require all columns to be present
# - "long" type colsets allow subsets of the axis cols
#
# - `is_` checks whether a colset vector matches this entry (including subsets
#   for long format)
# - `is_in_` checks whether a `move2` object contains the required columns
#   (including subsets for long format)
register_colset <- function(cols) {
  if (attr(cols, "type") == "burst") {
    list(
      cols = cols,
      is_ = function(x) setequal(x, cols) && length(x) == length(cols),
      is_in_ = function(m) all(cols %in% colnames(m))
    )
  } else {
    list(
      cols = cols,
      is_ = function(x) is_unique_named_subset(x, cols),
      is_in_ = function(m) any(cols %in% colnames(m))
    )
  }
}

# General helpers --------------------------------------------------------------

# Check that `x` is a non-empty, non-duplicated, name-value subset of `target`
is_unique_named_subset <- function(x, y) {
  length(x) > 0 &&
    anyDuplicated(names(x)) == 0 &&
    identical(x[names(x)], y[names(x)])
}

cols_empty <- function(x, cols) {
  assert_all_cols_present(x, cols)
  purrr::map_lgl(
    cols,
    function(col) all(is.na(unlist(x[[col]]))) || all(rlang::is_empty(unlist(x[[col]])))
  )
}

assert_all_cols_present <- function(x, cols, call = rlang::caller_env()) {
  if (!all(cols %in% colnames(x))) {
    cols <- cols[which(!cols %in% colnames(x))]
    
    rlang::abort(
      c(
        "Missing columns provided.",
        x = paste0("Could not find columns \"", paste(cols, collapse = "\", \""), "\"")
      ),
      call = call
    )
  }
}

abort_missing_colset <- function(sensor, call = rlang::caller_env()) {
  rlang::abort(
    c(
      paste0("Could not identify a full ", sensor, " column set in the input data."),
      "i" = paste0(
        "Use `movebank_", sensor, "_colsets()` to see supported ", sensor, " column sets."
      )
    ),
    call = call
  )
}
