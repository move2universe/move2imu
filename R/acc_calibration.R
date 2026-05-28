#' Create calibration functions for raw acceleration values
#'
#' @description
#' Generate an `acc_calibration` object containing a list of functions
#' with various calibration parameters to be used in [transform_imu()].
#'
#'   - Use `acc_calibration()` to specify calibration parameters manually.
#'     Arguments are vectorized and matched by index.
#'   - Use `as_acc_calibration()` to convert a data.frame containing row-wise
#'     burst calibration parameters to an `acc_calibration` object.
#'
#' This allows you to specify burst-specific calibration functions to
#' flexibly convert raw acceleration values to physical units in `acc` vectors
#' that contain data from heterogeneous sources.
#'
#' @details
#' Tags from e-obs have default calibration functions that vary depending on the
#' tag's generation. Use [eobs_default_specs()] for a summary table showing the
#' default offset, slope, and orientation parameters used for each e-obs
#' tag ID. The tag ID defines the tag generation. Note that tags from generation
#' 1 could be set either to low or high sensitivity, each with their own
#' default calibration parameters.
#' 
#' @param manufacturer Manufacturer of the tag. Currently, `"eobs"` and
#'   `"ornitela"` are supported. For other manufacturers, leave `NULL` and
#'   manually specify the calibration parameters below.
#' @param tag_id If `manufacturer = "eobs"`, the e-obs tag ID for the tag. See
#'   details.
#' @param sensitivity If `manufacturer = "eobs"`, the sensitivity of the tag.
#'   Defaults to `"low"` if none provided. See details.
#' @param offset,offset_x,offset_y,offset_z Custom offset to use when
#'   calibrating. To specify axis-specific offsets, use `offset_x`,
#'   `offset_y`, and/or `offset_z`.
#'   
#'   Required if no `manufacturer` is specified.
#' @param slope,slope_x,slope_y,slope_z Custom slope to use when
#'   calibrating. To specify axis-specific slope, use `slope_x`,
#'   `slope_y`, and/or `slope_z`.
#'   
#'   Required if no `manufacturer` is specified.
#' @param orientation,orientation_x,orientation_y,orientation_z Either `1` or
#'   `-1` indicating the orientation of the tag's axes. To 
#'   specify axis-specific orientations, use `orientation_x`, `orientation_y`, 
#'   and/or `orientation_z`. Defaults to `1`.
#'   
#'   This is useful to standardize orientations across tags of different
#'   manufacturers or generations.
#' @param units Output units. Either `"m/s^2"` (default) or `"standard_free_fall"`.
#' @param axes Character string specifying which axes to calibrate, e.g.,
#'   `"XYZ"` (default), `"XY"`, or `"Z"`. Only these axes will appear in the
#'   calibrated output.
#'
#' @returns An `acc_calibration` object.
#' @export
#'
#' @seealso [transform_imu()] to apply calibration functions to the entries
#'   in an `acc` vector.
#'
#' @examples
#' # Calibration for ornitela tags:
#' acc_calibration(manufacturer = "ornitela")
#' 
#' # E-obs tag defaults vary by tag_id and sensitivity (default `"low"`)
#' acc_calibration(manufacturer = "eobs", tag_id = 1000, sensitivity = "high")
#' acc_calibration(manufacturer = "eobs", tag_id = 4000)
#' 
#' # Provide vector arguments to generate element-wise calibrations:
#' acc_calibration(
#'   manufacturer = c("eobs", "ornitela"),
#'   tag_id = c(1000, NA)
#' )
#'
#' # Calibration with explicit offset and slope
#' acc_calibration(offset = 2048, slope = 1 / 512)
#' 
#' # Calibrate specific axes with axis-specific args:
#' cal <- acc_calibration(
#'   offset_x = 2048, 
#'   offset_y = 2046,
#'   offset_z = 2048,
#'   slope = 1 / 512, 
#'   orientation_y = -1 # Flip y axis orientation
#' )
#' 
#' # Apply calibration with transform_imu()
#' transform_imu(acc_example(), cal)
#'
#' # Convert a data.frame of calibration specs into a calibration vector
#' # (Useful if specifications are stored as metadata alongside acc bursts)
#' cal <- as_acc_calibration(
#'   data.frame(manufacturer = "eobs", tag_id = c(1000, 4000))
#' )
acc_calibration <- function(manufacturer = NULL,
                            tag_id = NULL,
                            sensitivity = NULL,
                            offset = NULL,
                            offset_x = offset,
                            offset_y = offset,
                            offset_z = offset,
                            slope = NULL,
                            slope_x = slope,
                            slope_y = slope,
                            slope_z = slope,
                            orientation = NULL,
                            orientation_x = orientation,
                            orientation_y = orientation,
                            orientation_z = orientation,
                            units = "m/s^2",
                            axes = "XYZ") {
  args <- list(
    tag_id = tag_id,
    manufacturer = manufacturer,
    sensitivity = sensitivity,
    offset_x = offset_x,
    offset_y = offset_y,
    offset_z = offset_z,
    slope_x = slope_x,
    slope_y = slope_y,
    slope_z = slope_z,
    orientation_x = orientation_x,
    orientation_y = orientation_y,
    orientation_z = orientation_z,
    units = units,
    axes = axes
  )
  
  # Coerce NULLs to NA so they recycle as length 1, not length 0
  args <- purrr::map(
    args,
    function(x) {
      if (is.null(x)) {
        NA
      } else {
        x
      }
    }
  )
  
  new_imu_calibration(purrr::pmap(args, acc_calibration_), sensor = "acc")
}

#' @param df data.frame containing columns corresponding to the available
#'   arguments in `acc_calibration()`
#' 
#' @export
#'
#' @rdname acc_calibration
as_acc_calibration <- function(df) {
  assertthat::assert_that(is.data.frame(df))

  args <- list(
    tag_id = df[["tag_id"]],
    manufacturer = df[["manufacturer"]],
    sensitivity = df[["sensitivity"]],
    offset_x = resolve_axis_col(df, "offset", "x"),
    offset_y = resolve_axis_col(df, "offset", "y"),
    offset_z = resolve_axis_col(df, "offset", "z"),
    slope_x = resolve_axis_col(df, "slope", "x"),
    slope_y = resolve_axis_col(df, "slope", "y"),
    slope_z = resolve_axis_col(df, "slope", "z"),
    orientation_x = resolve_axis_col(df, "orientation", "x"),
    orientation_y = resolve_axis_col(df, "orientation", "y"),
    orientation_z = resolve_axis_col(df, "orientation", "z"),
    units = df[["units"]] %||% "m/s^2",
    axes = df[["axes"]] %||% "XYZ"
  )
  
  do.call(acc_calibration, args)
}

acc_calibration_ <- function(manufacturer = NULL,
                             tag_id = NULL,
                             sensitivity = NULL,
                             offset_x = NULL,
                             offset_y = NULL,
                             offset_z = NULL,
                             slope_x = NULL,
                             slope_y = NULL,
                             slope_z = NULL,
                             orientation_x = NULL,
                             orientation_y = NULL,
                             orientation_z = NULL,
                             units = "m/s^2",
                             axes = "XYZ") {
  rlang::arg_match(units, c("m/s^2", "standard_free_fall"))
  axes <- strsplit(toupper(gsub("\\s", "", axes)), "")[[1]]
  
  # Resolve manufacturer defaults, then let user-provided values override
  if (!rlang::is_null(manufacturer) && !rlang::is_na(manufacturer)) {
    if (manufacturer == "eobs") {
      if (rlang::is_null(tag_id)) {
        rlang::abort("`tag_id` must be provided when `manufacturer = \"eobs\"`")
      }
      
      specs <- eobs_specs(tag_id, first_valid(sensitivity, "low"))
    } else if (manufacturer == "ornitela") {
      specs <- ornitela_specs()
    } else {
      rlang::abort(c(
        paste0("Unrecognized manufacturer: \"", manufacturer, "\""),
        i = "If provided, `manufacturer` must be \"eobs\" or \"ornitela\""
      ))
    }
    
    # User-provided values take priority over manufacturer defaults
    offset_x <- first_valid(offset_x, specs$offset)
    offset_y <- first_valid(offset_y, specs$offset)
    offset_z <- first_valid(offset_z, specs$offset)
    
    slope_x <- first_valid(slope_x, specs$slope)
    slope_y <- first_valid(slope_y, specs$slope)
    slope_z <- first_valid(slope_z, specs$slope)
    
    orientation_x <- first_valid(orientation_x, specs$orientation_x, 1)
    orientation_y <- first_valid(orientation_y, specs$orientation_y, 1)
    orientation_z <- first_valid(orientation_z, specs$orientation_z, 1)
  } else {
    # Custom path: offset and slope are required
    if (null_or_na(offset_x) && null_or_na(offset_y) && null_or_na(offset_z)) {
      rlang::abort("`offset` is required when no `manufacturer` is provided")
    }
    if (null_or_na(slope_x) && null_or_na(slope_y) && null_or_na(slope_z)) {
      rlang::abort("`slope` is required when no `manufacturer` is provided")
    }
  }
  
  # Fill remaining missing orientation with default
  orientation_x <- first_valid(orientation_x, 1L)
  orientation_y <- first_valid(orientation_y, 1L)
  orientation_z <- first_valid(orientation_z, 1L)
  
  assertthat::assert_that(orientation_x == -1L || orientation_x == 1L)
  assertthat::assert_that(orientation_y == -1L || orientation_y == 1L)
  assertthat::assert_that(orientation_z == -1L || orientation_z == 1L)
  
  # Restructure for `sweep()` later
  offset <- c(X = offset_x, Y = offset_y, Z = offset_z)
  slope <- c(X = slope_x, Y = slope_y, Z = slope_z)
  orientation <- c(X = orientation_x, Y = orientation_y, Z = orientation_z)
  
  scale <- slope * orientation
  
  cal <- function(x) {
    # Resolve axes against what's actually in the data
    active_axes <- intersect(axes, colnames(x))
    
    offset <- offset[active_axes]
    scale <- scale[active_axes]
    
    # Warn if any active axes have no calibration parameters
    na_axes <- active_axes[is.na(offset) | is.na(scale)]
    
    if (length(na_axes) > 0) {
      rlang::warn(paste0(
        "Missing calibration parameters for axis: ",
        paste0(na_axes, collapse = ", "),
        ". These axes will produce NA values."
      ))
    }
    
    # Apply calibration
    xt <- sweep(x[, active_axes, drop = FALSE], 2, offset, `-`)
    xt <- sweep(xt, 2, scale, `*`)
    
    if (units == "m/s^2") {
      xt <- xt * GRAV_CONST
    }
    
    colnames(xt) <- active_axes
    units::set_units(xt, units, mode = "standard")
  }
  
  imu_calibration_fn(cal)
}

#' Default e-obs tag configuration table
#'
#' Returns a data.frame of known e-obs tag generations with their tag ID
#' ranges and default calibration parameters.
#'
#' @return A data.frame with columns `tag_gen`, `min_tag_id`, `max_tag_id`,
#'   `sensitivity`, `orientation_x`, `orientation_y`, `orientation_z`,
#'   `offset`, and `slope`.
#'
#' @seealso [acc_calibration()] to set up tag-specific calibration specifications
#'   and [transform_imu()] to apply them to eobs acceleration values.
#'
#' @export
eobs_default_specs <- function() {
  data.frame(
    tag_gen = c(1, 1, 2, 3),
    min_tag_id = c(1, 1, 2242, 4118),
    max_tag_id = c(2241, 2241, 4117, Inf),
    sensitivity = c("low", "high", "low", "low"),
    orientation_x = c(1, 1, 1, 1),
    orientation_y = c(1, 1, -1, -1),
    orientation_z = c(1, 1, 1, 1),
    offset = c(2048, 2048, 2048, 2048),
    slope = c(0.0027, 0.001, 0.0022, 1/512)
  )
}

#' Look up e-obs tag calibration specifications
#'
#' Returns the offset, slope, and per-axis orientation parameters for one or
#' more e-obs tags based on their tag IDs and sensitivity settings. Tag
#' specifications are looked up from [eobs_default_specs()].
#'
#' @param tag_id Numeric e-obs tag ID(s). May be a vector for multiple tags.
#' @param sensitivity Accelerometer sensitivity setting(s): `"low"` (default)
#'   or `"high"`. Recycled to match the length of `tag_id`.
#'
#' @return A data.frame with columns `tag_id`, `offset`, `slope`,
#'   `orientation_x`, `orientation_y`, and `orientation_z`, one row per input
#'   tag ID.
#' @noRd
eobs_specs <- function(tag_id, sensitivity = "low") {
  tag_id <- as.numeric(tag_id)
  sensitivity <- rep_len(sensitivity, length(tag_id))
  rlang::arg_match(sensitivity, c("low", "high"), multiple = TRUE)
  
  if (any(is.na(tag_id))) {                                                                                                                                                                                     
    rlang::abort("Cannot look up eobs tag specs for missing `tag_id`")                                                                                                                                            
  }
  
  config <- eobs_default_specs()
  
  purrr::map2_dfr(
    tag_id,
    sensitivity,
    function(tid, sens) {
      matches <- config[tid >= config$min_tag_id &
                          tid <= config$max_tag_id &
                          config$sensitivity == sens, ]
      
      if (nrow(matches) == 0) {
        rlang::abort(c(
          paste0("Could not find an e-obs tag matching ID \"", tid, "\" and sensitivity \"", sens, "\"."),
          i = "See `eobs_default_specs()` for expected e-obs tag config parameters."
        ))
      } else if (nrow(matches) > 1) {
        rlang::abort(c(
          paste0("Multiple tags matched ID ", tid, " and sensitivity \"", sens, "\"."),
          i = "See `eobs_default_specs()` for expected e-obs tag config parameters."
        ))
      }
      
      data.frame(
        tag_id = tid,
        offset = matches$offset,
        slope = matches$slope,
        orientation_x = matches$orientation_x,
        orientation_y = matches$orientation_y,
        orientation_z = matches$orientation_z
      )
    }
  )
}

#' Look up Ornitela tag calibration specifications
#'
#' Returns the default offset, slope, and per-axis orientation parameters for
#' Ornitela tags.
#'
#' @return A data.frame with columns `offset`, `slope`, `orientation_x`,
#'   `orientation_y`, and `orientation_z`.
#' @noRd
ornitela_specs <- function() {
  data.frame(
    offset = 0, 
    slope = 0.001, 
    orientation_x = 1, 
    orientation_y = 1, 
    orientation_z = 1
  )
}

# Resolve per-axis column from a data.frame, falling back to scalar column.
# Axis-specific values take priority; NAs in the axis-specific column are
# filled from the scalar column where available.
resolve_axis_col <- function(df, col, axis) {
  axis_col <- paste0(col, "_", tolower(axis))
  v_axis <- df[[axis_col]]
  v_scalar <- df[[col]]
  
  if (!is.null(v_axis) && !is.null(v_scalar)) {
    # Prefer axis-specific; fill NAs from scalar
    ifelse(is.na(v_axis), v_scalar, v_axis)
  } else {
    v_axis %||% v_scalar
  }
}

GRAV_CONST <- 9.80665
