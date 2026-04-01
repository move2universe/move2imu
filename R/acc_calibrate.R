#' Convert raw acceleration values to physical units in an `acc` vector
#'
#' @description
#' Applies calibration function(s) to bursts of raw acceleration ADC values
#' in an `acc` vector. Specify a set of calibration functions 
#' with [acc_calibration()].
#'
#' @param acc An `acc` vector.
#' @param calibration An `acc_calibration` object containing the calibration 
#'   function(s) to apply to each burst in `acc`. See [acc_calibration()] to 
#'   specify calibration functions.
#'
#' @return An `acc` vector of the same length as the input with calibrated
#'   burst matrices.
#'
#' @seealso [acc_calibration()] and [as_acc_calibration()] to
#'   construct calibrations for use with `acc_calibrate()`.
#'
#' @export
#'
#' @examples
#' a <- acc_example()
#' 
#' acc_calibrate(a, acc_calibration("ornitela"))
#' 
#' acc_calibrate(a, acc_calibration("eobs", tag_id = 1000))
#' 
#' acc_calibrate(a, acc_calibration(offset = 2048, slope = 0.001))
#' 
#' # Specify different calibration parameters for each burst.
#' # Calibrations will be mapped to the input `acc` by index.
#' acc_calibrate(
#'   a, 
#'   acc_calibration(offset = c(2048, 2046), slope = c(0.001, 0.002))
#' )
acc_calibrate <- function(acc, calibration) {
  if (!inherits(calibration, "acc_calibration")) {
    rlang::abort(c(
      "`calibration` must be an `acc_calibration` object.",
      i = "Use `acc_calibration()` or `as_acc_calibration()` to create one."
    ))
  }
  
  calibration <- vctrs::vec_recycle(calibration, length(acc))
  
  field(acc, "bursts") <- new_acc_list(purrr::map2(
    bursts(acc),
    calibration,
    function(.br, .calibrate) {
      .calibrate(.br)
    }
  ))
  
  acc
}

#' Create calibration functions for raw acceleration values
#'
#' @description
#' Generate an `acc_calibration` object containing a list of functions 
#' with various calibration parameters to be used in [acc_calibrate()].
#'
#' Use `acc_calibration()` to specify calibration parameters manually.
#' Arguments are vectorized and matched by index.
#'
#' Use `as_acc_calibration()` to convert a data.frame containing row-wise
#' burst calibration parameters to an `acc_calibration` object.
#'
#' This allows you to specify burst-specific calibration functions to
#' flexibly convert raw acceleration values to physical units in `acc` vectors
#' that contain data from heterogeneous sources.
#'
#' @param manufacturer Manufacturer of the tag. Currently, `"eobs"` and
#'   `"ornitela"` are supported. For other manufacturers, leave `NULL` and
#'   manually specify the calibration parameters below.
#' @param tag_id If `manufacturer = "eobs"`, the e-obs tag ID for the tag.
#' @param sensitivity If `manufacturer = "eobs"`, the sensitivity of the tag.
#'   Defaults to `"low"` if none provided. Note that only e-obs generation 1 
#'   tags have a sensitivity setting.
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
#' @seealso [acc_calibrate()] to apply calibration functions to the entries
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
#' # Apply calibration with acc_calibrate()
#' acc_calibrate(acc_example(), cal)
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
  
  new_acc_calibration(purrr::pmap(args, acc_calibration_))
}

#' @param df data.frame containing columns corresponding to the available
#'   arguments in `acc_calibration()`
#' @rdname acc_calibration
as_acc_calibration <- function(df) {
  df <- validate_calibration_df(df)
  
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
  
  # calibration function for burst matrix `x` with resolved parameters
  function(x) {
    if (rlang::is_empty(x) || rlang::is_na(x)) {
      return(x)
    }
    
    if (inherits(x, "units")) {
      rlang::warn(
        "Cannot calibrate values that already contain units. Returning input."
      )
      return(x)
    }
    
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
    xt <- units::set_units(xt, units, mode = "standard")
    
    xt
  }
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
#'   and [acc_calibrate()] to calibrate eobs acceleration values.
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

validate_calibration_df <- function(df, call = rlang::caller_env()) {
  assertthat::assert_that(is.data.frame(df))
  
  expected_cols <- c(
    "manufacturer",
    "tag_id",
    "sensitivity",
    "offset",
    "offset_x",
    "offset_y",
    "offset_z",
    "slope",
    "slope_x",
    "slope_y",
    "slope_z",
    "orientation", 
    "orientation_x",
    "orientation_y",
    "orientation_z",
    "units",
    "axes"
  )
  
  if (any(!colnames(df) %in% expected_cols)) {
    rlang::warn(
      paste0(
        "Ignoring unrecognized colnames in `df`: \"",
        paste0(setdiff(colnames(df), expected_cols), collapse = "\", \""),
        "\""
      ),
      call = call
    )
  }
  
  # No duplicate tag_ids within the same manufacturer
  if ("tag_id" %in% colnames(df) && "manufacturer" %in% colnames(df)) {
    complete <- !is.na(df[["tag_id"]]) & !is.na(df[["manufacturer"]])
    keys <- paste(df[["manufacturer"]][complete], df[["tag_id"]][complete], sep = ":")
    dupes <- keys[duplicated(keys)]
    
    if (length(dupes) > 0) {
      rlang::abort(
        c(
          "Duplicate `tag_id` values within the same manufacturer in `df`",
          i = paste0("Duplicated: ", paste0(unique(dupes), collapse = ", "))
        ),
        call = call
      )
    }
  }
  
  # Validate manufacturer values
  valid_manufacturers <- c("eobs", "ornitela")
  mfr <- df[["manufacturer"]] %||% rep(NA_character_, nrow(df))
  has_manufacturer <- !is.na(mfr)
  
  invalid <- mfr[has_manufacturer & !mfr %in% valid_manufacturers]
  
  if (length(invalid) > 0) {
    rlang::abort(
      c(
        paste0(
          "Unrecognized manufacturer in `df`: ",
          paste0("\"", unique(invalid), "\"", collapse = ", ")
        ),
        i = "If provided, `manufacturer` must be \"eobs\" or \"ornitela\""
      ),
      call = call
    )
  }
  
  is_eobs <- has_manufacturer & mfr == "eobs"
  
  # Validate eobs rows have non-NA tag_id
  if (any(is_eobs & is.na(df[["tag_id"]]))) {
    rlang::abort(
      "All rows in `df` with `manufacturer = \"eobs\"` must have an associated `tag_id`",
      call = call
    )
  }
  
  # Validate custom rows have at least one offset and one slope value.
  # Not all axes need to be specified — bursts may not have all axes, and
  # missing per-axis values are handled at calibration time.
  col_na <- function(col) is.na(df[[col]] %||% NA)
  has_no_offset <- col_na("offset") & col_na("offset_x") & col_na("offset_y") & col_na("offset_z")
  has_no_slope <- col_na("slope") & col_na("slope_x") & col_na("slope_y") & col_na("slope_z")
  
  if (any(!has_manufacturer & (has_no_offset | has_no_slope))) {
    rlang::abort(
      "Tags without a manufacturer must have offset and slope values.",
      call = call
    )
  }
  
  df
}

new_acc_calibration <- function(x) {
  structure(x, class = c("acc_calibration", class(x)))
}

#' @export
print.acc_calibration <- function(x, ...) {
  cat(paste0("<acc_calibration[", length(x), "]>\n"))
  invisible(x)
}

GRAV_CONST <- 9.80665
