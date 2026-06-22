#' Create calibrations for raw acceleration values
#'
#' @description
#' Generate an `acc_calibration` object holding the per-burst calibration
#' parameters to be applied by [transform_imu()].
#'
#'   - Use `acc_calibration()` to specify calibration parameters manually.
#'     Arguments are vectorized and matched by index.
#'   - Use `as_acc_calibration()` to convert a data.frame containing row-wise
#'     burst calibration parameters to an `acc_calibration` object.
#'
#' This allows you to provide burst-specific calibration parameters to
#' flexibly convert raw acceleration values to physical units in `acc` vectors
#' that contain data from heterogeneous sources.
#'
#' @details
#' An `acc_calibration` can either be built from a `manufacturer` and `tag_id`
#' combination or from manual inputs of the `offset` and `slope` parameters.
#' If neither of these options is provided in full, then a calibration cannot
#' be built and `NA` is returned for that element. Passing missing calibrations
#' to [transform_imu()] returns `NA` for that burst.
#'
#' Currently if `manufacturer` is provided, it must be either `"ornitela"` or
#' `"eobs"`. If `"eobs"`, then a corresponding `tag_id` must also be provided.
#'
#' This is because e-obs tags have default calibration parameters that vary
#' depending on the tag's generation. Use [eobs_default_specs()] for a
#' summary table showing the
#' default offset, slope, and orientation parameters used for each e-obs
#' tag ID. The tag ID defines the tag generation. Note that tags from generation
#' 1 could be set either to low or high sensitivity, each with their own
#' default calibration parameters.
#'
#' If no manufacturer is provided, then both `offset_*` and `slope_*` must be
#' provided for at least one axis.
#'
#' If calibration parameters are provided for some axes and not others
#' (e.g. `offset_x = 2048` and `slope_x = 0.001`), then only those axes will
#' be transformed by [transform_imu()]. Values for other axes will be converted
#' to `NA`.
#'
#' If both `manufacturer` and a custom `offset` or `slope`, and/or `orientation`
#' are provided, then
#' the value of the custom parameters will override the manufacturer defaults
#' for that calibration entry.
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
#' @param units Output units. Either `"m/s^2"` (default) or
#'   `"standard_free_fall"`.
#'
#' @returns An `acc_calibration` vector.
#' @export
#'
#' @seealso [transform_imu()] to apply a calibration to the entries
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
#' # (Useful for instance if specifications are stored as metadata alongside
#' # acc data in a move2)
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
                            units = "m/s^2") {
  specs <- list(
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
    orientation_z = orientation_z
  )

  if (vctrs::vec_size_common(!!!specs) == 0L) {
    return(new_acc_calibration())
  }

  # As long as there is at least one set of cal params, we can recycle
  # units to match length
  args <- c(specs, list(units = units))
  args <- recycle_args(args, vctrs::vec_size_common(!!!args))

  build_calibrations(args)
}

#' @param df data.frame containing columns with names corresponding to the
#'   available arguments in `acc_calibration()`. Each row produces a single
#'   calibration.
#'
#' @export
#'
#' @rdname acc_calibration
as_acc_calibration <- function(df) {
  if (!is.data.frame(df)) {
    cli::cli_abort("{.arg df} must be a data frame.")
  }

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
    units = resolve_scalar_col(df, "units", "m/s^2")
  )

  # Recycle each argument to the nrow of the dataframe
  args <- recycle_args(args, nrow(df))
  build_calibrations(args)
}

# Build a vector of `acc_calibration`s from a named list of equal-length,
# per-field argument vectors. Each entry is resolved independently. An entry
# whose calibration can't be resolved becomes a missing (`NA`) calibration.
# This standardizes build behavior across acc_calibration()/as_acc_calibration()
build_calibrations <- function(args) {
  if (vctrs::vec_size(args[[1]]) == 0) {
    return(new_acc_calibration())
  }

  # Build each spec independently; one that can't be resolved becomes a missing
  # (`NA`) calibration. Capture the failure reason so they can be summarized
  # rather than silently dropped.
  results <- purrr::pmap(
    args,
    function(...) {
      tryCatch(
        list(cal = acc_calibration_(...), reason = NA_character_),
        error = function(cnd) {
          list(
            cal = vctrs::vec_init(new_acc_calibration(), 1L),
            # Keep only the leading line so multi-line condition messages (e.g.
            # an arg_match "Did you mean?" hint) read cleanly as one bullet.
            reason = sub("\n.*$", "", conditionMessage(cnd))
          )
        }
      )
    }
  )

  cals <- vctrs::vec_c(!!!purrr::map(results, "cal"))

  reasons <- purrr::map_chr(results, "reason")
  reasons <- reasons[!is.na(reasons)]

  if (length(reasons) > 0) {
    warn_unresolved_calibrations(reasons)
  }

  cals
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
                             units = "m/s^2") {
  rlang::arg_match(units, c("m/s^2", "standard_free_fall"))

  # Resolve manufacturer defaults, then let user-provided values override
  if (!rlang::is_null(manufacturer) && !rlang::is_na(manufacturer)) {
    if (manufacturer == "eobs") {
      if (rlang::is_null(tag_id) || rlang::is_na(tag_id)) {
        # Note: use stop() rather than rlang to improve processing time since these
        # errors are caught by build_calibrations() in the user-facing API anyway
        stop("`tag_id` must be provided when `manufacturer = \"eobs\"`")
      }

      specs <- eobs_specs(tag_id, first_valid(sensitivity, "low"))
    } else if (manufacturer == "ornitela") {
      specs <- ornitela_specs()
    } else {
      stop(paste0(
        "Unrecognized manufacturer: \"", manufacturer,
        "\". Must be \"eobs\" or \"ornitela\"."
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
    # An axis can only be calibrated with both an offset and a slope.
    # At least one axis must have both.
    complete_axis <- c(
      X = !null_or_na(offset_x) && !null_or_na(slope_x),
      Y = !null_or_na(offset_y) && !null_or_na(slope_y),
      Z = !null_or_na(offset_z) && !null_or_na(slope_z)
    )
    if (!any(complete_axis)) {
      stop("a custom calibration needs both an `offset` and a `slope` on at least one axis")
    }
  }

  # Fill remaining missing orientation with default
  orientation_x <- first_valid(orientation_x, 1L)
  orientation_y <- first_valid(orientation_y, 1L)
  orientation_z <- first_valid(orientation_z, 1L)

  if (!all(c(orientation_x, orientation_y, orientation_z) %in% c(-1L, 1L))) {
    stop("`orientation` must be 1 or -1.")
  }

  new_acc_calibration(
    offset_x = offset_x %||% NA,
    offset_y = offset_y %||% NA,
    offset_z = offset_z %||% NA,
    slope_x = slope_x %||% NA,
    slope_y = slope_y %||% NA,
    slope_z = slope_z %||% NA,
    orientation_x = orientation_x,
    orientation_y = orientation_y,
    orientation_z = orientation_z,
    units = units
  )
}

# `acc_calibration` constructor. Stores calibration specifications as
# fields in a vctrs record.
new_acc_calibration <- function(offset_x = double(),
                                offset_y = double(),
                                offset_z = double(),
                                slope_x = double(),
                                slope_y = double(),
                                slope_z = double(),
                                orientation_x = double(),
                                orientation_y = double(),
                                orientation_z = double(),
                                units = character()) {
  vctrs::new_rcrd(
    list(
      offset_x = as.double(offset_x),
      offset_y = as.double(offset_y),
      offset_z = as.double(offset_z),
      slope_x = as.double(slope_x),
      slope_y = as.double(slope_y),
      slope_z = as.double(slope_z),
      orientation_x = as.double(orientation_x),
      orientation_y = as.double(orientation_y),
      orientation_z = as.double(orientation_z),
      units = as.character(units)
    ),
    class = c("acc_calibration", "imu_calibration")
  )
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
#'
#' @examples
#' eobs_default_specs()
eobs_default_specs <- function() {
  data.frame(
    tag_gen = c(1, 1, 2, 3),
    min_tag_id = c(1, 1, 2242, 4118),
    max_tag_id = c(2241, 2241, 4117, Inf),
    sensitivity = c("low", "high", "low", "low"),
    orientation_x = c(1, 1, 1, 1),
    orientation_y = c(-1, -1, 1, 1),
    orientation_z = c(1, 1, 1, 1),
    offset = c(2048, 2048, 2048, 2048),
    slope = c(0.0027, 0.001, 0.0022, 1 / 512)
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
  # guard against factor tag ID, which as.numeric coerces to its level
  tag_id <- as.numeric(as.character(tag_id))
  sensitivity <- rep_len(sensitivity, length(tag_id))
  rlang::arg_match(sensitivity, c("low", "high"), multiple = TRUE)

  if (any(is.na(tag_id))) {
    rlang::abort("Cannot look up eobs tag specs for missing `tag_id`")
  }

  config <- eobs_default_specs()

  # Match each tag to its config row by ID range and sensitivity.
  row <- vapply(
    seq_along(tag_id),
    function(k) {
      hit <- which(
        tag_id[k] >= config$min_tag_id &
          tag_id[k] <= config$max_tag_id &
          config$sensitivity == sensitivity[k]
      )
      if (length(hit) == 1L) hit else NA_integer_
    },
    integer(1)
  )

  if (anyNA(row)) {
    bad <- which(is.na(row))

    # Distinguish an out-of-range ID from a valid ID that has no configuration
    # for the requested sensitivity, so the message points at the real problem.
    in_range <- vapply(
      bad,
      function(k) any(tag_id[k] >= config$min_tag_id & tag_id[k] <= config$max_tag_id),
      logical(1)
    )

    msgs <- ifelse(
      in_range,
      sprintf(
        "No \"%s\" sensitivity configuration for e-obs tag ID %s. See `eobs_default_specs()` for valid configurations.",
        sensitivity[bad], tag_id[bad]
      ),
      sprintf(
        "Could not find an e-obs tag matching ID \"%s\". See `eobs_default_specs()` for valid e-obs tag IDs.",
        tag_id[bad]
      )
    )

    rlang::abort(unique(msgs))
  }

  data.frame(
    tag_id = tag_id,
    offset = config$offset[row],
    slope = config$slope[row],
    orientation_x = config$orientation_x[row],
    orientation_y = config$orientation_y[row],
    orientation_z = config$orientation_z[row]
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

#' @export
format.acc_calibration <- function(x, ...) {
  if (vctrs::vec_size(x) == 0) {
    return(character(0))
  }

  d <- vctrs::vec_data(x)

  body <- sprintf(
    "offset=%s slope=%s",
    axis_fmt(d$offset_x, d$offset_y, d$offset_z),
    axis_fmt(d$slope_x, d$slope_y, d$slope_z)
  )

  # Only show orientation if at least one is flipped. Otherwise it's of
  # no interest.
  flipped <- (d$orientation_x %in% -1) |
    (d$orientation_y %in% -1) |
    (d$orientation_z %in% -1)

  body <- ifelse(
    flipped,
    paste0(body, " orientation=", axis_fmt(d$orientation_x, d$orientation_y, d$orientation_z)),
    body
  )

  out <- paste0("{", body, "}")
  out[vctrs::vec_detect_missing(x)] <- NA_character_
  out
}

# If axes have same values, print only 1. Otherwise print all 3.
axis_fmt <- function(a, b, c) {
  same <- function(p, q) (is.na(p) & is.na(q)) | (!is.na(p) & !is.na(q) & p == q)
  fmt <- function(v) format(v, digits = 3, trim = TRUE)

  ifelse(
    same(a, b) & same(b, c),
    paste0("[", fmt(a), "]"),
    paste0("[", fmt(a), ", ", fmt(b), ", ", fmt(c), "]")
  )
}

#' @export
vec_ptype_abbr.acc_calibration <- function(x, ...) {
  "acc_cal"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.acc_calibration <- function(x, ...) {
  out <- rep("<acc_cal>", length(x))
  out[vctrs::vec_detect_missing(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left")
}

# Maximum number of distinct failure reasons to list before collapsing the rest.
max_calibration_reasons <- 5L

# Warn about calibrations that resolved to NA, listing why. Identical reasons
# collapse to a single bullet, and the list is capped, so a large
# or heterogeneous batch of failures stays compact rather than flooding output.
warn_unresolved_calibrations <- function(reasons) {
  n_na <- length(reasons)

  # Glue-escape so a stray `{`/`}` in a reason can't break cli interpolation.
  bullets <- gsub("}", "}}", gsub("{", "{{", unique(reasons), fixed = TRUE), fixed = TRUE)

  overflow <- length(bullets) - max_calibration_reasons
  if (overflow > 0L) {
    bullets <- c(
      bullets[seq_len(max_calibration_reasons)],
      sprintf("... and %d more reason%s", overflow, if (overflow == 1L) "" else "s")
    )
  }

  cli::cli_warn(
    c(
      "Returning NA for {n_na} calibration{?s} that could not be resolved:",
      rlang::set_names(bullets, rep("*", length(bullets))),
      "i" = "See {.help [{.fun acc_calibration}](move2imu::acc_calibration)} for details about accepted calibration inputs."
    ),
    class = "move2imu_unresolved_calibration"
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

# Read a scalar spec column from `df`, converting both NULL and NA to
# defaults
resolve_scalar_col <- function(df, col, default) {
  v <- df[[col]] %||% default
  v[is.na(v)] <- default
  v
}

# Recycle every entry in a list to length n
recycle_args <- function(args, n) {
  lapply(args, function(x) vctrs::vec_recycle(x %||% NA, n))
}

GRAV_CONST <- 9.80665
