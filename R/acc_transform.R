# TODO: Will need some way to determine if these are raw or transformed vals though...
# TODO: do we want to allow set n-1g and n+1g instead of offset? This would allow auto calculation
#       of slope, so presumably the user already has done this to provide something to the slope param.
#       but not sure what is the most reasonable thing to expect users to have at the 
#       point at which they run this function...
eobs_transform <- function(acc, 
                           tag_config = NULL,
                           tag_gen = NULL,
                           sensitivity = NULL, 
                           offset = NULL, 
                           slope = NULL, 
                           units = "m/s^2") {
  warn_unexpected_names(offset, "offset")
  warn_unexpected_names(slope, "slope")
  
  map_acc(
    acc,
    function(.br, .ti) {
      if (!rlang::is_null(tag_config)) {
        # If a tag config df is provided, get transformation params for that tag
        # and replace with defaults if tag does not exist or is incomplete
        tag_specs <- tag_config[tag_config$tag_id == .ti, ]
        
        if (nrow(tag_specs) == 0) {
          rlang::warn(
            paste0(
              "Could not find tag_id ", .ti, 
              " in `tag_config`. Returning original values."
            )
          )
          return(.br)
        } else if (nrow(tag_specs) > 1) {
          rlang::abort(
            c(
              "`tag_config` must contain only one row per tag ID.",
              i = paste0("Multiple rows found for ID ", .ti)
            )
          )
        }
        
        tag_gen <- tag_specs$tag_gen %||% tag_gen
        sensitivity <- tag_specs$sensitivity %||% sensitivity
        
        # For params with axis-specific values, extract all that
        # exist safely into named vector
        offset <- extract_axis_arg(tag_specs, "offset") %||% offset
        slope <- extract_axis_arg(tag_specs, "slope")  %||% slope
      } else if (rlang::is_null(tag_gen)) {
        # For e-obs, can't identify transform params without a tag generation
        rlang::abort("`tag_gen` must be present if no `tag_config` provided.")
      }
      
      eobs_transform_(
        .br, 
        tag_gen = tag_gen, 
        sensitivity = sensitivity, 
        offset = offset, 
        slope = slope,
        units = units
      )
    }
  )
}

eobs_transform_ <- function(x,
                            tag_gen, 
                            sensitivity = NULL,
                            offset = NULL, 
                            slope = NULL, 
                            units = "m/s^2") {
  rlang::arg_match(units, c("m/s^2", "g"))
  
  # Maybe want better checks on whether the data are truly raw or not
  if (inherits(x, "units")) {
    rlang::warn(
      "Cannot transform values that already contain units. Returning input."
    )
    return(x)
  }
  
  # Get available axes and default tag config params
  axes <- intersect(AXES, colnames(x))
  
  if (!all(x[, axes] <= 4095 & x[, axes] >= 0)) {
    rlang::abort("Raw e-obs values should be between 0 and 4095.")
  }
  
  default_specs <- get_tag_specs(tag_gen, sensitivity %||% "low")
  
  # Set axis default vals and remove axes not found in input matrix
  offset <- recycle_to_axes(offset, default = default_specs$offset, axes = axes)
  slope <- recycle_to_axes(slope, default = default_specs$slope, axes = axes)
  
  offset <- as.numeric(offset)
  slope <- as.numeric(slope)
  
  # Handle flipped y-axis orientation on some tag generations
  y_adj <- c(X = 1, Y = default_specs$y, Z = 1)
  scale <- y_adj[axes] * slope
  
  # Apply transformation
  xt <- sweep(x[, axes, drop = FALSE], 2, offset, `-`)
  xt <- sweep(xt, 2, scale, `*`)
  
  if (units == "m/s^2") {
    xt <- xt * GRAV_CONST
  }
  
  colnames(xt) <- axes
  xt <- units::set_units(xt, units, mode = "standard")
  
  xt
}

extract_axis_arg <- function(config_row, col_pattern) {
  cols <- colnames(config_row)
  cols <- cols[grepl(cols, pattern = col_pattern)]
  cols_present <- intersect(cols, colnames(config_row))
  
  if (length(cols_present) == 0L) {
    return(NULL)
  }
  
  vals <- unlist(config_row[, cols_present, drop = FALSE])
  names(vals) <- AXES[match(cols_present, cols)]
  vals
}

# Recycle a scalar or named vector to a length-3 named vector (X, Y, Z).
# `default` fills any axis not supplied in a named vector. Axis names that do
# not match X/Y/Z are silently dropped
recycle_to_axes <- function(x, default = NA_real_, axes = AXES) {
  if (is.null(names(x))) {
    names(x) <- axes[seq_along(x)]
  }
  
  # Remove unrecognized axis labels
  x <- x[intersect(axes, names(x))]
  
  # Set defaults
  out <- rep(default, length(axes))
  names(out) <- axes
  
  # Replace non-default values
  out[names(x)] <- x
  out
}

get_tag_specs <- function(tag_gen, sensitivity = "low") {
  config <- eobs_tag_id_config()
  
  is_tag_gen <- tag_gen == config$generation
  tag_specs <- config[is_tag_gen & config$sensitivity == sensitivity, ]
  
  if (nrow(tag_specs) == 0) {
    rlang::abort("No tag detected")
  } else if (nrow(tag_specs) > 1) {
    rlang::abort("Multiple tags detected")
  }
  
  tag_specs
}

eobs_tag_id_config <- function() {
  data.frame(
    generation = c(1, 1, 2, 3),
    min_tag_id = c(0, 0, 2242, 4118),
    max_tag_id = c(2241, 2241, 4117, Inf),
    sensitivity = c("low", "high", "low", "low"),
    y = c(1, 1, -1, -1), # TODO: CHECK THIS
    offset = c(2048, 2048, 2048, 2048),
    slope = c(0.0027, 0.001, 0.0022, 1/512)
  )
}

warn_unexpected_names <- function(x, arg, nms = AXES) {
  d <- setdiff(names(x), nms)
  
  if (length(d) > 0) {
    rlang::warn(
      c(
        paste0("Ignoring unrecognized element(s) in arg `", arg, "`: \"", paste0(d, collapse = "\", \""), "\""),
        i = paste0("Supported names: \"", paste0(nms, collapse = "\", \""), "\"")
      )
    )
  }
}

GRAV_CONST <- 9.80665
AXES <- c("X", "Y", "Z")
