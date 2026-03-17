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
  
  tag_id <- field(acc, "tag_id")
  b <- field(acc, "bursts")
  
  purrr::map2(
    b,
    tag_id,
    function(.br, .ti) {
      if (!rlang::is_null(tag_config)) {
        tag_specs <- tag_config[tag_config$tag_id == .ti, ]
        
        tag_gen <- tag_specs$tag_gen %||% tag_gen
        sensitivity <- tag_specs$sensitivity %||% sensitivity
        offset <- extract_axis_arg(tag_specs, "offset") %||% offset
        slope <- extract_axis_arg(tag_specs, "slope")  %||% slope
      } else {
        if (rlang::is_null(tag_gen)) {
          rlang::abort("If no config provided, need a tag generation")
        }
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
  
  if (inherits(x, "units")) {
    rlang::warn("Cannot transform values that already contain units. Returning input.") # Maybe want better checks on whether the data are truly raw or not
    return(x)
  }
  
  if (!all(x[, axis_present] <= 4095 & x[, axis_present] >= 0)) {
    rlang::abort("Raw e-obs values should be between 0 and 4095.")
  }
  
  axis_present <- intersect(c("X", "Y", "Z"), colnames(x))
  
  sensitivity <- sensitivity %||% "low"
  tag_specs <- get_tag_specs(tag_gen, sensitivity)
  
  # Ensure that missing axis params get defaults and superfluous axis params are removed
  offset <- resolve_axis_arg(offset, c(X = 2048, Y = 2048, Z = 2048), axis_present)
  slope <- resolve_axis_arg(slope, c(X = tag_specs$slope, Y = tag_specs$slope, Z = tag_specs$slope), axis_present)

  offset <- as.numeric(offset)
  slope <- as.numeric(slope)
  
  y_adj <- c(X = 1, Y = tag_specs$y, Z = 1)
  y_adj <- y_adj[intersect(axis_present, names(y_adj))]
  
  scale <- y_adj * slope
  
  m <- sweep(x[, axis_present, drop = FALSE], 2, offset, `-`)
  m <- sweep(m, 2, scale, `*`)
  
  if (units == "m/s^2") {
    m <- m * GRAV_CONST
  }
  
  colnames(m) <- axis_present
  
  units::set_units(m, units, mode = "standard")
}

# Ensure input args have only X/Y/Z axes and remove axes missing from input matrix
resolve_axis_arg <- function(x, defaults, axis_present) {
  if (!is.null(x)) {
    bad <- setdiff(names(x), c("X", "Y", "Z"))
    if (length(bad) > 0) {
      x <- x[intersect(names(x), c("X", "Y", "Z"))]
    }
  }
  
  resolved <- defaults
  resolved[names(x)] <- x
  resolved[is.na(resolved)] <- defaults[is.na(resolved)]
  resolved[axis_present]
}

extract_axis_arg <- function(config_row, col_pattern) {
  axes <- c(X = "x", Y = "y", Z = "z")
  cols <- paste(col_pattern, axes, sep = "_")
  cols_present <- intersect(cols, colnames(config_row))
  
  if (length(cols_present) == 0L) {
    return(NULL)
  }
  
  vals <- unlist(config_row[, cols_present, drop = FALSE])
  names(vals) <- names(axes)[match(cols_present, cols)]
  vals
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

warn_unexpected_names <- function(x, arg, nms = c("X", "Y", "Z")) {
  d <- setdiff(names(x), nms)
  
  if (length(d) > 0) {
    rlang::warn(
      c(
        paste0("Ignoring unrecognized element(s) in arg `", arg, "`: \"", paste0(d, collapse = "\", \""), "\""),
        i = paste0(arg, " should have elements only for axes X, Y, and/or Z")
      )
    )
  }
}

GRAV_CONST <- 9.80665
