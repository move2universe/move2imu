# TODO: Will need some way to determine if these are raw or transformed vals though...
# TODO: do we want to allow set n-1g and n+1g instead of offset? This would allow auto calculation
#       of slope, so presumably the user already has done this to provide something to the slope param.
#       but not sure what is the most reasonable thing to expect users to have at the 
#       point at which they run this function...
eobs_transform <- function(acc, 
                           tag_id, 
                           sensitivity = "low", 
                           offset = NULL, 
                           slope = NULL, 
                           units = "m/s^2") {
  warn_unexpected_names(offset, "offset")
  warn_unexpected_names(slope, "slope")

  field(acc, "bursts") <- map_acc(
    acc,
    function(.br) {
      eobs_transform_(
        .br, 
        tag_id = tag_id, 
        sensitivity = sensitivity, 
        offset = offset, 
        slope = slope,
        units = units
      )
    }
  )
  
  acc
}

eobs_transform_ <- function(x,
                            tag_id, 
                            sensitivity = "low",
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
  
  tag_specs <- get_tag_specs(tag_id, sensitivity)
  
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
  resolved[axis_present]
}

get_tag_specs <- function(tag_id, sensitivity = "low") {
  config <- eobs_tag_id_config()
  
  tag_gen <- tag_id >= config$min_tag_id & tag_id <= config$max_tag_id
  tag_specs <- config[tag_gen & config$sensitivity == sensitivity, ]
  
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
