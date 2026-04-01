#' Specify acceleration data columns
#'
#' @description
#' Define which columns in a `move2` object contain acceleration data.
#' Use this function to manually specify the columns that should be used when
#' constructing an `acc` vector with `as_acc()`.
#'
#' For long-format data (one measurement per row), specify axis columns
#' with `acc_x`, `acc_y`, and/or `acc_z`.
#' 
#' For burst-format data (where each row contains burst data in a single string), 
#' specify the columns containing the raw burst data, axes, and sampling 
#' frequency.
#'
#' @param acc_x,acc_y,acc_z In long-format data, the column name(s) for the 
#'   X, Y, and/or Z acceleration axes.
#' @param bursts For burst-format data, the column name containing the raw burst 
#'   strings.
#' @param axes For burst-format data, the column name containing the axis 
#'   labels.
#' @param frequency For burst-format data, the column name containing the 
#'   sampling frequency.
#'
#' @returns An `acc_colset` object.
#'
#' @seealso [active_acc_colsets()] for automatic colset detection, [as_acc()] to
#'   construct an `acc` vector using a colset specification.
#'
#' @export
#'
#' @examples
#' # Specify the column names for long-format data for each axis
#' acc_colset(acc_x = "my_acc_x", acc_y = "my_acc_y", acc_z = "my_acc_z")
#'
#' # Long format data may consist of a subset of axes
#' acc_colset(acc_x = "my_acc_x", acc_y = "my_acc_y")
#'
#' # Specify the column names for the bursts, axes, and frequency for
#' # burst-format data
#' acc_colset(
#'   bursts = "my_raw_acc",
#'   axes = "my_axes",
#'   frequency = "my_freq"
#' )
acc_colset <- function(acc_x = NULL, 
                       acc_y = NULL, 
                       acc_z = NULL,
                       bursts = NULL, 
                       axes = NULL, 
                       frequency = NULL) {
  long_args <- list(X = acc_x, Y = acc_y, Z = acc_z)
  long_args <- purrr::compact(long_args)
  
  burst_args <- list(bursts, axes, frequency)
  burst_args <- purrr::compact(burst_args)
  
  has_long <- length(long_args) > 0
  has_burst <- length(burst_args) > 0
  
  if (has_long && has_burst) {
    rlang::abort(paste0(
      "Specify either axis columns (`acc_x`, `acc_y`, and/or `acc_z`) ",
      "or burst columns (`bursts`, `axes`, `frequency`), not both."
    ))
  }
  
  if (!has_long && !has_burst) {
    rlang::abort("No acc columns specified.")
  }
  
  if (has_burst) {
    if (length(burst_args) != 3) {
      rlang::abort(
        "Burst format requires `bursts`, `axes`, and `frequency` columns."
      )
    }
    
    new_acc_colset(
      cols = c(bursts = bursts, axes = axes, frequency = frequency),
      type = "burst"
    )
  } else {
    new_acc_colset(
      cols = unlist(long_args),
      type = "long"
    )
  }
}

new_acc_colset <- function(cols, type, axis_names = NULL, col_map = NULL) {
  structure(
    cols,
    type = type,
    class = c("acc_colset", class(cols))
  )
}

is_acc_colset <- function(x) {
  inherits(x, "acc_colset")
}

#' @export
print.acc_colset <- function(x, ...) {
  type <- attr(x, "type")
  if (type == "long") {
    cat(paste0(
      "<acc_colset> long-format [",
      paste0(names(x), "=", unclass(x), collapse = ", "),
      "]\n"
    ))
  } else {
    cat(paste0(
      "<acc_colset> burst-format [",
      paste0(names(x), "=", unclass(x), collapse = ", "),
      "]\n"
    ))
  }
  invisible(x)
}

#' Identify acceleration columns present in a `move2` object
#' 
#' @description
#' `active_acc_colsets()` determines the sets of columns that will be used 
#' by default to construct acceleration bursts with `as_acc()`. Column sets
#' are processed independently, but a given `move2` may contain multiple
#' column sets with acceleration data.
#' 
#' @details
#' `move2` objects store acceleration data in two ways: long-format
#' acceleration columns and burst-format acceleration columns.
#' 
#' Long-format columns store one acceleration measurement (possibly for multiple
#' axes) in a single row. Note that
#' not all axes need to be present for a long-format column set to be considered
#' active in a `move2` object.
#' 
#' Burst-format columns store a burst of acceleration data as a space-delimited
#' string. This string must be segmented into axis-specific measurements using
#' an associated column that indicates the axes present for the bursted data.
#' A further column provides the sampling frequency of the burst.
#' All three of these columns must be present for a burst-format column set
#' to be considered active in a `move2` object.
#' 
#' Standard long-format column sets for data from
#' Movebank include [acc_raw_xyz_cols()] and [acc_xyz_cols()]. Standard
#' burst-format column sets for data from Movebank include [acc_eobs_cols()]
#' and [acc_burst_cols()].
#' 
#' If your input data use different column names for these columns, use this
#' function to specify the column names that correspond to each of the
#' axes (for long-format data) or burst data and associated metadata (for
#' burst-format data).
#'
#' @inheritParams as_acc
#'
#' @returns A list of `acc_colset` objects.
#' 
#' @export
#' @rdname acc-cols
#' 
#' @seealso [valid_acc_colsets()] for currently supported default colsets,
#'   [as_acc()] to build an `acc` vector from a `move2` object.
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
active_acc_colsets <- function(x) {
  i <- which(
    purrr::map_lgl(acc_colset_config(), function(colset) colset$is_in_(x))
  )
  
  if (length(i) == 0) {
    abort_missing_acc_colset()
  }
  
  poss_colsets <- valid_acc_colsets()[i]
  
  colsets <- purrr::compact(
    purrr::map(
      poss_colsets,
      function(colset) {
        cols_in_x <- intersect(colset, colnames(x))
        cols_present <- cols_in_x[!cols_empty(x, cols_in_x)]
        
        if (!identical(cols_present, as.character(colset))) {
          if (attr(colset, "type") == "burst") {
            # Remove entire colset for types that require all cols present
            return(NULL)
          }
          
          # Rebuild long-format colset with only present columns
          return(
            new_acc_colset(
              cols = cols_present,
              type = attr(colset, "type")
            )
          )
        }
        
        colset
      }
    )
  )
  
  if (length(colsets) == 0) {
    abort_missing_acc_colset()
  }
  
  colsets
}


#' Identify rows with acceleration data from multiple column sets
#'
#' This function returns the row indices of a `move2` object 
#' where more than one acceleration column set contains data. `as_acc()` 
#' refuses to build `acc` objects for rows where multiple input sources exist. 
#' These rows can be modified to remove data from additional column sets. 
#' Alternatively, specific columns can be passed to the `acc_cols` argument 
#' of `as_acc()` to avoid processing duplicated records.
#'
#' @inheritParams as_acc
#' @param acc_cols Vector or list of column sets to check for overlap. Defaults
#'   to the column sets detected by [active_acc_colsets()].
#'
#' @returns An integer vector of row indices with duplicated acceleration data
#'   across column sets.
#'
#' @seealso 
#'   - [active_acc_colsets()] to identify available column sets in a `move2` object.
#'   - [as_acc()] to generate an `acc` vector from a `move2` object.
#'
#' @export
duplicated_acc_rows <- function(x, acc_cols = NULL) {
  colsets <- acc_cols %||% active_acc_colsets(x)
  
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }
  
  acc_rows <- unlist(
    purrr::map(
      colsets, 
      function(cols) which_acc_vals(x, acc_cols = cols)
    )
  )
  
  # Would be nice to return duplicated groups too so user knows what the issue is...
  sort(unique(acc_rows[duplicated(acc_rows) | duplicated(acc_rows, fromLast = TRUE)]))
}

#' Valid acceleration data column sets
#'
#' @description
#' These sets of columns can be used by [as_acc()] when parsing acceleration
#' bursts contained in a `move2` object. A `move2` object must contain one
#' of these column sets to be processed by `as_acc()`.
#' 
#' - `acc_eobs_cols()` and `acc_burst_cols()` must be present in their entirety
#' within a data source to be used when parsing acceleration data.
#' - For `acc_xyz_cols()` and `acc_raw_xyz_cols()`, any
#' subset of the set's columns can be used to parse acceleration
#' data.
#' 
#' To determine the default columns that will be used by `as_acc()` for a given
#' `move2` object, see [active_acc_colsets()].
#'
#' @returns For `valid_acc_colsets()`, a list of vectors of valid column sets.
#'   Otherwise, a character vector containing the names of the 
#' 
#' @export
#' 
#' @examples
#' valid_acc_colsets()
valid_acc_colsets <- function() {
  purrr::map(acc_colset_config(), function(colset) colset$cols)
}

#' @export
#' @rdname valid_acc_colsets
acc_eobs_cols <- function() {
  new_acc_colset(
    cols = c(
      axes = "eobs_acceleration_axes", 
      frequency = "eobs_acceleration_sampling_frequency_per_axis", 
      bursts = "eobs_accelerations_raw"
    ),
    type = "burst"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_burst_cols <- function() {
  new_acc_colset(
    cols = c(
      axes = "acceleration_axes", 
      frequency = "acceleration_sampling_frequency_per_axis", 
      bursts = "accelerations_raw"
    ),
    type = "burst"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_xyz_cols <- function() {
  new_acc_colset(
    cols = c(
      X = "acceleration_x", 
      Y = "acceleration_y", 
      Z = "acceleration_z"
    ),
    type = "long"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_raw_xyz_cols <- function() {
  new_acc_colset(
    cols = c(
      X = "acceleration_raw_x", 
      Y = "acceleration_raw_y", 
      Z = "acceleration_raw_z"
    ),
    type = "long"
  )
}

# Config to define each colset group and logical checks for that colset
#
# is_ functions designed to check whether an input colset `x` is a valid
# representation of the colset in the config
#
# is_in_ functions designed to check whether an input move2 `x` contains a given
# colset while accounting for fact that subsets are allowed for certain colsets
acc_colset_config <- function() {
  list(
    eobs = list(
      cols = acc_eobs_cols(), 
      is_ = function(x) is_acc_eobs_cols(x),
      is_in_ = function(x) all(acc_eobs_cols() %in% colnames(x))
    ),
    burst = list(
      cols = acc_burst_cols(), 
      is_ = function(x) is_acc_burst_cols(x),
      is_in_ = function(x) all(acc_burst_cols() %in% colnames(x))
    ),
    xyz = list(
      cols = acc_xyz_cols(), 
      is_ = function(x) is_acc_xyz_cols(x),
      is_in_ = function(x) any(acc_xyz_cols() %in% colnames(x))
    ),
    raw_xyz = list(
      cols = acc_raw_xyz_cols(), 
      is_ = function(x) is_acc_raw_xyz_cols(x),
      is_in_ = function(x) any(acc_raw_xyz_cols() %in% colnames(x))
    )
  )
}

is_valid_acc_colset <- function(cols) {
  any(purrr::map_lgl(acc_colset_config(), function(colset) colset$is_(cols)))
}

# is_* functions designed to check whether a vector represents a given colset
# while accounting for fact that subsets are allowed for certain colsets
is_acc_eobs_cols <- function(x) {
  setequal(x, acc_eobs_cols()) && length(x) == length(acc_eobs_cols())
}

is_acc_burst_cols <- function(x) {
  setequal(x, acc_burst_cols()) && length(x) == length(acc_burst_cols())
}

is_acc_raw_xyz_cols <- function(x) {
  is_unique_named_subset(x, acc_raw_xyz_cols())
}

is_acc_xyz_cols <- function(x) {
  is_unique_named_subset(x, acc_xyz_cols())
}

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

abort_missing_acc_colset <- function(call = rlang::caller_env()) {
  rlang::abort(
    c(
      "Could not identify a full acceleration column set in the input data.",
      "i" = "Use `valid_acc_colsets()` to see supported acceleration column sets."
    ),
    call = call
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
