#' Identify acceleration columns present in a `move2` object
#' 
#' @description
#' These functions identify the available columns of acceleration data that can
#' be used when constructing an `acc` vector with `as_acc()`.
#' 
#' `active_acc_cols()` determines the columns that will be used by default to 
#' construct acceleration bursts with `as_acc()`. For information about how
#' default columns are selected, see the details below.
#' 
#' `acc_colsets()` shows the full list of available acceleration column sets
#' that are present in the input data. Any of these sets can be passed to
#' `as_acc()` to construct bursts from non-default columns.
#' 
#' @details
#' By default, if only one full set of acceleration columns is detected, it is
#' considered the default. Note that some column sets 
#' ([acc_eobs_cols()], [acc_burst_cols()]) must be present in their entirety,
#' while all others can include subsets of the full set.
#' 
#' If multiple column sets are detected, the set that contains data values is
#' used as the default.
#' 
#' If multiple column sets are present and contain data values, sets will be
#' selected in the order returned by [valid_acc_colsets()].
#' 
#' It is not possible to reset the default column sets for a given `move2`
#' without modifying its columns. However, you can manually provide a column
#' set to the `acc_cols` argument of [as_acc()] to construct bursts from
#' non-default columns.
#'
#' @inheritParams as_acc
#'
#' @returns For `active_acc_cols()`, a character vector of acceleration column
#'   names. For `acc_colsets()`, a list of such character vectors.
#' @export
#' @rdname acc-cols
#'
#' @examples
#' active_acc_cols(albatrosses())
#' 
#' # Multiple colsets may be available
#' acc_colsets(gulls())
#' 
#' # But only one set is considered active
#' active_acc_cols(gulls())
#' 
#' # Missing columns are not included in the set
#' g <- gulls()
#' g$acceleration_raw_x <- NULL
#' active_acc_cols(g)
#' 
#' # Columns with no data are also removed
#' g$acceleration_raw_y <- NA
#' active_acc_cols(g)
#' 
#' # Some column sets must be present in their entirety
#' alb <- albatrosses()
#' alb$eobs_acceleration_axes <- NULL
#' 
#' \dontrun{
#'   active_acc_cols(alb)
#' }
active_acc_cols <- function(x) {
  colsets <- acc_colsets(x)
  
  if (length(colsets) > 1) {
    rlang::warn("Detected multiple valid acceleration column sets.")
  }
  
  colsets
}

#' @export
#' @rdname acc-cols
acc_colsets <- function(x) {
  i <- which(
    purrr::map_lgl(acc_colset_config(), function(colset) colset$is_in_(x))
  )
  
  if (length(i) == 0) {
    abort_missing_acc_cols()
  }
  
  poss_colsets <- valid_acc_colsets()[i]
  
  colsets <- purrr::compact(
    purrr::map(
      poss_colsets, 
      function(colset) {
        # Remove columns that don't exist in `x` or don't contain data
        colset <- intersect(colset, colnames(x))
        colset_present <- colset[!cols_empty(x, colset)]
        
        if (!identical(colset_present, colset)) {
          if (acc_cols_to_type(colset) %in% c("eobs", "burst")) {
            # Remove entire colset for types that require all cols present
            return(NULL)
          }
        }
        
        colset_present
      }
    )
  )
  
  if (length(colsets) == 0) {
    abort_missing_acc_cols()
  }
  
  colsets
}

duplicated_acc_rows <- function(x, colsets = NULL) {
  colsets <- colsets %||% acc_colsets(x)
  
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
#' `move2` object, see [active_acc_cols()].
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
  c(
    "eobs_acceleration_axes", 
    "eobs_acceleration_sampling_frequency_per_axis", 
    "eobs_accelerations_raw"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_burst_cols <- function() {
  c(
    "acceleration_axes",
    "acceleration_sampling_frequency_per_axis",
    "accelerations_raw"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_xyz_cols <- function() {
  c(
    "acceleration_x", 
    "acceleration_y", 
    "acceleration_z"
  )
}

#' @export
#' @rdname valid_acc_colsets
acc_raw_xyz_cols <- function() {
  c(
    "acceleration_raw_x", 
    "acceleration_raw_y", 
    "acceleration_raw_z"
  )
}

# This defines all colsets but also the order in which colsets are selected
# as default colsets by active_acc_cols(). Can also be expanded to accommodate
# more metadata if needed
#
# is_ functions designed to check whether an input colset `x` is a valid
# representation of the colset in the config
#
# is_in_ functions designed to check whether an input move2 `x` contains a given
# colset while accounting for fact that subsets are allowed for certain colsets
acc_colset_config <- function() {
  list(
    eobs = list(
      type = "eobs", 
      cols = acc_eobs_cols(), 
      is_ = function(x) is_acc_eobs_cols(x),
      is_in_ = function(x) all(acc_eobs_cols() %in% colnames(x))
    ),
    burst = list(
      type = "burst", 
      cols = acc_burst_cols(), 
      is_ = function(x) is_acc_burst_cols(x),
      is_in_ = function(x) all(acc_burst_cols() %in% colnames(x))
    ),
    xyz = list(
      type = "xyz", 
      cols = acc_xyz_cols(), 
      is_ = function(x) is_acc_xyz_cols(x),
      is_in_ = function(x) any(acc_xyz_cols() %in% colnames(x))
    ),
    raw_xyz = list(
      type = "raw_xyz", 
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
  all(x %in% acc_raw_xyz_cols()) && !anyDuplicated(x)
}

is_acc_xyz_cols <- function(x) {
  all(x %in% acc_xyz_cols()) && !anyDuplicated(x)
}

cols_empty <- function(x, cols) {
  assert_all_cols_present(x, cols)
  purrr::map_lgl(
    cols,
    function(col) all(is.na(unlist(x[[col]]))) || all(rlang::is_empty(unlist(x[[col]])))
  )
}

acc_types <- function() {
  purrr::map_chr(acc_colset_config(), function(colset) colset$type)
}

acc_cols_to_type <- function(acc_cols) {
  assert_valid_acc_colset(acc_cols)
  i <- purrr::map_lgl(valid_acc_colsets(), function(x) all(acc_cols %in% x))
  unname(acc_types()[i])
}

abort_missing_acc_cols <- function(call = rlang::caller_env()) {
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

assert_valid_acc_colset <- function(acc_cols, call = rlang::caller_env()) {
  if (!is_valid_acc_colset(acc_cols)) {
    rlang::abort(
      c(
        "Invalid acc columns provided.",
        x = paste0("Unexpected columns: \"", paste0(acc_cols, collapse = "\", \""), "\"")
      ),
      call = call
    )
  }
  TRUE
}
