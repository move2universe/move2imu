#' Convert to acc
#'
#' In many cases the `as_acc` function will directly create an acceleration 
#' vector from input data
#'
#' @param x A `move2` containing acceleration data as collected by EOBS,
#'   Ornitela, or similar tracking devices. Most of the time this will be 
#'   either loaded from disk using [move2::mt_read] or downloaded using 
#'   [move2::movebank_download_study].
#' @param tolerance Numeric value indicating the maximum allowable gap (in
#'   seconds) between consecutive timestamps that should be included in the 
#'   same burst. Ignored for acceleration data that are already grouped 
#'   into bursts (e.g. e-obs data).
#' @param ... currently not used
#'
#' @details The resulting vector will be as long as the input. This means it 
#' can, for example, be added as a column to a `data.frame`. For some tags 
#' this means `NA` values are inserted when one burst is stored over multiple 
#' rows of a `data.frame`.
#'
#' @export
as_acc <- function(x, ...) {
  UseMethod("as_acc")
}

#' @export
as_acc.default <- function(x, ...) {
  vctrs::vec_cast(x, new_acc())
}

#' @export
as_acc.move2 <- function(x, tolerance = 0.5, ...) {
  acc_cols <- active_acc_cols(x)
  
  acc <- switch(
    acc_cols_to_type(acc_cols),
    "eobs" = as_acc_move2_eobs(x, ...),
    "burst" = as_acc_move2_burst(x, ...),
    "xyz" = as_acc_move2_xyz(x, tolerance = tolerance, ...),
    "raw_xyz" = as_acc_move2_raw_xyz(x, tolerance = tolerance, ...),
    "tilt" = as_acc_move2_tilt(x, tolerance = tolerance, ...),
    abort_unsupported_cols()
  )
  
  acc
}

as_acc_move2_eobs <- function(x, ...) {
  assertthat::assert_that(has_acc_eobs_cols(x))
  
  as_acc_burst(
    x[["eobs_accelerations_raw"]],
    x[["eobs_acceleration_axes"]],
    x[["eobs_acceleration_sampling_frequency_per_axis"]]
  )
}

as_acc_move2_burst <- function(x, ...) {
  assertthat::assert_that(has_acc_burst_cols(x))
  
  as_acc_burst(
    x[["accelerations_raw"]],
    x[["acceleration_axes"]],
    x[["acceleration_sampling_frequency_per_axis"]]
  )
}

as_acc_move2_raw_xyz <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_raw_xyz_cols(x))
  as_acc_long(x, tolerance = tolerance)
}

as_acc_move2_xyz <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_xyz_cols(x))
  as_acc_long(x, tolerance = tolerance)
}

# TODO: decide whether tilt is supported? It seems to co-occur with raw xyz cols
as_acc_move2_tilt <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_tilt_cols(x))
  as_acc_long(x, tolerance = tolerance)
}

as_acc_burst <- function(acc, axes, freq, start_timestamp = NULL) {
  colnms <- strsplit(as.character(axes), "")
  n_axis <- nchar(as.character(axes))
  mlist <- lapply(strsplit(acc, " "), as.integer)
  
  i <- !is.na(n_axis)
  
  mlist[!i] <- list(NULL)
  
  mlist[i] <- mapply(
    matrix, 
    mlist[i], 
    ncol = n_axis[i], 
    MoreArgs = list(byrow = TRUE), 
    SIMPLIFY = FALSE
  )
  
  mlist[i] <- mapply("colnames<-", mlist[i], colnms[i], SIMPLIFY = FALSE)
  
  new_acc(mlist, frequency = freq)
}

as_acc_long <- function(x, tolerance = 1, acc_cols = NULL, ...) {
  acc_cols <- acc_cols %||% active_acc_cols(x, quiet = TRUE)
  
  assertthat::assert_that(is_valid_acc_colset(acc_cols))
  assert_matched_acc_units(x, acc_cols)
  
  m <- as.matrix(data.frame(x)[, acc_cols])
  colnames(m) <- toupper(regmatches(acc_cols, regexpr("(.)$", acc_cols)))
  
  # TODO: may want a safer way to handle units. Some acc will have units, others not
  if (inherits(x[[acc_cols[[1]]]], "units")) {
    m <- m * units::as_units(units::deparse_unit(x[[acc_cols[[1]]]]))
  }
  
  # Generate vector of ids for each distinct burst based on sequential
  # timestamps within a given temporal tolerance
  ts_grps <- parse_bursts(x, tolerance = tolerance)
  
  # Split all rows with acc data into burst groups based on timestamp groups
  idx <- unname(split(which_acc_vals(x), ts_grps))
  
  # Extract records for each burst into a separate matrix
  acc_lst <- lapply(idx, function(i) m[i, , drop = FALSE])
  
  # Calculate mean frequency for each burst
  freq <- unname(unlist(
    lapply(
      split(move2::mt_time(x[which_acc_vals(x), ]), ts_grps), 
      function(y) {
        ifelse(length(y) <= 1, NA, mean(1 / units::as_units(diff(y))))
      }
    )
  ))
  
  # Attach acc bursts to index of the first record that belongs to that burst
  acc <- vec_rep(new_acc(list(NULL), units::set_units(NA, "Hz")), nrow(x))
  s <- sapply(idx, function(x) x[1]) # first index of each ts group
  
  if (length(s) > 0) {
    acc[s] <- new_acc(acc_lst, units::as_units(freq, "Hz"))
  }
  
  acc
}

which_acc_vals <- function(x, acc_cols = NULL, non_na = "any") {
  acc_cols <- acc_cols %||% active_acc_cols(x, quiet = TRUE)
  
  x <- as.data.frame(x) # Drop sticky move2 columns
  non_na <- rlang::arg_match(non_na, values = c("any", "all"))
  
  if (non_na == "any") {
    has_vals <- which(rowSums(!is.na(x[acc_cols])) > 0)
  } else {
    has_vals <- which(rowSums(!is.na(x[acc_cols])) == length(acc_cols))
  }
  
  has_vals
}

parse_bursts <- function(x, tolerance = 0.5) {
  tolerance <- units::as_units(tolerance, "s")
  acc_i <- which_acc_vals(x)
  idx <- split(acc_i, as.character(move2::mt_track_id(x[acc_i, ])))
  
  grps <- lapply(
    idx,
    function(i) {
      d <- units::as_units(diff(move2::mt_time(x[i, ])), "s")
      i[cumsum(c(TRUE, d > tolerance))]
    }
  )
  
  unname(unlist(grps))
}

assert_matched_acc_units <- function(x, cols) {
  for (i in seq_len(length(cols) - 1)) {
    assertthat::assert_that(
      get_units(x[[cols[[1]]]]) == get_units(x[[cols[[i + 1]]]])
    )
  }
}

# Hacky unit comparison for now. Some acc cols don't come with units
get_units <- function(x) {
  if (inherits(x, "units")) {
    units(x)
  } else {
    "None"
  }
}

