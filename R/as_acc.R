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
#' @param merge_continuous Logical value indicating whether to merge
#'   adjacent acceleration bursts. Two adjacent bursts can be merged if the
#'   first burst ends at the same time that the second starts and the
#'   burst frequency is identical between the two. This is useful for
#'   processing continuous acceleration data that have been stored in chunks
#'   split at regular intervals.
#' @param drop Logical indicating whether empty acceleration objects should
#'   be dropped from the output. If `drop = FALSE`, then the length of the
#'   output will match the number of rows in the input data `x` and acceleration
#'   bursts will be stored at the index location corresponding to the start time
#'   of the burst.
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

#' @rdname as_acc
#' @export
as_acc.default <- function(x, ...) {
  vctrs::vec_cast(x, new_acc())
}

#' @rdname as_acc
#' @export
as_acc.move2 <- function(x, tolerance = 0.5, merge_continuous = TRUE, drop = TRUE, ...) {
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
  
  if (merge_continuous) {
    acc <- merge_continuous_acc(acc)
  }
  
  if (drop) {
    acc <- acc[!is.na(acc)]
  }
  
  acc
}

as_acc_move2_eobs <- function(x, ...) {
  assertthat::assert_that(has_acc_eobs_cols(x))
  
  as_acc_burst(
    x[["eobs_accelerations_raw"]],
    x[["eobs_acceleration_axes"]],
    x[["eobs_acceleration_sampling_frequency_per_axis"]],
    timestamp = move2::mt_time(x),
    ...
  )
}

as_acc_move2_burst <- function(x, ...) {
  assertthat::assert_that(has_acc_burst_cols(x))
  
  as_acc_burst(
    x[["accelerations_raw"]],
    x[["acceleration_axes"]],
    x[["acceleration_sampling_frequency_per_axis"]],
    timestamp = move2::mt_time(x),
    ...
  )
}

as_acc_move2_raw_xyz <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_raw_xyz_cols(x))
  as_acc_long(x, tolerance = tolerance, ...)
}

as_acc_move2_xyz <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_xyz_cols(x))
  as_acc_long(x, tolerance = tolerance, ...)
}

# TODO: decide whether tilt is supported? It seems to co-occur with raw xyz cols
as_acc_move2_tilt <- function(x, tolerance = 0.5, ...) {
  assertthat::assert_that(has_acc_tilt_cols(x))
  as_acc_long(x, tolerance = tolerance, ...)
}

as_acc_burst <- function(acc, axes, freq, timestamp) {
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
  
  acc(mlist, frequency = freq, start = timestamp)
}

# TODO: this should maybe be refactored to be analogous to `as_acc_burst` which doesn't
# take input move2 `x`, just takes the data cols.
as_acc_long <- function(x, 
                        tolerance = 1, 
                        acc_cols = NULL, 
                        timestamp = move2::mt_time(x), 
                        frq_digits = 4,
                        ...) {
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
  acc_lst <- lapply(idx, function(i) {
    x <- m[i, , drop = FALSE]
    rownames(x) <- NULL # Standardize data.frame and tibble inputs
    x
  })
  
  # Calculate mean frequency for each burst
  freq <- unname(unlist(
    lapply(
      split(move2::mt_time(x[which_acc_vals(x), ]), ts_grps), 
      function(y) {
        ifelse(length(y) <= 1, NA, mean(1 / units::as_units(diff(y))))
      }
    )
  ))
  
  freq <- round(freq, digits = frq_digits)
  
  # Attach acc bursts to index of the first record that belongs to that burst
  acc <- vec_rep(
    acc(
      list(NULL), 
      units::set_units(NA, "Hz"), 
      start = as.POSIXct(NA, tz = "UTC")
    ), 
    nrow(x)
  )
  
  i <- sapply(idx, function(x) x[1]) # first index of each ts group
  
  if (length(i) > 0) {
    acc[i] <- acc(acc_lst, units::as_units(freq, "Hz"), start = timestamp[i])
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

#' Group long-format acceleration records into bursts
#'
#' @description
#' Based on the timestamps of the records in long-format acceleration
#' data, identify bursts based on the observed time gaps between records. Gaps
#' that exceed a set threshold will be used to group records into bursts.
#' Further, any observed changes in data collection frequency will also be
#' used to split records into distinct bursts.
#' 
#' @details
#' For continuous data, sensors may dynamically update collection frequency.
#' However, an `acc` burst should not contain data from multiple collection
#' frequencies, so we must split these data into distinct bursts, despite the
#' fact that there may be no gap in collection.
#' 
#' For acceleration records at the boundary of a frequency change, there is
#' a fundamental ambiguity as to whether these records should be included in
#' the burst prior to or the burst after the boundary timestamp. Our approach
#' groups records at the boundary time with the burst prior to the boundary
#' timestamp.
#'
#' @param x move2 object with long-format acceleration data
#' @param tolerance Numeric value indicating the maximum allowable gap (in
#'   seconds) between consecutive timestamps that should be included in the 
#'   same burst.
#' @param freq_tol Noise parameter used when comparing frequencies to identify
#'   changes in data collection frequency in continuous data. Time differences
#'   that are within this value will be considered equal for the purposes of
#'   identifying consistent runs of a given collection frequency. This avoids
#'   error associated with floating point representation and sensor collection
#'   noise when identifying bursts.
#'
#' @returns Integer vector of IDs identifying burst groups
#' @noRd
parse_bursts <- function(x, tolerance = 0.5, freq_tol = 1e-6) {
  tolerance <- units::as_units(tolerance, "s")
  acc_i <- which_acc_vals(x)
  idx <- split(acc_i, as.character(move2::mt_track_id(x[acc_i, ])))
  
  grps <- lapply(
    idx,
    function(i) {
      d <- units::as_units(diff(move2::mt_time(x[i, ])), "s")
      
      # Identify collection gaps
      above_tol <- c(TRUE, d > tolerance)
      
      # Identify frequency changes with some error tolerance
      frq_change <- !near(
        as.numeric(d[-1]), 
        as.numeric(d[-length(d)]), 
        tol = freq_tol
      )
      
      # The frequency at adjacent times must be different and both diffs 
      # involved in this frequency change must be below the threshold
      frq_change <- frq_change & d[-1] <= tolerance & d[-length(d)] <= tolerance
        
      # First two records cannot be resolved as a frequency change because
      # of lack of temporal resolution, but this is consistent with our approach
      # of including boundary records with the previous burst
      frq_change <- c(FALSE, FALSE, frq_change)
      
      i[cumsum(frq_change | above_tol)]
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

