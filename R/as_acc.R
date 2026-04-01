#' Convert to acc
#'
#' In many cases the `as_acc` function will directly create an acceleration 
#' vector from input data
#'
#' @param x A `move2` containing acceleration data as collected by EOBS,
#'   Ornitela, or similar tracking devices. Most of the time this will be 
#'   either loaded from disk using [move2::mt_read] or downloaded using 
#'   [move2::movebank_download_study].
#' @param acc_cols Vector or list of column sets specifying the columns of `x` 
#'   that contain acceleration data. By default, constructs bursts for all 
#'   column sets that are detected in `x` that also contain data 
#'   (see [active_acc_colsets()]). 
#'   
#'   Use [acc_colset()] to specify a custom set
#'   of columns to use when identifying acceleration data in `x`.
#' @param min_frq Numeric value indicating the 
#'   minimum allowable within-burst data collection frequency when identifying
#'   bursts in long-format acceleration data. Any two adjacent timestamps 
#'   that fall outside of the period defined by this frequency will be split
#'   into separate bursts. If no units are provided, this value is assumed to
#'   be in Hz. 
#'   
#'   Ignored if data are already in predefined bursts (e.g. e-obs).
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
as_acc.move2 <- function(x, acc_cols = NULL, min_frq = 1, merge_continuous = TRUE, drop = TRUE, ...) {
  assertthat::assert_that(move2::mt_is_track_id_cleaved(x))
  assertthat::assert_that(move2::mt_is_time_ordered(x))
  
  if (!rlang::is_null(acc_cols)) {
    if (is_acc_colset(acc_cols)) {
      colsets <- acc_cols
    } else if (rlang::is_list(acc_cols) && all(purrr::map_lgl(acc_cols, is_acc_colset))) {
      colsets <- acc_cols
    } else {
      rlang::abort(
        c(
          "`acc_cols` must be an `acc_colset` object or a list of such objects.", 
          i = "Use `acc_colset()` to create an `acc_colset` object.")
      )
    }
  } else {
    colsets <- active_acc_colsets(x)
    
    if (length(colsets) > 1) {
      rlang::warn("Detected multiple valid acceleration column sets.")
    }
  }
  
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }
  
  dup <- duplicated_acc_rows(x, acc_cols = colsets)
  
  if (length(dup) > 0) {
    rlang::abort(c(
      paste0("`x` contains ", length(dup), " timestamps with multiple sources of acceleration data."),
      i = "Use `duplicated_acc_rows()` to identify duplications."
    ))
  }
  
  acc <- purrr::map(
    colsets, 
    function(cols) {
      as_acc_move2_(
        x,
        acc_cols = cols,
        min_frq = min_frq,
        merge_continuous = merge_continuous,
        drop = FALSE,
        ...
      )
    }
  )
  
  acc <- purrr::reduce(acc, function(.x, .y) dplyr::coalesce(.x, .y))
  
  if (drop) {
    acc <- acc[!is.na(acc)]
  }
  
  acc
}

as_acc_move2_ <- function(x, acc_cols, min_frq = 1, merge_continuous = TRUE, drop = TRUE, force_int = NULL, ...) {
  check_acc_cols(x, acc_cols)
  
  acc_type <- attr(acc_cols, "type")
  
  if (acc_type == "long") {
    acc <- as_acc_move2_long(x, acc_cols = acc_cols, min_frq = min_frq, ...)
  } else if (acc_type == "burst") {
    acc <- as_acc_burst(
      x[[acc_cols[["bursts"]]]],
      x[[acc_cols[["axes"]]]],
      x[[acc_cols[["frequency"]]]],
      timestamp = move2::mt_time(x),
      force_int = force_int %||% is_acc_eobs_cols(acc_cols),
      ...
    )
  } else {
    abort_missing_acc_colset()
  }
  
  if (merge_continuous) {
    acc <- merge_continuous_acc(acc, acc_ids = move2::mt_track_id(x), drop = drop)
  }
  
  if (drop) {
    acc <- acc[!is.na(acc)]
  }
  
  acc
}

as_acc_burst <- function(acc, axes, freq, timestamp, force_int = FALSE) {
  colnms <- strsplit(as.character(axes), "")
  n_axis <- nchar(as.character(axes))
  acc_split <- strsplit(as.character(acc), " ")
  
  if (force_int) {
    all_acc <- unlist(acc_split)
    all_acc <- all_acc[!is.na(all_acc)]
    
    if (any((as.numeric(all_acc) %% 1) != 0)) {
      rlang::warn(
        paste0(
          "Detected numeric acceleration values, but expected integers. ",
          "Some precision will be lost."
        )
      )
    }
    
    mlist <- purrr::map(acc_split, function(x) as.integer(as.numeric(x)))
  } else {
    mlist <- purrr::map(acc_split, function(x) as.numeric(x))
  }
  
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
as_acc_move2_long <- function(x,
                              acc_cols,
                              min_frq = 1,
                              timestamp = move2::mt_time(x),
                              frq_digits = 4,
                              ...) {
  col_names <- as.character(acc_cols)
  m <- as.matrix(data.frame(x)[, col_names])
  
  colnames(m) <- names(acc_cols)
  
  # TODO: may want a safer way to handle units. Some acc will have units, others not
  if (inherits(x[[acc_cols[[1]]]], "units")) {
    m <- m * units::as_units(units::deparse_unit(x[[acc_cols[[1]]]]))
  }
  
  # Generate vector of ids for each distinct burst based on sequential
  # timestamps collected at a minimum frequency
  ts_grps <- parse_bursts(x, acc_cols = acc_cols, min_frq = min_frq)
  
  acc_i <- which_acc_vals(x, acc_cols = acc_cols)
  
  # Split all rows with acc data into burst groups based on timestamp groups
  idx <- unname(split(acc_i, ts_grps))
  
  # Extract records for each burst into a separate matrix
  acc_lst <- lapply(idx, function(i) {
    x <- m[i, , drop = FALSE]
    rownames(x) <- NULL # Standardize data.frame and tibble inputs
    x
  })
  
  # Calculate mean frequency for each burst
  freq <- unname(unlist(
    lapply(
      split(move2::mt_time(x[acc_i, ]), ts_grps), 
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
      start = as.POSIXct(NA, tz = attr(timestamp, "tzone") %||% "UTC")
    ), 
    nrow(x)
  )
  
  i <- sapply(idx, function(x) x[1]) # first index of each ts group
  
  if (length(i) > 0) {
    acc[i] <- acc(acc_lst, units::as_units(freq, "Hz"), start = timestamp[i])
  }
  
  acc
}

which_acc_vals <- function(x, acc_cols) {
  assert_all_cols_present(x, acc_cols)
  
  x <- as.data.frame(x) # Drop sticky move2 columns
  
  type <- attr(acc_cols, "type")
  
  # Long-format columns only need at least one column to have data
  if (type == "long") {
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
#' the burst prior to or the burst after the boundary timestamp. See comments
#' to `frq_changes` for details on our approach.
#'
#' @param x move2 object with long-format acceleration data
#' @param min_frq Numeric value indicating the 
#'   minimum allowable within-burst data collection frequency when identifying
#'   bursts in long-format acceleration data. Any two adjacent timestamps 
#'   that fall outside of the period defined by this frequency will be split
#'   into separate bursts. If no units are provided, this value is assumed to
#'   be in Hz.
#' @param freq_tol Noise parameter used when comparing frequencies to identify
#'   changes in data collection frequency in continuous data. Time differences
#'   that are within this value will be considered equal for the purposes of
#'   identifying consistent runs of a given collection frequency. This avoids
#'   error associated with floating point representation and sensor collection
#'   noise when identifying bursts.
#'
#' @returns Integer vector of IDs identifying burst groups
#' @noRd
parse_bursts <- function(x, acc_cols, min_frq = 1, freq_tol = 1e-6) {
  assertthat::assert_that(min_frq >= 0)
  
  if (!inherits(min_frq, "units")) {
    min_frq <- units::set_units(min_frq, "Hz")
  }
  
  burst_gap_thresh <- units::set_units(1 / min_frq, "s")
  
  acc_i <- which_acc_vals(x, acc_cols = acc_cols)
  idx <- split(acc_i, as.character(move2::mt_track_id(x[acc_i, ])))
  
  grps <- lapply(
    idx,
    function(i) {
      d <- units::as_units(diff(move2::mt_time(x[i, ])), "s")
      
      # Identify collection split points based on min frq and frq changes
      below_frq <- c(TRUE, d > burst_gap_thresh)
      frq_bounds <- frq_changes(as.numeric(d), freq_tol = freq_tol)
      
      i[cumsum(below_frq | frq_bounds)]
    }
  )
  
  unname(unlist(grps))
}

# Identify transition points from one frequency to another within a sequential
# time difference vector.
#
# Sequential acceleration data may change frequency. This can occur either from
# legitimate burst gaps or from changes in collection frequency. In general,
# when a change of frequency is detected, we create a new group of acceleration
# values. See `new_frq_regime()` for more on the logic of how split points
# are determined in ambiguous cases.
frq_changes <- function(x, freq_tol = 1e-6) {
  # Get runs of values within a given tolerance
  freq_within_tol <- cumsum(c(TRUE, as.numeric(abs(diff(x))) > freq_tol))
  r <- rle(freq_within_tol)
  
  # Adjust first run length to account for loss of initial value from `diff()`
  r$lengths[1] <- r$lengths[1] + 1
  
  runs <- list()
  runs[1] <- list(new_frq_regime(r$lengths[1]))
  
  # Length of subsequent run. Used when deciding which run to attach
  # ambiguous split points to
  n_next <- c(r$lengths[-1], 0)
  
  # Generate logical vector with TRUE values marking transition states to
  # new frequency regimes
  for (i in seq_len(length(r$lengths))[-1]) {
    runs[i] <- list(
      new_frq_regime(
        r$lengths[i], 
        n_next = n_next[i], 
        prev_run = runs[[i - 1]]
      )
    )
  }
  
  unlist(runs)
}

# Helper to build logical runs identifying sequences of frequency regimes
#
# In a sequence of time diffs, we identify the start of a new frequency
# regime where there is a change in frequency from one index to the next.
# The following time gap is established as the frequency of the next
# regime. This function generates a logical vector for each run of 
# consistent frequency values. TRUE values mark start indexes of new 
# frequency regimes. FALSE values mark indexes that will be grouped with the
# closest TRUE value that precedes them.
#
# Where multiple frequency changes happen in succession, there is ambiguity as
# to how values should be grouped, as no frequency regime can definitively be
# established for a series of length-1 sequences. That is, each of these single
# values could just as reasonably be grouped with the value prior to them or
# after them. In these cases, we group
# the record immediately following the initial frequency change (t + 1) with that 
# initial frequency change (t), unless the subsequent run starting with record 
# (t + 2) is longer than 1. In these cases, we consider 
# the (t + 1) record to belong to the (t + 2) sequence and the (t) record becomes
# an isolated length-1 sequence.
new_frq_regime <- function(n, n_next = 0, prev_run = FALSE) {
  # If the previous run ends in FALSE, this run should start a new regime
  start <- !prev_run[length(prev_run)]
  
  # Force this record to join with next run if it is length-1 and that run is 
  # longer than length-1. (This addresses cases where a length-1 value could
  # either be joined to its previous run or its subsequent run)
  if (n == 1 && n_next > 1) {
    start <- TRUE
  }
  
  c(start, rep(FALSE, n - 1))
}

check_acc_cols <- function(x, acc_cols, call = rlang::caller_env()) {
  assert_all_cols_present(x, acc_cols, call = call)
  
  if (attr(acc_cols, "type") == "burst") {
    assert_burst_col_types(x, acc_cols, call = call)
  } else {
    assert_matched_acc_units(x, acc_cols, call = call)
    assert_acc_cols_numeric(x, acc_cols, call = call)
  }
}

assert_matched_acc_units <- function(x, cols, call = rlang::caller_env()) {
  unique_units <- unique(
    purrr::map(
      cols, 
      function(col) {
        if (inherits(x[[col]], "units")) {
          units(x[[col]])
        } else {
          NA
        }
      }
    )
  )
  
  if (length(unique_units) != 1) {
    rlang::abort(
      c(
        "Multiple units detected across input acc columns.",
        i = "All acceleration columns must have consistent units."
      ),
      call = call
    )
  }
}

assert_acc_cols_numeric <- function(x, acc_cols, call = rlang::caller_env()) {
  cols_num <- purrr::map_lgl(acc_cols, function(col) is.numeric(x[[col]]))
  
  if (any(!cols_num)) {
    rlang::abort(
      c(
        paste0(
          "Detected non-numeric columns: \"", 
          paste0(acc_cols[!cols_num], collapse = "\", \""), "\""
        ),
        i = "Acceleration columns must contain numeric data."
      ),
      call = call
    )
  }
}

assert_burst_col_types <- function(x, acc_cols, call = rlang::caller_env()) {
  bursts_col <- acc_cols[["bursts"]]
  axes_col <- acc_cols[["axes"]]
  freq_col <- acc_cols[["frequency"]]
  
  if (!is.character(x[[bursts_col]]) && !is.factor(x[[bursts_col]])) {
    rlang::abort(c(
      paste0("`bursts` column \"", bursts_col, "\" must be character, not ", class(x[[bursts_col]])[1])
    ), call = call)
  }
  
  if (!is.character(x[[axes_col]]) && !is.factor(x[[axes_col]])) {
    rlang::abort(c(
      paste0("`axes` column \"", axes_col, "\" must be character, not ", class(x[[axes_col]])[1])
    ), call = call)
  }
  
  if (!is.numeric(x[[freq_col]])) {
    rlang::abort(c(
      paste0("`frequency` column \"", freq_col, "\" must be numeric, not ", class(x[[freq_col]])[1])
    ), call = call)
  }
}
