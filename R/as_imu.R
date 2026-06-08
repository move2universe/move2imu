as_imu <- function(x, sensor, ...) {
  UseMethod("as_imu")
}

#' @export
as_imu.default <- function(x, sensor, ...) {
  vctrs::vec_cast(x, new_imu(sensor))
}

#' @export
as_imu.move2 <- function(x, sensor, colset = NULL, min_freq = 1, merge_continuous = TRUE, drop = FALSE, ...) {
  colsets <- parse_colsets(x, colset, sensor)
  dup <- duplicated_imu_rows(x, colsets = colsets)
  
  if (length(dup) > 0) {
    dup_fn <- paste0("duplicated_", sensor, "_rows")
    cli::cli_abort(c(
      "{.arg x} contains {length(dup)} timestamp{?s} with multiple sources of {sensor} data.",
      "i" = "Use {.help [{.fun {dup_fn}}](move2imu::{dup_fn})} to identify duplications."
    ))
  }

  # Use lapply as we don't need purr's index errors here. User likely
  # will not realize we are iterating over colsets.
  out <- lapply(
    colsets,
    function(cols) {
      as_imu_move2_(
        x,
        sensor = sensor,
        colset = cols,
        min_freq = min_freq,
        merge_continuous = merge_continuous,
        drop = FALSE,
        ...
      )
    }
  )
  
  out <- purrr::reduce(out, function(.x, .y) dplyr::coalesce(.x, .y))
  
  if (drop) {
    out <- out[!is.na(out)]
  }
  
  out
}

# Pipeline internals -----------------------------------------------------------

as_imu_move2_ <- function(x, sensor, colset, min_freq = 1, merge_continuous = TRUE, drop = FALSE, force_int = NULL, ...) {
  check_colset(x, colset)
  
  colset_type <- attr(colset, "type")
  
  if (colset_type == "expanded") {
    out <- as_imu_move2_expanded(x, colset = colset, sensor = sensor, min_freq = min_freq, ...)
  } else if (colset_type == "compact") {
    # eobs bursts are integer-encoded; other compact sources are numeric.
    # This is the only IMU-class-specific default in the compact pipeline.
    is_acc_eobs_cols <- sensor == "acc" && acc_colset_config()[["eobs"]]$is_(colset)
    
    out <- as_imu_compact(
      x[[colset[["bursts"]]]],
      x[[colset[["axes"]]]],
      x[[colset[["frequency"]]]],
      sensor = sensor,
      timestamp = move2::mt_time(x),
      force_int = force_int %||% is_acc_eobs_cols,
      ...
    )
  } else {
    abort_missing_colset(sensor)
  }
  
  if (merge_continuous) {
    out <- merge_imu(out, ids = move2::mt_track_id(x), drop = drop)
  }
  
  if (drop) {
    out <- out[!is.na(out)]
  }
  
  out
}

as_imu_compact <- function(x, axes, freq, sensor, timestamp, force_int = FALSE) {
  colnms <- strsplit(as.character(axes), "")
  n_axis <- nchar(as.character(axes))
  vals_split <- strsplit(as.character(x), " ")
  
  if (force_int) {
    all_vals <- unlist(vals_split)
    all_vals <- all_vals[!is.na(all_vals)]
    
    if (any((as.numeric(all_vals) %% 1) != 0)) {
      cli::cli_warn(
        "Detected numeric values, but expected integers. Some precision will be lost."
      )
    }
    
    mlist <- purrr::map(vals_split, function(x) as.integer(as.numeric(x)))
  } else {
    mlist <- purrr::map(vals_split, function(x) as.numeric(x))
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
  
  imu(sensor = sensor, bursts = mlist, frequency = freq, start = timestamp)
}

as_imu_move2_expanded <- function(x,
                                  colset,
                                  sensor,
                                  min_freq = 1,
                                  timestamp = move2::mt_time(x),
                                  freq_digits = 4,
                                  ...) {
  col_names <- as.character(colset)
  m <- as.matrix(as.data.frame(x)[, col_names])

  colnames(m) <- names(colset)
  
  # TODO: may want a safer way to handle units. Some columns will have units, others not
  if (inherits(x[[colset[[1]]]], "units")) {
    m <- m * units::as_units(units::deparse_unit(x[[colset[[1]]]]))
  }
  
  # Generate vector of ids for each distinct burst based on sequential
  # timestamps collected at a minimum frequency
  ts_grps <- parse_bursts(x, colset = colset, min_freq = min_freq)
  
  vals_i <- which_imu_vals(x, colset = colset)
  
  # Split all rows with IMU data into burst groups based on timestamp groups
  idx <- unname(split(vals_i, ts_grps))
  
  # Extract records for each burst into a separate matrix
  burst_lst <- lapply(idx, function(i) {
    x <- m[i, , drop = FALSE]
    rownames(x) <- NULL # Standardize data.frame and tibble inputs
    x
  })
  
  # Calculate mean frequency for each burst
  freq <- unname(unlist(
    lapply(
      split(move2::mt_time(x[vals_i, ]), ts_grps),
      function(y) {
        ifelse(length(y) <= 1, NA, mean(1 / units::as_units(diff(y))))
      }
    )
  ))
  
  freq <- round(freq, digits = freq_digits)
  
  # Attach bursts to index of the first record that belongs to that burst
  out <- vec_rep(
    imu(
      sensor,
      bursts = list(NULL),
      frequency = units::set_units(NA, "Hz"),
      start = as.POSIXct(NA, tz = attr(timestamp, "tzone") %||% "UTC")
    ),
    nrow(x)
  )
  
  i <- sapply(idx, function(x) x[1]) # first index of each ts group
  
  if (length(i) > 0) {
    out[i] <- imu(sensor, bursts = burst_lst, frequency = units::as_units(freq, "Hz"), start = timestamp[i])
  }
  
  out
}

# Resolve user-supplied `colset` into a list of validated IMU colsets.
# Falls back to colsets detected in `x` when `colset` is NULL.
parse_colsets <- function(x, colset, sensor, quiet = FALSE) {
  if (!rlang::is_null(colset)) {
    if (is_imu_colset(colset)) {
      colsets <- colset
    } else if (rlang::is_list(colset) && all(purrr::map_lgl(colset, is_imu_colset))) {
      colsets <- colset
    } else {
      cli::cli_abort(c(
        "{.arg colset} must be an {.cls imu_colset} object or a list of {.cls imu_colset} objects.",
        "i" = "Use {.help [{.fun imu_colset}](move2imu::imu_colset)} to create an {.cls imu_colset} object."
      ))
    }
  } else {
    colsets <- active_colsets_(x, sensor = sensor)

    if (!quiet && length(colsets) > 1) {
      cli::cli_warn("Detected multiple valid {sensor} column sets.")
    }
  }
  
  # Standardize case where user supplied a single colset as a vector
  if (!rlang::is_list(colsets)) {
    colsets <- list(colsets)
  }
  
  colsets
}

which_imu_vals <- function(x, colset) {
  assert_all_cols_present(x, colset)
  
  x <- as.data.frame(x) # Drop sticky move2 columns
  
  type <- attr(colset, "type")
  
  # Expanded-format columns only need at least one column to have data
  if (type == "expanded") {
    has_vals <- which(rowSums(!is.na(x[colset])) > 0)
  } else {
    has_vals <- which(rowSums(!is.na(x[colset])) == length(colset))
  }
  
  has_vals
}

#' Group expanded-format IMU samples into bursts
#'
#' @description
#' Based on the timestamps of the samples in expanded-format IMU
#' data, identify bursts based on the observed time gaps between samples. Gaps
#' that exceed a set threshold will be used to group samples into bursts.
#' Further, any observed changes in data collection frequency will also be
#' used to split samples into distinct bursts.
#'
#' @details
#' For continuous data, IMUs may dynamically update collection frequency.
#' However, a burst should not contain data from multiple collection
#' frequencies, so we must split these data into distinct bursts, despite the
#' fact that there may be no gap in collection.
#'
#' For samples at the boundary of a frequency change, there is
#' a fundamental ambiguity as to whether these samples should be included in
#' the burst prior to or the burst after the boundary timestamp. See comments
#' to `freq_changes` for details on our approach.
#'
#' @param x move2 object with expanded-format IMU data
#' @param min_freq Numeric value indicating the
#'   minimum allowable within-burst data collection frequency when identifying
#'   bursts in expanded-format IMU data. Any two adjacent timestamps
#'   that fall outside of the period defined by this frequency will be split
#'   into separate bursts. If no units are provided, this value is assumed to
#'   be in Hz.
#' @param freq_tol Noise parameter used when comparing frequencies to identify
#'   changes in data collection frequency in continuous data. Time differences
#'   that are within this value will be considered equal for the purposes of
#'   identifying consistent runs of a given collection frequency. This avoids
#'   error associated with floating point representation and IMU collection
#'   noise when identifying bursts.
#'
#' @returns Integer vector of IDs identifying burst groups
#' @noRd
parse_bursts <- function(x, colset, min_freq = 1, freq_tol = 1e-6) {
  if (min_freq < 0) {
    cli::cli_abort("{.arg min_freq} must be greater than or equal to 0.")
  }
  
  if (!inherits(min_freq, "units")) {
    min_freq <- units::set_units(min_freq, "Hz")
  }
  
  burst_gap_thresh <- units::set_units(1 / min_freq, "s")
  
  vals_i <- which_imu_vals(x, colset = colset)
  idx <- split(vals_i, as.character(move2::mt_track_id(x[vals_i, ])))
  
  grps <- lapply(
    idx,
    function(i) {
      d <- units::as_units(diff(move2::mt_time(x[i, ])), "s")
      
      # Identify collection split points based on min freq and freq changes
      below_freq <- c(TRUE, d > burst_gap_thresh)
      freq_bounds <- freq_changes(as.numeric(d), freq_tol = freq_tol)
      
      i[cumsum(below_freq | freq_bounds)]
    }
  )
  
  unname(unlist(grps))
}

# Identify transition points from one frequency to another within a sequential
# time difference vector.
#
# Sequential IMU data may change frequency. This can occur either from
# legitimate burst gaps or from changes in collection frequency. In general,
# when a change of frequency is detected, we create a new group of IMU
# values. See `new_freq_regime()` for more on the logic of how split points
# are determined in ambiguous cases.
freq_changes <- function(x, freq_tol = 1e-6) {
  # Get runs of values within a given tolerance
  freq_within_tol <- cumsum(c(TRUE, as.numeric(abs(diff(x))) > freq_tol))
  r <- rle(freq_within_tol)
  
  # Adjust first run length to account for loss of initial value from `diff()`
  r$lengths[1] <- r$lengths[1] + 1
  
  runs <- list()
  runs[1] <- list(new_freq_regime(r$lengths[1]))
  
  # Length of subsequent run. Used when deciding which run to attach
  # ambiguous split points to
  n_next <- c(r$lengths[-1], 0)
  
  # Generate logical vector with TRUE values marking transition states to
  # new frequency regimes
  for (i in seq_len(length(r$lengths))[-1]) {
    runs[i] <- list(
      new_freq_regime(
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
new_freq_regime <- function(n, n_next = 0, prev_run = FALSE) {
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

# Colset validation ------------------------------------------------------------

check_colset <- function(x, colset, call = rlang::caller_env()) {
  assert_all_cols_present(x, colset, call = call)
  assert_colset_has_data(x, colset, call = call)
  
  if (attr(colset, "type") == "compact") {
    assert_compact_col_types(x, colset, call = call)
  } else {
    assert_matched_units(x, colset, call = call)
    assert_colset_numeric(x, colset, call = call)
  }
}

assert_colset_has_data <- function(x, colset, call = rlang::caller_env()) {
  if (all(cols_empty(x, colset))) {
    cli::cli_abort(
      c(
        "The provided {.arg colset} columns contain no data.",
        "x" = "Column{?s} {.val {colset}} {?is/are} empty."
      ),
      call = call
    )
  }
}

assert_matched_units <- function(x, cols, call = rlang::caller_env()) {
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
    cli::cli_abort(
      c(
        "Multiple units detected across input columns.",
        "i" = "All columns must have consistent units."
      ),
      call = call
    )
  }
}

assert_colset_numeric <- function(x, colset, call = rlang::caller_env()) {
  cols_num <- purrr::map_lgl(colset, function(col) is.numeric(x[[col]]))
  
  if (any(!cols_num)) {
    non_numeric <- colset[!cols_num]
    cli::cli_abort(
      c(
        "Detected non-numeric column{?s}: {.val {non_numeric}}.",
        "i" = "Columns must contain numeric data."
      ),
      call = call
    )
  }
}

assert_compact_col_types <- function(x, colset, call = rlang::caller_env()) {
  bursts_col <- colset[["bursts"]]
  axes_col <- colset[["axes"]]
  freq_col <- colset[["frequency"]]
  
  if (!is.character(x[[bursts_col]]) && !is.factor(x[[bursts_col]])) {
    cli::cli_abort(
      "{.arg bursts} column {.val {bursts_col}} must be character, not {.cls {class(x[[bursts_col]])[1]}}.",
      call = call
    )
  }

  if (!is.character(x[[axes_col]]) && !is.factor(x[[axes_col]])) {
    cli::cli_abort(
      "{.arg axes} column {.val {axes_col}} must be character, not {.cls {class(x[[axes_col]])[1]}}.",
      call = call
    )
  }

  if (!is.numeric(x[[freq_col]])) {
    cli::cli_abort(
      "{.arg frequency} column {.val {freq_col}} must be numeric, not {.cls {class(x[[freq_col]])[1]}}.",
      call = call
    )
  }
}
