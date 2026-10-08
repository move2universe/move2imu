#' Flatten an IMU vector into a data frame of samples
#'
#' @description
#' Convert an IMU vector to a data frame with one row for each sample in each
#' burst. Each row records which burst the sample came from, when it was
#' recorded, and its value on each axis.
#'
#' Most analyses can be done on IMU vectors directly. This function is intended
#' primarily for methods that are hard to apply to IMU vectors, such as those
#' that require individual sample timestamps and values.
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`).
#'
#' @details
#' The time of each sample is calculated from its burst's start time and
#' sampling frequency. Samples from bursts without a start time or frequency
#' are recorded with a `NA` time.
#'
#' The `burst` column shows the index position in `x` for the burst that the
#' sample belongs to. Missing bursts contribute no rows to the output but
#' are included in the index count. If `x` is a column in a `data.frame` or
#' `move2` object, this means that the value of `burst` can be used to look up
#' attributes in the corresponding rows of that data source by index, for
#' instance to attach burst-level metadata (e.g., track ID) to the sample
#' data frame (see examples). Attributes that can vary within a burst should
#' not be matched this way, as the value of that attribute at the start of the
#' burst will be used for the entirety of the burst in the sample output. In
#' those cases, consider joining on time instead.
#'
#' Axis values keep the units of `x`. All bursts must have the same units, or
#' all must be unitless. Use [set_imu_units()] to convert bursts to a single
#' unit, or [transform_imu()] to calibrate raw (unitless) values, before calling
#' `imu_samples()`.
#'
#' Axes that are not present in certain bursts are filled with `NA` values
#' for that burst's rows.
#'
#' @returns A data frame with one row per sample and columns:
#'   - `burst`: The position of the sample's burst in `x`.
#'   - `time`: The time the sample was recorded.
#'   - `frequency`: The sampling frequency of the sample's burst.
#'   - `X`, `Y`, `Z`: The sample's value on each axis recorded in `x`.
#'
#' @export
#'
#' @examples
#' a <- set_imu_units(acc_example(), "standard_free_fall")
#'
#' samples <- imu_samples(a)
#'
#' # (Times are recorded to sub-second precision despite printing to the second)
#' head(samples)
#'
#' @examplesIf rlang::is_installed("move2")
#' # Store acc bursts in a column of the input data
#' alb <- albatrosses()
#' alb$acc <- as_acc(alb)
#'
#' samples <- imu_samples(alb$acc)
#'
#' # Use the `burst` ID column to look up attributes from the input. `burst`
#' # is the row of `alb` containing the burst that each sample belongs to.
#' samples$track <- move2::mt_track_id(alb)[samples$burst]
#'
#' head(samples)
imu_samples <- function(x) {
  assert_imu(x)

  burst_i <- which(!is.na(x))
  x <- x[burst_i]

  n <- n_samples(x)
  freq <- freqs(x)

  # Offset time from the start of the burst to each of its samples.
  offset <- unlist(
    purrr::map2(as.numeric(freq), n, function(f, n) (seq_len(n) - 1) / f)
  )

  # All bursts must share one unit (or none) so it can be applied to every
  # axis column at the end. Unitless bursts count as their own unit here.
  u <- unique(imu_units(x))

  if (length(u) > 1) {
    cli::cli_abort(c(
      "All bursts in {.arg x} must have the same units.",
      "i" = paste0(
        "See {.help [{.fn transform_imu}](move2imu::transform_imu)} or ",
        "{.help [{.fn set_imu_units}](move2imu::set_imu_units)}."
      )
    ))
  }

  b <- bursts(drop_imu_units(x))

  # Build one column per axis (in X, Y, Z order), filling with NA for bursts
  # that did not record that axis.
  axis_names <- intersect(c("X", "Y", "Z"), unique(unlist(lapply(b, colnames))))

  axes <- lapply(
    axis_names,
    function(axis) {
      # Extract all values for a given axis across bursts
      vals <- lapply(b, function(m) {
        if (axis %in% colnames(m)) as.numeric(m[, axis]) else rep(NA_real_, nrow(m))
      })

      # Combine axis vals
      col <- unlist(vals, use.names = FALSE)

      if (length(u) == 1 && !is.na(u)) {
        col <- units::set_units(col, u, mode = "standard")
      }

      col
    }
  )

  names(axes) <- axis_names
  axes <- vctrs::new_data_frame(axes, n = sum(n))

  vctrs::vec_cbind(
    data.frame(
      burst = rep(burst_i, n),
      time = rep(starts(x), n) + offset,
      frequency = rep(freq, n)
    ),
    axes
  )
}
