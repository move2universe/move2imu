#' Extract IMU samples into a data frame
#'
#' @description
#' Convert an IMU vector to a data frame with one row for each sample in each
#' burst. Each row records which burst the sample came from, when it was
#' recorded, and its value on each axis.
#'
#' @param x An IMU vector (`acc`, `mag`, or `gyro`).
#'
#' @details
#' The time of each sample is calculated from its burst's start time and
#' sampling frequency. Samples from bursts without a start time or frequency
#' are recorded with a `NA` time.
#'
#' Axes that are not present in certain bursts are also filled with `NA` values
#' for that burst's rows.
#'
#' Missing bursts have no samples and therefore are not included in the output.
#' However, the ID in the `burst` column is index-matched to the input and
#' therefore matches the input burst in position `burst`, accounting for
#' missing bursts.
#'
#' Axis values keep the units of `x`. All bursts must have the same units, if
#' any exist. Use [set_imu_units()] to convert bursts to a single unit or
#' [drop_imu_units()] to remove units before calling `imu_samples()`.
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
        "Use {.fn set_imu_units} to convert all bursts to one unit, or ",
        "{.fn drop_imu_units} to remove units."
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
