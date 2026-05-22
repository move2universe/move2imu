#' Calculate the peak frequency per axis for bursts
#'
#' @inheritParams n_axis
#' @param resolution A scalar with the [units][units::units] Hertz
#'
#' @returns returns a list with the same length as `x` with the peak frequency per axis
#'
#' @details
#' Use the `resolution` argument to increase the resolution of the result by
#' padding the sample vector with zeros. Note that increasing resolution without
#' increasing the number of samples in a burst has only a limited ability to
#' more closely determine the true frequency.
#'
#' @noRd
#'
#' @examples
#' a <- acc(
#'   list(
#'     cbind(
#'       X = sin(1:200 / (5 / (pi * 2))),
#'       Z = cos(1:200 / (80 / (pi * 2)))
#'     )
#'   ),
#'   units::set_units(400, "Hz")
#' )
#'
#' peak_frequency(a)
#'
#' peak_frequency(a, units::set_units(.25, "Hz"))
#'
#' # Increasing resolution more
#' peak_frequency(a, units::set_units(.005, "Hz"))
#'
#' a <- acc(
#'   list(
#'     cbind(
#'       X = sin((1:200) / (5 / (pi * 2))),
#'       Z = cos(80 + 1:200 / (80 / (pi * 2)))
#'     )
#'   ),
#'   units::set_units(400, "Hz")
#' )
#'
#' peak_frequency(a, units::set_units(.005, "Hz"))
peak_frequency <- function(x, resolution = NA) {
  x_na <- is.na(x)

  if (all(x_na)) {
    return(as.list(rep(NA_real_, length(x))))
  }

  x_keep <- x[!x_na]

  # Operate on scalars and reattach units. Processing units per step
  # is time intensive.
  fqs <- freqs(x_keep)
  fq_unit <- if (inherits(fqs, "units")) units(fqs) else NULL
  fqs_num <- as.numeric(fqs)

  res_num <- if (!is.na(resolution)) {
    if (!is.null(fq_unit)) {
      as.numeric(units::set_units(resolution, fq_unit, mode = "standard"))
    } else {
      as.numeric(resolution)
    }
  } else {
    NA_real_
  }

  peak_freq_non_na <- purrr::map2(
    bursts(x_keep),
    fqs_num,
    function(b, fq) {
      peak_freq_(b, fq, resolution = res_num)
    }
  )

  if (!is.null(fq_unit)) {
    peak_freq_non_na <- lapply(
      peak_freq_non_na,
      function(v) {
        units::set_units(v, fq_unit, mode = "standard")
      }
    )
  }

  if (all(!x_na)) {
    return(peak_freq_non_na)
  }

  peak_freq <- vector("list", length(x))
  peak_freq[x_na] <- list(NA_real_)
  peak_freq[!x_na] <- peak_freq_non_na
  peak_freq
}

# Peak frequency for a single burst. `freq` and `resolution` are plain numeric
peak_freq_ <- function(burst, freq, resolution = NA_real_) {
  if (inherits(burst, "units")) {
    burst <- units::drop_units(burst)
  }

  b_centered <- sweep(burst, 2, colMeans(burst), FUN = "-")

  if (!is.na(resolution)) {
    to_pad <- ceiling(freq / resolution) - nrow(burst)
    b_centered <- rbind(
      b_centered,
      matrix(0, nrow = to_pad, ncol = ncol(b_centered))
    )
  }

  b_mod <- Mod(stats::mvfft(b_centered))

  # Keep positive frequencies only.
  half <- ceiling(nrow(b_mod) / 2)
  b_mod <- b_mod[seq_len(half), , drop = FALSE]

  peak <- apply(b_mod, 2, which.max)

  (peak - 1) * (freq / nrow(b_centered))
}
