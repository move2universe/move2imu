#' Calculate the peak frequency per axis for bursts
#'
#' For each burst, find the frequency at which each axis oscillates most
#' strongly, such as a wingbeat or stride frequency.
#'
#' @inheritParams n_axis
#' @param resolution The spacing of the frequencies used to search for the peak
#'   frequency, in [units][units::units] convertible to Hz. For example, 0.1 Hz
#'   searches on a 0.1 Hz grid. By default, the spacing is the inverse of the
#'   burst duration. A finer grid locates a single clear peak more precisely,
#'   but two peaks can only be told apart if they are more than roughly
#'   1 / (burst duration) apart.
#'
#' @returns a list with the same length as `x` with the peak frequency per axis
#'
#' @details
#' The peak is found with a Fourier transform. By default, the grid spacing used
#' is 1 / (burst duration): for example, 0.5 Hz for a 2-second burst.
#' `resolution` makes the spacing finer. If the sampling frequency is not an
#' exact multiple of `resolution`, the spacing is made slightly finer than
#' requested. Bursts whose default spacing is already finer than `resolution`
#' keep their default spacing.
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
  if (!is.na(resolution)) {
    resolution <- as_hz(resolution)
  }

  # A peak frequency is undefined without both a burst and a known sampling
  # frequency, so missing bursts and present bursts with an unknown frequency
  # (e.g. single-sample bursts) both resolve to NA.
  x_na <- is.na(x) | is.na(freqs(x))

  if (all(x_na)) {
    return(as.list(rep(NA_real_, length(x))))
  }

  x_keep <- x[!x_na]

  # Operate on plain numbers and reattach Hz at the end, since units
  # arithmetic per burst is slow. Frequencies and `resolution` are both Hz.
  fqs_num <- as.numeric(freqs(x_keep))
  res_num <- if (!is.na(resolution)) as.numeric(resolution) else NA_real_

  if (!is.na(res_num)) {
    n_have <- vapply(bursts(x_keep), nrow, integer(1))
    n_need <- ceiling(fqs_num / res_num)
    n_fallback <- sum(n_have > n_need)
    if (n_fallback > 0) {
      cli::cli_warn(paste0(
        "Using the default spacing for {n_fallback} ",
        "burst{?s} with default spacing finer than {.arg resolution}."
      ))
    }
  }

  peak_freq_non_na <- purrr::map2(
    bursts(x_keep),
    fqs_num,
    function(b, fq) {
      peak_freq_(b, fq, resolution = res_num)
    }
  )

  peak_freq_non_na <- lapply(peak_freq_non_na, as_hz)

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
    if (to_pad > 0) {
      b_centered <- rbind(
        b_centered,
        matrix(0, nrow = to_pad, ncol = ncol(b_centered))
      )
    }
  }

  b_mod <- Mod(stats::mvfft(b_centered))

  # Keep positive frequencies only.
  half <- ceiling(nrow(b_mod) / 2)
  b_mod <- b_mod[seq_len(half), , drop = FALSE]

  peak <- apply(b_mod, 2, which.max)

  (peak - 1) * (freq / nrow(b_centered))
}
