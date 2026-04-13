
#' Calculate the peak frequency per axis for acceleration bursts
#'
#' @param x An `acc` vector
#' @param resolution A scalar with the [units][units::units] Hertz
#'
#' @returns returns a list with the same length as `x` with the peak frequency per axis
#'
#' @details
#'
#' To increase the resolution of the result zero padding can be used. This can be controlled using the resolution
#' argument. Note that increasing resolution without increasing the number of samples in a acceleration burst has a
#' limited ability to get closer to the true frequency.
#'
#' @export
#'
#' @examples
#'   a<-acc(list(cbind(z=cos(1:200/(80/(pi*2))),
#'   x=sin(1:200/(5/(pi*2))))), units::set_units(400,'Hz'))
#'   peak_frequency(a)
#'   peak_frequency(a, units::set_units(.25, "Hz"))
#'   # Increasing resolution more
#'   peak_frequency(a, units::set_units(.005, "Hz"))
#'   a<-acc(list(cbind(z=cos(80+1:200/(80/(pi*2))),
#'   x=sin((1:200)/(5/(pi*2))))), units::set_units(400,'Hz'))
#'   peak_frequency(a, units::set_units(.005, "Hz"))
peak_frequency <- function(x, resolution = NA) {
  x_na <- is.na(x)

  if (all(x_na)) {
    return(as.list(rep(NA_real_, length(x))))
  }

  peak_freq_non_na <- map_acc(
    x[!x_na],
    function(.br, .fq) peak_freq_(.br, .fq, resolution = resolution)
  )

  if (all(!x_na)) {
    return(peak_freq_non_na)
  }

  peak_freq <- vector("list", length(x))
  peak_freq[x_na] <- list(NA_real_)
  peak_freq[!x_na] <- peak_freq_non_na
  peak_freq
}

# Peak frequency for a single burst and freq
peak_freq_ <- function(burst, freq, resolution = NA) {
  if (inherits(burst, "units")) {
    burst <- units::drop_units(burst)
  }
  
  b_centered <- t(burst) - colMeans(burst)
  
  if(!is.na(resolution)){
    to_pad <- units::drop_units(freq / resolution) - nrow(burst)
    b_centered <- cbind(b_centered, matrix(0, ncol = to_pad, nrow = nrow(b_centered)))
  }
  
  b_mod <- do.call(rbind, lapply(apply(b_centered, 1, stats::fft, simplify = F), Mod))[, 1:ceiling(ncol(b_centered)/2), drop = FALSE]
  peak<- apply(b_mod, 1, which.max)
  
  (peak - 1) * (freq / ncol(b_mod) / 2)
}
