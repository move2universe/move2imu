#' Calculate dynamic body acceleration (DBA) for an `acc` vector
#'
#' @description
#' Compute vectorial dynamic body acceleration (VeDBA) or overall
#' dynamic body acceleration (ODBA) for each burst in an `acc` vector.
#'
#' Dynamic body acceleration is computed by subtracting the static
#' acceleration component from each axis and then summarizing the remaining
#' dynamic acceleration with:
#'
#' - VeDBA: mean of the Euclidean norm across samples
#' - ODBA: mean of the sum of absolute values across samples
#'
#' @param x An `acc` vector.
#'
#' @returns A numeric vector the same length as `x`.
#'
#' @noRd
#'
#' @examples
#' a <- acc_example()
#'
#' vedba(a)
#'
#' odba(a)
vedba <- function(x) {
  if (!is_acc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls acc} vector.")
  }
  dba_(x, .f = function(.br) vedba_(.br))
}

#' @noRd
odba <- function(x) {
  if (!is_acc(x)) {
    cli::cli_abort("{.arg x} must be an {.cls acc} vector.")
  }
  dba_(x, .f = function(.br) odba_(.br))
}

vedba_ <- function(b, ...) {
  if (inherits(b, "units")) {
    u <- units(b)
    b <- t(b) - units::set_units(colMeans(b), u, mode = "standard")

    vedba <- mean(sqrt(colSums(b^2)))
    vedba <- units::set_units(vedba, u, mode = "standard")
  } else {
    b <- t(b) - colMeans(b)
    vedba <- mean(sqrt(colSums(b^2)))
  }

  vedba
}

odba_ <- function(b, ...) {
  if (inherits(b, "units")) {
    u <- units(b)
    b <- t(b) - units::set_units(colMeans(b), u, mode = "standard")

    odba <- mean(colSums(abs(b)))
    odba <- units::set_units(odba, u, mode = "standard")
  } else {
    b <- t(b) - colMeans(b)
    odba <- mean(colSums(abs(b)))
  }

  odba
}

# Handle NA value logic. Process only non-NA entries, then reassign.
# For speed considerations, as accumulation of NA values can add meaningful
# processing time when iterating.
dba_ <- function(x, .f) {
  if (length(x) == 0) {
    return(NULL)
  }

  x_na <- is.na(x)

  if (all(x_na)) {
    return(rep(NA_real_, length(x)))
  }

  dba_non_na <- purrr::list_simplify(purrr::map(bursts(x[!x_na]), .f))

  if (all(!x_na)) {
    return(dba_non_na)
  }

  if (inherits(dba_non_na, "units")) {
    dba <- units::set_units(
      rep(NA_real_, length(x)),
      units(dba_non_na),
      mode = "standard"
    )
  } else {
    dba <- rep(NA_real_, length(x))
  }

  dba[!x_na] <- dba_non_na

  dba
}
