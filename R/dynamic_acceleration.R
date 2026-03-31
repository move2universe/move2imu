#' @export
vedba <- function(x) {
  dba_(x, .f = function(.br) vedba_(.br))
}

#' @export
odba <- function(x) {
  dba_(x, .f = function(.br) odba_(.br))
}

vedba_ <- function(b, ...) {
  if (inherits(b, "units")) {
    u <- units(b)
    b <- t(b) - units::set_units(colMeans(b), u, mode = "standard")

    vedba <- mean(sqrt(colSums(b ^ 2)))
    vedba <- units::set_units(vedba, u, mode = "standard")
  } else {
    b <- t(b) - colMeans(b)
    vedba <- mean(sqrt(colSums(b ^ 2)))
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
# processing time in map_acc()
dba_ <- function(x, .f) {
  if (length(x) == 0) {
    return(NULL)
  }
  
  x_na <- is.na(x)
  
  if (all(x_na)) {
    return(rep(NA_real_, length(x)))
  }
  
  dba_non_na <- map_acc(x[!x_na], function(.br) .f(.br), simplify = TRUE)
  
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
