#' @export
vedba <- function(x) {
  map_acc(x, function(.br) vedba_(.br), simplify = TRUE)
}

#' @export
odba <- function(x) {
  map_acc(x, function(.br) odba_(.br), simplify = TRUE)
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
