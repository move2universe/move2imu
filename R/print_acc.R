#' @import vctrs
NULL

#' @export
format.acc <- function(x, ...) {
  format_one <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }
    m <- round(apply(x, 2, mean), 2)

    if (inherits(x, "units")) {
      u <- (units(x))
      gr <- units::units_options("group")
      e <- paste0(" ", gr[1], u, gr[2])
    } else {
      e <- ""
    }
    paste0("(", paste(m, collapse = " "), ")", e)
  }
  vapply(bursts(x), format_one, character(1))
}

#' @export
obj_print_data.acc <- function(x, ...) {
  if (length(x) != 0) {
    print(format(x), quote = FALSE)
  }
}

#' @export
vec_ptype_abbr.acc <- function(x, ...) {
  "acc"
}

#' @export
vec_ptype_full.acc <- function(x, ...) {
  "acceleration"
}

# todo does this need export?
pillar_shaft.acc <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "right")
}
#' @export
obj_print_footer.acc <- function(x, ...) {
  f <- freqs(x)[!is.na(x)]

  if (length(unique(f)) <= 1) {
    r <- format(f[1])
  } else {
    r <- paste(format(range(f)), collapse = " - ")
  }
  cat("# frequency:", r)
}
