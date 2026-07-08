#' @import vctrs
NULL

#' @export
format.imu <- function(x, ...) {
  format_one <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }
    m <- round(colMeans(x), 2)

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
obj_print_data.imu <- function(x, ...) {
  total <- length(x)
  if (total == 0) {
    return(invisible(x))
  }

  # Only format the bursts that will actually be displayed
  show_n <- min(total, getOption("max.print", 99999L))

  if (show_n < total) {
    print(format(x[seq_len(show_n)]), quote = FALSE)

    msg <- paste0(
      " [ reached `getOption(\"max.print\")` -- omitted ",
      format(total - show_n, big.mark = ",", trim = TRUE),
      " entries ]"
    )

    cat(pillar::style_subtle(msg), "\n", sep = "")
  } else {
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

#' @export
vec_ptype_abbr.mag <- function(x, ...) {
  "mag"
}

#' @export
vec_ptype_full.mag <- function(x, ...) {
  "magnetometer"
}

#' @export
vec_ptype_abbr.gyro <- function(x, ...) {
  "gyro"
}

#' @export
vec_ptype_full.gyro <- function(x, ...) {
  "gyroscope"
}

#' @export
pillar_shaft.imu <- function(x, ...) {
  out <- format(x)
  pillar::new_pillar_shaft_simple(out, align = "right")
}

#' @export
obj_print_footer.imu <- function(x, ...) {
  f <- freqs(x[!is.na(x)])
  f <- f[!is.na(f)]

  if (length(f) == 0) {
    r <- "[ no data ]"
  } else if (length(unique(f)) <= 1) {
    r <- format(f[1])
  } else {
    r <- paste(format(range(f)), collapse = " - ")
  }
  cat("# frequency:", r)
}
