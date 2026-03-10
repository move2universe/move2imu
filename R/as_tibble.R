as_tibble.acc <- function(x, include_bursts = FALSE, ...) {
  tbl <- tibble::as_tibble(as.data.frame(x))
  
  if (include_bursts) {
    tbl$bursts <- bursts(x)
  }
  
  tbl
}

#' @export
as.data.frame.acc <- function(x, ...) {
  data.frame(
    id = acc_id(x),
    frequency = freqs(x),
    start = starts(x)
  )
}