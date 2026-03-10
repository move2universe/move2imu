.onLoad <- function(...) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    vctrs::s3_register("tibble::as_tibble", "acc")
  }
}