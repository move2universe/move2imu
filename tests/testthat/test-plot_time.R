test_that("plot_time", {
  expect_silent(
    graph <- plot_time(acc_example(), Sys.time() + c(0,10))
  )
  expect_s3_class(graph, "dygraphs")
})

test_that("plot_time uses seconds regardless of frequency unit", {
  # Equivalent frequencies: 20 Hz and 1200/min.
  burst <- matrix(seq_len(20), ncol = 1, dimnames = list(NULL, "X"))
  start <- as.POSIXct("2026-01-01", tz = "UTC")

  a_hz  <- acc(list(burst), units::set_units(20,   "Hz"),    start = start)
  a_min <- acc(list(burst), units::set_units(1200, "1/min"), start = start)

  g_hz  <- plot_time(a_hz)
  g_min <- plot_time(a_min)

  # The dygraph time series should be identical — i.e. the dt offsets
  # were correctly normalized to seconds before adding to the start time.
  expect_equal(g_min$x$data[[1]], g_hz$x$data[[1]])
})
