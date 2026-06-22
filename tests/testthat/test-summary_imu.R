test_that("summary returns imu_summary object", {
  s <- summary(acc_example())
  expect_s3_class(s, "imu_summary")
  expect_equal(s$n, 2)
  expect_equal(s$n_na, 0)
})

test_that("summary prints header with burst count and NAs", {
  out <- capture.output(print(summary(c(acc_example(), NA))))
  expect_true(any(grepl("3 acc bursts", out)))
  expect_true(any(grepl("1 NA", out)))
})

test_that("summary prints axis combinations inline", {
  out <- capture.output(print(summary(acc_example())))
  axes_line <- out[grepl("Axes:", out)]
  expect_length(axes_line, 1)
  expect_match(axes_line, "XY")
  expect_match(axes_line, "XYZ")
})

test_that("summary handles empty acc", {
  s <- summary(acc())
  expect_s3_class(s, "imu_summary")
  expect_equal(s$n, 0)
  out <- capture.output(print(s))
  expect_true(any(grepl("0 acc bursts", out)))
})

test_that("summary shows units when present", {
  out <- capture.output(print(summary(units::set_units(acc_example(), "m/s^2"))))
  expect_true(any(grepl("Units:.*m/s\\^2", out)))
})

test_that("summary shows no units when bursts are unitless", {
  out <- capture.output(print(summary(acc_example())))
  expect_true(any(grepl("Units:.*no units", out)))
})

test_that("summary captures intervals", {
  s <- summary(acc_example())
  expect_true(!is.null(s$intervals))
  expect_length(s$intervals, 1)
})

test_that("summary captures frequencies, samples, durations, values", {
  s <- summary(acc_example())
  expect_equal(s$freqs, c(20, 20))
  expect_equal(s$freq_unit, "Hz")
  expect_equal(s$samples, c(30L, 20L))
  expect_equal(s$durations, c(30 / 20, 20 / 20))
  expect_equal(s$dur_unit, "s")
  expect_type(s$values, "double")
  expect_length(s$values, 150)
})

# Single-burst: always-present empty intervals --------------------------------

single_burst_acc <- function() {
  acc(
    list(cbind(X = 1.2, Y = 0.3, Z = 9.8)),
    frequency = units::set_units(20, "Hz"),
    start = as.POSIXct("2026-01-01", tz = "UTC")
  )
}

test_that("single-burst summary stores empty intervals (not NULL)", {
  s <- summary(single_burst_acc())
  expect_identical(s$intervals, numeric(0))
})

test_that("single-burst summary prints 'no data' for intervals", {
  out <- capture.output(print(summary(single_burst_acc())))
  intervals_line <- out[grepl("^Intervals:", out)]
  expect_length(intervals_line, 1)
  expect_match(intervals_line, "no data")
})

# Mixed-unit path -------------------------------------------------------------

mixed_unit_acc <- function() {
  b1 <- cbind(X = c(1, 2, 3))
  units(b1) <- units::as_units("m/s^2")
  b2 <- cbind(X = c(2000, 2010, 2020)) # unitless
  acc(
    list(b1, b2),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(
      c(
        "2026-01-01 00:00:00",
        "2026-01-01 00:00:10"
      ),
      tz = "UTC"
    )
  )
}

test_that("summary lists multiple unit groups in Units footer when mixed", {
  out <- capture.output(print(summary(mixed_unit_acc())))
  units_line <- out[grepl("^Units:", out)]
  expect_length(units_line, 1)
  expect_match(units_line, "m/s\\^2")
  expect_match(units_line, "no units")
})

# format_quantiles helper -----------------------------------------------------

test_that("format_quantiles renders", {
  with_unit <- format_quantiles(c(1, 2, 3, 4, 5), "s")
  expect_match(with_unit, "[ 1 / 2 / 3 / 4 / 5 ]", fixed = TRUE)
  expect_match(with_unit, "[s]", fixed = TRUE)
  expect_match(with_unit, "(min/Q1/med/Q3/max)", fixed = TRUE)

  no_unit <- format_quantiles(c(1, 2, 3, 4, 5))
  expect_match(no_unit, "[ 1 / 2 / 3 / 4 / 5 ]", fixed = TRUE)
  expect_false(grepl("[s]", no_unit, fixed = TRUE))
})

test_that("format_quantiles renders empty input as 'no data'", {
  expect_equal(format_quantiles(numeric(0)), "[ no data ]")
  expect_equal(format_quantiles(numeric(0), "s"), "[ no data ]")
})

# plot.imu_summary smoke tests ------------------------------------------------

with_pdf <- function(expr) {
  pdf(tempfile())
  on.exit(dev.off())
  force(expr)
}

test_that("plot.imu_summary draws without error", {
  s <- summary(acc_example())
  expect_no_error(with_pdf(plot(s)))
})

test_that("plot.imu_summary handles single-burst (empty Interval panel)", {
  s <- summary(single_burst_acc())
  expect_no_error(with_pdf(plot(s)))
})

test_that("plot.imu_summary `panel` selects by name and by integer", {
  s <- summary(acc_example())
  expect_no_error(with_pdf({
    plot(s, panel = "Values")
    plot(s, panel = c("Frequency", "Values"))
    plot(s, panel = 1)
    plot(s, panel = c(1, 5))
  }))
})

test_that("plot.imu_summary errors on unknown panel name", {
  s <- summary(acc_example())
  pdf(tempfile())
  on.exit(dev.off())
  expect_error(plot(s, panel = "NotAPanel"))
})

test_that("plot.imu_summary errors on out-of-range integer `panel`", {
  s <- summary(acc_example())
  pdf(tempfile())
  on.exit(dev.off())
  expect_error(plot(s, panel = 99), "between 1 and")
  expect_error(plot(s, panel = 0), "between 1 and")
})

test_that("plot.imu_summary forwards extra args to hist()", {
  s <- summary(acc_example())
  expect_no_error(with_pdf(plot(s, breaks = 5)))
})

# Other sensor types ---------------------------------------------------------

test_that("summary dispatches and prints correctly for mag and gyro", {
  m <- mag(
    list(cbind(X = 1, Y = 2, Z = 3)),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct("2026-01-01", tz = "UTC")
  )
  s_m <- summary(m)
  expect_s3_class(s_m, "imu_summary")
  expect_equal(s_m$sensor, "mag")
  expect_match(capture.output(print(s_m))[1], "mag bursts")

  g <- gyro(
    list(cbind(X = 1, Y = 2, Z = 3)),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct("2026-01-01", tz = "UTC")
  )
  s_g <- summary(g)
  expect_s3_class(s_g, "imu_summary")
  expect_equal(s_g$sensor, "gyro")
  expect_match(capture.output(print(s_g))[1], "gyro bursts")
})
