# Most `mag` behavior is inherited from `imu` and is exercised by the
# `test-acc.R` suite. These tests target only the mag-specific dispatch and
# class layer: the concrete `mag` subclass, its S3 methods, and the
# same-class / cross-class combination rules.

test_that("mag() produces an empty mag vector with the expected class chain", {
  m <- mag()

  expect_s3_class(m, c("mag", "imu", "vctrs_rcrd", "vctrs_vctr"), exact = TRUE)
  expect_length(m, 0)
})

test_that("mag() stamps the mag_list subclass onto the bursts field", {
  m <- mag(
    list(cbind(X = 1:3, Y = 1:3, Z = 1:3)),
    units::set_units(20, "Hz")
  )

  expect_s3_class(
    vctrs::field(m, "bursts"),
    c("mag_list", "burst_list", "vctrs_list_of", "vctrs_vctr", "list"),
    exact = TRUE
  )
})

test_that("is_mag() and is_acc() don't cross-match", {
  expect_true(is_mag(mag()))
  expect_false(is_mag(acc()))

  expect_true(is_acc(acc()))
  expect_false(is_acc(mag()))

  expect_false(is_mag(1:3))
  expect_false(is_mag(NA))
})

test_that("c(mag, mag) preserves the mag subclass and stays in Hz", {
  # A non-Hz frequency is normalized to Hz at construction (1200 min^-1 = 20 Hz),
  # so combining always yields a single Hz-valued frequency.
  m1 <- mag(list(cbind(X = 1:5)), units::set_units(20, "Hz"))
  m2 <- mag(list(cbind(X = 6:10)), units::set_units(1200, "min^-1"))

  r <- c(m1, m2)

  expect_s3_class(r, "mag")
  expect_length(r, 2)
  expect_identical(units::deparse_unit(freqs(r)), "Hz")
  expect_equal(as.numeric(freqs(r)), c(20, 20))
})

test_that("c(acc, mag) errors — cross-sensor combination is rejected", {
  a <- acc(list(cbind(X = 1:5)), units::set_units(20, "Hz"))
  m <- mag(list(cbind(X = 1:5)), units::set_units(20, "Hz"))

  expect_error(c(a, m))
  expect_error(c(m, a))
})

test_that("mag() enforces X/Y/Z axis names via burst_list validation", {
  expect_error(
    mag(list(cbind(A = 1:3, B = 1:3))),
    "X.*Y.*Z"
  )
})

test_that("mag() recycles frequency and start to match burst length", {
  m <- mag(
    list(cbind(X = 1:3), cbind(X = 1:3)),
    frequency = units::set_units(20, "Hz"),
    start = as.POSIXct("2024-01-01", tz = "UTC")
  )

  expect_length(freqs(m), 2)
  expect_length(starts(m), 2)
})

test_that("mag() harmonizes metadata to NA when bursts are missing", {
  m <- mag(
    list(cbind(X = 1:3), NULL),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC")
  )

  expect_equal(as.numeric(freqs(m)), c(20, NA))
  expect_true(is.na(starts(m)[2]))
})
