# Most `gyro` behavior is inherited from `imu` and is exercised by
# the `test-acc.R` suite. These tests target only the gyro-specific dispatch
# and class layer: the concrete `gyro` subclass, its S3 methods, and the
# same-sensor / cross-sensor combination rules.

test_that("gyro() produces an empty gyro vector with the expected class chain", {
  g <- gyro()

  expect_s3_class(g, c("gyro", "imu", "vctrs_rcrd", "vctrs_vctr"), exact = TRUE)
  expect_length(g, 0)
})

test_that("gyro() stamps the gyro_list subclass onto the bursts field", {
  g <- gyro(
    list(cbind(X = 1:3, Y = 1:3, Z = 1:3)),
    units::set_units(20, "Hz")
  )

  expect_s3_class(
    vctrs::field(g, "bursts"),
    c("gyro_list", "burst_list", "vctrs_list_of", "vctrs_vctr", "list"),
    exact = TRUE
  )
})

test_that("is_gyro(), is_acc(), and is_mag() don't cross-match", {
  expect_true(is_gyro(gyro()))
  expect_false(is_gyro(acc()))
  expect_false(is_gyro(mag()))

  expect_false(is_acc(gyro()))
  expect_false(is_mag(gyro()))

  expect_false(is_gyro(1:3))
  expect_false(is_gyro(NA))
})

test_that("c(gyro, gyro) preserves the gyro subclass and stays in Hz", {
  # A non-Hz frequency is normalized to Hz at construction (1200 min^-1 = 20 Hz),
  # so combining always yields a single Hz-valued frequency.
  g1 <- gyro(list(cbind(X = 1:5)), units::set_units(20, "Hz"))
  g2 <- gyro(list(cbind(X = 6:10)), units::set_units(1200, "min^-1"))

  r <- c(g1, g2)

  expect_s3_class(r, "gyro")
  expect_length(r, 2)
  expect_identical(units::deparse_unit(freqs(r)), "Hz")
  expect_equal(as.numeric(freqs(r)), c(20, 20))
})

test_that("c(acc, gyro) and c(mag, gyro) error — cross-sensor combination rejected", {
  a <- acc(list(cbind(X = 1:5)), units::set_units(20, "Hz"))
  m <- mag(list(cbind(X = 1:5)), units::set_units(20, "Hz"))
  g <- gyro(list(cbind(X = 1:5)), units::set_units(20, "Hz"))

  expect_error(c(a, g))
  expect_error(c(g, a))
  expect_error(c(m, g))
  expect_error(c(g, m))
})

test_that("gyro() enforces X/Y/Z axis names via burst_list validation", {
  expect_error(
    gyro(list(cbind(A = 1:3, B = 1:3))),
    "X.*Y.*Z"
  )
})

test_that("gyro() recycles frequency and start to match burst length", {
  g <- gyro(
    list(cbind(X = 1:3), cbind(X = 1:3)),
    frequency = units::set_units(20, "Hz"),
    start = as.POSIXct("2024-01-01", tz = "UTC")
  )

  expect_length(freqs(g), 2)
  expect_length(starts(g), 2)
})

test_that("gyro() harmonizes metadata to NA when bursts are missing", {
  g <- gyro(
    list(cbind(X = 1:3), NULL),
    frequency = units::set_units(c(20, 20), "Hz"),
    start = as.POSIXct(c("2024-01-01", "2024-01-02"), tz = "UTC")
  )

  expect_equal(as.numeric(freqs(g)), c(20, NA))
  expect_true(is.na(starts(g)[2]))
})
