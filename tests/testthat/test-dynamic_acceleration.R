# Single-burst acc vector sampled at 1 Hz, so that a window in seconds equals
# the same number of samples
acc_1hz <- function(...) {
  acc(list(cbind(...)), units::set_units(1, "Hz"), .as.POSIXct(0))
}

test_that("Axes are combined as VeDBA and ODBA", {
  # Burst means are (1, 2, 1), so the dynamic components are (3, 4, 0) and
  # (-3, -4, 0). VeDBA = sqrt(3^2 + 4^2) = 5 and ODBA = 3 + 4 = 7.
  a <- acc_1hz(X = c(4, -2), Y = c(6, -2), Z = c(1, 1))

  expect_equal(vedba(a, window = "burst"), 5)
  expect_equal(odba(a, window = "burst"), 7)
})

test_that("Output keeps the units of the input", {
  a <- set_imu_units(acc_1hz(X = c(4, -2), Y = c(6, -2)), "m/s^2")

  v <- vedba(a, window = "burst")
  o <- odba(a, window = "burst")

  expect_equal(v, units::set_units(5, "m/s^2"))
  expect_equal(o, units::set_units(7, "m/s^2"))
})

test_that("Running mean is centered and excludes burst edges", {
  # With a 3 sample window, the static estimates for samples 2 to 7 are
  # (1, 3, 3, 3, 4, 3), so the dynamic components are (2, -3, 3, 0, -4, 6).
  # Samples 1 and 8 have no full window and are excluded.
  a <- acc_1hz(X = c(0, 3, 0, 6, 3, 0, 9, 0))

  # Mean of |dynamic| = (2 + 3 + 3 + 0 + 4 + 6) / 6 = 3. A trailing window
  # would give 2.5 instead.
  expect_equal(vedba(a, window = 3), 3)
  expect_equal(odba(a, window = 3), 3)
})

test_that("Missing samples are excluded", {
  a <- acc_1hz(X = c(0, 3, 0, NA, 3, 0, 9, 0))

  # The missing sample leaves samples 3 to 5 without a static estimate, so
  # only samples 2, 6 and 7 remain: (2 + 4 + 6) / 3 = 4
  expect_equal(vedba(a, window = 3), 4)

  # A missing sample makes the burst mean undefined
  expect_true(is.na(vedba(a, window = "burst")))
})

test_that("Window can be given in any time unit or as bare seconds", {
  a <- set_imu_units(acc_example(), "standard_free_fall")

  expected <- vedba(a, window = units::set_units(0.5, "s"))

  expect_equal(vedba(a, window = 0.5), expected)
  expect_equal(vedba(a, window = units::set_units(500, "ms")), expected)
})

test_that("Window is converted to the nearest odd number of samples", {
  expect_equal(dba_window_samples(0.5, 20), 11L)
  expect_equal(dba_window_samples(0.45, 20), 9L)
  expect_equal(dba_window_samples(0.3, 20), 7L)
  expect_equal(dba_window_samples(3, 5), 15L)
  expect_equal(dba_window_samples(1, c(10, 25)), c(11L, 25L))
  expect_true(is.na(dba_window_samples(1, NA_real_)))
})

test_that("Bursts too short for the window return NA with a warning", {
  # Bursts of 30 and 20 samples at 20 Hz
  a <- set_imu_units(acc_example(), "standard_free_fall")

  # 1.2 s = 25 samples, longer than the second burst only
  expect_warning(v <- vedba(a, window = 1.2), "1 burst too short")
  expect_false(is.na(v[1]))
  expect_true(is.na(v[2]))

  # 0.05 s = 1 sample
  expect_warning(v <- vedba(a, window = 0.05), "2 bursts too short")
  expect_true(all(is.na(v)))
})

test_that("Single-sample bursts return NA with a warning", {
  a <- set_imu_units(
    acc(
      acc_burst_example(1, 2, 3),
      frequency = units::set_units(10, "Hz"),
      start = .as.POSIXct(1)
    ),
    "standard_free_fall"
  )

  expect_warning(v <- vedba(a, window = "burst"), "too short")
  expect_true(is.na(v))

  expect_warning(o <- odba(a, window = 1), "too short")
  expect_true(is.na(o))
})

test_that("window must be valid", {
  a <- set_imu_units(acc_example(), "standard_free_fall")

  expect_error(vedba(a), "missing")
  expect_error(odba(a), "missing")
  expect_error(vedba(a, window = "foo"), "duration or")
  expect_error(vedba(a, window = c(1, 2)), "single duration")
  expect_error(vedba(a, window = NA), "single duration")
  expect_error(vedba(a, window = -1), "greater than 0")
  expect_error(vedba(a, window = 0), "greater than 0")
})

test_that("x must be an acc vector", {
  m <- mag(list(cbind(X = 1:5, Y = 1:5, Z = 1:5)), units::set_units(1, "Hz"))

  expect_error(vedba(m, window = "burst"), "must be an")
  expect_error(odba(1:3, window = "burst"), "must be an")
})

test_that("Return empty numeric on empty acc", {
  expect_identical(vedba(acc(), window = "burst"), numeric(0))
  expect_identical(odba(acc(), window = 1), numeric(0))
})

test_that("vedba and odba return NA for NA elements (unitless)", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])

  v <- vedba(a_na, window = "burst")
  o <- odba(a_na, window = 0.5)

  expect_length(v, 3)
  expect_length(o, 3)
  expect_true(is.na(v[2]))
  expect_true(is.na(o[2]))
  expect_false(is.na(v[1]))
  expect_false(is.na(o[1]))
})

test_that("vedba and odba return units NA for NA elements (with units)", {
  a <- set_imu_units(acc_example(), "m/s^2")
  a_na <- c(acc(list(NULL), units::set_units(NA, "Hz")), a)

  v <- vedba(a_na, window = "burst")
  o <- odba(a_na, window = 0.5)

  expect_length(v, 3)
  expect_true(is.na(v[1]))
  expect_true(is.na(o[1]))
  expect_s3_class(v, "units")
  expect_s3_class(o, "units")
})

test_that("All-NA input returns NA", {
  a <- acc(list(NULL, NULL), units::set_units(c(NA, NA), "Hz"))
  expect_equal(vedba(a, window = "burst"), c(NA_real_, NA_real_))
})

test_that("dba uses first available units for output", {
  a1 <- set_imu_units(acc_example()[1], "m/s^2")
  a2 <- set_imu_units(acc_example()[2], "standard_free_fall")

  v1 <- vedba(c(a1, a2), window = "burst")
  o1 <- odba(c(a1, a2), window = "burst")

  expect_s3_class(v1, "units")
  expect_s3_class(o1, "units")
  expect_equal(units(v1)$numerator, "m")
  expect_equal(units(v1)$denominator, c("s", "s"))
  expect_equal(units(o1)$numerator, "m")
  expect_equal(units(o1)$denominator, c("s", "s"))

  v2 <- vedba(c(a2, a1), window = "burst")
  o2 <- odba(c(a2, a1), window = "burst")

  expect_true(inherits(v2, "units"))
  expect_true(inherits(o2, "units"))
  expect_equal(units(v2)$numerator, "standard_free_fall")
  expect_equal(units(o2)$numerator, "standard_free_fall")

  expect_equal(as.numeric(rev(v2) * 9.80665), as.numeric(v1))
  expect_equal(as.numeric(rev(o2) * 9.80665), as.numeric(o1))
})

test_that("vedba and odba error on mixed unitless and units bursts", {
  a <- c(acc_example()[1], set_imu_units(acc_example()[2], "m/s^2"))

  expect_error(vedba(a, window = "burst"), "Can't combine")
  expect_error(odba(a, window = "burst"), "Can't combine")
})
