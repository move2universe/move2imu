# Single-burst acc vector sampled at 1 Hz, so that a window in seconds equals
# the same number of samples
acc_1hz <- function(...) {
  acc(list(cbind(...)), units::set_units(1, "Hz"), .as.POSIXct(0))
}

test_that("Axes are combined as VeDBA and ODBA, in the units of the input", {
  # Burst means are (1, 2, 1), so the dynamic components are (3, 4, 0) and
  # (-3, -4, 0). VeDBA = sqrt(3^2 + 4^2) = 5 and ODBA = 3 + 4 = 7.
  a <- set_imu_units(acc_1hz(X = c(4, -2), Y = c(6, -2), Z = c(1, 1)), "m/s^2")

  expect_equal(vedba(a, window = "burst"), units::set_units(5, "m/s^2"))
  expect_equal(odba(a, window = "burst"), units::set_units(7, "m/s^2"))

  # Unitless input gives unitless output
  expect_equal(vedba(drop_imu_units(a), window = "burst"), 5)
})

test_that("Running mean is centered and excludes burst edges", {
  # With a 3 sample window, the static estimates for samples 2 to 7 are
  # (1, 3, 3, 3, 4, 3), so the dynamic components are (2, -3, 3, 0, -4, 6).
  # Samples 1 and 8 have no full window and are excluded.
  a <- acc_1hz(X = c(0, 3, 0, 6, 3, 0, 9, 0))

  # Mean of |dynamic| = (2 + 3 + 3 + 0 + 4 + 6) / 6 = 3. A trailing window
  # would give 2.5 instead.
  expect_equal(vedba(a, window = 3), 3)
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
  expect_equal(samples_in_window(0.5, 20), 11L)
  expect_equal(samples_in_window(0.45, 20), 9L)
  expect_equal(samples_in_window(0.3, 20), 7L)
  expect_equal(samples_in_window(3, 5), 15L)
  expect_equal(samples_in_window(1, c(10, 25)), c(11L, 25L))
  expect_true(is.na(samples_in_window(1, NA_real_)))
})

test_that("Bursts too short for the window return NA with a warning", {
  # Bursts of 30 and 20 samples at 20 Hz
  a <- set_imu_units(acc_example(), "standard_free_fall")

  # 1.2 s = 25 samples, longer than the second burst only
  expect_warning(v <- vedba(a, window = 1.2), "1 burst too short")
  expect_false(is.na(v[1]))
  expect_true(is.na(v[2]))

  # 0.05 s = 1 sample, too few for a running mean
  expect_warning(v <- vedba(a, window = 0.05), "2 bursts too short")
  expect_true(all(is.na(v)))

  # A single sample is too few for the burst mean
  single <- acc(
    acc_burst_example(1, 2, 3),
    frequency = units::set_units(10, "Hz"),
    start = .as.POSIXct(1)
  )

  expect_warning(v <- vedba(single, window = "burst"), "1 burst too short")
  expect_true(is.na(v))
})

test_that("Inputs are validated", {
  a <- set_imu_units(acc_example(), "standard_free_fall")

  expect_error(vedba(a))
  expect_error(vedba(a, window = "foo"), "duration or")
  expect_error(vedba(a, window = c(1, 2)), "single duration")
  expect_error(vedba(a, window = NA), "single duration")
  expect_error(vedba(a, window = -1), "greater than 0")
  expect_error(vedba(a, window = 0), "greater than 0")

  m <- mag(list(cbind(X = 1:5, Y = 1:5, Z = 1:5)), units::set_units(1, "Hz"))
  expect_error(vedba(m, window = "burst"), "must be an")
  expect_error(vedba(1:3, window = "burst"), "must be an")
})

test_that("Missing bursts give NA", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])

  expect_equal(is.na(vedba(a_na, window = 0.5)), c(FALSE, TRUE, FALSE))

  all_na <- acc(list(NULL, NULL), units::set_units(c(NA, NA), "Hz"))
  expect_equal(vedba(all_na, window = "burst"), c(NA_real_, NA_real_))

  expect_identical(vedba(acc(), window = "burst"), numeric(0))
})

test_that("NA results keep the units of the input", {
  a <- set_imu_units(acc_example(), "m/s^2")

  # A missing burst
  v <- vedba(c(acc(list(NULL), units::set_units(NA, "Hz")), a), window = 0.5)
  expect_s3_class(v, "units")
  expect_true(is.na(v[1]))

  # Every burst too short for the window
  expect_warning(v <- vedba(a, window = 3), "2 bursts too short")
  expect_equal(v, units::set_units(c(NA_real_, NA_real_), "m/s^2"))
})

test_that("Bursts in different units are converted to the first burst's units", {
  unitless <- as.numeric(vedba(acc_example(), window = "burst"))

  a1 <- set_imu_units(acc_example()[1], "m/s^2")
  a2 <- set_imu_units(acc_example()[2], "standard_free_fall")

  expect_equal(
    vedba(c(a1, a2), window = "burst"),
    units::set_units(c(unitless[1], unitless[2] * 9.80665), "m/s^2")
  )
  expect_equal(
    vedba(c(a2, a1), window = "burst"),
    units::set_units(
      c(unitless[2], unitless[1] / 9.80665),
      "standard_free_fall"
    )
  )
})

test_that("Bursts with and without units can't be combined", {
  a <- c(acc_example()[1], set_imu_units(acc_example()[2], "m/s^2"))
  expect_error(vedba(a, window = "burst"), "Can't combine")
})

test_that("static_acc() and dynamic_acc() return per-sample components", {
  # Same burst as the running mean test above
  a <- acc_1hz(X = c(0, 3, 0, 6, 3, 0, 9, 0))

  s <- static_acc(a, window = 3)
  d <- dynamic_acc(a, window = 3)

  expect_s3_class(s, "acc")
  expect_s3_class(d, "acc")
  expect_equal(starts(s), starts(a))
  expect_equal(freqs(d), freqs(a))

  expect_equal(bursts(s)[[1]][, "X"], c(NA, 1, 3, 3, 3, 4, 3, NA))
  expect_equal(bursts(d)[[1]][, "X"], c(NA, 2, -3, 3, 0, -4, 6, NA))
})

test_that("Static and dynamic components add up to the input", {
  # Comparing with units also checks that both components keep them
  a <- set_imu_units(acc_example(), "standard_free_fall")

  s <- bursts(static_acc(a, window = "burst"))
  d <- bursts(dynamic_acc(a, window = "burst"))

  expect_equal(s[[1]] + d[[1]], bursts(a)[[1]])
  expect_equal(s[[2]] + d[[2]], bursts(a)[[2]])
})

test_that("vedba() and odba() are per-burst means of dynamic_acc()", {
  a <- set_imu_units(acc_example(), "standard_free_fall")
  d <- bursts(drop_imu_units(dynamic_acc(a, window = 0.5)))

  v <- purrr::map_dbl(d, function(b) mean(sqrt(rowSums(b^2)), na.rm = TRUE))
  o <- purrr::map_dbl(d, function(b) mean(rowSums(abs(b)), na.rm = TRUE))

  expect_equal(as.numeric(vedba(a, window = 0.5)), v)
  expect_equal(as.numeric(odba(a, window = 0.5)), o)
})

test_that("Components are NA for bursts too short for the window", {
  # Bursts of 30 and 20 samples at 20 Hz; 1.2 s = 25 samples
  a <- set_imu_units(acc_example(), "standard_free_fall")

  expect_warning(d <- dynamic_acc(a, window = 1.2), "1 burst too short")

  # The short burst keeps its shape and start time, but all values are NA
  expect_equal(n_samples(d), n_samples(a))
  expect_equal(starts(d), starts(a))
  expect_false(all(is.na(bursts(d)[[1]])))
  expect_true(all(is.na(bursts(d)[[2]])))
})

test_that("Components pass through missing and empty elements", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])

  expect_equal(is.na(static_acc(a_na, window = 0.5)), c(FALSE, TRUE, FALSE))
  expect_length(dynamic_acc(acc(), window = 1), 0)
})
