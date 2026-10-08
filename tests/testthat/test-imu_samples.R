test_that("Each sample gets a row with its burst, time and frequency", {
  a <- acc(
    list(cbind(X = 1:3, Y = 4:6), cbind(X = 7:8, Y = 9:10)),
    frequency = units::set_units(c(2, 10), "Hz"),
    start = .as.POSIXct(c(0, 100))
  )

  s <- imu_samples(a)

  expect_s3_class(s, "data.frame")
  expect_named(s, c("burst", "time", "frequency", "X", "Y"))
  expect_equal(s$burst, c(1L, 1L, 1L, 2L, 2L))
  expect_equal(s$time, .as.POSIXct(c(0, 0.5, 1, 100, 100.1)))
  expect_equal(s$frequency, units::set_units(c(2, 2, 2, 10, 10), "Hz"))
  expect_equal(s$X, c(1, 2, 3, 7, 8))
  expect_equal(s$Y, c(4, 5, 6, 9, 10))
})

test_that("Missing bursts are skipped but burst positions refer to x", {
  a <- c(acc_example()[1], acc(list(NULL), units::set_units(NA, "Hz")), acc_example()[2])

  s <- imu_samples(a)

  expect_equal(unique(s$burst), c(1L, 3L))
  expect_equal(nrow(s), sum(n_samples(a), na.rm = TRUE))
})

test_that("Axes are ordered X, Y, Z and missing axes are filled with NA", {
  a <- c(
    acc(list(cbind(Z = 1:2, X = 3:4)), 1, .as.POSIXct(0)),
    acc(list(cbind(Y = 5:6)), 1, .as.POSIXct(0))
  )

  s <- imu_samples(a)

  expect_named(s, c("burst", "time", "frequency", "X", "Y", "Z"))
  expect_equal(s$X, c(3, 4, NA, NA))
  expect_equal(s$Y, c(NA, NA, 5, 6))
})

test_that("Units shared by all bursts are kept", {
  a <- set_imu_units(acc_example(), "standard_free_fall")

  s <- imu_samples(a)

  expect_s3_class(s$X, "units")
  expect_equal(units(s$X), units(units::set_units(1, "standard_free_fall")))
  expect_equal(
    as.numeric(s$X[s$burst == 1]),
    as.numeric(bursts(a)[[1]][, "X"])
  )
})

test_that("Single-axis bursts with units keep their axis names", {
  a <- set_imu_units(
    c(
      acc(list(cbind(Y = 1:2)), 1, .as.POSIXct(0)),
      acc(list(cbind(X = 3:4)), 1, .as.POSIXct(10))
    ),
    "m/s^2"
  )

  s <- imu_samples(a)

  expect_named(s, c("burst", "time", "frequency", "X", "Y"))
  expect_equal(as.numeric(s$X), c(NA, NA, 3, 4))
  expect_equal(as.numeric(s$Y), c(1, 2, NA, NA))
})

test_that("Bursts with different units give an error", {
  a1 <- set_imu_units(acc_example()[1], "standard_free_fall")
  a2 <- set_imu_units(acc_example()[2], "m/s^2")

  expect_error(imu_samples(c(a1, a2)), "must have the same units")

  # Bursts with units mixed with unitless bursts
  expect_error(
    imu_samples(c(a1, drop_imu_units(a2))),
    "must have the same units"
  )
})

test_that("Row names of burst matrices are not kept", {
  m <- cbind(X = 1:2)
  rownames(m) <- c("a", "b")

  s <- imu_samples(acc(list(m), 1, .as.POSIXct(0)))

  expect_equal(rownames(s), c("1", "2"))
})

test_that("Bursts without a start time or frequency get missing times", {
  m <- mag(list(cbind(X = 1:3)), 1)
  s <- imu_samples(m)

  expect_true(all(is.na(s$time)))
  expect_equal(s$X, c(1, 2, 3))

  freqs(m) <- NA
  starts(m) <- 1

  s <- imu_samples(m)

  expect_true(all(is.na(s$frequency)))
  expect_true(all(is.na(s$time)))
})

test_that("Empty input gives a data frame with no rows", {
  s <- imu_samples(acc())

  expect_s3_class(s, "data.frame")
  expect_equal(nrow(s), 0)
  expect_named(s, c("burst", "time", "frequency"))
})

test_that("x must be an IMU vector", {
  expect_error(imu_samples(1:3), "must be an IMU vector")
})
