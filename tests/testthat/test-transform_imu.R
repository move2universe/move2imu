test_that("transform_imu() returns an acc object", {
  a <- acc_example()
  result <- transform_imu(a, acc_calibration(offset = 2048, slope = 0.001))
  expect_true(is_acc(result))
  expect_length(result, length(a))
  expect_true(inherits(bursts(a), "acc_list"))
})

test_that("transform_imu() applies correct calibration per burst", {
  a <- acc_example()
  tf <- acc_calibration(manufacturer = "eobs", tag_id = c(1000, 4000))
  result <- transform_imu(a, tf)

  sp1 <- eobs_specs(1000)
  sp2 <- eobs_specs(4000)

  manual_1 <- transform_burst(
    bursts(a)[[1]],
    acc_calibration(
      offset = sp1$offset,
      slope = sp1$slope,
      orientation_x = sp1$orientation_x,
      orientation_y = sp1$orientation_y,
      orientation_z = sp1$orientation_z
    )[1]
  )

  manual_2 <- transform_burst(
    bursts(a)[[2]],
    acc_calibration(
      offset = sp2$offset,
      slope = sp2$slope,
      orientation_x = sp2$orientation_x,
      orientation_y = sp2$orientation_y,
      orientation_z = sp2$orientation_z
    )[1]
  )

  expect_identical(bursts(result)[[1]], manual_1)
  expect_identical(bursts(result)[[2]], manual_2)
})

test_that("transform_imu() recycles length-1 calibration", {
  a <- acc_example()
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  expect_length(tf, 1)
  result <- transform_imu(a, tf)
  expect_true(is_acc(result))
  expect_true(inherits(bursts(result)[[1]], "units"))
  expect_true(inherits(bursts(result)[[2]], "units"))
})

test_that("transform_imu() errors on non-imu_calibration input", {
  a <- acc_example()
  expect_error(transform_imu(a, "not a calibration"), "imu_calibration")
  expect_error(transform_imu(a, list(1, 2)), "imu_calibration")
})

test_that("transform_imu() errors when calibration class mismatches sensor", {
  a <- acc_example()
  # Construct a calibration tagged as a different sensor type to exercise the
  # sensor/calibration mismatch branch without depending on a mag constructor.
  fake_mag_cal <- vctrs::new_rcrd(list(x = 1), class = c("mag_calibration", "imu_calibration"))
  expect_error(
    transform_imu(a, fake_mag_cal),
    "Cannot apply.*mag_calibration.*acc"
  )
})

test_that("transform_imu() errors on incompatible calibration length", {
  a <- acc_example()
  tf <- acc_calibration(offset = c(1, 2, 3), slope = 0.001)
  expect_error(transform_imu(a, tf))
})

test_that("transform_imu() preserves NA bursts", {
  a <- acc_example()
  a_with_na <- c(a, acc(list(NULL), units::set_units(NA, "Hz")))
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  result <- transform_imu(a_with_na, tf)
  expect_length(result, 3)
  expect_true(inherits(bursts(result)[[1]], "units"))
  expect_true(inherits(bursts(result)[[2]], "units"))
  expect_true(is.na(result[3]))
})

test_that("transform_imu() warns on already-calibrated data", {
  a <- acc_example()
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  calibrated <- transform_imu(a, tf)
  expect_warning(transform_imu(calibrated[1], tf), "pre-existing units")
})

test_that("transform_imu() warns on axes missing calibration params", {
  b <- cbind(X = as.double(1:5), Y = as.double(1:5), Z = as.double(1:5))
  a <- acc(bursts = rep(list(b), 5), frequency = units::set_units(rep(20, 5), "Hz"))

  expect_warning(
    result <- transform_imu(a, acc_calibration(offset_x = 2048, slope_x = 0.001)),
    "introduced NA values"
  )
})

test_that("transform_imu() warns on missing params even when input has NAs", {
  # Pre-existing NAs in the raw values must not mask axes that the calibration
  # has no parameters for.
  b <- cbind(X = as.double(1:5), Y = as.double(1:5), Z = as.double(1:5))
  b[1, "X"] <- NA_real_

  a <- acc(bursts = list(b), frequency = units::set_units(20, "Hz"))

  expect_warning(
    result <- transform_imu(a, acc_calibration(offset_x = 2048, slope_x = 0.001)),
    "introduced NA values"
  )
  expect_true(all(is.na(bursts(result)[[1]][, c("Y", "Z")])))
})

test_that("transform_imu() does not warn when NAs come only from the input", {
  b <- cbind(X = as.double(1:5), Y = as.double(1:5), Z = as.double(1:5))
  b[1, "Z"] <- NA_real_

  a <- acc(bursts = list(b), frequency = units::set_units(20, "Hz"))

  expect_no_warning(
    result <- transform_imu(a, acc_calibration(offset = 2048, slope = 0.001))
  )
  expect_identical(sum(is.na(bursts(result)[[1]])), 1L)
})

test_that("transform_imu() handles bursts with differing axes under one calibration", {
  bxyz <- cbind(X = as.double(1:5), Y = as.double(1:5), Z = as.double(1:5))
  bxy <- cbind(X = as.double(1:5), Y = as.double(1:5))

  a <- acc(bursts = list(bxyz, bxy, bxyz), frequency = units::set_units(rep(20, 3), "Hz"))
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  result <- transform_imu(a, cal)

  expect_identical(colnames(bursts(result)[[2]]), c("X", "Y"))
  expect_identical(bursts(result)[[2]], transform_burst(bxy, cal[1]))
  expect_identical(bursts(result)[[3]], transform_burst(bxyz, cal[1]))
})

test_that("transform_imu() maps grouped calibrations back to the right elements", {
  b <- cbind(X = as.double(1:5), Y = as.double(1:5), Z = as.double(1:5))
  a <- acc(bursts = rep(list(b), 3), frequency = units::set_units(rep(20, 3), "Hz"))

  # Elements 1 and 3 share a calibration; element 2 differs
  cal <- c(
    acc_calibration(offset = 2048, slope = 0.001),
    acc_calibration(offset = 100, slope = 0.5),
    acc_calibration(offset = 2048, slope = 0.001)
  )
  result <- transform_imu(a, cal)

  expect_identical(bursts(result)[[1]], transform_burst(b, cal[1]))
  expect_identical(bursts(result)[[2]], transform_burst(b, cal[2]))
  expect_identical(bursts(result)[[3]], transform_burst(b, cal[3]))
})

test_that("transform_imu() units argument passes through", {
  a <- acc_example()
  result_g <- transform_imu(a, acc_calibration(offset = 100, slope = 0.5, units = "standard_free_fall"))
  result_ms2 <- transform_imu(a, acc_calibration(offset = 100, slope = 0.5, units = "m/s^2"))

  expect_equal(
    as.numeric(bursts(result_ms2)[[1]]),
    as.numeric(bursts(result_g)[[1]]) * GRAV_CONST
  )
})

test_that("transform_imu() transforms bursts with no calibration to NA", {
  a <- acc_example()

  cal <- c(
    acc_calibration(offset = 2048, slope = 0.001),
    vec_init(new_acc_calibration())
  )

  expect_true(is.na(cal)[2])

  out <- suppressWarnings(transform_imu(a, cal))
  expect_false(is.na(out)[1])
  expect_true(is.na(out)[2])

  expect_equal(as.numeric(freqs(out)[1]), 20)
  expect_true(is.na(freqs(out)[2]))

  expect_true(inherits(bursts(out)[[1]], "units"))
  expect_null(bursts(out)[[2]])
})

test_that("transform_imu() warns only when data is lost to a missing calibration", {
  a <- acc_example() # both bursts have data

  cal_missing <- c(
    acc_calibration(offset = 2048, slope = 0.001),
    vec_init(new_acc_calibration())
  )

  cal_complete <- acc_calibration(offset = 2048, slope = 0.001)

  expect_warning(transform_imu(a, cal_missing), "Returning NA")
  expect_no_warning(transform_imu(a, cal_complete))

  # Missing calibration shouldn't warn if there aren't any data to begin with
  a[2] <- NA
  expect_no_warning(transform_imu(a, cal_missing))
})
