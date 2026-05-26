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

  manual_1 <- acc_calibration(
    offset = sp1$offset,
    slope = sp1$slope,
    orientation_x = sp1$orientation_x,
    orientation_y = sp1$orientation_y,
    orientation_z = sp1$orientation_z
  )[[1]]

  manual_2 <- acc_calibration(
    offset = sp2$offset,
    slope = sp2$slope,
    orientation_x = sp2$orientation_x,
    orientation_y = sp2$orientation_y,
    orientation_z = sp2$orientation_z
  )[[1]]

  manual_1 <- manual_1(bursts(a)[[1]])
  manual_2 <- manual_2(bursts(a)[[2]])

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
  fake_mag_cal <- new_imu_calibration(list(function(x) x), sensor = "mag")
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
  expect_warning(transform_imu(calibrated[1], tf), "already contain units")
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
