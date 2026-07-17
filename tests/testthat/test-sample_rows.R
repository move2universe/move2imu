skip_if_not_installed("move2")

test_that("acc_sample_rows flags rows with raw acc data (compact format)", {
  # For compact-format data (one row per burst), every row that contributes
  # data also stores a non-NA burst in the as_acc() output
  alb <- albatrosses()
  expect_identical(acc_sample_rows(alb), !is.na(as_acc(alb)))
})

test_that("acc_sample_rows flags rows with raw acc data (expanded format)", {
  gul <- gulls()

  h <- acc_sample_rows(gul)

  expect_identical(h, !is.na(gul$acceleration_raw_x))
  expect_equal(sum(h), sum(n_samples(as_acc(gul, drop = TRUE))))
})

test_that("acc_sample_rows returns a logical vector parallel to nrow(x)", {
  alb <- albatrosses()

  h <- acc_sample_rows(alb)

  expect_type(h, "logical")
  expect_length(h, nrow(alb))
  expect_false(anyNA(h))
})

test_that("acc_sample_rows respects an explicit colset", {
  gul <- gulls()

  gul$acc_x <- gul$acceleration_raw_x
  gul$acc_y <- gul$acceleration_raw_y
  gul$acc_z <- gul$acceleration_raw_z

  h_default <- acc_sample_rows(gul)

  gul$acceleration_raw_x <- NULL
  gul$acceleration_raw_y <- NULL
  gul$acceleration_raw_z <- NULL

  h_explicit <- acc_sample_rows(
    gul,
    colset = imu_colset(x = "acc_x", y = "acc_y", z = "acc_z")
  )

  expect_identical(h_default, h_explicit)
})

test_that("*_sample_rows() returns all-FALSE when no active colset is detected", {
  alb <- albatrosses()

  h_mag <- mag_sample_rows(alb)
  h_gyro <- gyro_sample_rows(alb)

  expect_length(h_mag, nrow(alb))
  expect_length(h_gyro, nrow(alb))
  expect_false(any(h_mag))
  expect_false(any(h_gyro))
})

test_that("acc_sample_rows returns TRUE for rows where multiple colsets overlap", {
  gul <- gulls()
  gul$acceleration_x <- gul$acceleration_raw_x
  gul$acceleration_y <- gul$acceleration_raw_y
  gul$acceleration_z <- gul$acceleration_raw_z

  expect_error(suppressWarnings(as_acc(gul)), "multiple sources")

  h <- acc_sample_rows(gul)

  expect_no_warning(acc_sample_rows(gul))
  expect_length(h, nrow(gul))
  expect_identical(h, !is.na(gul$acceleration_raw_x))
})

test_that("acc_sample_rows returns the union when colsets cover disjoint rows", {
  # Partition the acc data in gulls() into two disjoint colsets.
  # acc_sample_rows() should identify TRUE when either colset contains acc data
  gul <- gulls()

  acc_rows <- which(!is.na(gul$acceleration_raw_x))
  to_xyz <- acc_rows[seq_along(acc_rows) %% 2 == 0]

  gul$acceleration_x <- NA_real_
  gul$acceleration_y <- NA_real_
  gul$acceleration_z <- NA_real_

  gul$acceleration_x[to_xyz] <- gul$acceleration_raw_x[to_xyz]
  gul$acceleration_y[to_xyz] <- gul$acceleration_raw_y[to_xyz]
  gul$acceleration_z[to_xyz] <- gul$acceleration_raw_z[to_xyz]

  gul$acceleration_raw_x[to_xyz] <- NA_real_
  gul$acceleration_raw_y[to_xyz] <- NA_real_
  gul$acceleration_raw_z[to_xyz] <- NA_real_

  h <- acc_sample_rows(gul)
  expected <- !is.na(gul$acceleration_raw_x) | !is.na(gul$acceleration_x)

  expect_identical(h, expected)
  expect_equal(sum(h), length(acc_rows))
})

test_that("acc_sample_rows handles a zero-row input", {
  h <- acc_sample_rows(data.frame())
  expect_type(h, "logical")
  expect_length(h, 0)
})
