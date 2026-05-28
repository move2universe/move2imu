skip_if_not_installed("move2")

test_that("has_acc flags rows with raw acc data (burst format)", {
  # For burst-format data (one row per burst), every row that contributes
  # data also stores a non-NA burst in the as_acc() output
  alb <- albatrosses()
  expect_identical(has_acc(alb), !is.na(as_acc(alb)))
})

test_that("has_acc flags rows with raw acc data (long format)", {
  gul <- gulls()

  h <- has_acc(gul)
  
  expect_identical(h, !is.na(gul$acceleration_raw_x))
  expect_equal(sum(h), sum(n_samples(as_acc(gul, drop = TRUE))))
})

test_that("has_acc returns a logical vector parallel to nrow(x)", {
  alb <- albatrosses()

  h <- has_acc(alb)

  expect_type(h, "logical")
  expect_length(h, nrow(alb))
  expect_false(anyNA(h))
})

test_that("has_acc respects an explicit colset", {
  gul <- gulls()
  
  gul$acc_x <- gul$acceleration_raw_x
  gul$acc_y <- gul$acceleration_raw_y
  gul$acc_z <- gul$acceleration_raw_z
  
  h_default <- has_acc(gul)
  
  gul$acceleration_raw_x <- NULL
  gul$acceleration_raw_y <- NULL
  gul$acceleration_raw_z <- NULL
  
  h_explicit <- has_acc(
    gul, 
    colset = imu_colset(x = "acc_x", y = "acc_y", z = "acc_z")
  )

  expect_identical(h_default, h_explicit)
})

test_that("has_*() returns all-FALSE when no active colset is detected", {
  alb <- albatrosses()

  h_mag <- has_mag(alb)
  h_gyro <- has_gyro(alb)

  expect_length(h_mag, nrow(alb))
  expect_length(h_gyro, nrow(alb))
  expect_false(any(h_mag))
  expect_false(any(h_gyro))
})

test_that("has_acc returns TRUE for rows where multiple colsets overlap", {
  gul <- gulls()
  gul$acceleration_x <- gul$acceleration_raw_x
  gul$acceleration_y <- gul$acceleration_raw_y
  gul$acceleration_z <- gul$acceleration_raw_z

  expect_error(suppressWarnings(as_acc(gul)), "multiple sources")

  h <- has_acc(gul)

  expect_no_warning(has_acc(gul))
  expect_length(h, nrow(gul))
  expect_identical(h, !is.na(gul$acceleration_raw_x))
})

test_that("has_acc returns the union when colsets cover disjoint rows", {
  # Partition the acc data in gulls() into two disjoint colsets.
  # has_acc() should identify TRUE when either colset contains acc data
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

  h <- has_acc(gul)
  expected <- !is.na(gul$acceleration_raw_x) | !is.na(gul$acceleration_x)

  expect_identical(h, expected)
  expect_equal(sum(h), length(acc_rows))
})

test_that("has_acc handles a zero-row input", {
  h <- has_acc(data.frame())
  expect_type(h, "logical")
  expect_length(h, 0)
})
