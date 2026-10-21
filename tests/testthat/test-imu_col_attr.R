test_that("setter records the column and getter returns it", {
  df <- move2_with_imu()
  
  df <- mt_set_acc_column(df, "a")
  df <- mt_set_mag_column(df, "m")
  df <- mt_set_gyro_column(df, "g")
  
  expect_identical(attr(df, "acc_column"), "a")
  expect_identical(attr(df, "mag_column"), "m")
  expect_identical(attr(df, "gyro_column"), "g")
  expect_identical(mt_acc_column(df), "a")
  expect_identical(mt_mag_column(df), "m")
  expect_identical(mt_gyro_column(df), "g")
})

test_that("Value accessor extracts data from correct column", {
  df <- move2_with_imu()
  
  df <- mt_set_acc_column(df, "a")
  df <- mt_set_mag_column(df, "m")
  df <- mt_set_gyro_column(df, "g")
  
  a <- mt_acc(df)
  m <- mt_mag(df)
  g <- mt_gyro(df)
  
  expect_true(is_acc(a))
  expect_identical(a, df$a)
  
  expect_true(is_mag(m))
  expect_identical(m, df$m)
  
  expect_true(is_gyro(g))
  expect_identical(g, df$g)
})

test_that("setter rejects a column of the wrong sensor type", {
  m <- move2_with_imu()

  expect_error(
    mt_set_mag_column(m, "a"),
    class = "move2imu_error_imu_sensor_incompatible"
  )
  expect_error(
    mt_set_acc_column(m, "m"),
    class = "move2imu_error_imu_sensor_incompatible"
  )
  expect_error(
    mt_set_acc_column(m, "id"),
    class = "move2imu_error_imu_invalid"
  )
})

test_that("setter rejects a non-string or non-existent column", {
  m <- move2_with_imu()

  expect_error(mt_set_acc_column(m, 1), "length-1 character")
  expect_error(mt_set_acc_column(m, c("a", "m")), "length-1 character")
  expect_error(
    mt_set_acc_column(m, "foobar"),
    class = "move2imu_error_imu_column_not_found"
  )
})

test_that("getter errors when no attribute is set", {
  m <- move2_with_imu()

  expect_error(
    mt_acc_column(m),
    "No `acc_column` detected.+mt_set_acc_column",
    class = "move2imu_error_imu_column_missing"
  )
  expect_error(
    mt_gyro_column(m),
    "No `gyro_column` detected.+mt_set_gyro_column",
    class = "move2imu_error_imu_column_missing"
  )
})

test_that("getter rejects a malformed column attribute", {
  m <- mt_set_acc_column(move2_with_imu(), "a")

  attr(m, "acc_column") <- 1L
  expect_error(mt_acc_column(m), class = "move2imu_error_imu_column_invalid")
  expect_error(mt_acc(m), class = "move2imu_error_imu_column_invalid")

  attr(m, "acc_column") <- c("a", "a")
  expect_error(mt_acc_column(m), class = "move2imu_error_imu_column_invalid")

  attr(m, "acc_column") <- character(0)
  expect_error(mt_acc_column(m), class = "move2imu_error_imu_column_invalid")

  attr(m, "acc_column") <- list("a")
  expect_error(mt_acc_column(m), class = "move2imu_error_imu_column_invalid")
})

test_that("malformed attribute is rejected for every sensor", {
  m <- move2_with_imu()

  attr(m, "mag_column") <- 1L
  attr(m, "gyro_column") <- c("g", "g")

  expect_error(
    mt_mag_column(m),
    "`mag_column` attribute must be.+character",
    class = "move2imu_error_imu_column_invalid"
  )
  expect_error(
    mt_gyro_column(m),
    "`gyro_column` attribute must be.+character",
    class = "move2imu_error_imu_column_invalid"
  )
})

test_that("getter allows an NA column attribute (consistency with move2)", {
  m <- mt_set_acc_column(move2_with_imu(), "a")
  attr(m, "acc_column") <- NA_character_

  expect_identical(mt_acc_column(m), NA_character_)
  expect_error(mt_acc(m), class = "move2imu_error_imu_column_not_found")
})

test_that("Error when designated column has inconsistencies", {
  m <- mt_set_acc_column(move2_with_imu(), "a")

  m$a <- NULL
  expect_error(mt_acc(m), class = "move2imu_error_imu_column_not_found")

  m$a <- m$m
  expect_error(mt_acc(m), class = "move2imu_error_imu_sensor_incompatible")
})

test_that("accessors reject non-move2 input", {
  df <- data.frame(a = 1:2)
  df$a <- acc_example()

  expect_error(mt_set_acc_column(df, "a"), "must be a <move2> object")
  expect_error(mt_acc_column(df), "must be a <move2> object")
  expect_error(mt_acc(df), "must be a <move2> object")
})
