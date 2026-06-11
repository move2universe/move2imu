test_that("create zero length", {
  expect_length(acc(), 0)
})

test_that("manipulation", {
  x <- acc_example()
  expect_identical(head(x, 1), x[1])
  expect_length(x[1], 1)
  expect_length(x[rep(1, 3)], 3)
})

test_that("logical",{
  expect_false(is_acc(NA))
  expect_false(is_acc(cbind(1,1:3)))
  expect_true(is_acc(acc()))
  expect_true(is_acc(acc_example()))
  expect_true(is_acc(acc(list(NULL), frequency = NA)))
})

test_that("properties are correctly calculated",{
  xa <- acc(
    c(
      acc_burst_example(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
      acc_burst_example(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
    ),
    frequency = units::as_units(c(20, 30), "Hz"),
    start = as.POSIXct(c(1, 2), tz = "UTC")
  )
  
  x <- c(xa, NA)
  expect_true(is_acc(x))
  expect_length(x,3)
  expect_identical(is.na(x),c(F,F,T))
  expect_identical(n_axis(x), c(3L,2L,NA))
  expect_identical(n_samples(x), c(30L,20L,NA))
  expect_false(is_uniform(x))
  expect_true(is_uniform(x[c(1,3)]))
  
  x2 <- vec_c(NA,xa)
  expect_true(is_acc(x2))
  expect_length(x2,3)
  expect_identical(is.na(x2),c(T,F,F))
  expect_identical(n_axis(x2), c(NA,3L,2L))
  expect_identical(n_samples(x2), c(NA,30L,20L))
  expect_false(is_uniform(x2))
  expect_true(is_uniform(x2[c(1,3)]))

})

test_that("is_uniform checks burst units strictly", {
  burst_unit <- function(vals, unit) {
    new_burst_list(
      list(units::set_units(cbind(X = vals), unit, mode = "standard")),
      "acc"
    )
  }

  same_units <- acc(
    c(burst_unit(1:5, "m/s^2"), burst_unit(6:10, "m/s^2")),
    frequency = units::as_units(c(10, 10), "Hz")
  )
  expect_true(is_uniform(same_units))

  mixed_units <- acc(
    c(burst_unit(1:5, "m/s^2"), burst_unit(6:10, "g")),
    frequency = units::as_units(c(10, 10), "Hz")
  )
  expect_false(is_uniform(mixed_units))

  mixed_class <- acc(
    c(burst_unit(1:5, "m/s^2"), acc_burst_example(6:10)),
    frequency = units::as_units(c(10, 10), "Hz")
  )
  expect_false(is_uniform(mixed_class))
})

test_that("constructor replaces metadata with NA when bursts are missing", {
  a <- acc(
    list(NULL),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(0, tz = "UTC")
  )
  expect_true(is.na(a))
  expect_true(vctrs::vec_detect_missing(a))
  expect_true(is.na(freqs(a)))
  expect_true(is.na(starts(a)))
})

test_that("frequency must be in frequency-compatible units", {
  expect_error(
    acc(list(cbind(X = 1:3)), frequency = units::set_units(10, "km")),
    "frequency unit"
  )

  # Valid frequency units should work
  expect_no_error(
    a <- acc(list(cbind(X = 1:3)), frequency = units::set_units(10, "kHz"))
  )
  expect_equal(units::deparse_unit(freqs(a)), "kHz")
  
  # Bare numeric is coerced to Hz
  a <- acc(list(cbind(X = 1:3)), frequency = 10)
  expect_equal(units::deparse_unit(freqs(a)), "Hz")
})

test_that("burst matrix columns must be named X, Y, or Z", {
  # Unnamed columns
  expect_error(
    acc(list(matrix(1:6, ncol = 2)), frequency = 10),
    "named"
  )

  # A mix of named and unnamed bursts should also error
  expect_error(
    acc(list(cbind(X = 1:3, Y = 4:6), matrix(1:6, ncol = 2)), frequency = 10),
    "named"
  )

  # Invalid column names
  expect_error(
    acc(list(cbind(A = 1:3, B = 4:6)), frequency = 10),
    "named"
  )

  # NULL bursts (NA entries) don't need column names
  expect_no_error(
    acc(list(NULL), frequency = NA)
  )
})

test_that("c() handles different frequency units", {
  a1 <- acc(
    acc_burst_example(1:10, 1:10),
    frequency = units::set_units(20, "kHz"),
    start = as.POSIXct(0, tz = "UTC")
  )
  a2 <- acc(
    acc_burst_example(1:10, 1:10),
    frequency = units::set_units(20, "Hz"),
    start = as.POSIXct(1, tz = "UTC")
  )

  a <- c(a1, a2)
  expect_length(a, 2)

  # Both frequencies should be in the same unit
  expect_identical(
    units::deparse_unit(freqs(a)[1]),
    units::deparse_unit(freqs(a)[2])
  )

  # Numeric values should be correctly converted
  expect_equal(as.numeric(freqs(a)[1]), 20)
  expect_equal(as.numeric(freqs(a)[2]), 0.02)

  # NA frequencies are preserved
  a_na <- acc(list(NULL), frequency = units::set_units(NA, "Hz"))
  a <- c(a1, a_na)
  expect_length(a, 2)
  expect_equal(as.numeric(freqs(a)[1]), 20)
  expect_true(is.na(freqs(a)[2]))

  # Combining with empty acc
  a <- c(a1, acc())
  expect_length(a, 1)
  expect_equal(as.numeric(freqs(a)[1]), 20)

  # Three-way combine with mixed units
  a3 <- acc(
    acc_burst_example(1:10, 1:10),
    frequency = units::set_units(0.001, "MHz"),
    start = as.POSIXct(2, tz = "UTC")
  )
  a <- c(a1, a2, a3)
  expect_length(a, 3)
  expect_equal(as.numeric(freqs(a)), c(20, 0.02, 1))
  expect_identical(
    units::deparse_unit(freqs(a)),
    "kHz"
  )
})

test_that("c() preserves non-UTC timezone", {
  tz <- "America/New_York"
  a1 <- acc(
    acc_burst_example(1:10),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(1730610000, tz = tz)
  )
  a2 <- acc(
    acc_burst_example(11:20),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(1730610010, tz = tz)
  )

  a <- c(a1, a2)
  expect_identical(attr(starts(a), "tzone"), tz)
  expect_identical(starts(a), c(starts(a1), starts(a2)))
})

test_that("duration is correctly calculated", {
  a <- acc(
    c(
      acc_burst_example(x = sin(1:30 / 10), y = cos(1:30 / 10), z = 1),
      acc_burst_example(x = sin(1:20 / 10 + 2), y = cos(1:20 / 10 + 3))
    ),
    frequency = units::as_units(c(20, 30), "Hz"),
    start = as.POSIXct(c(1, 2), tz = "UTC")
  )
  
  b <- bursts(a)
  f <- freqs(a)
  
  d <- burst_dur(a)
  
  expect_equal(d[[1]], units::set_units(nrow(b[[1]]) / f[[1]], "s"))
  expect_equal(d[[1]], units::set_units(1.5, "s"))
  expect_equal(d[[2]], units::set_units(nrow(b[[2]]) / f[[2]], "s"))
  expect_equal(d[[2]], units::set_units(2 / 3, "s"))
  
  expect_equal(as.numeric(burst_dur(acc(acc_burst_example(1, 1), 20))), 0.05)
  expect_true(is.na(burst_dur(acc(acc_burst_example(1, 1), NA))))
})

test_that("burst_dur converts non-Hz frequencies to seconds", {
  # 30 samples at 60/min (= 1 Hz)
  a <- acc(
    acc_burst_example(1:30),
    frequency = units::set_units(60, "1/min")
  )
  expect_equal(burst_dur(a), units::set_units(30, "s"))
})

test_that("burst_intervals measures gaps and start-to-start", {
  b <- acc_burst_example(1:20, 1:20, 1:20)
  
  a <- acc(
    rep(b, 3),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
  )

  expect_equal(
    burst_intervals(a, from = "start"), 
    units::set_units(c(NA, 60, 60), "s")
  )

  expect_equal(
    burst_intervals(a), 
    units::set_units(c(NA, 59, 59), "s")
  )
})

test_that("burst_intervals is NA where a neighbouring start is missing", {
  b <- acc_burst_example(1:20, 1:20, 1:20)
  
  a <- acc(
    rep(b, 4),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct(c("2020-01-01 00:00:00", NA, "2020-01-01 00:02:00", "2020-01-01 00:03:00"), tz = "UTC")
  )
  
  expect_equal(
    burst_intervals(a, from = "start"),
    units::set_units(c(NA, NA, NA, 60), "s")
  )
  expect_equal(
    burst_intervals(a, from = "end"),
    units::set_units(c(NA, NA, NA, 59), "s")
  )
})

test_that("burst_intervals does not measure across ID boundaries", {
  b <- acc_burst_example(1:20, 1:20, 1:20)
  
  a <- acc(
    rep(b, 4),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120, 180)
  )

  # First burst of each group has no preceding burst within the group -> NA
  expect_equal(
    burst_intervals(a, from = "start", ids = c("a", "a", "b", "b")),
    units::set_units(c(NA, 60, NA, 60), "s")
  )

  expect_error(
    burst_intervals(a, ids = c("a", "b")),
    "same length"
  )
})

test_that("burst_intervals does not bridge across an interleaved group", {
  b <- acc_burst_example(1:20, 1:20, 1:20)

  a <- acc(
    rep(b, 3),
    frequency = units::as_units(20, "Hz"),
    start = as.POSIXct("2020-01-01", tz = "UTC") + c(0, 60, 120)
  )

  # Intervals are measured against the immediate neighbour in vector order,
  # never the nearest same-group burst, so the third "a" is not measured
  # across the intervening "b".
  expect_equal(
    burst_intervals(a, from = "start", ids = c("a", "b", "a")),
    units::set_units(c(NA, NA, NA), "s")
  )
})

test_that("imu_units are safely extracted", {
  a <- acc_example()

  # Unitless bursts return NA
  expect_identical(imu_units(a), c(NA_character_, NA_character_))

  # Units bursts return unit string
  a_u <- set_imu_units(a, "m/s^2")
  expect_identical(imu_units(a_u), c("m/s^2", "m/s^2"))

  # NA acc elements return NA
  a_na <- c(a_u[1], acc(list(NULL), units::set_units(NA, "Hz")), a_u[2])
  expect_identical(imu_units(a_na), c("m/s^2", NA_character_, "m/s^2"))

  # Mixed units are reported per element
  a_mixed <- c(
    set_imu_units(a[1], "m/s^2"),
    set_imu_units(a[2], "standard_free_fall")
  )
  expect_identical(imu_units(a_mixed), c("m/s^2", "standard_free_fall"))
})
