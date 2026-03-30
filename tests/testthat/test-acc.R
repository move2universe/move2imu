test_that("create zero length", {
  expect_identical(acc(), new_acc())
  expect_length(new_acc(), 0)
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
