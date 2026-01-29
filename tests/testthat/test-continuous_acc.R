test_that("Can combine adjacent bursts into single burst", {
  d <- albatrosses()
  
  # Simulate bursts that start at the end point of the previous burst
  move2::mt_time(d) <- seq(
    min(move2::mt_time(d)), 
    by = "12 s",
    length.out = nrow(d)
  )
  
  a <- as_acc(d)
  i <- min(which(!is.na(a)))
  
  ts <- field(a, "start")
  timediff <- ts + units::as_difftime(burst_dur(a))
  
  expect_true(
    all(timediff[-length(timediff)] == field(a, "start")[-1], na.rm = TRUE)
  )
  
  a2 <- merge_continuous_acc(a)
  
  expect_true(is_acc(a2))
  expect_identical(which(is.na(a)), which(is.na(a2)))
  expect_length(a2[!is.na(a2)], 1)
  expect_identical(field(a2[i], "start"), field(a[i], "start"))
  expect_identical(field(a2[i], "frequency"), field(a[i], "frequency"))
  expect_identical(
    field(a2[i], "bursts")[[1]],
    do.call(rbind, field(a[!is.na(a)], "bursts"))
  )
})

# TODO: this is a big of a clunky test, but it covers many possible
# combinations of issues that would prevent elements from being collapsed
# together. This could be refactored and we could build more atomic
# unit tests for these behaviors by building explicit test cases with 
# acc_burst_example()
test_that("Do not combine bursts with different n axes or frequencies", {
  a <- as_acc(albatrosses_messy())
  a2 <- merge_continuous_acc(a)
  
  # Hard-coding the split indices that we should expect from the 
  # test data:
  split_i <- c(1, 4, 7, 11, 12, 13, 31, 33, 44)
  
  expect_true(is_acc(a2))
  expect_length(a2, length(split_i))
  
  expect_identical(field(a2, "start"), field(a, "start")[split_i])
  expect_identical(field(a2, "frequency"), field(a, "frequency")[split_i])
  
  # Manually confirming all the groups we expect. Easiest way to be thorough
  # in this case.
  expect_identical(
    field(a2, "bursts")[[1]],
    do.call(rbind, field(a, "bursts")[1:3])
  )
  expect_identical(
    field(a2, "bursts")[[2]],
    do.call(rbind, field(a, "bursts")[4:6])
  )
  expect_identical(
    field(a2, "bursts")[[3]],
    do.call(rbind, field(a, "bursts")[7:10])
  )
  expect_identical(
    field(a2, "bursts")[[4]],
    field(a, "bursts")[[11]]
  )
  expect_identical(
    field(a2, "bursts")[[5]],
    field(a, "bursts")[[12]]
  )
  expect_identical(
    field(a2, "bursts")[[6]],
    do.call(rbind, field(a, "bursts")[13:30])
  )
  expect_identical(
    field(a2, "bursts")[[7]],
    do.call(rbind, field(a, "bursts")[31:32])
  )
  expect_identical(
    field(a2, "bursts")[[8]],
    do.call(rbind, field(a, "bursts")[33:43])
  )
  expect_identical(
    field(a2, "bursts")[[9]],
    do.call(rbind, field(a, "bursts")[44:45])
  )
})

test_that("Don't combine bursts without start time", {
  a <- acc(
    c(acc_burst_example(x = 1:10), acc_burst_example(x = 1:10)),
    frequency = units::set_units(1, "Hz")
  )
  
  expect_identical(a, merge_continuous_acc(a))
})

test_that("Handle empty acc vectors when binding", {
  expect_identical(merge_continuous_acc(acc()), acc())
  expect_identical(merge_continuous_acc(c(acc(), acc())), acc())
})

test_that("Can split acc at a given interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  interval <- 0.5
  split <- split_continuous_acc(a, interval = interval)
  
  expect_length(split, units::drop_units(sum(ceiling(burst_dur(a) / interval))))
  expect_true(all(units::drop_units(burst_dur(split)) == interval))
  
  expect_equal(
    purrr::map_int(field(split, "bursts"), nrow),
    c(rep(10, 6), rep(20, 2))
  )
  expect_equal(
    do.call(rbind, field(split, "bursts")[1:6]),
    field(a, "bursts")[[1]]
  )
  expect_equal(
    do.call(rbind, field(split, "bursts")[7:8]),
    field(a, "bursts")[[2]]
  )
  expect_equal(
    field(split, "frequency"),
    units::set_units(c(rep(20, 6), rep(40, 2)), "Hz")
  )
  expect_identical(
    field(a, "start")[1] + cumsum(c(0, rep(interval, 5))),
    field(split, "start")[1:6]
  )
  expect_identical(
    field(a, "start")[2] + cumsum(c(0, interval)),
    field(split, "start")[7:8]
  )
})

test_that("Correctly split when burst length not divisible by interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  interval <- 0.7
  split <- split_continuous_acc(a, interval = interval)
  dur <- burst_dur(a)
  
  expect_length(split, units::drop_units(sum(ceiling(burst_dur(a) / interval))))
  
  # Bursts should be split into equal time lengths other than for the last
  # element of each split burst, which will capture whatever burst duration remains
  expect_equal(
    units::drop_units(burst_dur(split)),
    c(
      c(rep(interval, dur[1] %/% interval), dur[1] - (interval * dur[1] %/% interval)),
      c(rep(interval, dur[2] %/% interval), dur[2] - (interval * dur[2] %/% interval))
    )
  )
})

test_that("Can recover split continuous data by merging", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  expect_identical(
    merge_continuous_acc(split_continuous_acc(a, interval = 0.5)),
    a
  )
})

test_that("Long intervals do not modify input acc", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  expect_identical(split_continuous_acc(a, interval = max(burst_dur(a))), a)
})

test_that("Can standardize interval units when splitting", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "kHz"), units::set_units(40, "kHz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )
  
  # Default should be in 1/frq units
  expect_length(split_continuous_acc(a, interval = 0.5), 8)
  expect_identical(
    split_continuous_acc(a, interval = 0.5),
    split_continuous_acc(a, interval = units::set_units(0.5 / 1000, "s"))
  )
})