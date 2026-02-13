test_that("Can combine adjacent bursts into single burst", {
  d <- albatrosses()
  
  # Force same ID for a simpler expected merge output for this test
  d[[move2::mt_track_id_column(d)]] <- "tmp"
  
  # Simulate bursts that start at the end point of the previous burst
  move2::mt_time(d) <- seq(
    min(move2::mt_time(d)), 
    by = "12 s",
    length.out = nrow(d)
  )
  
  a <- as_acc(d, merge_continuous = FALSE)
  a2 <- merge_continuous_acc(a)
  
  expect_true(is_acc(a2))
  expect_length(a2, 9)
  
  # Split unmerged into acc groups based on whether the start timestamp plus
  # the burst duration is equal to the next start timestamp (these are records
  # that should have been merged in a2)
  acc_grps <- split(
    a, 
    cumsum(c(TRUE, diff(starts(a)) != as.numeric(burst_dur(a)[-1])))
  )
  
  expect_length(acc_grps, length(a2))
  
  # All start timestamps after merging should correspond to the start timestamp
  # of the first entry in each grouped acc from above
  expect_identical(
    as.POSIXct(
      unname(unlist(purrr::map(acc_grps, function(x) starts(x[1])))), 
      "UTC"
    ),
    starts(a2)
  )
  # Merged bursts should match bursts formed by rbind-ing the grouped bursts
  expect_identical(
    bursts(a2), 
    purrr::map(acc_grps, function(x) do.call(rbind, bursts(x))),
    ignore_attr = TRUE
  )
})

test_that("Do not combine bursts with different axes", {
  t <- data.frame(
    id = 1,
    acceleration_axes = c("XYZ", "XYZ", "XY", "XYZ"),
    acceleration_sampling_frequency_per_axis = 10,
    accelerations_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " "),
      paste0(rep(11:15, each = 2), collapse = " "),
      paste0(rep(16:20, each = 3), collapse = " ")
    ),
    timestamp = as.POSIXct(c(1, 1.5, 2, 2.5), "UTC"),
    x = 1,
    y = 1
  )
  
  m <- move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  a <- as_acc(m)
  
  expect_length(a, 3)
  expect_identical(
    purrr::map(bursts(a), colnames),
    list(c("X", "Y", "Z"), c("X", "Y"), c("X", "Y", "Z"))
  )
  expect_identical(burst_n(a), as.integer(c(10, 5, 5)))
})

test_that("Do not combine bursts with different frequencies", {
  t <- data.frame(
    id = 1,
    acceleration_axes = "XYZ",
    acceleration_sampling_frequency_per_axis = c(10, 10, 20, 10),
    accelerations_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " "),
      paste0(rep(11:20, each = 3), collapse = " "),
      paste0(rep(21:25, each = 3), collapse = " ")
    ),
    timestamp = as.POSIXct(c(1, 1.5, 2, 2.5), "UTC"),
    x = 1,
    y = 1
  )
  
  m <- move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  a <- as_acc(m)
  
  expect_length(a, 3)
  expect_identical(as.numeric(freqs(a)), c(10, 20, 10))
  expect_identical(as.numeric(burst_dur(a)), c(1, 0.5, 0.5))
})

test_that("Do not combine bursts with different IDs", {
  t <- data.frame(
    id = c(1, 1, 1, 2),
    acceleration_axes = "XYZ",
    acceleration_sampling_frequency_per_axis = 10,
    accelerations_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " "),
      paste0(rep(11:15, each = 3), collapse = " "),
      paste0(rep(16:20, each = 3), collapse = " ")
    ),
    timestamp = as.POSIXct(c(1, 1.5, 2, 2.5), "UTC"),
    x = 1,
    y = 1
  )
  
  m <- move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  a <- as_acc(m)
  
  expect_length(a, 2)
  expect_identical(acc_id(a), c("1", "2"))
  expect_identical(burst_n(a), as.integer(c(15, 5)))
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
    purrr::map_int(bursts(split), nrow),
    c(rep(10, 6), rep(20, 2))
  )
  expect_equal(
    do.call(rbind, bursts(split)[1:6]),
    bursts(a)[[1]]
  )
  expect_equal(
    do.call(rbind, bursts(split)[7:8]),
    bursts(a)[[2]]
  )
  expect_equal(
    freqs(split),
    units::set_units(c(rep(20, 6), rep(40, 2)), "Hz")
  )
  expect_identical(
    starts(a)[1] + cumsum(c(0, rep(interval, 5))),
    starts(split)[1:6]
  )
  expect_identical(
    starts(a)[2] + cumsum(c(0, interval)),
    starts(split)[7:8]
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
