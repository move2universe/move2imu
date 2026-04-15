d <- albatrosses()

# Force same ID for a simpler expected merge output for this test
d <- move2::mt_set_track_id(d, rep("tmp", nrow(d)))

# Simulate bursts that start at the end point of the previous burst
move2::mt_time(d) <- seq(
  min(move2::mt_time(d)), 
  by = "12 s",
  length.out = nrow(d)
)

test_that("Can combine adjacent bursts into single burst", {
  a <- as_acc(d, merge_continuous = FALSE)
  a2 <- merge_acc(a)
  
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

test_that("Can merge with drop = FALSE", {
  a <- as_acc(d, merge_continuous = TRUE, drop = FALSE)
  
  expect_length(a, nrow(d))
  expect_length(a[!is.na(a)], 9)
  expect_identical(a[!is.na(a)], as_acc(d))
})

test_that("Same merged result if drop = TRUE regardless of NAs", {
  a1 <- as_acc(d, merge_continuous = FALSE)
  a2 <- as_acc(d, merge_continuous = FALSE, drop = FALSE)
  expect_identical(merge_acc(a1), merge_acc(a2))
})

test_that("Can combine adjacent bursts with embedded NA", {
  times <- seq(
    min(move2::mt_time(d)), 
    by = "12 s",
    length.out = nrow(d)
  )
  
  # Simulate an adjacent time gap across a NA value at position 7-8
  times[7:length(times)] <- times[7:length(times)] - 12
  
  # Simulate bursts that start at the end point of the previous burst
  d2 <- d
  move2::mt_time(d2) <- times
  
  a <- as_acc(d2, merge_continuous = FALSE, drop = FALSE)
  a2 <- merge_acc(a)
  
  expect_true(is_acc(a2))
  expect_length(a2, 8)
  
  # Split unmerged into acc groups based on whether the start timestamp plus
  # the burst duration is equal to the next start timestamp (these are records
  # that should have been merged in a2)
  i <- !is.na(a)
  acc_grps <- split(
    a[i], 
    cumsum(c(TRUE, diff(starts(a[i])) != as.numeric(burst_dur(a[i])[-1])))
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

test_that("drop = FALSE places merged bursts at correct indices", {                                                                                                                             
  a <- as_acc(d, merge_continuous = TRUE, drop = FALSE)   
  
  # Start times of merged bursts should match the dropped version                                                                                                                               
  expect_identical(starts(a[!is.na(a)]), starts(as_acc(d)))
  
  # Positions of non-NA entries should be a subset of the original burst positions                                                                                                              
  a_raw <- as_acc(d, merge_continuous = FALSE, drop = FALSE)                                                                                                                                    
  expect_true(all(which(!is.na(a)) %in% which(!is.na(a_raw))))                                                                                                                                  
})

test_that("Non-mergeable bursts ignore merge arg regardless of drop arg", {
  g1 <- as_acc(gulls(), drop = FALSE, acc_cols = acc_raw_xyz_cols())
  g2 <- as_acc(gulls(), merge_continuous = FALSE, drop = FALSE, acc_cols = acc_raw_xyz_cols())
  expect_identical(g1, g2)
})

test_that("Partial merge with drop = FALSE respects ID boundaries", {                                                                                                                           
  # 4 adjacent bursts, same freq, but IDs split at position 3
  a <- acc(
    c(
      acc_burst_example(1:30),
      acc_burst_example(31:60),
      acc_burst_example(61:90),
      acc_burst_example(91:120)
    ),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(c(0, 3, 6, 9), tz = "UTC")
  )

  merged <- merge_acc(a, acc_ids = c("a", "a", "b", "b"), drop = FALSE)
  
  expect_length(merged, 4)                                
  expect_identical(which(!is.na(merged)), c(1L, 3L))
  expect_equal(n_samples(merged[1]), as.integer(nrow(bursts(a)[[1]]) + nrow(bursts(a)[[2]])))                                                                                                     
  expect_equal(n_samples(merged[3]), as.integer(nrow(bursts(a)[[3]]) + nrow(bursts(a)[[4]])))                                                                                                     
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
  expect_identical(n_samples(a), as.integer(c(10, 5, 5)))
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
  expect_identical(n_samples(a), as.integer(c(15, 5)))
})

test_that("Don't combine bursts without start time", {
  a <- acc(
    c(acc_burst_example(x = 1:10), acc_burst_example(x = 1:10)),
    frequency = units::set_units(1, "Hz")
  )
  
  expect_identical(a, merge_acc(a))
})

test_that("Handle empty acc vectors when binding", {
  expect_identical(merge_acc(acc()), acc())
  expect_identical(merge_acc(c(acc(), acc())), acc())
})

test_that("split_acc() on empty acc returns empty list", {
  expect_identical(split_acc(acc(), 1), list())
})

test_that("split_acc() on single-element acc returns length-1 list", {
  a <- acc(
    acc_burst_example(1:20),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(0, tz = "UTC")
  )

  sp <- split_acc(a, 0.5)

  expect_length(sp, 1)
  expect_true(is_acc(sp[[1]]))
  expect_length(sp[[1]], 4)
  expect_identical(merge_acc(purrr::reduce(sp, c)), a)
})

test_that("Can split acc at a given interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )

  interval <- 0.5
  split <- split_acc(a, interval = interval)

  # Returns a list the same length as the input
  expect_length(split, length(a))
  expect_true(is.list(split))
  expect_true(all(purrr::map_lgl(split, is_acc)))

  # Individual elements contain the expected number of split bursts
  expect_length(split[[1]], 6)
  expect_length(split[[2]], 2)

  # Flatten to check burst properties
  flat <- purrr::reduce(split, c)

  expect_true(all(units::drop_units(burst_dur(flat)) == interval))

  expect_equal(
    purrr::map_int(bursts(flat), nrow),
    c(rep(10, 6), rep(20, 2))
  )
  expect_equal(
    do.call(rbind, bursts(flat)[1:6]),
    bursts(a)[[1]]
  )
  expect_equal(
    do.call(rbind, bursts(flat)[7:8]),
    bursts(a)[[2]]
  )
  expect_equal(
    freqs(flat),
    units::set_units(c(rep(20, 6), rep(40, 2)), "Hz")
  )
  expect_identical(
    starts(a)[1] + cumsum(c(0, rep(interval, 5))),
    starts(flat)[1:6]
  )
  expect_identical(
    starts(a)[2] + cumsum(c(0, interval)),
    starts(flat)[7:8]
  )
})

test_that("Correctly split when burst length not divisible by interval", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )

  interval <- 0.7
  split <- split_acc(a, interval = interval)
  flat <- purrr::reduce(split, c)
  dur <- burst_dur(a)

  expect_length(flat, units::drop_units(sum(ceiling(burst_dur(a) / interval))))

  # Bursts should be split into equal time lengths other than for the last
  # element of each split burst, which will capture whatever burst duration remains
  expect_equal(
    units::drop_units(burst_dur(flat)),
    c(
      c(rep(interval, dur[1] %/% interval), dur[1] - (interval * dur[1] %/% interval)),
      c(rep(interval, dur[2] %/% interval), dur[2] - (interval * dur[2] %/% interval))
    )
  )
})

test_that("split_acc() retains NA", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), new_acc_list(list(NULL)), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(NA, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10, 10), tz = "UTC")
  )

  sp <- split_acc(a, 0.5)

  expect_length(sp, length(a))

  # NA element produces a length-1 NA acc
  expect_length(sp[[2]], 1)
  expect_true(is.na(sp[[2]]))

  # Flattened non-NA results match splitting only the non-NA input
  flat <- purrr::reduce(sp, c)
  flat_no_na <- purrr::reduce(split_acc(a[!is.na(a)], 0.5), c)
  expect_identical(flat[!is.na(flat)], flat_no_na)
})

test_that("Can recover split continuous data by merging", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )

  flat <- purrr::reduce(split_acc(a, interval = 0.5), c)
  expect_identical(merge_acc(flat), a)
})

test_that("Can recover split continuous data by merging with NA", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), new_acc_list(list(NULL)), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(NA, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10, 10), tz = "UTC")
  )

  flat <- purrr::reduce(split_acc(a, interval = 0.5), c)
  expect_identical(merge_acc(flat), a[!is.na(a)])
})

test_that("split_acc() preserves 1-sample bursts", {
  a <- acc(
    c(acc_burst_example(42, 43), acc_burst_example(1:20, 1:20)),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(c(0, 5), tz = "UTC")
  )

  sp <- split_acc(a, 0.5)

  # 1-sample burst should pass through unchanged
  expect_length(sp[[1]], 1)
  expect_false(is.na(sp[[1]]))
  expect_identical(sp[[1]], a[1])

  # Multi-sample burst still splits normally
  expect_length(sp[[2]], 4)

  # Round-trip preserves the 1-sample burst
  flat <- purrr::reduce(sp, c)
  expect_identical(merge_acc(flat), a)
})

test_that("Long intervals do not modify input acc", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "Hz"), units::set_units(40, "Hz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )

  split <- split_acc(a, interval = max(burst_dur(a)))
  expect_identical(purrr::reduce(split, c), a)
})

test_that("Can standardize interval units when splitting", {
  a <- acc(
    c(acc_burst_example(1:60, 1:60), acc_burst_example(101:140)),
    frequency = c(units::set_units(20, "kHz"), units::set_units(40, "kHz")),
    start = as.POSIXct(c(0, 10), tz = "UTC")
  )

  split <- split_acc(a, interval = 0.5)
  flat <- purrr::reduce(split, c)

  # Default should be in 1/freq units
  expect_length(flat, 8)
  expect_identical(
    split,
    split_acc(a, interval = units::set_units(0.5 / 1000, "s"))
  )
})

test_that("split_acc() errors on invalid interval", {
  a <- acc(
    acc_burst_example(1:20),
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(0, tz = "UTC")
  )

  expect_error(split_acc(a, 0), "`interval` must be a positive")
  expect_error(split_acc(a, -1), "`interval` must be a positive")
  
})

test_that("split_acc() round-trip in dataframe workflow", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  # Covers normal bursts, adjacent bursts, NA element, and 1-sample burst
  a <- acc(
    c(
      acc_burst_example(1:60, 1:60),
      acc_burst_example(61:100, 61:100),
      new_acc_list(list(NULL)),
      acc_burst_example(42, 43),
      acc_burst_example(101:140)
    ),
    frequency = c(
      units::set_units(20, "Hz"), units::set_units(20, "Hz"),
      units::set_units(NA, "Hz"),
      units::set_units(10, "Hz"), units::set_units(40, "Hz")
    ),
    start = as.POSIXct(c(0, 3, 10, 20, 30), tz = "UTC")
  )

  tbl <- tibble::tibble(
    id = c("x", "x", "y", "z", "z"),
    a = a,
    row_id = seq_len(5)
  )

  # Split, unnest, re-merge with row_id to prevent cross-row merging, filter
  result <- tbl |>
    dplyr::mutate(a = split_acc(a, units::set_units(1, "s"))) |>
    tidyr::unnest(a) |>
    dplyr::mutate(a2 = merge_acc(a, acc_ids = row_id, drop = FALSE)) |>
    dplyr::filter(!is.na(a2))

  # NA row drops after filter, all others recover
  expect_equal(nrow(result), 4)
  expect_identical(result$id, c("x", "x", "z", "z"))
  expect_identical(result$a2, a[!is.na(a)])
})
