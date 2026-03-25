test_that("Can get acc from burst-format acc data", {
  alb_data <- albatrosses()
  
  acc <- as_acc(alb_data)
  
  non_na <- which(!is.na(alb_data$eobs_acceleration_axes))
  
  expect_s3_class(acc, "acc")
  expect_length(acc, nrow(alb_data[non_na, ]))
  expect_true(is_uniform(acc))
  expect_identical(
    purrr::map_chr(
      bursts(acc), 
      function(x) paste0(colnames(x), collapse = "")
    ),
    as.character(alb_data[non_na, ]$eobs_acceleration_axes)
  )
  expect_identical(
    purrr::map_chr(
      bursts(acc),
      function(x) paste(t(x), collapse = " ")
    ),
    alb_data[non_na, ]$eobs_accelerations_raw
  )
  expect_identical(
    freqs(acc),
    alb_data[non_na, ]$eobs_acceleration_sampling_frequency_per_axis
  )
  expect_identical(
    starts(acc),
    alb_data[non_na, ]$timestamp
  )
})

test_that("Can get acc from long-format acc data", {
  gulls_data <- gulls()
  
  acc_i <- which(gulls_data$sensor_type_id == 2365683)
  non_na <- which(!is.na(gulls_data$acceleration_raw_x))
  
  # Identify time series gap points
  gap_i <- which(c(TRUE, diff(gulls_data$timestamp[non_na]) > 0.5))
  
  expect_warning(
    acc <- as_acc(gulls_data), 
    "Detected multiple valid acceleration column sets"
  )
  
  expect_s3_class(acc, "acc")
  expect_length(acc, length(gap_i))
  expect_true(is_uniform(acc))
  expect_identical(
    unique(purrr::map(bursts(acc), colnames))[[1]],
    c("X", "Y", "Z")
  )
  expect_identical(
    unlist(purrr::map(bursts(acc), ~ .x[, "X"])),
    gulls_data[non_na, ]$acceleration_raw_x
  )
  expect_identical(
    unlist(purrr::map(bursts(acc), ~ .x[, "Y"])),
    gulls_data[non_na, ]$acceleration_raw_y
  )
  expect_identical(
    unlist(purrr::map(bursts(acc), ~ .x[, "Z"])),
    gulls_data[non_na, ]$acceleration_raw_z
  )
  expect_identical(
    unique(freqs(acc)),
    units::set_units(20, "Hz")
  )
  expect_identical(
    starts(acc),
    sort(gulls_data[non_na, ][gap_i, ]$timestamp)
  )
})

test_that("Can manually specify acc columns to use for parsing", {
  cols <- acc_tilt_cols()
  
  a <- as_acc(gulls(), acc_cols = cols)
  i <- which_acc_vals(gulls(), acc_cols = cols)
  
  expect_equal(
    unlist(map_acc(a, ~ .br[, 1])),
    units::drop_units(gulls()[[cols[[1]]]][i])
  )
  expect_equal(
    unlist(map_acc(a, ~ .br[, 2])),
    units::drop_units(gulls()[[cols[[2]]]][i])
  )
  expect_equal(
    unlist(map_acc(a, ~ .br[, 3])),
    units::drop_units(gulls()[[cols[[3]]]][i])
  )
})

test_that("Can manually specify a subset of long-format cols", {
  col <- "acceleration_raw_y"
  
  a <- as_acc(gulls(), acc_cols = col)
  i <- which_acc_vals(gulls(), acc_cols = col)
  
  expect_equal(unlist(map_acc(a, ~ .br[, 1])), gulls()[[col]][i])
})

test_that("Can manually specify acc columns in mixed acc type data", {
  d <- move2::mt_stack(albatrosses(), gulls())
  
  expect_warning(as_acc(d), "multiple valid acceleration column sets")
  expect_identical(
    as_acc(albatrosses()),
    as_acc(d, acc_cols = acc_eobs_cols())
  )
  expect_identical(
    suppressWarnings(as_acc(gulls())),
    as_acc(d, acc_cols = acc_raw_xyz_cols())
  )
})

test_that("Correctly error on bad acc_cols specifications", {
  expect_error(as_acc(gulls(), acc_cols = acc_eobs_cols()), "Missing columns")
  expect_error(as_acc(gulls(), acc_cols = "foobar"), "Invalid acc columns")
})

test_that("Can split long-format data into bursts by inferred frequency", {
  t1 <- data.frame(
    id = 1,
    acceleration_x = 1:69,
    acceleration_y = 1:69,
    acceleration_z = 1:69,
    timestamp = as.POSIXct(
      c(
        seq(1, 3, by = 0.5), 4, 5.5, 
        seq(6, 10, by = 0.5), seq(10.5, 50, by = 0.75)
      ), 
      "UTC"
    ),
    x = 1, 
    y = 1
  )
  
  m1 <- move2::mt_as_move2(
    t1,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  a <- as_acc(m1)
  
  expect_length(a, 4)
  expect_equal(purrr::map_int(bursts(a), nrow), c(5, 1, 11, 52))
  expect_equal(as.numeric(freqs(a)), c(2, NA, 2, 1.3333))
})

test_that("Can use `min_frq` to avoid building bursts below frq thresh", {
  t1 <- data.frame(
    id = 1,
    acceleration_x = 1:69,
    acceleration_y = 1:69,
    acceleration_z = 1:69,
    timestamp = as.POSIXct(
      c(
        seq(1, 3, by = 0.5), 4, 5.5, 
        seq(6, 10, by = 0.5), seq(10.5, 50, by = 0.75)
      ), 
      "UTC"
    ),
    x = 1, 
    y = 1
  )
  
  m1 <- move2::mt_as_move2(
    t1,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  a1 <- as_acc(m1, min_frq = 1)
  a2 <- as_acc(m1, min_frq = 2)
  
  # First bursts should be identical, but final burst should be split 
  # fully into length-1 "bursts"
  expect_identical(a2[1:3], a1[1:3])
  expect_length(a2, length(a1) - 1 + nrow(bursts(a1)[[4]]))
  expect_identical(do.call(rbind, bursts(a2)[4:length(a2)]), bursts(a1)[[4]])
  expect_true(all(is.na(freqs(a2)[4:length(a2)])))
  
  expect_length(as_acc(m1, min_frq = Inf), nrow(m1))
  expect_identical(a1, as_acc(m1, min_frq = 0))
  
  # If `drop = FALSE`, partitioned bursts should fill indices that were
  # previously empty, and overall vector length should stay the same.
  expect_length(
    suppressWarnings(as_acc(gulls(), min_frq = 40, drop = FALSE)),
    nrow(gulls())
  )
})

test_that("Can drop missing acc values", {
  gulls_data <- gulls()
  
  # Provide cols explicitly below to avoid irrelevant multi-col warnings
  cols <- acc_raw_xyz_cols()
  
  acc <- as_acc(gulls_data, acc_cols = cols, drop = FALSE)
  acc_i <- which(gulls_data$sensor_type_id == 2365683)
  
  expect_identical(as_acc(gulls_data, acc_cols = cols), acc[!is.na(acc)])
  expect_length(acc, nrow(gulls_data))
  expect_equal(
    is.na(acc[acc_i]), 
    duplicated(parse_bursts(gulls_data, acc_cols = cols))
  )
  
  acc <- as_acc(albatrosses(), drop = FALSE)
  expect_identical(as_acc(albatrosses()), acc[!is.na(acc)])
})

test_that("Retain burst dimensions when missing data in some axes", {
  g <- gulls()
  
  g[["acceleration_raw_x"]][1:100] <- NA
  
  a <- as_acc(g, acc_cols = acc_raw_xyz_cols())
  expect_true(all(purrr::map(bursts(a), ncol) == 3))
})

test_that("Preserve time zone", {
  g <- gulls()
  a <- albatrosses()
  
  expect_equal(attr(starts(acc()), "tzone"), "UTC")
  expect_equal(attr(starts(acc(acc_burst_example(), 1)), "tzone"), "UTC")
  
  expect_equal(attr(starts(as_acc(albatrosses())), "tzone"), "UTC")
  
  a$timestamp <- 1:nrow(a)
  expect_equal(attr(starts(as_acc(a)), "tzone"), "UTC")
  
  a$timestamp <- as.POSIXct(a$timestamp, tz = "CET")
  g$timestamp <- as.POSIXct(g$timestamp, tz = "CET")
  expect_equal(attr(starts(as_acc(a)), "tzone"), "CET")
  expect_equal(attr(starts(suppressWarnings(as_acc(g))), "tzone"), "CET")
})

test_that("Equivalent data in burst and long format produce same acc", {
  t1 <- data.frame(
    id = 1,
    acceleration_x = as.numeric(1:10),
    acceleration_y = as.numeric(1:10),
    acceleration_z = as.numeric(1:10),
    timestamp = as.POSIXct(seq(1, 1.9, by = 0.1), "UTC"),
    x = 1, 
    y = 1
  )
  
  t2 <- data.frame(
    id = 1,
    acceleration_axes = "XYZ",
    acceleration_sampling_frequency_per_axis = 10,
    accelerations_raw = c(
      paste0(rep(1:5, each = 3), collapse = " "),
      paste0(rep(6:10, each = 3), collapse = " ")
    ),
    timestamp = as.POSIXct(c(1, 1.5), "UTC"),
    x = 1,
    y = 1
  )
  
  m1 <- move2::mt_as_move2(
    t1,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  m2 <- move2::mt_as_move2(
    t2,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  expect_identical(as_acc(m1), as_acc(m2))
})

test_that("Coerce to integer for eobs", {
  t <- data.frame(
    id = 1,
    eobs_acceleration_axes = "XYZ",
    eobs_acceleration_sampling_frequency_per_axis = 10,
    eobs_accelerations_raw = c(
      paste0(rep(1.1:5.1, each = 3), collapse = " "),
      paste0(rep(6.1:10.1, each = 3), collapse = " ")
    ),
    timestamp = as.POSIXct(c(1, 1.5), "UTC"),
    x = 1,
    y = 1
  )
  
  m <- move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  expect_warning(a <- as_acc(m), "Detected numeric acceleration")
  expect_identical(unlist(bursts(a)), rep(1:10, 3))
})

test_that("Don't coerce non-eobs burst cols", {
  t <- data.frame(
    id = 1,
    acceleration_axes = "XYZ",
    acceleration_sampling_frequency_per_axis = 10,
    accelerations_raw = c(
      paste0(rep(1.1:5.1, each = 3), collapse = " "),
      paste0(rep(6.1:10.1, each = 3), collapse = " ")
    ),
    timestamp = as.POSIXct(c(1, 1.5), "UTC"),
    x = 1,
    y = 1
  )
  
  m <- move2::mt_as_move2(
    t,
    coords = c("x", "y"),
    time_column = "timestamp",
    track_id_column = "id"
  )
  
  expect_silent(a <- as_acc(m))
  expect_identical(unlist(bursts(a)), rep(1.1:10.1, 3))
})

test_that("as_acc() checks long-format coltypes", {
  g <- gulls()
  g[["acceleration_raw_x"]] <- "foobar"
  
  expect_error(
    as_acc(g, acc_cols = acc_raw_xyz_cols()),
    "Detected non-numeric columns"
  )
  expect_silent(as_acc(g, acc_cols = "acceleration_raw_y"))
})
