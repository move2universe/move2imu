skip_if_not_installed("move2")

test_that("Can get acc from compact-format acc data", {
  alb_data <- albatrosses()

  acc <- as_acc(alb_data, drop = TRUE)

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

test_that("Can get acc from expanded-format acc data", {
  gulls_data <- gulls()

  acc_i <- which(gulls_data$sensor_type_id == 2365683)
  non_na <- which(!is.na(gulls_data$acceleration_raw_x))

  # Identify time series gap points
  gap_i <- which(c(TRUE, diff(gulls_data$timestamp[non_na]) > 0.5))

  acc <- as_acc(gulls_data, colset = acc_colset_raw_xyz(), drop = TRUE)

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
  cols <- acc_colset_raw_xyz()

  a <- as_acc(gulls(), colset = cols)
  i <- which_imu_vals(gulls(), colset = cols)

  expect_equal(
    unlist(lapply(bursts(a), function(b) b[, 1])),
    gulls()[[cols[[1]]]][i]
  )
  expect_equal(
    unlist(lapply(bursts(a), function(b) b[, 2])),
    gulls()[[cols[[2]]]][i]
  )
  expect_equal(
    unlist(lapply(bursts(a), function(b) b[, 3])),
    gulls()[[cols[[3]]]][i]
  )
})

test_that("Can manually specify a subset of expanded-format cols", {
  col <- imu_colset(y = "acceleration_raw_y")

  a <- as_acc(gulls(), colset = col)
  i <- which_imu_vals(gulls(), colset = col)

  expect_equal(unlist(lapply(bursts(a), function(b) b[, 1])), gulls()[[as.character(col)]][i])
})

test_that("Can manually specify acc columns in mixed acc type data", {
  d <- move2::mt_stack(albatrosses(), gulls())

  expect_identical(
    as_acc(albatrosses(), drop = TRUE),
    as_acc(d, colset = acc_colset_eobs(), drop = TRUE)
  )
  expect_identical(
    as_acc(gulls(), colset = acc_colset_raw_xyz(), drop = TRUE),
    as_acc(d, colset = acc_colset_raw_xyz(), drop = TRUE)
  )
  expect_identical(
    suppressWarnings(as_acc(d, drop = TRUE)),
    as_acc(d, colset = list(acc_colset_eobs(), acc_colset_raw_xyz()), drop = TRUE)
  )
  expect_identical(
    suppressWarnings(as_acc(d, drop = TRUE)),
    as_acc(d, colset = list(acc_colset_raw_xyz(), acc_colset_eobs()), drop = TRUE)
  )
})

test_that("Error on duplicate acc rows across colsets", {
  m <- move2::mt_stack(gulls(), albatrosses())

  # Add raw xyz values to rows that already have eobs data
  eobs_rows <- which(!is.na(m$eobs_accelerations_raw))
  m$acceleration_raw_x[eobs_rows[1:3]] <- 1
  m$acceleration_raw_y[eobs_rows[1:3]] <- 1
  m$acceleration_raw_z[eobs_rows[1:3]] <- 1

  expect_error(
    suppressWarnings(as_acc(m)),
    "multiple sources of acc data"
  )
})

test_that("Automatically get all available colsets", {
  m <- move2::mt_stack(gulls(), albatrosses())

  expect_warning(a <- as_acc(m, drop = TRUE), "Detected multiple")
  a2 <- c(as_acc(gulls(), drop = TRUE), as_acc(albatrosses(), drop = TRUE))

  # Ensure same ordering on comparison
  expect_identical(a2[order(starts(a2))], a[order(starts(a))])
})

test_that("Multi-colset coalesce preserves index alignment with drop = FALSE", {
  m <- move2::mt_stack(gulls(), albatrosses())
  suppressWarnings(a <- as_acc(m, drop = FALSE))

  expect_length(a, nrow(m))
})

test_that("Multi-colset coalesce places values at correct indices", {
  m <- move2::mt_stack(gulls(), albatrosses())

  a_eobs <- as_acc(m, colset = acc_colset_eobs(), drop = FALSE)
  a_raw <- as_acc(m, colset = acc_colset_raw_xyz(), drop = FALSE)
  suppressWarnings(a_all <- as_acc(m, drop = FALSE))

  eobs_idx <- which(!is.na(a_eobs))
  raw_idx <- which(!is.na(a_raw))

  # No overlap between colsets
  expect_length(intersect(eobs_idx, raw_idx), 0)

  # Combined non-NA count matches sum of individual colsets
  expect_equal(sum(!is.na(a_all)), length(eobs_idx) + length(raw_idx))

  # Values at each colset's indices match
  expect_identical(a_all[eobs_idx], a_eobs[eobs_idx])
  expect_identical(a_all[raw_idx], a_raw[raw_idx])
})

test_that("Multi-colset drop = TRUE is subset of drop = FALSE", {
  m <- move2::mt_stack(gulls(), albatrosses())

  suppressWarnings(a_drop <- as_acc(m, drop = TRUE))
  suppressWarnings(a_no_drop <- as_acc(m, drop = FALSE))

  expect_identical(a_drop, a_no_drop[!is.na(a_no_drop)])
})

test_that("Correctly error on bad colset specifications", {
  expect_error(as_acc(gulls(), colset = acc_colset_eobs()), "Missing columns")
  expect_error(as_acc(gulls(), colset = "foobar"), "must be an <imu_colset>")
})

test_that("Error on a user-supplied colset whose columns are present but empty", {
  g <- gulls()
  g$acceleration_raw_x <- NA_real_
  g$acceleration_raw_y <- NA_real_
  g$acceleration_raw_z <- NA_real_

  # Columns exist (so they pass the "missing columns" check) but hold no data.
  # Without the guard this would fail later with a cryptic `round()` error.
  expect_error(
    as_acc(g, colset = acc_colset_raw_xyz()),
    "contain no data"
  )
})

test_that("Can split expanded-format data into bursts by inferred frequency", {
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

  a <- as_acc(m1, drop = TRUE)

  expect_length(a, 4)
  expect_equal(purrr::map_int(bursts(a), nrow), c(5, 1, 11, 52))
  expect_equal(as.numeric(freqs(a)), c(2, NA, 2, 1.3333))
})

test_that("Can use `min_freq` to avoid building bursts below freq thresh", {
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

  a1 <- as_acc(m1, min_freq = 1, drop = TRUE)
  a2 <- as_acc(m1, min_freq = 2, drop = TRUE)

  # First bursts should be identical, but final burst should be split
  # fully into length-1 "bursts"
  expect_identical(a2[1:3], a1[1:3])
  expect_length(a2, length(a1) - 1 + nrow(bursts(a1)[[4]]))
  expect_identical(do.call(rbind, bursts(a2)[4:length(a2)]), bursts(a1)[[4]])
  expect_true(all(is.na(freqs(a2)[4:length(a2)])))

  expect_length(as_acc(m1, min_freq = Inf, drop = TRUE), nrow(m1))
  expect_identical(a1, as_acc(m1, min_freq = 0, drop = TRUE))

  # If `drop = FALSE`, partitioned bursts should fill indices that were
  # previously empty, and overall vector length should stay the same.
  expect_length(
    as_acc(gulls(), min_freq = 40, drop = FALSE),
    nrow(gulls())
  )
})

test_that("Can drop missing acc values", {
  gulls_data <- gulls()

  # Provide cols explicitly below to avoid irrelevant multi-col warnings
  cols <- acc_colset_raw_xyz()

  acc <- as_acc(gulls_data, colset = cols, drop = FALSE)
  acc_i <- which(gulls_data$sensor_type_id == 2365683)

  expect_identical(as_acc(gulls_data, colset = cols, drop = TRUE), acc[!is.na(acc)])
  expect_length(acc, nrow(gulls_data))
  expect_equal(
    is.na(acc[acc_i]),
    duplicated(parse_bursts(gulls_data, colset = cols))
  )

  acc <- as_acc(albatrosses(), drop = FALSE)
  expect_identical(as_acc(albatrosses(), drop = TRUE), acc[!is.na(acc)])
})

test_that("Retain burst dimensions when missing data in some axes", {
  g <- gulls()

  g[["acceleration_raw_x"]][1:100] <- NA

  a <- as_acc(g, colset = acc_colset_raw_xyz(), drop = TRUE)
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
  expect_equal(attr(starts(as_acc(g)), "tzone"), "CET")
})

test_that("Equivalent data in burst and expanded format produce same acc", {
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

  expect_identical(as_acc(m1, drop = TRUE), as_acc(m2, drop = TRUE))
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

  expect_warning(a <- as_acc(m), "Detected numeric values")
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

test_that("as_acc() checks expanded-format coltypes", {
  g <- gulls()
  g[["acceleration_raw_x"]] <- "foobar"

  expect_error(
    as_acc(g, colset = acc_colset_raw_xyz()),
    "Detected non-numeric column"
  )
  expect_silent(as_acc(g, colset = imu_colset(y = "acceleration_raw_y")))
})

test_that("as_acc() rejects plain character vector for colset", {
  expect_error(
    as_acc(albatrosses(), colset = c("eobs_accelerations_raw")),
    "must be an <imu_colset>"
  )
})

test_that("as_acc() errors on swapped burst column types", {
  a <- albatrosses()

  # Swap bursts and frequency columns
  expect_error(
    as_acc(
      a,
      colset = imu_colset(
        bursts = "eobs_acceleration_sampling_frequency_per_axis",
        axes = "eobs_acceleration_axes",
        frequency = "eobs_accelerations_raw"
      )
    ),
    "must be character"
  )
})

test_that("Custom compact-format colset works end-to-end", {
  alb <- albatrosses()

  a <- as_acc(alb, colset = acc_colset_eobs())

  colnames(alb)[colnames(alb) == "eobs_acceleration_axes"] <- "my_axes"
  colnames(alb)[colnames(alb) == "eobs_acceleration_sampling_frequency_per_axis"] <- "my_freq"
  colnames(alb)[colnames(alb) == "eobs_accelerations_raw"] <- "my_bursts"

  # Use the eobs columns via a custom colset (equivalent to acc_colset_eobs())
  custom <- imu_colset(
    bursts = "my_bursts",
    axes = "my_axes",
    frequency = "my_freq"
  )

  # adjust for fact that eobs uses force_int = TRUE, but custom cols don't
  expect_identical(
    as_acc(alb, colset = custom, force_int = TRUE),
    a
  )
})

test_that("Custom expanded-format colset works end-to-end", {
  gul <- gulls()

  a <- as_acc(gul, colset = acc_colset_raw_xyz())

  colnames(gul)[colnames(gul) == "acceleration_raw_x"] <- "acc_x"
  colnames(gul)[colnames(gul) == "acceleration_raw_y"] <- "acc_y"
  colnames(gul)[colnames(gul) == "acceleration_raw_z"] <- "acc_z"

  # Use the eobs columns via a custom colset (equivalent to acc_colset_eobs())
  custom <- imu_colset(x = "acc_x", y = "acc_y", z = "acc_z")

  # adjust for fact that eobs uses force_int = TRUE, but custom cols don't
  expect_identical(
    as_acc(gul, colset = custom),
    a
  )
})
