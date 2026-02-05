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
  
  a <- as_acc(m1, tolerance = 1)
  
  expect_length(a, 4)
  expect_equal(purrr::map_int(bursts(a), nrow), c(5, 1, 11, 52))
  expect_equal(as.numeric(freqs(a)), c(2, NA, 2, 1.3333))
  expect_length(as_acc(m1, tolerance = 0.6), 55)
})

test_that("Can drop missing acc values", {
  gulls_data <- gulls()
  
  acc <- suppressWarnings(as_acc(gulls_data, drop = FALSE))
  acc_i <- which(gulls_data$sensor_type_id == 2365683)
  
  expect_identical(suppressWarnings(as_acc(gulls_data)), acc[!is.na(acc)])
  expect_length(acc, nrow(gulls_data))
  expect_equal(is.na(acc[acc_i]), duplicated(parse_bursts(gulls_data)))
  
  acc <- as_acc(albatrosses(), drop = FALSE)
  expect_identical(as_acc(albatrosses()), acc[!is.na(acc)])
})

test_that("Equivalent data in burst and long format produce same acc", {
  t1 <- data.frame(
    id = 1,
    acceleration_x = 1:10,
    acceleration_y = 1:10,
    acceleration_z = 1:10,
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
