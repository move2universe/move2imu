test_that("Config predicates validate colsets against supported defaults", {
  matches_any <- function(cols, config) {
    any(purrr::map_lgl(config, function(entry) entry$is_(cols)))
  }
  cfg <- acc_colset_config()

  expect_true(matches_any(acc_colset_eobs(), cfg))
  expect_true(matches_any(acc_colset_raw(), cfg))
  expect_true(matches_any(acc_colset_xyz(), cfg))
  expect_true(matches_any(acc_colset_raw_xyz(), cfg))

  # Compact-format acc cols must contain all listed cols
  expect_false(matches_any(acc_colset_eobs()[1:2], cfg))
  expect_false(matches_any(acc_colset_raw()[1], cfg))

  # Expanded-format acc cols can consist of a subset of allowable cols
  expect_true(matches_any(acc_colset_xyz()[1:2], cfg))
  expect_true(matches_any(acc_colset_raw_xyz()[3], cfg))

  # Duplicates excluded
  expect_false(matches_any(c(acc_colset_raw_xyz(), acc_colset_xyz()), cfg))
  expect_false(matches_any(c(acc_colset_xyz(), acc_colset_xyz()), cfg))
})

test_that("Can find active colsets in move2 object", {
  skip_if_not_installed("move2")
  expect_identical(active_acc_colsets(albatrosses()), list(eobs = acc_colset_eobs()))
  expect_identical(active_acc_colsets(gulls()), list(raw_xyz = acc_colset_raw_xyz()))
})

test_that("Correctly subset active colsets for expanded-format acc cols", {
  skip_if_not_installed("move2")
  gulls_data <- gulls()
  gulls_sub <- gulls_data[, setdiff(colnames(gulls_data), "acceleration_raw_y")]
  expect_identical(
    active_acc_colsets(gulls_sub),
    list(raw_xyz = new_imu_colset(
      c(X = "acceleration_raw_x", Z = "acceleration_raw_z"),
      type = "expanded"
    ))
  )
})

test_that("Can find active colsets in move2 object with multiple colsets", {
  skip_if_not_installed("move2")
  cols <- active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
  expect_identical(
    cols, 
    list(eobs = acc_colset_eobs(), raw_xyz = acc_colset_raw_xyz())
  )
})

test_that("Error if no colset detected", {
  skip_if_not_installed("move2")
  alb_data <- albatrosses()
  
  col_subset <- setdiff(colnames(alb_data), "eobs_acceleration_axes")
  alb_data <- alb_data[, col_subset]
  
  expect_error(
    active_acc_colsets(alb_data),
    "Could not identify a full acc column set"
  )
})

test_that("Use data values to determine active colset if multiple present", {
  skip_if_not_installed("move2")
  m <- move2::mt_stack(gulls(), albatrosses())
  
  # Missing data shouldn't matter if at least one of the set still contains data
  m[["acceleration_raw_x"]] <- NA
  m[["acceleration_raw_y"]] <- NA
  
  colsets <- active_acc_colsets(m)
  expect_identical(
    colsets$raw_xyz,
    new_imu_colset(c(Z = "acceleration_raw_z"), type = "expanded")
  )
  
  # If all cols in a set are missing, then the next colset will be used
  m[["acceleration_raw_z"]] <- NA
  
  expect_identical(active_acc_colsets(m), list(eobs = acc_colset_eobs()))
  
  # Unless neither have data, in which case first is used
  m[["eobs_acceleration_axes"]] <- NA
  m[["eobs_acceleration_sampling_frequency_per_axis"]] <- NA
  m[["eobs_accelerations_raw"]] <- NA
  
  expect_error(active_acc_colsets(m), "Could not identify a full")
})

test_that("Correctly identify that a non-full compact-format colset is invalid", {
  skip_if_not_installed("move2")
  alb <- albatrosses()
  alb$eobs_acceleration_axes <- NA
  expect_error(active_acc_colsets(alb))
  
  alb$eobs_acceleration_axes <- rep(list(NULL), nrow(alb))
  expect_error(active_acc_colsets(alb))
  
  alb$eobs_acceleration_axes <- NULL
  expect_error(active_acc_colsets(alb))
})

test_that("Currently supported colsets", {
  expect_identical(
    movebank_acc_colsets(),
    list(
      eobs = acc_colset_eobs(),
      raw = acc_colset_raw(),
      xyz = acc_colset_xyz(),
      raw_xyz = acc_colset_raw_xyz()
    )
  )
})

test_that("is_unique_named_subset correctly identifies subsets", {
  tgt <- acc_colset_raw_xyz()

  # Exact match
  expect_true(is_unique_named_subset(tgt, tgt))

  # Valid subset
  expect_true(is_unique_named_subset(tgt[c("X", "Z")], tgt))
  expect_true(is_unique_named_subset(tgt["Y"], tgt))

  # Superset (concatenated colsets)
  expect_false(is_unique_named_subset(c(acc_colset_raw_xyz(), acc_colset_xyz()), tgt))

  # Wrong name-value mapping (Y mapped to X's column)
  expect_false(is_unique_named_subset(
    imu_colset(y = "acceleration_raw_x"),
    tgt
  ))

  # Duplicate names
  expect_false(is_unique_named_subset(c(tgt["X"], tgt["X"]), tgt))

  # Custom columns not in target
  expect_false(is_unique_named_subset(imu_colset(x = "my_col"), tgt))

  # Empty input
  expect_false(is_unique_named_subset(character(0), tgt))
  
  # Names are not required if not present in both
  expect_true(is_unique_named_subset(c("A", "B"), c("A", "B", "C")))
  expect_false(is_unique_named_subset(c("A", "B"), c(A = "A", B = "B", C = "C")))
})

test_that("imu_colset() errors on invalid specifications", {
  # No columns specified
  expect_error(imu_colset(), "No IMU data columns specified")

  # Incomplete burst args
  expect_error(imu_colset(bursts = "b"), "requires")
  expect_error(imu_colset(bursts = "b", axes = "a"), "requires")

  # Mixed formats
  expect_error(
    imu_colset(x = "x", bursts = "b", axes = "a", frequency = "f"),
    "Cannot mix"
  )
})

test_that("Can get colset type from colset", {
  expect_equal(attr(acc_colset_eobs(), "type"), "compact")
  expect_equal(attr(acc_colset_raw(), "type"), "compact")
  expect_equal(attr(acc_colset_xyz(), "type"), "expanded")
  expect_equal(attr(acc_colset_raw_xyz(), "type"), "expanded")
})
