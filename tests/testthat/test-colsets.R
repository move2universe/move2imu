test_that("Can validate colsets", {
  expect_true(is_valid_acc_colset(acc_eobs_cols()))
  expect_true(is_valid_acc_colset(acc_burst_cols()))
  expect_true(is_valid_acc_colset(acc_xyz_cols()))
  expect_true(is_valid_acc_colset(acc_raw_xyz_cols()))
  
  # Burst-format acc cols must contain all listed cols
  expect_false(is_valid_acc_colset(acc_eobs_cols()[1:2]))
  expect_false(is_valid_acc_colset(acc_burst_cols()[1]))
  
  # Long-format acc cols can consist of a subset of allowable cols
  expect_true(is_valid_acc_colset(acc_xyz_cols()[1:2]))
  expect_true(is_valid_acc_colset(acc_raw_xyz_cols()[3]))
  
  # Duplicates excluded
  expect_false(is_valid_acc_colset(c(acc_raw_xyz_cols(), acc_xyz_cols())))
  expect_false(is_valid_acc_colset(c(acc_xyz_cols(), acc_xyz_cols())))
})

test_that("Can find active colsets in move2 object", {
  expect_identical(active_acc_colsets(albatrosses()), list(eobs = acc_eobs_cols()))
  expect_identical(active_acc_colsets(gulls()), list(raw_xyz = acc_raw_xyz_cols()))
})

test_that("Correctly subset active colsets for long-format acc cols", {
  gulls_data <- gulls()
  gulls_sub <- gulls_data[, setdiff(colnames(gulls_data), "acceleration_raw_y")]
  expect_identical(
    active_acc_colsets(gulls_sub),
    list(raw_xyz = new_acc_colset(
      c("acceleration_raw_x", "acceleration_raw_z"),
      type = "long"
    ))
  )
})

test_that("Can find active colsets in move2 object with multiple colsets", {
  cols <- active_acc_colsets(move2::mt_stack(albatrosses(), gulls()))
  expect_identical(
    cols, 
    list(eobs = acc_eobs_cols(), raw_xyz = acc_raw_xyz_cols())
  )
})

test_that("Error if no colset detected", {
  alb_data <- albatrosses()
  
  col_subset <- setdiff(colnames(alb_data), "eobs_acceleration_axes")
  alb_data <- alb_data[, col_subset]
  
  expect_error(
    active_acc_colsets(alb_data),
    "Could not identify a full acceleration column set"
  )
})

test_that("Use data values to determine active colset if multiple present", {
  m <- move2::mt_stack(gulls(), albatrosses())
  
  # Missing data shouldn't matter if at least one of the set still contains data
  m[["acceleration_raw_x"]] <- NA
  m[["acceleration_raw_y"]] <- NA
  
  acc_cols <- active_acc_colsets(m)
  expect_identical(
    acc_cols$raw_xyz,
    new_acc_colset("acceleration_raw_z", type = "long")
  )
  
  # If all cols in a set are missing, then the next colset will be used
  m[["acceleration_raw_z"]] <- NA
  
  expect_identical(active_acc_colsets(m), list(eobs = acc_eobs_cols()))
  
  # Unless neither have data, in which case first is used
  m[["eobs_acceleration_axes"]] <- NA
  m[["eobs_acceleration_sampling_frequency_per_axis"]] <- NA
  m[["eobs_accelerations_raw"]] <- NA
  
  expect_error(active_acc_colsets(m), "Could not identify a full")
})

test_that("Correctly identify that a non-full burst colset is invalid", {
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
    valid_acc_colsets(),
    list(
      eobs = acc_eobs_cols(),
      burst = acc_burst_cols(),
      xyz = acc_xyz_cols(),
      raw_xyz = acc_raw_xyz_cols()
    )
  )
})

test_that("is_unique_named_subset correctly identifies subsets", {
  tgt <- acc_raw_xyz_cols()

  # Exact match
  expect_true(is_unique_named_subset(tgt, tgt))

  # Valid subset
  expect_true(is_unique_named_subset(tgt[c("X", "Z")], tgt))
  expect_true(is_unique_named_subset(tgt["Y"], tgt))

  # Superset (concatenated colsets)
  expect_false(is_unique_named_subset(c(acc_raw_xyz_cols(), acc_xyz_cols()), tgt))

  # Wrong name-value mapping (Y mapped to X's column)
  expect_false(is_unique_named_subset(
    acc_colset(acc_y = "acceleration_raw_x"),
    tgt
  ))

  # Duplicate names
  expect_false(is_unique_named_subset(c(tgt["X"], tgt["X"]), tgt))

  # Custom columns not in target
  expect_false(is_unique_named_subset(acc_colset(acc_x = "my_col"), tgt))

  # Empty input
  expect_false(is_unique_named_subset(character(0), tgt))
  
  # Names are not required if not present in both
  expect_true(is_unique_named_subset(c("A", "B"), c("A", "B", "C")))
  expect_false(is_unique_named_subset(c("A", "B"), c(A = "A", B = "B", C = "C")))
})

test_that("acc_colset() errors on invalid specifications", {
  # No columns specified
  expect_error(acc_colset(), "No acc columns")

  # Both long and burst args
  expect_error(
    acc_colset(acc_x = "x", bursts = "b", axes = "a", frequency = "f"),
    "not both"
  )

  # Incomplete burst args
  expect_error(acc_colset(bursts = "b"), "requires")
  expect_error(acc_colset(bursts = "b", axes = "a"), "requires")
})

test_that("Can get colset type from colset", {
  expect_equal(attr(acc_eobs_cols(), "type"), "burst")
  expect_equal(attr(acc_burst_cols(), "type"), "burst")
  expect_equal(attr(acc_xyz_cols(), "type"), "long")
  expect_equal(attr(acc_raw_xyz_cols(), "type"), "long")
})
