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
  expect_identical(active_acc_cols(albatrosses()), list(eobs = acc_eobs_cols()))
  expect_identical(active_acc_cols(gulls()), list(raw_xyz = acc_raw_xyz_cols()))
})

test_that("Correctly subset active colsets for long-format acc cols", {
  gulls_data <- gulls()
  gulls_sub <- gulls_data[, setdiff(colnames(gulls_data), "acceleration_raw_y")]
  expect_identical(
    active_acc_cols(gulls_sub),
    list(raw_xyz = c("acceleration_raw_x", "acceleration_raw_z"))
  )
})

test_that("Can find active colsets in move2 object with multiple colsets", {
  expect_warning(
    cols <- active_acc_cols(move2::mt_stack(albatrosses(), gulls())), 
    "Detected multiple valid acceleration column sets"
  )
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
    active_acc_cols(alb_data),
    "Could not identify a full acceleration column set"
  )
})

test_that("Use data values to determine active colset if multiple present", {
  m <- move2::mt_stack(gulls(), albatrosses())
  
  # Missing data shouldn't matter if at least one of the set still contains data
  m[["acceleration_raw_x"]] <- NA
  m[["acceleration_raw_y"]] <- NA
  
  expect_warning(acc_cols <- active_acc_cols(m), "Detected multiple")
  expect_identical(acc_cols$raw_xyz, "acceleration_raw_z")
  
  # If all cols in a set are missing, then the next colset will be used
  m[["acceleration_raw_z"]] <- NA
  
  expect_identical(active_acc_cols(m), list(eobs = acc_eobs_cols()))
  
  # Unless neither have data, in which case first is used
  m[["eobs_acceleration_axes"]] <- NA
  m[["eobs_acceleration_sampling_frequency_per_axis"]] <- NA
  m[["eobs_accelerations_raw"]] <- NA
  
  expect_error(active_acc_cols(m), "Could not identify a full")
})

test_that("Correctly identify that a non-full burst colset is invalid", {
  alb <- albatrosses()
  alb$eobs_acceleration_axes <- NA
  expect_error(active_acc_cols(alb))
  
  alb$eobs_acceleration_axes <- rep(list(NULL), nrow(alb))
  expect_error(active_acc_cols(alb))
  
  alb$eobs_acceleration_axes <- NULL
  expect_error(active_acc_cols(alb))
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

test_that("Can get colset type from colset", {
  expect_equal(acc_cols_to_type(acc_eobs_cols()), "eobs")
  expect_equal(acc_cols_to_type(acc_burst_cols()), "burst")
  expect_equal(acc_cols_to_type(acc_xyz_cols()), "xyz")
  expect_equal(acc_cols_to_type(acc_raw_xyz_cols()), "raw_xyz")
  expect_error(acc_cols_to_type("foo"), "Invalid acc columns")
})
