test_that("Can validate colsets", {
  expect_true(is_valid_acc_colset(acc_eobs_cols()))
  expect_true(is_valid_acc_colset(acc_burst_cols()))
  expect_true(is_valid_acc_colset(acc_xyz_cols()))
  expect_true(is_valid_acc_colset(acc_raw_xyz_cols()))
  expect_true(is_valid_acc_colset(acc_tilt_cols()))
  
  # Burst-format acc cols must contain all listed cols
  expect_false(is_valid_acc_colset(acc_eobs_cols()[1:2]))
  expect_false(is_valid_acc_colset(acc_burst_cols()[1]))
  
  # Long-format acc cols can consist of a subset of allowable cols
  expect_true(is_valid_acc_colset(acc_xyz_cols()[1:2]))
  expect_true(is_valid_acc_colset(acc_raw_xyz_cols()[3]))
  expect_true(is_valid_acc_colset(acc_tilt_cols()[c(1, 3)]))
  
  # Duplicates excluded
  expect_false(is_valid_acc_colset(c(acc_raw_xyz_cols(), acc_xyz_cols())))
  expect_false(is_valid_acc_colset(c(acc_xyz_cols(), acc_xyz_cols())))
})

test_that("Can find active colsets in move2 object", {
  expect_identical(active_acc_cols(albatrosses), acc_eobs_cols())
  expect_warning(
    gulls_cols <- active_acc_cols(gulls), 
    "Detected multiple valid acceleration column sets"
  )
  expect_identical(gulls_cols, acc_raw_xyz_cols())
  
  # Subsets allowed for long format acc cols
  gulls_sub <- gulls[, setdiff(colnames(gulls), "acceleration_raw_y")]
  expect_identical(
    active_acc_cols(gulls_sub, quiet = TRUE),
    c("acceleration_raw_x", "acceleration_raw_z")
  )
})

test_that("Error if no colset detected", {
  col_subset <- setdiff(colnames(albatrosses), "eobs_acceleration_axes")
  albatrosses <- albatrosses[, col_subset]
  
  expect_error(
    active_acc_cols(albatrosses),
    "Could not identify a full acceleration column set"
  )
})

test_that("Use data values to determine active colset if multiple present", {
  gulls_na <- gulls
  
  # Missing data shouldn't matter if at least one of the set still contains data
  gulls_na[["acceleration_raw_x"]] <- NA
  gulls_na[["acceleration_raw_y"]] <- NA
  
  expect_identical(active_acc_cols(gulls_na, quiet = TRUE), acc_raw_xyz_cols())
  
  # If all cols in a set are missing, then the next colset will be used
  gulls_na[["acceleration_raw_z"]] <- NA
  
  expect_identical(active_acc_cols(gulls_na), acc_tilt_cols())
  
  # Unless neither have data, in which case first is used
  gulls_na[["tilt_x"]] <- NA
  gulls_na[["tilt_y"]] <- NA
  gulls_na[["tilt_z"]] <- NA
  
  expect_identical(active_acc_cols(gulls_na), acc_raw_xyz_cols())
})

test_that("Currently supported colsets", {
  expect_identical(
    valid_acc_colsets(),
    list(
      acc_eobs_cols(),
      acc_burst_cols(),
      acc_xyz_cols(),
      acc_raw_xyz_cols(),
      acc_tilt_cols()
    )
  )
})

test_that("Can get colset type from colset", {
  expect_equal(acc_cols_to_type(acc_eobs_cols()), "eobs")
  expect_equal(acc_cols_to_type(acc_burst_cols()), "burst")
  expect_equal(acc_cols_to_type(acc_xyz_cols()), "xyz")
  expect_equal(acc_cols_to_type(acc_raw_xyz_cols()), "raw_xyz")
  expect_equal(acc_cols_to_type(acc_tilt_cols()), "tilt")
  expect_length(acc_cols_to_type("foo"), 0)
})
