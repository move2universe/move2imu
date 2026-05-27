b <- bursts(acc_example())[[1]]

# --- acc_calibration() ---------------------------------------------------

test_that("acc_calibration() returns an acc_calibration object", {
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  expect_s3_class(tf, "acc_calibration")
  expect_s3_class(tf, "imu_calibration")
  expect_true(is.list(tf))
  expect_true(all(purrr::map_lgl(tf, is.function)))
})

test_that("acc_calibration() prints with its sensor-specific class", {
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  expect_output(print(tf), "<acc_calibration\\[1\\]>")
})

test_that("as_acc_calibration() returns an acc_calibration object", {
  df <- data.frame(manufacturer = "ornitela")
  tf <- as_acc_calibration(df)
  expect_s3_class(tf, "acc_calibration")
})

test_that("acc_calibration() vectorizes arguments", {
  tf <- acc_calibration(offset = c(2048, 2000), slope = 0.001)
  expect_length(tf, 2)
})

test_that("acc_calibration() applies offset and slope correctly (m/s^2)", {
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  result <- tf[[1]](b)
  manual <- units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2")
  expect_identical(result, manual)
})

test_that("acc_calibration() applies offset and slope correctly (gravity)", {
  tf <- acc_calibration(offset = 2048, slope = 0.001, units = "standard_free_fall")
  result <- tf[[1]](b)
  manual <- units::set_units(((b - 2048) * 0.001), "standard_free_fall")
  expect_identical(result, manual)
})

test_that("acc_calibration() applies different calibrations when vectorized", {
  tf <- acc_calibration(offset = c(2048, 0), slope = c(0.001, 1))
  r1 <- tf[[1]](b)
  r2 <- tf[[2]](b)
  expect_identical(r1, units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2"))
  expect_identical(r2, units::set_units(((b - 0) * 1) * GRAV_CONST, "m/s^2"))
})

test_that("acc_calibration() applies scalar orientation correctly", {
  tf <- acc_calibration(offset = 2048, slope = 0.001, orientation = -1)
  result <- tf[[1]](b)
  manual <- units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2")

  expect_identical(result[, 1], manual[, 1] * -1)
  expect_identical(result[, 2], manual[, 2] * -1)
  expect_identical(result[, 3], manual[, 3] * -1)
})

test_that("acc_calibration() applies per-axis orientation", {
  tf <- acc_calibration(offset = 2048, slope = 0.001, orientation_y = -1)
  result <- tf[[1]](b)
  manual <- units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2")

  expect_identical(result[, "X"], manual[, "X"])
  expect_identical(result[, "Y"], manual[, "Y"] * -1)
  expect_identical(result[, "Z"], manual[, "Z"])
})

test_that("acc_calibration() applies per-axis offset and slope", {
  tf <- acc_calibration(
    offset_x = 0, offset_y = 2048, offset_z = 2000,
    slope_x = 1, slope_y = 0.001, slope_z = 0.001
  )
  result <- tf[[1]](b)

  expect_identical(
    result[, "X"],
    units::set_units((b[, "X"] - 0) * 1 * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    result[, "Y"],
    units::set_units((b[, "Y"] - 2048) * 0.001 * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    result[, "Z"],
    units::set_units((b[, "Z"] - 2000) * 0.001 * GRAV_CONST, "m/s^2")
  )
})

test_that("acc_calibration() output has units class attached", {
  tf <- acc_calibration(offset = 0, slope = 1)
  result <- tf[[1]](b)
  expect_true(inherits(result, "units"))
})

test_that("acc_calibration() warns on already-calibrated data", {
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  calibrated <- tf[[1]](b)
  expect_warning(tf[[1]](calibrated), "already contain units")
})

test_that("acc_calibration() handles NULL/empty burst", {
  tf <- acc_calibration(offset = 2048, slope = 0.001)
  expect_null(tf[[1]](NULL))
})

test_that("acc_calibration() errors on invalid units", {
  expect_error(
    acc_calibration(offset = 0, slope = 1, units = "feet"),
    "units"
  )
})

test_that("acc_calibration() errors when no manufacturer and no offset", {
  expect_error(
    acc_calibration(slope = 0.001),
    "offset.*required"
  )
})

test_that("acc_calibration() errors when no manufacturer and no slope", {
  expect_error(
    acc_calibration(offset = 2048),
    "slope.*required"
  )
})

test_that("acc_calibration() errors on invalid orientation value", {
  expect_error(acc_calibration(offset = 2048, slope = 0.001, orientation = 0))
})

test_that("acc_calibration() errors on unrecognized manufacturer", {
  expect_error(
    acc_calibration(manufacturer = "foobar", offset = 1, slope = 1),
    "Unrecognized manufacturer"
  )
})

test_that("acc_calibration() eobs requires tag_id", {
  expect_error(acc_calibration(manufacturer = "eobs"), "tag_id")
})

# --- Manufacturer defaults ---------------------------------------------------

test_that("acc_calibration() with eobs uses correct defaults per generation", {
  sp1 <- eobs_specs(1000)
  sp3 <- eobs_specs(5000)

  tf <- acc_calibration(manufacturer = "eobs", tag_id = c(1000, 5000))

  # Gen 1 (1000) has orientation_y = 1, gen 3 (5000) has orientation_y = -1
  # Y axis should have opposite signs
  y1 <- as.numeric(tf[[1]](b)[1, "Y"])
  y3 <- as.numeric(tf[[2]](b)[1, "Y"])
  expect_true(sign(y1) != sign(y3))
})

test_that("acc_calibration() with ornitela uses correct defaults", {
  tf <- acc_calibration(manufacturer = "ornitela")
  sp <- ornitela_specs()
  result <- tf[[1]](b)
  manual <- units::set_units(((b - sp$offset) * sp$slope) * GRAV_CONST, "m/s^2")
  expect_identical(result, manual)
})

# --- User override of manufacturer defaults -----------------------------------

test_that("user-provided offset overrides manufacturer default", {
  tf <- acc_calibration(manufacturer = "eobs", tag_id = 1000, offset_x = 9999)
  sp <- eobs_specs(1000)
  r <- tf[[1]](b)
  # X should use custom offset 9999, Y/Z should use eobs default
  expect_identical(
    r[, "X"],
    units::set_units((b[, "X"] - 9999) * sp$slope * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    r[, "Y"],
    units::set_units((b[, "Y"] - sp$offset) * sp$slope * GRAV_CONST, "m/s^2")
  )
})

test_that("user-provided orientation overrides manufacturer default", {
  # eobs gen 2 default orientation_y = -1; override to 1
  tf <- acc_calibration(manufacturer = "eobs", tag_id = 3000, orientation_y = 1)
  sp <- eobs_specs(3000)
  r <- tf[[1]](b)
  # Y should use orientation 1 (not the gen 2 default of -1)
  expect_identical(
    r[, "Y"],
    units::set_units((b[, "Y"] - sp$offset) * sp$slope * 1 * GRAV_CONST, "m/s^2")
  )
  # Confirm this differs from the default (orientation_y = -1)
  r_default <- acc_calibration(manufacturer = "eobs", tag_id = 3000)[[1]](b)
  expect_identical(r[, "Y"], r_default[, "Y"] * -1)
})

test_that("NA values fall through to manufacturer default", {
  tf <- acc_calibration(manufacturer = "eobs", tag_id = 3000, orientation_y = NA)
  tf_default <- acc_calibration(manufacturer = "eobs", tag_id = 3000)
  expect_identical(tf[[1]](b), tf_default[[1]](b))
})

# --- axes parameter -----------------------------------------------------------

test_that("axes restricts output to specified axes", {
  tf <- acc_calibration(offset = 2048, slope = 0.001, axes = "XY")
  result <- tf[[1]](b)
  expect_equal(colnames(result), c("X", "Y"))
  expect_equal(ncol(result), 2)
})

test_that("axes suppresses NA warnings for excluded axes", {
  tf <- acc_calibration(offset_x = 2048, slope = 0.001, axes = "X")
  expect_no_warning(tf[[1]](b))
})

test_that("axes warns on missing params for included axes", {
  tf <- acc_calibration(offset_x = 2048, slope = 0.001, axes = "XY")
  expect_warning(tf[[1]](b), "Missing calibration parameters")
})

test_that("axes accepts lowercase and whitespace", {
  tf <- acc_calibration(offset = 2048, slope = 0.001, axes = "x y")
  result <- tf[[1]](b)
  expect_equal(colnames(result), c("X", "Y"))
})

test_that("per-axis params with omitted axes calibrate correctly", {
  cal <- acc_calibration(
    offset_x = 1000, offset_y = 900,
    slope_x = 0.001, slope_y = 0.002
  )

  result <- suppressWarnings(cal[[1]](b))

  # X and Y are calibrated
  expect_equal(as.numeric(result[, "X"]), (b[, "X"] - 1000) * 0.001 * GRAV_CONST)
  expect_equal(as.numeric(result[, "Y"]), (b[, "Y"] - 900) * 0.002 * GRAV_CONST)

  # Z has no params, so produces NAs
  expect_true(all(is.na(result[, "Z"])))
})

test_that("axes vectorizes across elements", {
  tf <- acc_calibration(
    offset = c(2048, 2048),
    slope = 0.001,
    axes = c("XYZ", "XY")
  )
  expect_equal(ncol(tf[[1]](b)), 3)
  expect_equal(ncol(tf[[2]](b)), 2)
})

# --- as_acc_calibration() ------------------------------------------------

test_that("as_acc_calibration() creates functions from data.frame", {
  df <- data.frame(tag_id = c(1000, NA), manufacturer = c("eobs", "ornitela"))
  tf <- as_acc_calibration(df)
  expect_length(tf, 2)
  expect_true(all(purrr::map_lgl(tf, is.function)))
})

test_that("as_acc_calibration() scalar col fills missing axis cols", {
  df <- data.frame(tag_id = 1, offset = 2048, offset_x = NA_real_, slope = 0.001)
  tf <- as_acc_calibration(df)
  tf_ref <- acc_calibration(offset = 2048, slope = 0.001)
  expect_identical(tf[[1]](b), tf_ref[[1]](b))
})

test_that("as_acc_calibration() axis-specific col overrides scalar", {
  df <- data.frame(tag_id = 1, offset = 2048, offset_x = 9999, slope = 0.001)
  tf <- as_acc_calibration(df)
  r <- tf[[1]](b)
  # X should use axis-specific 9999, Y/Z should use scalar 2048
  expect_identical(
    r[, "X"],
    units::set_units((b[, "X"] - 9999) * 0.001 * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    r[, "Y"],
    units::set_units((b[, "Y"] - 2048) * 0.001 * GRAV_CONST, "m/s^2")
  )
})

test_that("as_acc_calibration() NA orientation falls back to manufacturer default", {
  df <- data.frame(tag_id = 3000, manufacturer = "eobs", orientation_y = NA_real_)
  tf <- as_acc_calibration(df)
  tf_default <- acc_calibration(manufacturer = "eobs", tag_id = 3000)
  expect_identical(tf[[1]](b), tf_default[[1]](b))
})

test_that("as_acc_calibration() silently ignores unrecognized columns", {
  df <- data.frame(tag_id = 1, offset = 2048, slope = 0.001, notes = "test")
  expect_silent(as_acc_calibration(df))
})

test_that("as_acc_calibration() errors on unrecognized manufacturer", {
  expect_error(
    as_acc_calibration(data.frame(tag_id = 1, manufacturer = "foobar", offset = 1, slope = 1)),
    "Unrecognized manufacturer"
  )
})

test_that("as_acc_calibration() errors when custom rows lack offset/slope", {
  expect_error(
    as_acc_calibration(data.frame(tag_id = 1)),
    "offset.*required|slope.*required"
  )
})

test_that("as_acc_calibration() handles mixed manufacturer and custom rows", {
  df <- data.frame(
    tag_id = c(1000, 3000, NA, 1),
    manufacturer = c("eobs", "eobs", "ornitela", NA),
    offset = c(NA, NA, NA, 100),
    slope = c(NA, NA, NA, 0.5),
    orientation_y = c(NA, 1, NA, -1)
  )
  tf <- as_acc_calibration(df)
  expect_length(tf, 4)

  r <- lapply(tf, function(f) f(b))

  # Row 1: eobs gen 1 defaults, orientation_y = 1
  sp1 <- eobs_specs(1000)
  expect_identical(r[[1]][, "Y"], units::set_units((b[, "Y"] - sp1$offset) * sp1$slope * GRAV_CONST, "m/s^2"))

  # Row 2: eobs gen 2, orientation_y overridden to 1 (default is -1)
  sp2 <- eobs_specs(3000)
  expect_identical(r[[2]][, "Y"], units::set_units((b[, "Y"] - sp2$offset) * sp2$slope * 1 * GRAV_CONST, "m/s^2"))

  # Row 3: ornitela defaults
  sp3 <- ornitela_specs()
  expect_identical(r[[3]], units::set_units((b - sp3$offset) * sp3$slope * GRAV_CONST, "m/s^2"))

  # Row 4: custom with orientation_y = -1
  expect_identical(r[[4]][, "Y"], units::set_units((b[, "Y"] - 100) * 0.5 * -1 * GRAV_CONST, "m/s^2"))
})

test_that("as_acc_calibration() works with no manufacturer column", {
  df <- data.frame(tag_id = c(1, 2), offset = c(2048, 100), slope = c(0.001, 0.5))
  tf <- as_acc_calibration(df)
  expect_length(tf, 2)

  r1 <- tf[[1]](b)
  r2 <- tf[[2]](b)
  expect_identical(r1, units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2"))
  expect_identical(r2, units::set_units(((b - 100) * 0.5) * GRAV_CONST, "m/s^2"))
})

test_that("as_acc_calibration() does 1:1 row-to-calibration conversion", {
  tf <- as_acc_calibration(
    data.frame(
      tag_id = c(1000, 1000), 
      manufacturer = "eobs",
      offset = c(2048, 2100), 
      slope = 0.001
    )
  )
  
  expect_length(tf, 2)
  expect_identical(
    tf[[1]](b),
    acc_calibration("eobs", tag_id = 1000, offset = 2048, slope = 0.001)[[1]](b)
  )
  expect_identical(
    tf[[2]](b),
    acc_calibration("eobs", tag_id = 1000, offset = 2100, slope = 0.001)[[1]](b)
  )
})

# --- eobs_specs() -------------------------------------------------------------

test_that("eobs_specs() returns correct defaults for gen 1 low sensitivity", {
  sp <- eobs_specs(100)
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 0.0027)
  expect_equal(sp$orientation_y, 1)
})

test_that("eobs_specs() returns correct defaults for gen 1 high sensitivity", {
  sp <- eobs_specs(100, sensitivity = "high")
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 0.001)
  expect_equal(sp$orientation_y, 1)
})

test_that("eobs_specs() returns correct defaults for gen 2", {
  sp <- eobs_specs(3000)
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 0.0022)
  expect_equal(sp$orientation_y, -1)
})

test_that("eobs_specs() returns correct defaults for gen 3", {
  sp <- eobs_specs(5000)
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 1 / 512)
  expect_equal(sp$orientation_y, -1)
})

test_that("eobs_specs() works with multiple tag_ids", {
  sp <- eobs_specs(c(100, 3000, 5000))
  expect_equal(nrow(sp), 3)
  expect_equal(sp$offset, c(2048, 2048, 2048))
  expect_equal(sp$slope, c(0.0027, 0.0022, 1 / 512))
  expect_equal(sp$orientation_y, c(1, -1, -1))
})

test_that("eobs_specs() works with mixed sensitivities", {
  sp <- eobs_specs(c(100, 100), sensitivity = c("low", "high"))
  expect_equal(nrow(sp), 2)
  expect_equal(sp$slope, c(0.0027, 0.001))
})

test_that("eobs_specs() errors on NA tag_id", {
  expect_error(eobs_specs(NA), "missing `tag_id`")
})

test_that("eobs_specs() errors on tag_id outside known ranges", {
  expect_error(eobs_specs(0), "Could not find")
})

test_that("eobs_specs() errors on invalid sensitivity value", {
  expect_error(eobs_specs(100, sensitivity = "medium"), "sensitivity")
})

# --- ornitela_specs() ---------------------------------------------------------

test_that("ornitela_specs() returns correct defaults", {
  sp <- ornitela_specs()
  expect_equal(sp$offset, 0)
  expect_equal(sp$slope, 0.001)
  expect_equal(sp$orientation_x, 1)
  expect_equal(sp$orientation_y, 1)
  expect_equal(sp$orientation_z, 1)
})

# --- eobs_default_specs() -----------------------------------------------------

test_that("eobs_default_specs() tag_id ranges do not have gaps or overlaps", {
  config <- eobs_default_specs()
  config <- config[config$sensitivity == "low", ]

  for (i in seq_len(nrow(config) - 1)) {
    expect_true(config$max_tag_id[i] == config$min_tag_id[i + 1] - 1)
  }
})
