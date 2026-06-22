b <- bursts(acc_example())[[1]]

# --- acc_calibration() ---------------------------------------------------

test_that("acc_calibration() returns an acc_calibration object", {
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  expect_s3_class(cal, "acc_calibration")
  expect_s3_class(cal, "imu_calibration")
  expect_s3_class(cal, "vctrs_rcrd")
  expect_length(cal, 1)
  expect_named(
    vctrs::vec_data(cal),
    c(
      "offset_x", "offset_y", "offset_z", "slope_x", "slope_y", "slope_z",
      "orientation_x", "orientation_y", "orientation_z", "units"
    )
  )
})

test_that("acc_calibration() prints with its sensor-specific class", {
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  expect_output(print(cal), "<acc_calibration\\[1\\]>")
})

test_that("format() collapses uniform axes and hides default orientation", {
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  expect_identical(format(cal), "{offset=[2048] slope=[0.001]}")
  expect_false(grepl("orient", format(cal)))
})

test_that("format() expands differing axes and shows a flipped orientation", {
  cal <- acc_calibration(offset_x = 2048, offset_y = 2046, slope = 1 / 512, orientation_y = -1)
  out <- format(cal)
  expect_match(out, "offset=[2048, 2046, NA]", fixed = TRUE)
  expect_match(out, "slope=[0.00195]", fixed = TRUE)
  expect_match(out, "orientation=[1, -1, 1]", fixed = TRUE)
})

test_that("format() renders a missing calibration as NA", {
  cal <- suppressWarnings(as_acc_calibration(
    data.frame(manufacturer = c("ornitela", "foobar"), offset = c(NA, 1), slope = c(NA, 1))
  ))
  expect_false(is.na(format(cal)[1]))
  expect_identical(format(cal)[2], NA_character_)
})

test_that("acc_calibration abbreviates and renders compactly in columns", {
  skip_if_not_installed("pillar")
  cal <- suppressWarnings(as_acc_calibration(
    data.frame(manufacturer = c("ornitela", "foobar"), offset = c(NA, 1), slope = c(NA, 1))
  ))
  expect_equal(vctrs::vec_ptype_abbr(cal), "acc_cal")

  cells <- format(pillar::pillar_shaft(cal), width = 12)
  expect_match(cells[1], "acc_cal")
  expect_match(cells[2], "NA")
})

test_that("as_acc_calibration() returns an acc_calibration object", {
  df <- data.frame(manufacturer = "ornitela")
  cal <- as_acc_calibration(df)
  expect_s3_class(cal, "acc_calibration")
})

test_that("acc_calibration() vectorizes arguments", {
  cal <- acc_calibration(offset = c(2048, 2000), slope = 0.001)
  expect_length(cal, 2)
})

test_that("acc_calibration() with no specs returns a length-0 vector", {
  expect_silent(cal <- acc_calibration())
  expect_s3_class(cal, "acc_calibration")
  expect_length(cal, 0)
})

test_that("format() of a length-0 calibration is character(0)", {
  expect_identical(format(acc_calibration()), character(0))
  expect_output(print(acc_calibration()), "<acc_calibration\\[0\\]>")
})

test_that("acc_calibration() applies offset and slope correctly (m/s^2)", {
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  result <- transform_burst(cal[1], b)
  manual <- units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2")
  expect_identical(result, manual)
})

test_that("acc_calibration() applies offset and slope correctly (gravity)", {
  cal <- acc_calibration(offset = 2048, slope = 0.001, units = "standard_free_fall")
  result <- transform_burst(cal[1], b)
  manual <- units::set_units(((b - 2048) * 0.001), "standard_free_fall")
  expect_identical(result, manual)
})

test_that("acc_calibration() applies different calibrations when vectorized", {
  cal <- acc_calibration(offset = c(2048, 0), slope = c(0.001, 1))
  r1 <- transform_burst(cal[1], b)
  r2 <- transform_burst(cal[2], b)
  expect_identical(r1, units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2"))
  expect_identical(r2, units::set_units(((b - 0) * 1) * GRAV_CONST, "m/s^2"))
})

test_that("acc_calibration() applies scalar orientation correctly", {
  cal <- acc_calibration(offset = 2048, slope = 0.001, orientation = -1)
  result <- transform_burst(cal[1], b)
  manual <- units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2")

  expect_identical(result[, 1], manual[, 1] * -1)
  expect_identical(result[, 2], manual[, 2] * -1)
  expect_identical(result[, 3], manual[, 3] * -1)
})

test_that("acc_calibration() applies per-axis orientation", {
  cal <- acc_calibration(offset = 2048, slope = 0.001, orientation_y = -1)
  result <- transform_burst(cal[1], b)
  manual <- units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2")

  expect_identical(result[, "X"], manual[, "X"])
  expect_identical(result[, "Y"], manual[, "Y"] * -1)
  expect_identical(result[, "Z"], manual[, "Z"])
})

test_that("acc_calibration() applies per-axis offset and slope", {
  cal <- acc_calibration(
    offset_x = 0, offset_y = 2048, offset_z = 2000,
    slope_x = 1, slope_y = 0.001, slope_z = 0.001
  )
  result <- transform_burst(cal[1], b)

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
  cal <- acc_calibration(offset = 0, slope = 1)
  result <- transform_burst(cal[1], b)
  expect_true(inherits(result, "units"))
})

test_that("acc_calibration() warns and returns NA on invalid units", {
  expect_warning(
    cal <- acc_calibration(offset = 0, slope = 1, units = "feet"),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

test_that("acc_calibration() warns and returns NA when no manufacturer and no offset", {
  expect_warning(
    cal <- acc_calibration(slope = 0.001),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

test_that("acc_calibration() warns and returns NA when no manufacturer and no slope", {
  expect_warning(
    cal <- acc_calibration(offset = 2048),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

test_that("acc_calibration() needs offset and slope on the same axis", {
  # offset on X but slope on Y calibrates nothing -> NA, not a usable cal
  expect_warning(
    cal <- acc_calibration(offset_x = 10, slope_y = 1),
    "could not be resolved"
  )
  expect_true(is.na(cal))

  # both on the same axis is a usable (partial) calibration
  expect_false(is.na(acc_calibration(offset_x = 10, slope_x = 1)))
})

test_that("acc_calibration() warns and returns NA on invalid orientation value", {
  expect_warning(
    cal <- acc_calibration(offset = 2048, slope = 0.001, orientation = 0),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

test_that("acc_calibration() warns and returns NA on unrecognized manufacturer", {
  expect_warning(
    cal <- acc_calibration(manufacturer = "foobar", offset = 1, slope = 1),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

test_that("acc_calibration() eobs warns and returns NA without tag_id", {
  expect_warning(
    cal <- acc_calibration(manufacturer = "eobs"),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

# --- Manufacturer defaults ---------------------------------------------------

test_that("acc_calibration() with eobs uses correct defaults per generation", {
  sp1 <- eobs_specs(1000)
  sp3 <- eobs_specs(5000)

  cal <- acc_calibration(manufacturer = "eobs", tag_id = c(1000, 5000))

  # Gen 1 (1000) has orientation_y = -1, gen 3 (5000) has orientation_y = 1
  # Y axis should have opposite signs
  y1 <- as.numeric(transform_burst(cal[1], b)[1, "Y"])
  y3 <- as.numeric(transform_burst(cal[2], b)[1, "Y"])
  expect_true(sign(y1) != sign(y3))
})

test_that("eobs_specs() resolves a factor tag_id by label, not integer code", {
  expect_identical(eobs_specs(factor(c("1000", "5000"))), eobs_specs(c(1000, 5000)))

  fac <- suppressWarnings(as_acc_calibration(
    data.frame(manufacturer = "eobs", tag_id = factor(c("1000", "5000")))
  ))
  num <- suppressWarnings(as_acc_calibration(
    data.frame(manufacturer = "eobs", tag_id = c(1000, 5000))
  ))
  expect_identical(fac, num)
})

test_that("acc_calibration() with ornitela uses correct defaults", {
  cal <- acc_calibration(manufacturer = "ornitela")
  sp <- ornitela_specs()
  result <- transform_burst(cal[1], b)
  manual <- units::set_units(((b - sp$offset) * sp$slope) * GRAV_CONST, "m/s^2")
  expect_identical(result, manual)
})

# --- User override of manufacturer defaults -----------------------------------

test_that("user-provided offset overrides manufacturer default", {
  cal <- acc_calibration(manufacturer = "eobs", tag_id = 1000, offset_x = 9999)
  sp <- eobs_specs(1000)
  r <- transform_burst(cal[1], b)
  # X should use custom offset 9999, Y/Z should use eobs default
  expect_identical(
    r[, "X"],
    units::set_units((b[, "X"] - 9999) * sp$slope * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    r[, "Y"],
    units::set_units((b[, "Y"] - sp$offset) * sp$slope * sp$orientation_y * GRAV_CONST, "m/s^2")
  )
})

test_that("user-provided orientation overrides manufacturer default", {
  # eobs gen 2 default orientation_y = 1; override to -1
  cal <- acc_calibration(manufacturer = "eobs", tag_id = 3000, orientation_y = -1)
  sp <- eobs_specs(3000)
  r <- transform_burst(cal[1], b)
  # Y should use orientation -1 (not the gen 2 default of 1)
  expect_identical(
    r[, "Y"],
    units::set_units((b[, "Y"] - sp$offset) * sp$slope * -1 * GRAV_CONST, "m/s^2")
  )
  # Confirm this differs from the default (orientation_y = 1)
  r_default <- transform_burst(acc_calibration(manufacturer = "eobs", tag_id = 3000)[1], b)
  expect_identical(r[, "Y"], r_default[, "Y"] * -1)
})

test_that("NA values fall through to manufacturer default", {
  cal <- acc_calibration(manufacturer = "eobs", tag_id = 3000, orientation_y = NA)
  cal_default <- acc_calibration(manufacturer = "eobs", tag_id = 3000)
  expect_identical(transform_burst(cal[1], b), transform_burst(cal_default[1], b))
})

# --- output axes (dimension-preserving) ---------------------------------------

test_that("transform preserves the burst's columns", {
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  result <- transform_burst(cal[1], b)
  expect_equal(colnames(result), colnames(b))
  expect_equal(ncol(result), ncol(b))
})

test_that("transform does not warn when every burst axis is calibrated", {
  cal <- acc_calibration(offset = 2048, slope = 0.001)
  expect_no_warning(transform_burst(cal[1], b))
})

test_that("transform warns and NA-fills burst axes with no calibration params", {
  # Only X has both offset and slope; Y/Z have no params
  cal <- acc_calibration(offset_x = 2048, slope_x = 0.001)
  expect_warning(result <- transform_burst(cal[1], b), "Missing calibration parameters")
  expect_equal(colnames(result), colnames(b)) # dims preserved
  expect_false(any(is.na(result[, "X"])))
  expect_true(all(is.na(result[, "Y"])))
  expect_true(all(is.na(result[, "Z"])))
})

test_that("per-axis params with omitted axes calibrate correctly", {
  cal <- acc_calibration(
    offset_x = 1000, offset_y = 900,
    slope_x = 0.001, slope_y = 0.002
  )

  result <- suppressWarnings(transform_burst(cal[1], b))

  # X and Y are calibrated
  expect_equal(as.numeric(result[, "X"]), (b[, "X"] - 1000) * 0.001 * GRAV_CONST)
  expect_equal(as.numeric(result[, "Y"]), (b[, "Y"] - 900) * 0.002 * GRAV_CONST)

  # Z has no params, so produces NAs
  expect_true(all(is.na(result[, "Z"])))
})

# --- units --------------------------------------------------------------------

test_that("units vectorizes independently of scalar specs", {
  # Regression: scalar specs + vectorized units must not error on recycling
  cal <- acc_calibration(
    offset = 2048,
    slope = 0.001,
    units = c("m/s^2", "standard_free_fall")
  )
  expect_length(cal, 2)
  expect_identical(
    transform_burst(cal[1], b),
    units::set_units((b - 2048) * 0.001 * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    transform_burst(cal[2], b),
    units::set_units((b - 2048) * 0.001, "standard_free_fall")
  )
})

test_that("units alone (no specs) returns a length-0 vector", {
  expect_silent(cal <- acc_calibration(units = c("m/s^2", "standard_free_fall")))
  expect_length(cal, 0)
})

test_that("an explicit NA units warns and returns NA", {
  # Unlike a stray NA in an `as_acc_calibration()` column, an explicit
  # `units = NA` is an unclear request and is treated as invalid.
  expect_warning(
    cal <- acc_calibration(offset = 2048, slope = 0.001, units = NA),
    "could not be resolved"
  )
  expect_true(is.na(cal))
})

# --- as_acc_calibration() ------------------------------------------------

test_that("as_acc_calibration() creates a calibration from a data.frame", {
  df <- data.frame(tag_id = c(1000, NA), manufacturer = c("eobs", "ornitela"))
  cal <- as_acc_calibration(df)
  expect_s3_class(cal, "acc_calibration")
  expect_length(cal, 2)
  expect_false(any(is.na(cal)))
})

test_that("as_acc_calibration() scalar col fills missing axis cols", {
  df <- data.frame(tag_id = 1, offset = 2048, offset_x = NA_real_, slope = 0.001)
  cal <- as_acc_calibration(df)
  cal_ref <- acc_calibration(offset = 2048, slope = 0.001)
  expect_identical(transform_burst(cal[1], b), transform_burst(cal_ref[1], b))
})

test_that("as_acc_calibration() axis-specific col overrides scalar", {
  df <- data.frame(tag_id = 1, offset = 2048, offset_x = 9999, slope = 0.001)
  cal <- as_acc_calibration(df)
  r <- transform_burst(cal[1], b)
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
  cal <- as_acc_calibration(df)
  cal_default <- acc_calibration(manufacturer = "eobs", tag_id = 3000)
  expect_identical(transform_burst(cal[1], b), transform_burst(cal_default[1], b))
})

test_that("as_acc_calibration() silently ignores unrecognized columns", {
  df <- data.frame(tag_id = 1, offset = 2048, slope = 0.001, notes = "test")
  expect_silent(as_acc_calibration(df))
})

test_that("as_acc_calibration() warns and returns NA for rows it cannot build", {
  expect_warning(
    cal <- as_acc_calibration(
      data.frame(
        manufacturer = c("ornitela", "foobar", NA),
        offset = c(NA, 1, NA), slope = c(NA, 1, NA)
      )
    ),
    "for 2 calibrations"
  )
  expect_length(cal, 3)
  expect_false(is.na(cal)[1])
  expect_true(is.na(cal)[2])
  expect_true(is.na(cal)[3])
})

test_that("as_acc_calibration() keeps its type when every row is unresolved", {
  # All-NA results must still be an `acc_calibration`, not collapse to logical
  cal <- suppressWarnings(as_acc_calibration(data.frame(notes = c("a", "b"))))
  expect_s3_class(cal, "acc_calibration")
  expect_length(cal, 2)
  expect_true(all(is.na(cal)))
})

test_that("acc_calibration() and as_acc_calibration() agree on unresolved entries", {
  vec <- suppressWarnings(
    acc_calibration(manufacturer = c("eobs", "eobs"), tag_id = c(1000, NA))
  )
  df <- suppressWarnings(
    as_acc_calibration(data.frame(manufacturer = "eobs", tag_id = c(1000, NA)))
  )
  expect_identical(is.na(vec), is.na(df))
  expect_identical(
    transform_burst(vec[1], b),
    transform_burst(df[1], b)
  )
})

test_that("acc_calibration() warns once for multiple unresolved entries", {
  expect_warning(
    cal <- acc_calibration(manufacturer = "eobs", tag_id = c(NA, NA, 1000)),
    "for 2 calibrations"
  )
  expect_equal(is.na(cal), c(TRUE, TRUE, FALSE))
})

test_that("unresolved warning lists each distinct reason as a bullet", {
  w <- tryCatch(
    as_acc_calibration(data.frame(
      manufacturer = c("ornitela", "foobar", NA),
      offset = c(NA, 1, NA), slope = c(NA, 1, NA)
    )),
    warning = function(w) w
  )
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_match(msg, "Unrecognized manufacturer")
  expect_match(msg, "needs both an `offset` and a `slope`")
})

test_that("unresolved warning collapses identical reasons", {
  w <- tryCatch(acc_calibration(slope = rep(0.001, 50)), warning = function(w) w)
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_equal(lengths(regmatches(msg, gregexpr("offset.*slope", msg)))[1], 1L)
})

test_that("unresolved warning caps the reason list with an overflow line", {
  w <- tryCatch(
    as_acc_calibration(data.frame(
      manufacturer = letters[1:7], offset = 1, slope = 1
    )),
    warning = function(w) w
  )
  msg <- cli::ansi_strip(conditionMessage(w))
  expect_match(msg, "and 2 more reasons")
})

test_that("as_acc_calibration() handles mixed manufacturer and custom rows", {
  df <- data.frame(
    tag_id = c(1000, 3000, NA, 1),
    manufacturer = c("eobs", "eobs", "ornitela", NA),
    offset = c(NA, NA, NA, 100),
    slope = c(NA, NA, NA, 0.5),
    orientation_y = c(NA, -1, NA, -1)
  )
  cal <- as_acc_calibration(df)
  expect_length(cal, 4)

  r <- lapply(seq_along(cal), function(i) transform_burst(cal[i], b))

  # Row 1: eobs gen 1 defaults, orientation_y = -1
  sp1 <- eobs_specs(1000)
  expect_identical(r[[1]][, "Y"], units::set_units((b[, "Y"] - sp1$offset) * sp1$slope * sp1$orientation_y * GRAV_CONST, "m/s^2"))

  # Row 2: eobs gen 2, orientation_y overridden to -1 (default is 1)
  sp2 <- eobs_specs(3000)
  expect_identical(r[[2]][, "Y"], units::set_units((b[, "Y"] - sp2$offset) * sp2$slope * -1 * GRAV_CONST, "m/s^2"))

  # Row 3: ornitela defaults
  sp3 <- ornitela_specs()
  expect_identical(r[[3]], units::set_units((b - sp3$offset) * sp3$slope * GRAV_CONST, "m/s^2"))

  # Row 4: custom with orientation_y = -1
  expect_identical(r[[4]][, "Y"], units::set_units((b[, "Y"] - 100) * 0.5 * -1 * GRAV_CONST, "m/s^2"))
})

test_that("as_acc_calibration() works with no manufacturer column", {
  df <- data.frame(tag_id = c(1, 2), offset = c(2048, 100), slope = c(0.001, 0.5))
  cal <- as_acc_calibration(df)
  expect_length(cal, 2)

  r1 <- transform_burst(cal[1], b)
  r2 <- transform_burst(cal[2], b)
  expect_identical(r1, units::set_units(((b - 2048) * 0.001) * GRAV_CONST, "m/s^2"))
  expect_identical(r2, units::set_units(((b - 100) * 0.5) * GRAV_CONST, "m/s^2"))
})

test_that("as_acc_calibration() does 1:1 row-to-calibration conversion", {
  cal <- as_acc_calibration(
    data.frame(
      tag_id = c(1000, 1000),
      manufacturer = "eobs",
      offset = c(2048, 2100),
      slope = 0.001
    )
  )

  expect_length(cal, 2)
  expect_identical(
    transform_burst(cal[1], b),
    transform_burst(acc_calibration("eobs", tag_id = 1000, offset = 2048, slope = 0.001)[1], b)
  )
  expect_identical(
    transform_burst(cal[2], b),
    transform_burst(acc_calibration("eobs", tag_id = 1000, offset = 2100, slope = 0.001)[1], b)
  )
})

test_that("as_acc_calibration() treats NA units as the default", {
  # A stray NA in a units column (e.g. after a join) must not invalidate an
  # otherwise complete calibration spec.
  cal_u <- as_acc_calibration(data.frame(offset = 2048, slope = 0.001, units = NA))
  expect_false(is.na(cal_u)[1])
  expect_identical(
    transform_burst(cal_u[1], b),
    transform_burst(acc_calibration(offset = 2048, slope = 0.001)[1], b)
  )
})

test_that("as_acc_calibration() reads per-row units from a column", {
  cal <- as_acc_calibration(data.frame(
    offset = 2048,
    slope = 0.001,
    units = c("m/s^2", "standard_free_fall")
  ))
  expect_length(cal, 2)
  expect_identical(
    transform_burst(cal[1], b),
    units::set_units((b - 2048) * 0.001 * GRAV_CONST, "m/s^2")
  )
  expect_identical(
    transform_burst(cal[2], b),
    units::set_units((b - 2048) * 0.001, "standard_free_fall")
  )
})

# --- eobs_specs() -------------------------------------------------------------

test_that("eobs_specs() returns correct defaults for gen 1 low sensitivity", {
  sp <- eobs_specs(100)
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 0.0027)
  expect_equal(sp$orientation_y, -1)
})

test_that("eobs_specs() returns correct defaults for gen 1 high sensitivity", {
  sp <- eobs_specs(100, sensitivity = "high")
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 0.001)
  expect_equal(sp$orientation_y, -1)
})

test_that("eobs_specs() returns correct defaults for gen 2", {
  sp <- eobs_specs(3000)
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 0.0022)
  expect_equal(sp$orientation_y, 1)
})

test_that("eobs_specs() returns correct defaults for gen 3", {
  sp <- eobs_specs(5000)
  expect_equal(sp$offset, 2048)
  expect_equal(sp$slope, 1 / 512)
  expect_equal(sp$orientation_y, 1)
})

test_that("eobs_specs() works with multiple tag_ids", {
  sp <- eobs_specs(c(100, 3000, 5000))
  expect_equal(nrow(sp), 3)
  expect_equal(sp$offset, c(2048, 2048, 2048))
  expect_equal(sp$slope, c(0.0027, 0.0022, 1 / 512))
  expect_equal(sp$orientation_y, c(-1, 1, 1))
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
