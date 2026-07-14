test_that("Correct frequency found", {
  a <- acc(
    acc_burst_example(sin(1:200 / (50 / (pi * 2)))),
    units::set_units(200, "Hz")
  )

  expect_equal(peak_frequency(a), list(units::set_units(c(X = 4), "Hz")))
})

test_that("Multiple axis peak freq and changing freq", {
  x <- sin(1:200 / (5 / (pi * 2)))
  z <- cos(1:200 / (100 / (pi * 2)))

  acc_l <- acc_burst_example(x = x, z = z)

  a <- acc(acc_l, units::set_units(100, "Hz"))
  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 20, Z = 1), "Hz"))
  )

  a <- acc(acc_l, units::set_units(200, "Hz"))
  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 40, Z = 2), "Hz"))
  )

  a <- acc(acc_l, units::set_units(400, "Hz"))
  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 80, Z = 4), "Hz"))
  )
})

test_that("length does not influnce result", {
  x <- sin(1:199 / (5 / (pi * 2)))
  z <- cos(1:199 / (100 / (pi * 2)))

  acc_l <- acc_burst_example(x = x, z = z)

  a <- acc(acc_l, units::set_units(100, "Hz"))
  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 20, Z = 1), "Hz"))
  )

  a <- acc(acc_l, units::set_units(200, "Hz"))
  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 40, Z = 2), "Hz"))
  )

  a <- acc(acc_l, units::set_units(400, "Hz"))
  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 80, Z = 4), "Hz"))
  )
})

test_that("Multiple axis peak freq intercept does not matter", {
  x <- 3 * (2 + sin(1:200 / (50 / (pi * 2))))
  z <- -3 + (.1 * cos(1:200 / (100 / (pi * 2))))

  a <- acc(acc_burst_example(x = x, z = z), units::set_units(200, "Hz"))
  expect_equal(peak_frequency(a), list(units::set_units(c(X = 4, Z = 2), "Hz")))
})

test_that("Resolution alows to identify partial frequencies", {
  x <- sin(1:200 / (5 / (pi * 2)))
  z <- cos(1:200 / (80 / (pi * 2)))

  a <- acc(acc_burst_example(x = x, z = z), units::set_units(200, "Hz"))

  expect_equal(
    peak_frequency(a),
    list(units::set_units(c(X = 40, Z = 3), "Hz"))
  )
  expect_equal(
    peak_frequency(a, resolution = units::set_units(.5, "Hz")),
    list(units::set_units(c(X = 40, Z = 2.5), "Hz"))
  )
  expect_equal(
    peak_frequency(a, resolution = units::set_units(.25, "Hz")),
    list(units::set_units(c(X = 40, Z = 2.5), "Hz"))
  )
})

test_that("Resolution alows to identify partial frequencies", {
  m <- matrix(runif(30), ncol = 3)
  colnames(m) <- c("X", "Y", "Z")

  a <- acc(list(m), units::set_units(23, "Hz"))

  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(.005, "Hz"))
  ))
  expect_equal((((p / .005) + .5) %% 1) - .5, rep(0, 3))

  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(.025, "Hz"))
  ))
  expect_equal((((p / .025) + .5) %% 1) - .5, rep(0, 3))

  m <- matrix(runif(300), ncol = 3)
  colnames(m) <- c("X", "Y", "Z")

  a <- acc(list(m), units::set_units(23, "Hz"))

  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(.005, "Hz"))
  ))
  expect_equal((((p / .005) + .5) %% 1) - .5, rep(0, 3))

  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(0.025, "Hz"))
  ))
  expect_equal((((p / .025) + .5) %% 1) - .5, rep(0, 3))
})

test_that("peak_frequency returns NA for NA elements", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])

  result <- peak_frequency(a_na)

  expect_length(result, 3)
  expect_identical(result[[2]], NA_real_)
  expect_false(is.na(result[[1]][[1]]))
})

test_that("peak_frequency returns NA for a burst with a missing frequency", {
  a <- acc(list(cbind(X = sin(1:64 / 3))), units::set_units(NA, "Hz"))

  expect_identical(peak_frequency(a), list(NA_real_))
  expect_identical(
    peak_frequency(a, resolution = units::set_units(.5, "Hz")),
    list(NA_real_)
  )

  ok <- acc(acc_burst_example(sin(1:64 / 3)), units::set_units(20, "Hz"))
  res <- peak_frequency(c(ok, a))
  expect_false(is.na(res[[1]][[1]]))
  expect_identical(res[[2]], NA_real_)
})

test_that("works with and without units", {
  acc_l <- acc_burst_example(c(1:5, 5:1, 1:5), rep(c(4, 3, 4), 5))

  a <- acc(
    acc_l,
    units::set_units(23, "Hz")
  )

  b <- acc(
    list(units::set_units(acc_l[[1]], "m/s")),
    units::set_units(23, "Hz")
  )

  expect_equal(peak_frequency(a), peak_frequency(b))
})

test_that("peak_frequency is unit-equivalent across convertible frequency units", {
  # Same physical signal at 200 Hz vs 12000/min
  m <- acc_burst_example(sin(1:200 / (100 / (pi * 2))))

  a_hz <- acc(m, units::set_units(200, "Hz"))
  a_min <- acc(m, units::set_units(12000, "1/min"))

  # Without resolution
  p_hz <- peak_frequency(a_hz)[[1]]
  p_min <- peak_frequency(a_min)[[1]]
  expect_equal(
    units::set_units(p_min, "Hz", mode = "standard"),
    p_hz
  )

  # With resolution
  res <- units::set_units(0.5, "Hz")
  p_hz_r <- peak_frequency(a_hz, resolution = res)[[1]]
  p_min_r <- peak_frequency(a_min, resolution = res)[[1]]
  expect_equal(
    units::set_units(p_min_r, "Hz", mode = "standard"),
    p_hz_r
  )
})
