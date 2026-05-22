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

test_that("when N does not contain a whole number of cycles, the reported peak is the nearest FFT bin", {
  # Period-5 and period-100 sinusoids over N=199 samples. N is not a
  # multiple of either period, so no FFT bin lands exactly on the true
  # frequency. The reported peak should be the nearest available bin:
  # bin * fs / N, where bin is round(N/period).
  N <- 199
  x <- sin(seq_len(N) / (5 / (pi * 2)))
  z <- cos(seq_len(N) / (100 / (pi * 2)))
  acc_l <- acc_burst_example(x = x, z = z)

  for (fs in c(100, 200, 400)) {
    a <- acc(acc_l, units::set_units(fs, "Hz"))
    expect_equal(
      peak_frequency(a),
      list(units::set_units(c(X = 40, Z = 2) * fs / N, "Hz"))
    )
  }
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

test_that("non-integer fs/resolution rounds up rather than truncating", {
  # When freq / resolution is not round, we want to give a slightly finer
  # grid than requested.
  N <- 200
  fs <- 200
  m <- cbind(X = sin(2 * pi * seq_len(N) / 16))
  a <- acc(list(m), units::set_units(fs, "Hz"))

  # A 12.5 Hz tone lands closest to bin 42 of the 667-point FFT.
  expect_equal(
    peak_frequency(a, resolution = units::set_units(0.3, "Hz")),
    list(units::set_units(c(X = 42 * fs / 667), "Hz"))
  )
})

test_that("Use natural frequency when resolution too coarse", {
  fs <- 200
  m_short <- cbind(X = sin(2 * pi * seq_len(100) / 20)) # natural = 2 Hz
  m_long <- cbind(X = sin(2 * pi * seq_len(400) / 20)) # natural = 0.5 Hz
  a <- acc(list(m_short, m_long), units::set_units(fs, "Hz"))

  # Request 1 Hz: short burst (natural 2 Hz) gets padded; long burst
  # (natural 0.5 Hz) is already finer than requested and falls back.
  expect_warning(
    out <- peak_frequency(a, resolution = units::set_units(1, "Hz")),
    "1 of 2"
  )
  expect_length(out, 2)
  # Long-burst peak matches the no-resolution result on its natural grid.
  expect_equal(out[[2]], peak_frequency(a)[[2]])
})

test_that("peak_frequency returns NA for NA elements", {
  a <- acc_example()
  a_na <- c(a[1], acc(list(NULL), units::set_units(NA, "Hz")), a[2])

  result <- peak_frequency(a_na)

  expect_length(result, 3)
  expect_identical(result[[2]], NA_real_)
  expect_false(is.na(result[[1]][[1]]))
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
