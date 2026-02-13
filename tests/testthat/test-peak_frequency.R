test_that("Correct frequency found", {
  a <- acc(
    acc_burst_example(sin(1:200 / (50 / (pi * 2)))),
    units::set_units(200,"Hz")
  )
  
  expect_equal(peak_frequency(a), list(units::set_units(c(X = 4),"Hz")))
})

test_that("Multiple axis peak freq and changing freq", {
  x <- sin(1:200 / (5   / (pi * 2)))
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
  x <- sin(1:199 / (5   / (pi * 2)))
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
  x <- 3  * (2  + sin(1:200 / (50  / (pi * 2))))
  z <- -3 + (.1 * cos(1:200 / (100 / (pi * 2))))
  
  a <- acc(acc_burst_example(x = x, z = z), units::set_units(200, "Hz"))
  expect_equal(peak_frequency(a), list(units::set_units(c(X = 4, Z = 2), "Hz")))
})

test_that("NA returns empty", {
  expect_equal(
    peak_frequency(new_acc(list(NULL), frequency = NA, start = as.POSIXct(NA), id = NA)), 
    list(NULL)
  )
  expect_equal(
    peak_frequency(new_acc(list(NULL, NULL), frequency = c(NA, NA), start = rep(as.POSIXct(NA), 2), id = c(NA, NA))), 
    list(NULL, NULL)
  )
  
  x <- sin(1:200 / (50 / (pi * 2)))
  a <- acc(
    c(acc_burst_example(), acc_burst_example(x = x), acc_burst_example()),
    frequency = units::set_units(c(NA, 200, NA),"Hz")
  )
  
  expect_equal(
    peak_frequency(a),
    list(NULL, units::set_units(c(X = 4), "Hz"), NULL)
  )
})

test_that("Resolution alows to identify partial frequencies", {
  x <- sin(1:200 / (5  / (pi * 2)))
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
  m <- matrix(runif(100), ncol=10)
  colnames(m) <- LETTERS[1:10]
  
  a <- acc(list(m), units::set_units(23, "Hz"))
  
  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(.005, "Hz"))
  ))
  expect_equal((((p / .005) + .5) %% 1) - .5, rep(0, 10))
  
  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(.025, "Hz"))
  ))
  expect_equal((((p / .025) + .5) %% 1) - .5, rep(0, 10))
  
  m <- matrix(runif(1000), ncol=10)
  colnames(m) <- LETTERS[1:10]
  
  a <- acc(list(m), units::set_units(23, "Hz"))
  
  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(.005,"Hz"))
  ))
  expect_equal((((p / .005) + .5) %% 1) - .5, rep(0, 10))
  
  p <- unname(unlist(
    peak_frequency(a, resolution = units::set_units(0.025,"Hz"))
  ))
  expect_equal((((p / .025) + .5) %% 1) - .5, rep(0, 10))
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
