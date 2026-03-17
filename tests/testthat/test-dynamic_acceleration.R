test_that("Can calcluate vedba (with units)", {
  a <- acc_example()
  field(a, "bursts") <- map_acc(a, ~ units::set_units(.br, "m/s^2"))
  
  result <- vedba(a)
  
  expect_s3_class(result[[1]], "units")
  expect_equal(units(result[[1]]), units(bursts(a)[[1]]))
  expect_equal(as.numeric(result[[1]]), mean(sqrt(c(6.75, 0.75, 0.75, 6.75))))
  expect_equal(as.numeric(result[[2]]), mean(sqrt(c(4.5, 0.5, 0.5, 4.5))))
})

test_that("Can calculate odba (with units)", {
  a <- acc_example()
  field(a, "bursts") <- map_acc(a, ~ units::set_units(.br, "m/s^2"))
  
  result <- odba(a)
  
  expect_s3_class(result[[1]], "units")
  expect_equal(units(result[[1]]), units(bursts(a)[[1]]))
  expect_equal(as.numeric(result[[1]]), mean(c(4.5, 1.5, 1.5, 4.5)))
  expect_equal(as.numeric(result[[2]]), mean(c(3, 1, 1, 3)))
})

test_that("Can calculate vedba (without units)", {
  a <- acc_example()
  result <- vedba(a)
  
  expect_length(result, length(a))
  expect_true(all(result >= 0))
  
  expect_equal(result[[1]], mean(sqrt(c(6.75, 0.75, 0.75, 6.75))))
  expect_equal(result[[2]], mean(sqrt(c(4.5, 0.5, 0.5, 4.5))))
})

test_that("Can calculate odba (without units)", {
  a <- acc_example()
  result <- odba(a)
  
  expect_length(result, length(a))
  expect_true(all(result >= 0))
  
  expect_equal(result[[1]], mean(c(4.5, 1.5, 1.5, 4.5)))
  expect_equal(result[[2]], mean(c(3, 1, 1, 3)))
})

test_that("Single-sample burst returns zero", {
  a <- acc(
    acc_burst_example(1, 2, 3), 
    frequency = units::set_units(10, "Hz"),
    start = as.POSIXct(1, tz = "UTC")
  )
  
  # A single sample has colMeans equal to itself, so centered values are all 0
  expect_equal(vedba(a)[[1]], 0)
  expect_equal(odba(a)[[1]], 0)
})

test_that("Return NULL dba on empty acc", {
  expect_null(vedba(acc()))
  expect_null(odba(acc()))
})
