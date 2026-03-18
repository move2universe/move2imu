test_that("acc map function generator always returns a function", {
  expect_true(is.function(as_acc_mapper(~ .br)))
  expect_true(is.function(as_acc_mapper(function(.br) .br)))
})

test_that("returned function always accepts .br, .fq, .st, .ti by name", {
  f <- as_acc_mapper(function(.br) .br)
  expect_no_error(f(.br = 1, .fq = 2, .st = 3, .ti = "1092"))
})

test_that("reserved function arguments interpreted correctly", {
  a <- acc_example()
  
  expect_equal(
    map_acc(a, function(.br) nrow(.br) * 2, simplify = TRUE),
    purrr::map_dbl(bursts(a), nrow) * 2
  )
  expect_equal(
    map_acc(a, function(.fq) as.numeric(.fq) + 10, simplify = TRUE),
    purrr::map_dbl(freqs(a), as.numeric) + 10
  )
  expect_equal(
    map_acc(a, function(.st) as.numeric(.st + 1), simplify = TRUE),
    purrr::map_dbl(starts(a), as.numeric) + 1
  )
})

test_that("Can use multiple reserved function args", {
  a <- acc_example()
  
  expect_identical(
    map_acc(a, function(.br, .fq) nrow(.br) + as.numeric(.fq)),
    purrr::map2(bursts(a), freqs(a), ~ nrow(.x) + as.numeric(.y))
  )
  expect_identical(
    map_acc(a, function(.fq, .st) .st + as.numeric(.fq)),
    purrr::map2(starts(a), freqs(a), ~ .x + as.numeric(.y))
  )
  expect_identical(
    map_acc(a, function(.br, .fq, .st) nrow(.br) + as.numeric(.fq) + .st),
    purrr::pmap(
      list(bursts(a), freqs(a), starts(a)), 
      function(b, f, s) nrow(b) + as.numeric(f) + s
    )
  )
})

test_that("Can use formula syntax", {
  a <- acc_example()
  
  expect_identical(
    map_acc(acc_example(), function(.br) .br * 2),
    map_acc(acc_example(), ~ .br * 2)
  )
  expect_identical(
    map_acc(a, function(.fq, .st) .st + as.numeric(.fq)),
    map_acc(a, ~ .st + as.numeric(.fq))
  )
  expect_identical(
    map_acc(a, function(.br, .fq, .st) nrow(.br) + as.numeric(.fq) + .st),
    map_acc(a, ~ nrow(.br) + as.numeric(.fq) + .st)
  )
})

test_that("formula captures enclosing environment correctly", {
  scale <- 100

  expect_identical(
    map_acc(acc_example(), function(.br) .br * scale),
    purrr::map(bursts(acc_example()), ~ .x * 100)
  )
})

test_that("Can call predefined function", {
  my_fun <- function(.br, .st) nrow(.br) * 2 + .st
  a <- acc_example()
  
  expect_identical(
    map_acc(a, my_fun),
    purrr::map2(bursts(a), starts(a), function(x, y) nrow(x) * 2 + y)
  )
})

test_that("Can call predefined function with additional arguments", {
  my_fun <- function(.br, scale = 1) nrow(.br) * scale
  a <- acc_example()
  
  expect_identical(
    map_acc(a, my_fun),
    purrr::map(bursts(a), ~ nrow(.x) * 1)
  )
  expect_identical(
    map_acc(a, ~ my_fun(.br, scale = 2)),
    purrr::map(bursts(a), ~ nrow(.x) * 2)
  )
  expect_identical(
    map_acc(a, ~ my_fun(.br, scale = .fq)),
    purrr::map2(bursts(a), freqs(a), ~ nrow(.x) * .y)
  )
})

test_that("Can simplify", {
  expect_identical(
    map_acc(acc_example(), ~ .fq, simplify = TRUE),
    freqs(acc_example())
  )
  expect_error(
    map_acc(acc_example(), ~ .br, simplify = TRUE),
    "must have size"
  )
})

test_that("Error on unrecognized arg names (do not match by position)", {
  my_fun <- function(x, y) nrow(x) + y
  
  expect_error(map_acc(acc_example(), function(x) nrow(x)))
  expect_error(map_acc(acc_example(), my_fun))
})

test_that("map_acc() basic requirements", {
  expect_error(map_acc(acc_example(), "foobar"), "must be a function or")
  expect_error(map_acc(acc_example(), NULL), "must be a function or")
  expect_error(map_acc(1, length), "does not inherit from class acc")
})

