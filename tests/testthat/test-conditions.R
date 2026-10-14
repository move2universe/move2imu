test_that("move2imu_condition_class() adds subclasses and root class", {
  expect_identical(
    move2imu_condition_class("error", c("colset_not_detected", "colset")),
    c("move2imu_error_colset_not_detected", "move2imu_error_colset", "move2imu_error")
  )
  expect_identical(
    move2imu_condition_class("warning", "bursts_omitted"),
    c("move2imu_warning_bursts_omitted", "move2imu_warning")
  )
  expect_identical(move2imu_condition_class("error"), "move2imu_error")
})

test_that("abort_move2imu() signals a classed error", {
  f <- function(n) {
    abort_move2imu("Got {n} thing{?s}.", class = c("foo_bar", "foo"))
  }
  cnd <- rlang::catch_cnd(f(2))

  # Our classes come first, most specific first
  expect_identical(
    head(class(cnd), 3),
    c("move2imu_error_foo_bar", "move2imu_error_foo", "move2imu_error")
  )
  # Message is interpolated in the caller's environment
  expect_match(conditionMessage(cnd), "Got 2 things.")
  # Error is attributed to the caller, not the helper
  expect_identical(cnd$call, quote(f(2)))
})

test_that("warn_move2imu() signals a classed warning", {
  n <- 1
  cnd <- rlang::catch_cnd(
    warn_move2imu("Dropped {n} burst{?s}.", class = "foo")
  )
  expect_identical(
    head(class(cnd), 2),
    c("move2imu_warning_foo", "move2imu_warning")
  )
  expect_match(conditionMessage(cnd), "Dropped 1 burst.")
})

test_that("stop_move2imu() signals a classed error", {
  cnd <- rlang::catch_cnd(
    stop_move2imu("Error", class = c("foo_bar", "foo"))
  )
  expect_identical(
    head(class(cnd), 3),
    c("move2imu_error_foo_bar", "move2imu_error_foo", "move2imu_error")
  )
})
