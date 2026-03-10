test_that("Can convert to tibble", {
  skip_if_not_installed("dplyr")
  
  a <- as_acc(albatrosses(), drop = FALSE)
  tbl1 <- tibble::as_tibble(a)
  tbl2 <- tibble::as_tibble(a, include_bursts = TRUE)
  
  expect_equal(nrow(tbl1), length(a))
  expect_equal(tbl1$frequency, freqs(a))
  expect_equal(tbl1$start, starts(a))
  expect_false("bursts" %in% colnames(tbl1))
  expect_equal(tbl2$bursts, bursts(a))
})

test_that("Can convert to data.frame", {
  a <- as_acc(albatrosses(), drop = FALSE)
  tbl <- as.data.frame(a)
  
  expect_equal(nrow(tbl), length(a))
  expect_equal(tbl$frequency, freqs(a))
  expect_equal(tbl$start, starts(a))
})
