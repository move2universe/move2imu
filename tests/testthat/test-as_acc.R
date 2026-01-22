test_that("Can get acc from burst-format acc data", {
  x <- as_acc(albatrosses)
  
  i <- which(!is.na(x))[1]
  
  expect_s3_class(x, "acc")
  expect_equal(
    colnames(field(x, "bursts")[[i]]), 
    strsplit(as.character(albatrosses$eobs_acceleration_axes[i]), "")[[1]]
  )
  expect_length(x, nrow(albatrosses))
  expect_equal(is.na(x), is.na(albatrosses$eobs_accelerations_raw))
  expect_true(is_uniform(x))
})

test_that("Can get acc from long-format acc data", {
  acc_i <- which(gulls$sensor_type_id == 2365683)
  
  expect_warning(
    x <- as_acc(gulls), 
    "Detected multiple valid acceleration column sets"
  )
  
  expect_s3_class(x, "acc")
  expect_length(x, nrow(gulls))
  expect_equal(sum(!is.na(x)), length(unique(parse_bursts(gulls)))) 
  expect_equal(is.na(x[acc_i]), duplicated(parse_bursts(gulls)))
  expect_true(is_uniform(x))
  expect_equal(
    unique(purrr::map(field(x[!is.na(x)], "bursts"), colnames))[[1]],
    c("X", "Y", "Z")
  )
})
