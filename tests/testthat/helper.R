acc_example <- function() {
  acc(
    c(acc_burst_example(1:4, 5:8, 9:12), acc_burst_example(1:4, 5:8)), 
    frequency = units::set_units(2:3, "Hz"),
    start = as.POSIXct(c(1, 10))
  )
}

acc_burst_example <- function(x = NULL, y = NULL, z = NULL) {
  vec_size_common(x, y, z)
  new_acc_list(list(do.call(cbind, list(X = x, Y = y, Z = z))))
}

# Build sample data source to simulate case where "bursted" data is actually
# continuous, as bursts are adjacent in time.
albatrosses_messy <- function() {
  d <- albatrosses()
  d <- d[d$sensor_type_id == 2365683, ]
  
  # Fake time series with some records that represent continuous data along
  # with some longer gaps
  ts1 <- seq(
    min(move2::mt_time(d)), 
    by = "12 s",
    length.out = 10
  )
  
  ts2 <- seq(
    ts1[length(ts1)] + units::as_difftime(units::set_units(5, "min")),
    by = "5 min",
    length.out = 2
  )
  
  ts3 <- seq(
    ts2[length(ts2)] + units::as_difftime(units::set_units(5, "min")),,
    by = "12 s",
    length.out = nrow(d) - (length(ts1) + length(ts2))
  )
  
  move2::mt_time(d) <- c(ts1, ts2, ts3)
  
  # Should not collapse continuous data across different axes
  levels(d$eobs_acceleration_axes) <- c("XY", "XZ", "XYZ")
  d[c(4, 5, 6), "eobs_acceleration_axes"] <- "XYZ"
  
  d[c(44, 45), "eobs_acceleration_axes"] <- "XZ"

  # Adjust so that the duration of the burst is the same:
  d[c(4, 5, 6), "eobs_accelerations_raw"] <- paste0(
    d[c(4, 5, 6), ][["eobs_accelerations_raw"]], " ", paste0(rep(1, 60), collapse = " ")
  )

  # Should not collapse continuous data across different frequencies
  d[c(31, 32), "eobs_acceleration_sampling_frequency_per_axis"] <- units::set_units(10, "Hz")
  d[c(31, 32), "eobs_accelerations_raw"] <- paste0(
    d[c(31, 32), ][["eobs_accelerations_raw"]], " ", paste0(rep(1, 120), collapse = " ")
  )
  
  d
}
