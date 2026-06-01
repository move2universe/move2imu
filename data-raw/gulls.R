options("keyring_backend" = "env")

move2::movebank_store_credentials(
  Sys.getenv("MOVEBANK_USERNAME"),
  Sys.getenv("MOVEBANK_PASSWORD")
)

# LBBG_ZEEBRUGGE - Lesser black-backed gulls
d2 <- as.Date("2021-03-03")

gulls <- move2::movebank_download_study(
  985143423,
  sensor_type_id = c("gps", "acceleration"),
  individual_local_identifier = 5508292,
  timestamp_start = as.POSIXct(d2),
  timestamp_end = as.POSIXct(d2 + 1)
)

gulls <- gulls[order(move2::mt_track_id(gulls), move2::mt_time(gulls)), ]

saveRDS(gulls, "inst/extdata/gulls.rds")
