#' Plot bursts over time
#'
#' Plot the trace of IMU values from an IMU vector with time on the x-axis.
#'
#' If the bursts in the input come from multiple sources, traces may be
#' combined incorrectly. See examples.
#'
#' @inheritParams n_axis
#' @param ylab A character with the y axis label
#'
#' @export
#'
#' @examplesIf rlang::is_installed(c("dygraphs", "move2"))
#' plot_time(acc_example())
#'
#' # If bursts come from multiple sources (in this case, deployments),
#' # then lines from different bursts may be incorrectly connected:
#' alb <- albatrosses()
#' a <- as_acc(alb)
#'
#' plot_time(a)
#'
#' # To avoid this issue, plot only a single deployment's values:
#' plot_time(a[move2::mt_track_id(alb) == "4261-2228"])
plot_time <- function(x, ylab = "Value") {
  rlang::check_installed("dygraphs", "dplyr")

  time <- starts(x)

  # Only plot bursts that have both data and a start timestamp
  keep <- !is.na(x) & !is.na(time)

  if (!any(keep)) {
    cli::cli_abort(c(
      "{.fn plot_time} requires burst start timestamps in {.arg x}.",
      "i" = "Use {.code starts(x) <- ...} to assign timestamps."
    ))
  }

  n_no_start <- sum(!is.na(x) & is.na(time))
  if (n_no_start > 0) {
    cli::cli_warn("Omitting {n_no_start} burst{?s} with no start timestamp.")
  }

  dt <- mapply(
    # Convert to seconds before stripping units — otherwise non-Hz
    # frequencies (e.g. stored as "1/min") would yield offsets in minutes
    # that POSIXct silently treats as seconds.
    function(x, n) {
      c(units::drop_units(
        units::set_units((c(0, seq_len(n))) / x, "s")
      ))
    },
    x = freqs(x)[keep],
    n = n_samples(x)[keep],
    SIMPLIFY = F
  )

  df <- dplyr::bind_cols(
    time = do.call("c", mapply("+", time[keep], dt, SIMPLIFY = F)),
    dplyr::bind_rows(
      lapply(bursts(x)[keep], function(x) rbind(data.frame(x), NA))
    )
  )

  dygraphs::dygraph(df) |>
    dygraphs::dyRibbon() |>
    dygraphs::dyAxis("y", ylab)
}
