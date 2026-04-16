#' Plot acceleration bursts over time
#' 
#' Plot the trace of acceleration values from an `acc()` with time on the
#' x-axis.
#' 
#' If the bursts in the input `acc()` come from multiple sources, traces may
#' be combined incorrectly. See examples.
#' 
#' @param x an acc vector
#' @param ylab A character with the y axis label
#'
#' @export
#' 
#' @examplesIf rlang::is_installed(c("dygraphs", "move2"))
#' plot_time(acc_example())
#' 
#' # If acceleration comes from multiple sources (in this case,
#' # deployments), then lines from different bursts may be incorrectly
#' # connected:
#' alb <- albatrosses()
#' a <- as_acc(alb, drop = FALSE)
#' 
#' plot_time(a)
#' 
#' # To avoid this issue, plot only a single deployment's values:
#' plot_time(a[move2::mt_track_id(alb) == "4261-2228"])
plot_time <- function(x, ylab = "Acceleration") {
  rlang::check_installed("dygraphs", "dplyr")
  
  time <- starts(x)
  
  dt <- mapply(
    function(x, n) c(units::drop_units((c(0, seq_len(n))) / x)),
    x = freqs(x)[!is.na(x)],
    n = n_samples(x)[!is.na(x)], 
    SIMPLIFY = F
  )
  
  df <- dplyr::bind_cols(
    time = do.call("c", mapply("+", time[!is.na(x)], dt, SIMPLIFY = F)),
    dplyr::bind_rows(
      lapply(bursts(x)[!is.na(x)], function(x) rbind(data.frame(x), NA))
    )
  )
  
  dygraphs::dygraph(df) |>
    dygraphs::dyRibbon() |>
    dygraphs::dyAxis("y", ylab)
}
