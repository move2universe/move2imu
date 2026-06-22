#' move2imu example datasets
#'
#' @description
#' Two example datasets containing acceleration bursts from animal tracking
#' studies, used throughout move2imu examples and tests.
#' These data are publicly available and downloaded from
#' [Movebank](https://www.movebank.org/cms/movebank-main).
#'
#' See the [Movebank Attribute Dictionary](https://www.movebank.org/cms/movebank-content/movebank-attribute-dictionary)
#' for details on data attributes.
#'
#' ## Galapagos albatrosses
#'
#'    GPS and acceleration data for Galapagos albatrosses from Movebank study
#'    2911040. Waved albatrosses were tracked during breeding and non-breeding
#'    periods between 2008 and 2010. Acceleration data are provided in burst
#'    format from e-obs tags.
#'
#'    *Format:* A `move2` with 9 tracks and 54 features lasting from
#'    2008-07-27 00:00:00 UTC to 2008-07-27 01:00:00 UTC.
#'
#'    *Source:* <https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study2911040>
#'
#' ## Lesser black-backed gulls
#'
#'    GPS and acceleration data for lesser black-backed gulls
#'    (Larus fuscus, Laridae) breeding at the southern North Sea coast
#'    in Belgium and the Netherlands. Published by
#'    the Research Institute for Nature and Forest (INBO). Data collected by the
#'    [LifeWatch](https://lifewatch.be/birds) GPS
#'    tracking network for large birds for the project/study LBBG_ZEEBRUGGE.
#'    Acceleration data are provided in expanded format from
#'    trackers developed by the University of Amsterdam Bird Tracking
#'    System ([UvA-BiTS](http://www.uva-bits.nl)).
#'
#'    Only individual 5508292 is included.
#'
#'    *Format:* A `move2` with 1 track and 1239 features lasting from
#'    2021-03-03 00:57:06 UTC to 2021-03-03 23:44:55 UTC.
#'
#'    *Source:* <https://www.movebank.org/cms/webapp?gwt_fragment=page=studies,path=study985143423>
#'
#' @name example_data
#' @returns `move2` object with GPS and acceleration data columns
NULL

#' @rdname example_data
#' @export
albatrosses <- function() {
  rlang::check_installed("move2")
  readRDS(system.file("extdata", "albatrosses.rds", package = "move2imu"))
}

#' @rdname example_data
#' @export
gulls <- function() {
  rlang::check_installed("move2")
  readRDS(system.file("extdata", "gulls.rds", package = "move2imu"))
}
