#' Construct the detailed performance spectrum in the plot viewer
#'
#' @param eventlog The Event Log object on which you want to mine the performance spectrum
#' @param segment_coverage The percentage of cases in which each segment must be present in order to be visualised in the spectrum. Defaults to 0.2 (20%). Ignored if n_segments is specified.
#' @param n_segments Alternative to segments_coverage, Visualise only the top n segments in terms of frequency.
#' @param classification_attribute The variable defining the color legend. If \code{NULL} or \code{quartile}, a quartile variable dividing the durations of the segments in quartiles is calculated
#' @return A ggplot2 object describing the detailed performance spectrum
#' @examples
#' performance_detailed(eventlog = sepsis, segments_coverage = 0.2)
#'
#' @export

ps_detailed <- function(eventlog, segment_coverage, n_segments, classification_attribute) {
  UseMethod("ps_detailed")
}

#' @export

ps_detailed.eventlog <- function(eventlog,
                                 segment_coverage = 0.2,
                                 n_segments = NULL,
                                 classification_attribute = NULL) {


  if(is.null(classification_attribute)) classification_attribute <- "quartile"

  log <- eventlog %>%
    construct_segments(classification_attribute) %>%
    build_classifier(classification_attribute) %>%
    filter_segments(segment_coverage, n_segments, mapping(eventlog)) %>%
    order_segments(mapping(eventlog)) %>%
  # Transform to long format to construct the detailed performance spectrum
    gather(key, x, ta, tb) %>%
    mutate(y = if_else(key == "ta", 1, 0)) %>%
    filter(key %in% c("ta", "tb"))

  #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ####

  # Create the plot
  log %>%
    plot_detailed(classification_attribute, mapping(eventlog))

}
