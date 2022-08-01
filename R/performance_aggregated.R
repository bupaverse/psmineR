#' Construct the aggregated performance spectrum in the plot viewer
#'
#' @param eventlog The Event Log object on which you want to mine the performance spectrum
#' @param segment_coverage The percentage of cases in which each segment must be present to be visualised in the spectrum. Dominated by \code{n_segments}
#' @param n_segments Visualise only the top n segments in terms of frequency
#' @param classification_attribute The variable defining the color legend. If \code{NULL} or \code{quartile}, a quartile variable dividing the durations of the segments in quartiles is calculated
#' @param grouping The timestamps, start or complete, which are binned in the histogram
#' @param bins The number of bins in the histogram. If left at \code{NULL}, 30 bins are selected automatically
#' @return A ggplot2 object describing the aggregated performance spectrum
#' @examples
#' performance_aggregated(sepsis, "quartile", "start", 15, 0.2)
#' @export

performance_aggregated <- function(eventlog, segment_coverage = NULL, n_segments = NULL, classification_attribute = NULL, grouping = c("start", "stop"), bins = NULL) {

  grouping <- match.arg(grouping)

  if(is.null(classification_attribute)) classification_attribute <- "quartile"

  log <- eventlog %>%
    construct_segments() %>%
    build_classifier(classification_attribute) %>%
    filter_segments(segment_coverage, n_segments) %>%
    order_segments()

  # Select necessary columns
  # log <- eventlog %>%
  #   as.data.frame() %>%
  #   select(case_id, activity, lifecycle, timestamp, .order)

  # log <- log %>% preprocess_data(segment_coverage, classification_attribute)

  # Create the plot
  plot_aggregated(log, classification_attribute, grouping, bins)
}
