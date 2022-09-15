#' @title Aggregated Performance Spectrum
#'
#' @description Construct the aggregated performance spectrum in the plot viewer.
#'
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}},
#' \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.).
#' @param segment_coverage,n_segments \code{\link{numeric}}: Provide either `segment_coverage` or `n_segments`. If neither is
#' provided, `segment_coverage = 0.2` will be used.\cr
#' `segment_coverage`: The percentage of cases (default `0.2`) in which each segment must be present to be visualised in the spectrum.
#' Ignored if `n_segments` is specified.\cr
#' `n_segments`: Visualise only the top `n` segments based on frequency.
#' @param classification [`character`] (default [`NULL`]): The variable defining the colour legend.
#' If [`NULL`] or `"quartile"`, a quartile variable dividing the durations of the segments in quartiles is calculated.
#' @param grouping [`character`] (default `"start"`): The timestamps, `"start"` or `"complete"`, which are binned in the histogram.
#' @param bins The number of bins in the histogram. If left at \code{NULL}, 30 bins are selected automatically
#'
#' @return A [`ggplot2`] object describing the aggregated performance spectrum.
#'
#' @examples
#' ps_aggregated(sepsis, "quartile", "start", 15, 0.2)
#'
#' @export ps_aggregated
ps_aggregated <- function(log,
                          segment_coverage,
                          n_segments,
                          classification = NULL,
                          grouping = c("start", "stop"),
                          bins = NULL) {
  UseMethod("ps_aggregated")
}

#' @describeIn ps_aggregated Calculate aggregated performance spectrum for a \code{\link[bupaR]{log}}.
#' @export
ps_aggregated.log <- function(log,
                              segment_coverage,
                              n_segments,
                              classification = NULL,
                              grouping = c("start", "stop"),
                              bins = NULL) {

  grouping <- rlang::arg_match(grouping)

  if (is_missing(segment_coverage) && is_missing(n_segments)) {
    segment_coverage <- 0.2
  } else if (!is_missing(segment_coverage) && is_missing(n_segments)) {
    if (!is.numeric(segment_coverage) || is.na(segment_coverage) || segment_coverage < 0 || segment_coverage > 1) {
      abort("`segment_coverage` must be a number between 0 and 1.")
    }
  } else if (!is_missing(n_segments) && is_missing(segment_coverage)) {
    if (!rlang::is_integerish(n_segments, n = 1) || is.na(n_segments) || n_segments < 0) {
      abort(glue("`n_segments` must be a number larger than 0."))
    }
  } else {
    abort("Must supply `segment_coverage` or `n_segments`, but not both.")
  }

  n_segments <- maybe_missing(n_segments, NULL)

  if (is.null(classification)) {
    classification <- "quartile"
  }

  log %>%
    construct_segments(classification) %>%
    build_classifier(classification) %>%
    filter_segments(log = log, segment_coverage = segment_coverage, n_segments = n_segments) %>%
    order_segments(log) -> seg

  # Select necessary columns
  # log <- eventlog %>%
  #   as.data.frame() %>%
  #   select(case_id, activity, lifecycle, timestamp, .order)

  # log <- log %>% preprocess_data(segment_coverage, classification_attribute)

  # Create the plot
  plot_aggregated(seg, "CLASSIFICATION", grouping, bins)
}