#' @title Aggregated Performance Spectrum
#'
#' @description Plots the aggregated performance spectrum.
#'
#' @param log [`log`][`bupaR::log`]: Object of class [`log`][`bupaR::log`] or derivatives ([`grouped_log`][`bupaR::grouped_log`],
#' [`eventlog`][`bupaR::eventlog`], [`activitylog`][`bupaR::activitylog`], etc.).
#' @param segment_coverage,n_segments \code{\link{numeric}}: Provide either `segment_coverage` or `n_segments`. If neither is
#' provided, `segment_coverage = 0.2` will be used.\cr
#' `segment_coverage`: The percentage of cases (default `0.2`) in which each segment must be present to be visualised in the spectrum.
#' Ignored if `n_segments` is specified.\cr
#' `n_segments`: Visualise only the top `n` segments based on frequency.
#' @param classification [`character`] (default [`NULL`]): The variable defining the colour legend. This variable should be present in `log`.\cr
#' If [`NULL`] (default) when `log` is a [`grouped_log`][`bupaR::grouped_log`], the first grouping variable will be used as `classification`.\cr
#' If [`NULL`] (default) or `"quartile"` when `log` is an [`eventlog`][`bupaR::eventlog`] or [`activitylog`][`bupaR::activitylog`],
#' a quartile variable dividing the durations of the segments in quartiles is calculated.
#' @param grouping [`character`] (default `"start"`): The timestamps, `"start"` or `"complete"`, which are binned in the histogram.
#' @param bins [`numeric`] (default [`NULL`]): The number of bins in the aggregated performance spectrum.
#' If left [`NULL`], 30 bins are selected automatically.
#'
#' @return A [`ggplot2`] object describing the aggregated performance spectrum.
#'
#' @examples
#' library(psmineR)
#' library(eventdataR)
#'
#' sepsis %>%
#'  ps_aggregated(segment_coverage = 0.2,
#'                classification = "quartile",
#'                grouping = "start",
#'                bins = 15)
#'
#' @export ps_aggregated
ps_aggregated <- function(log,
                          segment_coverage,
                          n_segments,
                          classification = NULL,
                          grouping = c("start", "complete"),
                          bins = NULL) {
  UseMethod("ps_aggregated")
}

#' @describeIn ps_aggregated Calculate aggregated performance spectrum for a [`log`][`bupaR::log`].
#' @export
ps_aggregated.log <- function(log,
                              segment_coverage,
                              n_segments,
                              classification = NULL,
                              grouping = c("start", "complete"),
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
      abort("`n_segments` must be an integer number larger than 0.")
    }
  } else {
    abort("Must supply `segment_coverage` or `n_segments`, but not both.")
  }

  n_segments <- maybe_missing(n_segments, NULL)

  if (is.null(classification)) {
    classification <- "quartile"
  } else if (classification != "quartile" && !(classification %in% colnames(log))) {
    abort(glue("Invalid `classification`: \"{classification}\" is not present in log."))
  }

  log %>%
    construct_segments(classification) %>%
    build_classifier(classification) %>%
    filter_segments(log = log, segment_coverage = segment_coverage, n_segments = n_segments) %>%
    order_segments(log) -> seg

  class(seg) <- c("ps_aggregated", class(seg))

  # Select necessary columns
  # log <- eventlog %>%
  #   as.data.frame() %>%
  #   select(case_id, activity, lifecycle, timestamp, .order)

  # log <- log %>% preprocess_data(segment_coverage, classification_attribute)

  # Create the plot
  #plot_aggregated(seg, "CLASSIFICATION", grouping, bins)
  seg %>%
    plot(classification, grouping, bins)
}

#' @describeIn ps_aggregated Calculate aggregated performance spectrum for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
ps_aggregated.grouped_log <- function(log,
                                      segment_coverage,
                                      n_segments,
                                      classification = NULL,
                                      grouping = c("start", "complete"),
                                      bins = NULL) {

  if (is.null(classification)) {
    classification <- as.character(groups(log)[[1L]])
  }

  ps_aggregated.log(ungroup_eventlog(log),
                    maybe_missing(segment_coverage),
                    maybe_missing(n_segments),
                    classification,
                    grouping,
                    bins)
}
