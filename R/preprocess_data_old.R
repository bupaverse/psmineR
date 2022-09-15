


construct_segments_old <- function(eventlog, classification_attribute) {
  eventlog %>%
    rename(TIMESTAMP_CLASSIFIER_ = !!bupaR:::timestamp_(eventlog)) %>%
    data.table() -> dt

  if(classification_attribute == "quartile") {
    dt[, list(start_t = min(TIMESTAMP_CLASSIFIER_), end_t = max(TIMESTAMP_CLASSIFIER_)),
         by = c(case_id(eventlog), activity_id(eventlog), activity_instance_id(eventlog))] -> prepared_log
  } else {

    dt %>%
      rename(CLASSIFICATION = !!sym(classification_attribute)) -> dt

    dt[, list(start_t = min(TIMESTAMP_CLASSIFIER_), end_t = max(TIMESTAMP_CLASSIFIER_)),
         by = c(case_id(eventlog), activity_id(eventlog), activity_instance_id(eventlog), "CLASSIFICATION")] -> prepared_log
  }

  prepared_log %>%
    rename(ACTIVITY_ = !!bupaR:::activity_id_(eventlog),
           ACTIVITY_INSTANCE_ = !!bupaR:::activity_instance_id_(eventlog)) -> prepared_log

  if(classification_attribute == "quartile") {
    prepared_log[, list(ACTIVITY_,
                        ACTIVITY_INSTANCE_,
                        start_t,
                        end_t,
                        yb = shift(ACTIVITY_, type = "lead"),
                        tb = shift(start_t, type = "lead")) ,
                   by = c(case_id(eventlog))] -> prepared_log
  } else {
    prepared_log[, list(ACTIVITY_,
                        ACTIVITY_INSTANCE_,
                        start_t,
                        end_t,
                        CLASSIFICATION,
                        yb = shift(ACTIVITY_, type = "lead"),
                        tb = shift(start_t, type = "lead")) ,
                   by = c(case_id(eventlog))] -> prepared_log
  }



  prepared_log %>%
    filter(!is.na(yb)) %>%
    rename(ya = ACTIVITY_, ta = end_t) %>%
    mutate(segment = paste(ya, yb, sep = " >> "))

}

build_classifier_old <- function(eventlog, classification_attribute) {
  if(classification_attribute == "quartile") {

    eventlog <- eventlog %>%
      mutate(delta = as.double(tb - ta, units = "days")) %>%
      group_by(segment) %>%
      mutate(
        CLASSIFICATION = if_else(
          delta <= quantile(delta, 0.25), 1,
          if_else(
            delta <= quantile(delta, 0.5), 2,
            if_else(
              delta <= quantile(delta, 0.75), 3, 4
            )
          )
        ),
        CLASSIFICATION = as.factor(CLASSIFICATION)
      )
  }
  eventlog

}

filter_segments_old <- function(eventlog, segment_coverage = NULL, n_segments = NULL, mapping) {

  tot_n_cases <- length(unique(eventlog[[1]]))

  eventlog %>%
    group_by(segment) %>%
    summarize(n_cases = n_distinct(!!bupaR:::case_id_(mapping)), n = n()) -> counts

  if(is.null(n_segments)) {
    counts %>%
      filter(n_cases >= segment_coverage*tot_n_cases) -> counts
  } else {
    counts %>%
      arrange(-n) %>%
      slice(1:n_segments) -> counts
  }

  eventlog %>%
    semi_join(counts, by = "segment")
}

order_segments_old <- function(log, mapping) {
  seg_ordering <- log %>%
    group_by(!!bupaR:::case_id_(mapping)) %>%
    mutate(i = row_number(start_t)) %>%
    group_by(segment) %>%
    summarise(seg_order = median(i))

  log <- left_join(log, seg_ordering, by = "segment")
}
