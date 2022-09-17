#' @title psmineR
#' @description Performance Spectrum Miner For Event Data
#'
#' @docType package
#' @name psmineR

## usethis namespace: start
#' @import bupaR
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import shiny
#' @rawNamespace import(plotly, except = last_plot)
#' @import miniUI
#' @importFrom dplyr %>%
#' @importFrom forcats fct_reorder
#' @importFrom glue glue
#' @importFrom data.table data.table := .I .SD .N uniqueN setorderv setkeyv setnames shift fcase rowidv fifelse melt %chin%
#' @importFrom rlang is_missing maybe_missing abort warn sym
#' @importFrom stats median na.omit quantile
## usethis namespace: end

globalVariables(c(".", ":="))
"_PACKAGE"
NULL
