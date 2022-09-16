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
#' @import plotly
#' @import miniUI
#' @importFrom forcats fct_reorder
#' @importFrom glue glue
#' @importFrom data.table data.table := .I .SD .N uniqueN setorderv setkeyv setnames shift fcase rowidv
#' @importFrom rlang is_missing maybe_missing abort warn
#' @importFrom stats median na.omit quantile
## usethis namespace: end

globalVariables(c(".", ":="))
"_PACKAGE"
NULL
