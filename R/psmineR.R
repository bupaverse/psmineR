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
#' @import forcats
#' @importFrom glue glue
#' @importFrom data.table data.table na.omit := .I .SD .N uniqueN setorderv setkeyv
#' @importFrom lifecycle deprecated
#' @importFrom rlang is_missing maybe_missing abort warn
## usethis namespace: end

globalVariables(c(".", ":="))
"_PACKAGE"
NULL
