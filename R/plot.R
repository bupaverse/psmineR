#' @title Plot Methods
#'
#' @description  Visualize metric
#'
#' @param x Object of class `ps_aggregated` or `ps_detailed`.
#' @param ... Additional variables to pass further.
#'
#' @return A [`ggplot2`] object, which can be customised further.


#' @name plot
#' @method plot ps_aggregated
#' @export
plot.ps_aggregated <- function(x, ...) {

  return(plot_ps_aggregated(x, ...))
}