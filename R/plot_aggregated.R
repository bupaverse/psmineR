plot_aggregated <- function(log, classification_attribute, grouping = c("start", "stop"), bins = 10) {
  grouping <- match.arg(grouping)
  
  if(grouping == "start") plot_x <- "ta" else plot_x <- "tb"
  
  log %>% ggplot(aes(!! sym(plot_x), fill = !! sym(classification_attribute))) +
    geom_histogram(bins = bins) + facet_grid(fct_reorder(segment, seg_order)~., switch = "y", scales = "free_y") +
    theme_bw() +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0, "lines")
    ) -> p
  
  if(classification_attribute == "quartile") {
    p <- p + scale_fill_brewer(palette = "Blues")
  }
  
  return(p)
}
