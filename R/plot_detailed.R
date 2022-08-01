plot_detailed <- function(log, classification_attribute, mapping) {

  log %>%
    ggplot(aes(x, y, label = !!bupaR:::case_id_(mapping))) +
    geom_line(aes(group = ACTIVITY_INSTANCE_, color = CLASSIFICATION)) +
    facet_grid(fct_reorder(segment, seg_order)~., switch = "y") +
    theme_bw() +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.y = element_blank(), axis.title.y = element_blank(), axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          panel.spacing.y = unit(0, "lines")) -> p

  if(classification_attribute == "quartile") {
    p <- p + scale_color_brewer(palette = "RdYlBu", direction = -1)
  }

  return(p)
}
