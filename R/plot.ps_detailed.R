
plot_ps_detailed <- function(x, classification, label) {

  x %>%
    ggplot(aes_(~x, ~y, label = sym(label), group = ~ACTIVITY_INSTANCE, colour = sym(classification))) +
      geom_line() +
      facet_grid(fct_reorder(segment, seg_order) ~ ., switch = "y") +
      theme_bw() +
      theme(strip.text.y.left = element_text(angle = 0),
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank(),
            panel.spacing.y = unit(0, "lines")) -> p

  if(classification == "quartile") {
    p <- p + scale_color_brewer(palette = "RdYlBu", direction = -1)
  }

  return(p)
}
