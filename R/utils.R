cd_report_theme <- function(base_size = 10, base_family = "",
                            base_line_size = base_size/22,
                            base_rect_size = base_size/22) {

  half_line <- base_size/2
  small_rel <- 0.85
  small_size <- base_size * small_rel
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %>%
    theme(
      # line = element_line(colour = "black", size = base_line_size,
      #                     linetype = 1, lineend = "butt"),
      # rect = element_rect(fill = NA, colour = "black",
      #                     size = base_rect_size, linetype = 1),
      text = element_text(family = base_family, face = "plain",
                          colour = "black", size = base_size,
                          lineheight = 0.9, hjust = 0.5, vjust = 0.5,
                          angle = 0, margin = margin(), debug = FALSE),

      # axis.line = element_line(colour = "black", size = base_line_size),
      # axis.line.x = element_line(colour = "black", size = base_line_size),
      # axis.line.y = element_line(colour = "black", size = base_line_size),
      axis.text = element_text(size = small_size),
      axis.text.x = element_text(margin = margin(t = small_rel/2), vjust = 1),
      axis.text.y = element_text(margin = margin(r = small_rel/2), hjust = 1),
      # axis.ticks = element_line(colour = "black", size = base_line_size),
      axis.title = element_text(size = base_size),
      axis.title.x = element_text(margin = margin(t = half_line)),
      axis.title.y = element_text(angle = 90L, margin = margin(r = half_line)),

      # legend.key = element_rect(fill = "white", colour = NA),
      # legend.key.size = unit(0.8, "lines"),
      legend.background = element_rect(fill = alpha("white", 0)), # Transparent background
      legend.position = "bottom", # or "bottom"
      # legend.text = element_text(size = small_size),
      # legend.title.text = element_text(size = small_size),

      panel.background = element_blank(),
      # panel.border = element_rect(color = "black", fill = NA, size = 0.8),
      panel.grid = element_blank(),

      # plot.background = element_rect(fill = NA, colour = NA),
      plot.title = element_text(size = base_size, hjust = 0.5,
                                margin = margin(b = half_line)),
      plot.subtitle = element_text(size = base_size * 0.95, hjust = 0.5,
                                   margin = margin(b = half_line)),
      plot.caption = element_text(size = small_size, hjust = 0,
                                  margin = margin(t = half_line)),

      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = small_size)
    )
}
