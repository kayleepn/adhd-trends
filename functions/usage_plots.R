# Helper function for creating usage plots
# Plots yearly code usage including breakdowns if specified
plot_code_usage <- function(
  data,
  breakdown_col,
  title_label,
  common_x_limits,
  show_x = TRUE,
  show_legend = TRUE,
  plot_colours,
  plot_shapes,
  legend_ncol,
  n_breaks,
  y_millions = FALSE,
  text_size = 16,
  point_size = 4,
  x_label = "End date of yearly reporting period",
  y_label = "Usage"
) {
  # Get unique dates and picks evenly spaced ones for x-axis labels
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Create plot
  plot <- ggplot(
    data,
    aes(
      x = end_date,
      y = usage,
      colour = {{ breakdown_col }},
      shape = {{ breakdown_col }},
      fill = {{ breakdown_col }}
    )
  ) +
    geom_line(linewidth = 1, alpha = .2) +
    geom_point(size = point_size) +
    scale_colour_manual(values = plot_colours) +
    scale_fill_manual(values = plot_colours) +
    # Only need 3 shapes to alternate between to distinguish similar colours
    scale_shape_manual(values = plot_shapes) +
    # Use unit M for millions if `y_millions == TRUE`
    (if (y_millions == TRUE) {
      scale_y_continuous(
        limits = c(0, NA),
        labels = scales::label_number(
          scale = 1e-6,
          suffix = "M",
          big.mark = ","
        )
      )
    } else {
      scale_y_continuous(
        limits = c(0, NA),
        labels = scales::comma
      )
    }) +
    scale_x_date(
      limits = common_x_limits,
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    labs(x = x_label, y = y_label, title = title_label) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    # Other stylistic choices
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 20, hjust = .5),
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.ticks.x.bottom = element_line(),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      # Enable markdown in legend labels
      legend.text = element_markdown(size = 16),
      legend.title = element_blank(),
      legend.key.spacing.y = unit(5, "pt"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      )
    ) +
    guides(
      colour = guide_legend(
        override.aes = list(linetype = 0),
        ncol = legend_ncol
      )
    )

  # Control x-axis appearance
  if (show_x == FALSE) {
    plot <- plot +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }

  # Control legend appearance
  if (show_legend == FALSE) {
    plot <- plot +
      theme(legend.position = "none")
  }

  plot
}
