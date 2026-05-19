# Helper functions for creating usage plots

# Plots yearly code usage
plot_code_usage <- function(
  data,
  title,
  show_legend = TRUE,
  show_x = TRUE,
  n_groups, # Number of groups/codes for cycling through shapes
  colour_ids,
  text_size = 16,
  x_label = "End date of yearly reporting period",
  y_label = "Yearly usage",
  n_breaks
) {
  # Define common x-axis limits, this helps years align
  common_x_limits = as.Date(c("2012-07-31", "2025-07-31"))

  # Get unique dates and picks evenly spaced ones for x-axis labels
  # Same as previous analyses and OpenCodeCounts
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Generate shared colour palette
  plot_colours = viridis(8, end = 0.8, option = "B")

  # Create plot
  plot <- ggplot(
    data,
    aes(
      x = end_date,
      y = usage,
      colour = description,
      shape = description,
      fill = description
    )
  ) +
    geom_line(alpha = .7, linewidth = 2) +
    geom_point(size = 5) +
    scale_y_continuous(
      limits = c(0, NA),
      labels = scales::comma,
      expand = expansion(mult = c(0.02, 0.1))
    ) +
    scale_x_date(
      limits = common_x_limits,
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_colour_manual(values = plot_colours[colour_ids]) +
    scale_fill_manual(values = plot_colours[colour_ids]) +
    # Only need 3 shapes to alternate between to distinguish similar colours
    scale_shape_manual(values = rep(c(16, 23, 25), length.out = n_groups)) +
    labs(x = x_label, y = y_label, title = title) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    # Stylistic things
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 20, hjust = .5),
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black"
      )
    ) +
    guides(colour = guide_legend(override.aes = list(linetype = 0), ncol = 1))

  # Common x-axis
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

# Creates plots for ICD-10 breakdowns
# This is very similar to the above function
# Differences: no shapes in this and different legend formatting.
plot_icd10_breakdowns <- function(
  data,
  title_label,
  legend_title,
  show_x = TRUE,
  n_factors, # Number of breakdown factor levels for cycling through shapes
  text_size = 16,
  point_size = 3,
  x_label = "End date of yearly reporting period",
  y_label = "Usage count",
  n_breaks = 14
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
      colour = breakdown,
      shape = breakdown,
      fill = breakdown
    )
  ) +
    geom_line(linewidth = 1.5, alpha = .7) +
    geom_point(size = point_size, alpha = .7) +
    scale_colour_viridis_d(
      alpha = 0.7,
      end = 0.9,
      option = "H",
      name = legend_title
    ) +
    scale_fill_viridis_d(
      alpha = 0.7,
      end = 0.9,
      option = "H",
      name = legend_title
    ) +
    # Only need 3 shapes to alternate between to distinguish similar colours
    scale_shape_manual(
      values = rep(c(21, 22, 24), length.out = n_factors),
      name = legend_title
    ) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    labs(x = x_label, y = y_label, title = title_label, colour = legend_title) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
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
      # Place legend inside plot
      legend.position = c(.01, .99),
      legend.box.just = "left",
      legend.justification = c("left", "top"),
      # Enable markdown in legend labels
      legend.text = element_markdown(size = 16),
      legend.title = element_text(family = "Times New Roman"),
      legend.key.spacing.y = unit(5, "pt"),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      )
    ) +
    guides(colour = guide_legend(ncol = 2))

  # Common x-axis
  if (show_x == FALSE) {
    plot <- plot +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank()
      )
  }

  plot
}
