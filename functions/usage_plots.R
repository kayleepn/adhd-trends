# Helper functions for creating usage plots

# Summarise yearly code usage
summarise_yearly <- function(data, desc) {
  data |>
    group_by(end_date) |>
    summarise(usage = sum(usage), .groups = "drop") |>
    mutate(description = desc)
}

# Code usage plot
plot_icd10_breakdowns <- function(
  data,
  title_label,
  legend_title,
  text_size = 16,
  point_size = 2,
  x_label = "End date of yearly reporting period",
  y_label = "Usage count",
  n_breaks = 4
) {
  # Get unique dates and pick 4 evenly spaced ones for x-axis labels
  # Same as the `plot_breakdown_facet` function in `create_facet_plots.R`
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Create plot
  ggplot(
    data,
    aes(x = end_date, y = usage, colour = breakdown)
  ) +
    geom_line(alpha = .7) +
    geom_point(size = point_size, alpha = .7) +
    scale_colour_viridis_d(alpha = 0.7, end = 0.9, option = "H") +
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
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      # Enable markdown in legend labels
      legend.text = element_markdown(size = 14),
      legend.title = element_text(family = "Times New Roman"),
      legend.key.spacing.y = unit(5, "pt"),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      ),
    )
}

# This is very similar to the above function
plot_code_usage <- function(
  data,
  title,
  show_legend = TRUE,
  show_x = TRUE,
  text_size = 16,
  x_label = "End date of yearly reporting period",
  y_label = "Usage count",
  n_breaks
) {
  # Define common x-axis limits, this helps years align
  common_x_limits = as.Date(c("2012-07-31", "2025-07-31"))

  # Get unique dates and pick 4 evenly spaced ones for x-axis labels
  # Same as the `plot_breakdown_facet` function in `create_facet_plots.R`
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

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
    geom_line(alpha = .5, linewidth = 2) +
    geom_point(alpha = .7, size = 5) +
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
    # Wrapping legend label text and removing unnecessary label
    scale_colour_viridis_d(end = .9, option = "H") +
    # Have to manually specify shapes (and fill) for easier visualisation
    scale_shape_manual(values = c(24, 25, 21, 22, 23, 3)) +
    scale_fill_viridis_d(end = .9, option = "H") +
    labs(x = x_label, y = y_label, title = title) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 20, hjust = .5),
      axis.title.x = element_text(size = 20),
      axis.text.x = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      axis.text.y = element_text(size = 16),
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      # Place legend inside plot
      legend.position = c(.3, .7),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black"
      ),
      strip.text = element_text(size = text_size)
    ) +
    guides(colour = guide_legend(ncol = 1))

  # Common x-axis
  if (show_x == FALSE) {
    plot <- plot +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  }

  # Control legend appearance
  if (show_legend == FALSE) {
    plot <- plot +
      theme(legend.position = "none")
  }

  plot
}
