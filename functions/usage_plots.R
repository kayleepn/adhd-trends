# Helper functions for creating usage plots

# Summarise yearly code usage
summarise_yearly <- function(data, facet_name, desc) {
  data |>
    group_by(end_date) |>
    summarise(yearly_usage = sum(usage), .groups = "drop") |>
    mutate(facet_group = facet_name) |>
    mutate(description = desc)
}

# Code usage plot
plot_code_usage <- function(
  data,
  title_label,
  text_size = 16,
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
    aes(x = end_date, y = yearly_usage)
  ) +
    geom_line(alpha = .5) +
    geom_point(alpha = .5) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    labs(x = x_label, y = y_label, title = title_label) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    )
}

# Faceted plot - this is very similar to the above function
plot_usage_facets <- function(
  data,
  legend_nrow,
  text_size = 16,
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
    aes(x = end_date, y = yearly_usage, colour = description)
  ) +
    geom_line(alpha = .5) +
    geom_point(alpha = .5) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    labs(x = x_label, y = y_label) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    ) +
    guides(colour = guide_legend(nrow = legend_nrow)) +
    facet_wrap(
      vars(facet_group),
      labeller = label_wrap_gen(width = 25),
      scales = "free_y"
    )
}
