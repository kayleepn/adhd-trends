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
  title,
  label_to_remove,
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
    aes(
      x = end_date,
      y = yearly_usage,
      colour = description,
      shape = description,
      fill = description
    )
  ) +
    geom_line(alpha = .5, linewidth = 1) +
    geom_point(alpha = .5, size = 5) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    # Wrapping legend label text and removing unnecessary label
    scale_colour_viridis_d(
      labels = \(x) str_wrap(x, width = 40),
      breaks = \(x) x[x != label_to_remove],
      end = .75
    ) +
    # Have to manually specify shapes (and fill) as 7 is too many too handle automatically
    scale_shape_manual(
      values = c(21, 22, 23, 3, 4, 24, 25, 11),
      labels = \(legend_labels) str_wrap(legend_labels, width = 40),
      breaks = \(x) x[x != label_to_remove],
    ) +
    scale_fill_viridis_d(
      end = .75,
      labels = \(legend_labels) str_wrap(legend_labels, width = 40),
      breaks = \(x) x[x != label_to_remove],
    ) +
    labs(x = x_label, y = y_label, title = title) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    theme(
      # Place legend inside first facet plot
      legend.position = c(0.4, 0.9),
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black"
      ),
      strip.text = element_text(size = text_size)
    ) +
    guides(colour = guide_legend(ncol = 1)) +
    facet_wrap(
      vars(facet_group),
      ncol = 1,
      labeller = label_wrap_gen(width = 60),
      scales = "free_y"
    )
}
