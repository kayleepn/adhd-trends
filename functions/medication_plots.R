# Similar function to `plot_code_usage` for descriptive code usage figures
plot_med_usage <- function(
  data,
  usage_measure, # either items or ddd_quantity
  y_label,
  title_label,
  text_size = 16,
  point_size = 2,
  x_label = "Start date of monthly reporting period",
  n_breaks = 5
) {
  # Get unique dates and pick 4 evenly spaced ones for x-axis labels
  # Same as the `plot_breakdown_facet` function in `create_facet_plots.R`
  all_dates <- sort(unique(data$month))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Create plot
  ggplot(
    data,
    aes(
      x = month,
      # {{}} for dynamic column names
      y = {{ usage_measure }},
      colour = bnf_chemical_name,
      linetype = bnf_chemical_name
    )
  ) +
    geom_line(alpha = .7, linewidth = 1) +
    # geom_point(size = point_size, alpha = .5) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_colour_viridis_d(alpha = 0.7, end = .75) +
    labs(
      x = x_label,
      y = y_label,
      title = title_label,
      colour = "Chemical",
      linetype = "Chemical"
    ) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      # Place legend inside graphs
      legend.position = c(0.2, 0.75),
      # Enable markdown in legend labels and sets strip text size
      legend.text = element_markdown(size = 12),
      legend.title = element_text(family = "Times New Roman"),
      legend.key.spacing.y = unit(5, "pt"),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      )
    )
}

# Version with shapes, free x , and labels determined by data
plot_med_usage_shapes <- function(
  data,
  time_measure,
  usage_measure, # either items or ddd_quantity
  title_label,
  text_size = 16,
  point_size = 4
) {
  # Create plot
  ggplot(
    data,
    # {{}} for dynamic column names
    aes(
      x = {{ time_measure }},
      y = {{ usage_measure }},
      colour = bnf_chemical_name,
      shape = bnf_chemical_name
    )
  ) +
    geom_line(alpha = .7, linewidth = 1) +
    geom_point(size = point_size, alpha = .5) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_colour_viridis_d(alpha = 0.7, end = .75) +
    labs(title = title_label, colour = "Chemical", shape = "Chemical") +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      # Place legend inside graphs
      legend.position = c(0.2, 0.775),
      # Enable markdown in legend labels
      legend.text = element_markdown(size = 12),
      legend.title = element_text(family = "Times New Roman"),
      legend.key.spacing.y = unit(5, "pt"),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      )
    )
}
