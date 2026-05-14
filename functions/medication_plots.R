# # Similar function to `plot_code_usage` for descriptive code usage figures
# plot_med_usage <- function(
#   data,
#   usage_measure, # either items or ddd_quantity
#   y_label,
#   title_label,
#   text_size = 16,
#   point_size = 2,
#   x_label = "Start date of monthly reporting period",
#   n_breaks = 5
# ) {
#   # Get unique dates and pick 4 evenly spaced ones for x-axis labels
#   # Same as the `plot_breakdown_facet` function in `create_facet_plots.R`
#   all_dates <- sort(unique(data$month))
#   idx <- round(seq(1, length(all_dates), length.out = n_breaks))
#   scale_x_date_breaks <- all_dates[idx]

#   # Create plot
#   ggplot(
#     data,
#     aes(
#       x = month,
#       # {{}} for dynamic column names
#       y = {{ usage_measure }},
#       colour = bnf_chemical_name
#     )
#   ) +
#     geom_line(
#       alpha = .7,
#       linewidth = 1,
#       position = position_dodge(width = 0.2)
#     ) +
#     # geom_point(size = point_size, alpha = .5) +
#     scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
#     scale_x_date(
#       breaks = scale_x_date_breaks,
#       # x-axis scale labels: abbreviated month (new line) YYYY
#       labels = scales::label_date("%b\n%Y")
#     ) +
#     scale_colour_viridis_d(alpha = 0.7, end = .75) +
#     labs(
#       x = x_label,
#       y = y_label,
#       title = title_label,
#       colour = "Chemical"
#     ) +
#     # Using black and white theme
#     theme_bw(
#       base_size = text_size
#     ) +
#     theme(
#       text = element_text(family = "Times New Roman"),
#       axis.text.x = element_text(size = 14),
#       axis.text.y = element_text(size = 14),
#       # Place legend inside graphs
#       legend.position = c(0.2, 0.775),
#       # Enable markdown in legend labels and sets strip text size
#       legend.text = element_markdown(size = 12),
#       legend.title = element_text(family = "Times New Roman"),
#       legend.key.spacing.y = unit(5, "pt"),
#       legend.background = element_rect(
#         fill = "white",
#         linetype = "solid",
#         colour = "black",
#         linewidth = 0.5
#       )
#     )
# }

# Version with shapes, free x , and labels determined by data
plot_med_usage_shapes <- function(
  data,
  usage_measure, # either items or ddd_quantity
  title_label,
  x_label = "End date of yearly aggregation period",
  text_size = 14,
  point_size = 4,
  n_breaks
) {
  # Define common x-axis limits, this helps years align
  common_x_limits = as.Date(c("2016-07-31", "2025-07-31"))

  # Get unique dates and create yearly x-axis ticks for as many years there are
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Create plot
  ggplot(
    data,
    # {{}} for dynamic column names
    aes(
      x = end_date,
      y = {{ usage_measure }},
      colour = bnf_chemical_name,
      shape = bnf_chemical_name
    )
  ) +
    geom_line(alpha = .7, linewidth = 2) +
    geom_point(size = point_size, alpha = .5) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      limits = common_x_limits,
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_colour_viridis_d(end = .9, option = "H") +
    labs(
      x = x_label,
      title = title_label,
      colour = "Chemical",
      shape = "Chemical"
    ) +
    # Using black and white theme
    theme_bw(
      base_size = text_size
    ) +
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 16, hjust = .5),
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 14, angle = 45),
      # panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      # Place legend inside graphs
      legend.position = c(.05, .95),
      legend.box.just = "left",
      legend.justification = c("left", "top"),
      # Enable markdown in legend labels
      legend.text = element_markdown(size = 14),
      legend.title = element_text(family = "Times New Roman", size = 16),
      legend.key.spacing.y = unit(5, "pt"),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      ),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

# Function for yearly violin plots
plot_yearly_violins <- function(
  data,
  usage_measure, # either items or ddd_quantity
  title_label,
  x_label = "End date of yearly aggregation period",
  text_size = 16,
  point_size = 4,
  n_breaks
) {
  # Define common x-axis limits, this helps years align
  common_x_limits = as.Date(c("2016-07-31", "2025-07-31"))

  # Get unique dates and create yearly x-axis ticks for as many years there are
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Create violin plot
  ggplot(
    data,
    aes(x = end_date, y = {{ usage_measure }})
  ) +
    geom_violin(aes(group = end_date), fill = "#8df567", colour = NA) +
    geom_jitter(
      aes(group = end_date),
      colour = "black",
      alpha = .7,
      size = 2,
      shape = 16,
      position = position_jitter(width = 50, height = 0)
    ) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    labs(x = x_label, title = title_label) +
    theme_bw() +
    theme(
      text = element_text(family = "Times New Roman"),
      plot.title = element_text(size = 20, hjust = .5),
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size = 16),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    )
}
