# Similar function to `plot_code_usage` for descriptive code usage figures
plot_med_usage_shapes <- function(
  data,
  usage_measure, # either items or ddd_quantity
  title_label,
  y_label,
  x_label = "End date of yearly aggregation period",
  text_size = 16,
  point_size = 3,
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
    geom_point(size = point_size) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      limits = common_x_limits,
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y"),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_colour_viridis_d(end = .9, option = "G") +
    labs(
      x = x_label,
      y = y_label,
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
      plot.title = element_text(size = 20, hjust = .5),
      axis.title.x = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      # Enable markdown in legend labels
      legend.text = element_markdown(size = 16),
      legend.key.spacing.y = unit(5, "pt"),
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      )
    ) +
    guides(colour = guide_legend(override.aes = list(linetype = 0)))
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
    # Add blue triangles for yearly medians
    stat_summary(
      fun = median,
      geom = "point",
      colour = "blue",
      alpha = .8,
      shape = 17,
      size = 4
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
