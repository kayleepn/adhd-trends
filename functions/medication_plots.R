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
      legend.position = c(0.2, 0.775),
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

# Function for yearly violin plots
plot_yearly_violins <- function(
  data,
  usage_measure, # either items or ddd_quantity
  top_legend,
  bottom_legend,
  shape_values,
  title_label,
  text_size = 20,
  point_size = 4
) {
  # Create violin plot
  ggplot(
    data,
    aes(x = end_date, y = {{ usage_measure }})
  ) +
    geom_violin(fill = "grey80", colour = NA) +

    # Layer with top 5 ICBs
    geom_jitter(
      data = data |> filter(year_top5 == TRUE),
      aes(shape = icb_name, fill = icb_name),
      size = 3,
      alpha = .7,
      position = position_jitter(0.2)
    ) +
    scale_fill_viridis_d(
      name = "Top 5 ICBs",
      breaks = top_legend$icb_name,
      labels = top_legend$label,
      end = .8,
      option = "inferno",
      guide = guide_legend(order = 1, ncol = 2)
    ) +
    scale_shape_manual(
      name = "Top 5 ICBs",
      breaks = top_legend$icb_name,
      labels = top_legend$label,
      values = shape_values,
      guide = guide_legend(order = 1, ncol = 2)
    ) +
    ggnewscale::new_scale_colour() +
    # Layer with bottom 5 ICBs
    geom_jitter(
      data = data |> filter(year_bottom5 == TRUE),
      aes(colour = icb_name),
      size = 3,
      alpha = .7,
      shape = 16,
      position = position_jitter(0.2)
    ) +
    scale_color_viridis_d(
      name = "Bottom 5 ICBs",
      breaks = bottom_legend$icb_name,
      labels = bottom_legend$label,
      option = "turbo",
      guide = guide_legend(order = 2, ncol = 2)
    ) +
    # Layer with remaining ICBs
    geom_jitter(
      data = data |> filter(!year_top5 & !year_bottom5),
      colour = "grey50",
      size = 2,
      shape = 16,
      position = position_jitter(0.2)
    ) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    labs(title = title_label) +
    theme_bw() +
    theme(
      text = element_text(family = "Times New Roman"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      legend.text = element_markdown(size = 16),
      legend.title = element_text(family = "Times New Roman"),
      legend.key.spacing.y = unit(5, "pt"),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black",
        linewidth = 0.5
      )
    )
}
