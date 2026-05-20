# Similar function to `plot_code_usage` for descriptive code usage figures
plot_med_usage_shapes <- function(
  data,
  usage_measure, # either items or ddd_quantity
  title_label,
  y_label,
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

  # Create plot
  ggplot(
    data,
    # {{}} for dynamic column names
    aes(
      x = end_date,
      y = {{ usage_measure }},
      colour = bnf_chemical_name,
      fill = bnf_chemical_name,
      shape = bnf_chemical_name
    )
  ) +
    geom_line(alpha = .2, linewidth = 1) +
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
    scale_fill_viridis_d(end = .9, option = "G") +
    scale_shape_manual(
      values = c(21, 22, 23, 21, 22, 23)
    ) +
    labs(
      x = x_label,
      y = y_label,
      title = title_label,
      colour = "Chemical",
      shape = "Chemical",
      fill = "Chemical"
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
  y_label,
  outlier_map,
  n_breaks,
  text_size = 16,
  point_size = 3
) {
  # Get unique dates and create yearly x-axis ticks for as many years there are
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  # Create violin plot
  ggplot(
    data,
    aes(x = end_date, y = {{ usage_measure }})
  ) +
    geom_violin(aes(group = end_date), fill = "grey80", colour = NA) +
    # Dot plot layer with non-outlier ICBs
    geom_jitter(
      data = data |> filter(outlier == FALSE),
      aes(group = end_date),
      colour = "grey40",
      size = point_size,
      shape = 16,
      position = position_jitter(width = 50, height = 0)
    ) +
    # Dot plot layer with outlier ICBs
    geom_jitter(
      data = data |> filter(outlier == TRUE),
      aes(
        group = end_date,
        fill = icb_name,
        shape = icb_name
      ),
      size = point_size,
      position = position_jitter(width = 50, height = 0)
    ) +
    # Add crossbar for yearly medians
    stat_summary(
      aes(group = end_date),
      fun.data = function(x) {
        m <- median(x, na.rm = TRUE)
        data.frame(y = m, ymin = m, ymax = m)
      },
      geom = "crossbar",
      width = 250,
      linewidth = 1,
      fatten = 0,
      colour = "black"
    ) +
    # Colours and shapes for outlier ICBs
    scale_fill_manual(
      name = "**Outlier ICBs**",
      values = setNames(outlier_map$colour, outlier_map$icb_name),
      breaks = outlier_map$icb_name,
      labels = label_wrap(20)(outlier_map$label),
      drop = FALSE
    ) +
    scale_shape_manual(
      name = "**Outlier ICBs**",
      values = setNames(outlier_map$shape, outlier_map$icb_name),
      breaks = outlier_map$icb_name,
      labels = label_wrap(20)(outlier_map$label),
      drop = FALSE
    ) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      # x-axis scale labels: abbreviated month (new line) YYYY
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    labs(x = x_label, y = y_label, title = title_label) +
    theme_bw() +
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
      legend.box.background = element_rect(
        fill = "white",
        linetype = "solid",
        colour = "black"
      ),
      legend.title = element_markdown(size = 16, hjust = 0.5),
      legend.text = element_text(size = 16),
      legend.justification.right = "left",
      legend.key.spacing.y = unit(5, "pt")
    )
}
