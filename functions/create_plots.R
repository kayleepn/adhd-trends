#' Helper function to plot breakdown usage over time with facets
#' @param data A data frame with columns: end_date, usage, breakdown
#' @param ncol Number of columns for facet_wrap (default: 4)
#' @param text_size Base text size (default: 16)
#' @param strip_size Strip text size (default: 12)
#' @param date_breaks Date breaks for x-axis (default: "3 years")
#' @param point_size Size of points (default: 2)
#' @param x_label Label for x-axis (default: NULL)
#' @param y_label Label for y-axis (default: NULL)
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_x_date scale_y_continuous theme element_text facet_wrap labs scale_colour_viridis_d
#' @importFrom ggtext element_markdown
#' @importFrom scales comma label_date_short
#' @keywords internal
plot_breakdown_facet <- function(
  data,
  ncol = 4,
  text_size = 16,
  strip_size = 12,
  point_size = 2,
  x_label = "End date of yearly reporting period",
  y_label = NULL,
  n_breaks = 4
) {
  all_dates <- sort(unique(data$end_date))
  idx <- round(seq(1, length(all_dates), length.out = n_breaks))
  scale_x_date_breaks <- all_dates[idx]

  ggplot(data, aes(x = end_date, y = usage, colour = breakdown)) +
    geom_point(size = point_size) +
    geom_line(alpha = .3) +
    scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
    scale_x_date(
      breaks = scale_x_date_breaks,
      labels = scales::label_date("%b\n%Y")
    ) +
    scale_colour_viridis_d(end = .75) +
    labs(x = x_label, y = y_label) +
    theme(
      text = element_text(size = text_size),
      legend.position = "none",
      strip.text = element_markdown(size = strip_size),
      panel.spacing = unit(1.5, "lines")
    ) +
    facet_wrap(~breakdown, ncol = ncol)
}
