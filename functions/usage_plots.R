# Helper functions for creating usage plots

# Summarise yearly code usage
summarise_yearly <- function(data, label) {
  data |>
    group_by(end_date) |>
    summarise(yearly_usage = sum(usage), .groups = "drop") |>
    mutate(group = label)
}
