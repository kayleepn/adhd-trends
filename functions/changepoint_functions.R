# Calculate AIC for models with 0, 1, and 2 breakpoints
# Note that `glm()` is used to create a Poisson model instead
mod_aic <- function(data, chemical) {
  model0 <- glm(
    monthly_items ~ month_number,
    family = poisson,
    data = subset(data, bnf_chemical_name == chemical)
  )
  seg.model1 <- segmented(model0, npsi = 1)
  seg.model2 <- segmented(model0, npsi = 2)
  aic <- data.frame(
    num_breaks = c("0", "1", "2"),
    aic = c(AIC(model0), AIC(seg.model1), AIC(seg.model2))
  )
  return(aic)
}

# Create segmented model with optimal number of breakpoints minimising AIC
mod_select <- function(data, chemical) {
  dat <- data |>
    dplyr::filter(bnf_chemical_name == chemical)

  model0 <- glm(
    monthly_items ~ month_number,
    family = poisson,
    data = dat
  )
  selgmented(
    model0,
    type = "aic",
    refit = T,
    Kmax = 2,
    check.dslope = F
  )
}

# Predicted values for model with any number of changepoints
# Enter any string for chemical and source in quotation marks
# This is just to keep track of things when all predictions are combined as one file
predicted <- function(model, chemical, data, source) {
  # Generate predicted values using segmented model
  # `"response"` allows plotting on the scale of the response variable
  pred <- predict(model, se.fit = TRUE, type = "response")

  dat <- data |>
    dplyr::filter(bnf_chemical_name == chemical)

  df.pred <- data.frame(
    # Take month number from model frame
    month_number = model.frame(model)$month_number,
    # Take month (as date) from dat, this makes plotting easier and tells us when data is missing
    month = unique(dat$month),
    # Predicted values and 95% CIs
    pred = pred$fit,
    lci = pred$fit - 1.96 * pred$se.fit,
    uci = pred$fit + 1.96 * pred$se.fit,
    # Labelling columns for chemical and data source
    bnf_chemical = rep(chemical, length(pred$fit)),
    data_source = rep(source, length(pred$fit))
  )
  return(df.pred)
}

# Slope calculation for model with no breakpoints
slope0 <- function(model, chemical, source) {
  slope <- slope(model)

  # Extracting the output of `slope` into a df
  month <- slope(model)$month_number

  df.slope <- data.frame(
    slope1 = month[1, "Est."],
    lci1 = month[1, "CI(95%).l"],
    uci1 = month[1, "CI(95%).u"],

    change1 = 0,

    bnf_chemical = chemical,
    data_source = source
  )
}

# Slope calculation for model with 1 breakpoint
# Same logic as `slope0` but more segments
slope1 <- function(model, chemical, source) {
  slope <- slope(model)
  month <- slope(model)$month_number
  psi <- summary(model)$psi

  df.slope <- data.frame(
    slope1 = month[1, "Est."],
    lci1 = month[1, "CI(95%).l"],
    uci1 = month[1, "CI(95%).u"],

    slope2 = month[2, "Est."],
    lci2 = month[2, "CI(95%).l"],
    uci2 = month[2, "CI(95%).u"],

    change1 = psi[1, "Est."],

    bnf_chemical = chemical,
    data_source = source
  )
}

# Slope calculation for model with 2 breakpoints
# Again, same logic but more segments
slope2 <- function(model, chemical, source) {
  slope <- slope(model)
  month <- slope(model)$month_number
  psi <- summary(model)$psi

  df.slope <- data.frame(
    slope1 = month[1, "Est."],
    lci1 = month[1, "CI(95%).l"],
    uci1 = month[1, "CI(95%).u"],

    slope2 = month[2, "Est."],
    lci2 = month[2, "CI(95%).l"],
    uci2 = month[2, "CI(95%).u"],

    slope3 = month[3, "Est."],
    lci3 = month[3, "CI(95%).l"],
    uci3 = month[3, "CI(95%).u"],

    change1 = psi[1, "Est."],
    change2 = psi[2, "Est."],

    bnf_chemical = chemical,
    data_source = source
  )
}
