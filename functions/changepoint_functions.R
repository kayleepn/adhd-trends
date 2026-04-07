# Calculate AIC for models with 0, 1, 2, and 3 breakpoints
# Note that `glm()` is used to create a Poisson model instead
mod_aic <- function(data, chemical, monthly_measure) {
  model0 <- glm(
    reformulate("month_number", response = monthly_measure),
    family = poisson,
    data = data,
    subset = bnf_chemical_name == chemical
  )

  # As the lowest AIC is often found at 2 breakpoints, I wanted to also calculate AIC for 3 breakpoints
  # However when the data doesn't fit 3 breakpoints, AIC calculation can fail
  # So I'm using another function to return "NULL" when AIC calculation fails
  safe_seg <- function(...) {
    tryCatch(
      segmented::segmented(...),
      error = function(e) NULL
    )
  }
  seg.model1 <- safe_seg(model0, seg.Z = ~month_number, npsi = 1)
  seg.model2 <- safe_seg(model0, seg.Z = ~month_number, npsi = 2)
  seg.model3 <- safe_seg(model0, seg.Z = ~month_number, npsi = 3)

  get_aic <- function(model) {
    if (is.null(model)) NA else AIC(model)
  }

  aic <- data.frame(
    num_breaks = c("0", "1", "2", "3"),
    aic = c(
      AIC(model0),
      get_aic(seg.model1),
      get_aic(seg.model2),
      get_aic(seg.model3)
    )
  )
  return(aic)
}

# Create log-transformed linear segmented model with optimal number of breakpoints minimising AIC
log_mod_select <- function(data, chemical, monthly_measure) {
  dat <- data |>
    dplyr::filter(bnf_chemical_name == chemical)

  model0 <- lm(
    reformulate(
      "month_number",
      response = paste0("log(", monthly_measure, ")")
    ),
    data = dat
  )
  selgmented(
    model0,
    type = "aic",
    refit = T,
    Kmax = 3,
    check.dslope = F
  )
}

# # Create segmented model with optimal number of breakpoints minimising AIC
# mod_select <- function(data, chemical, monthly_measure) {
#   dat <- data |>
#     dplyr::filter(bnf_chemical_name == chemical)

#   model0 <- glm(
#     reformulate("month_number", response = monthly_measure),
#     family = poisson,
#     data = dat
#   )
#   selgmented(
#     model0,
#     type = "aic",
#     refit = T,
#     Kmax = 3,
#     check.dslope = F
#   )
# }

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

# Predicted values for log models with any number of changepoints, similar to `predicted` above
# Enter any string for chemical and source in quotation marks
# This is just to keep track of things when all predictions are combined as one file
predicted_exp <- function(model, chemical, data, source) {
  # Generate predicted values using segmented model
  # `"response"` allows plotting on the scale of the response variable
  pred <- predict(model, se.fit = TRUE)

  lowerci <- pred$fit - 1.96 * pred$se.fit
  upperci <- pred$fit + 1.96 * pred$se.fit

  dat <- data |>
    dplyr::filter(bnf_chemical_name == chemical)

  # Log-normal (σ²/2) bias correction
  sigma2 <- summary(model)$sigma^2

  df.pred <- data.frame(
    # Take month number from model frame
    month_number = model.frame(model)$month_number,
    # Take month (as date) from dat, this makes plotting easier and tells us when data is missing
    month = unique(dat$month),
    # Predicted values and 95% CIs
    pred = exp(pred$fit),
    lci = exp(lowerci + sigma2 / 2),
    uci = exp(upperci + sigma2 / 2),
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

# Slope calculation for model with 3 breakpoints
# Again, same logic but more segments
slope3 <- function(model, chemical, source) {
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

    slope4 = month[4, "Est."],
    lci4 = month[4, "CI(95%).l"],
    uci4 = month[4, "CI(95%).u"],

    change1 = psi[1, "Est."],
    change2 = psi[2, "Est."],
    change3 = psi[3, "Est."],

    bnf_chemical = chemical,
    data_source = source
  )
}
