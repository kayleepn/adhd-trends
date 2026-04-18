# Function to create log-transformed linear segmented model with optimal number of breakpoints minimising AIC
# Using dynamic column names to deal with monthly items and monthly DDD quantity
log_mod_select <- function(data, chemical, monthly_measure) {
  # Filter data; allows for analysis of individual chemicals
  dat <- data |>
    dplyr::filter(bnf_chemical_name == chemical)

  # Create initial model
  model0 <- lm(
    reformulate(
      "month_number",
      response = paste0("log(", monthly_measure, ")")
    ),
    data = dat
  )
  # These settings produce the closest results to Andrea's 'mod_aic' function
  selgmented(
    model0,
    type = "aic",
    refit = T, # improves model selection accuracy
    Kmax = 3, # max number of changepoints = 3
    check.dslope = T # removes breakpoints without a significant slope change
  )
}

# Predicted values for linear models with any number of changepoints
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
predicted_exp <- function(model, chemical, data, source, nw_se) {
  # Generate predicted values using segmented model
  # `"response"` allows plotting on the scale of the response variable
  pred <- predict(model, se.fit = TRUE)

  if (missing(nw_se)) {
    # Simply use `pred$se.fit` if no Newey-West CIs are needed
    lowerci <- pred$fit - 1.96 * pred$se.fit
    upperci <- pred$fit + 1.96 * pred$se.fit
  } else {
    # Use provided Newey-West SE vector to calculate 95% CI bounds
    lowerci <- pred$fit - 1.96 * nw_se
    upperci <- pred$fit + 1.96 * nw_se
  }

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
    pred = exp(pred$fit + sigma2 / 2),
    lci = exp(lowerci + sigma2 / 2),
    uci = exp(upperci + sigma2 / 2),
    # Labelling columns for chemical and data source
    bnf_chemical = chemical,
    data_source = source
  )
  return(df.pred)
}

# Slope calculation for linear models with up to 3 breakpoints
# Avoids needing different functions for models with different numbers of breakpoints
extract_lin_slopes <- function(model, chemical, source) {
  slope_mat <- slope(model)$month_number
  # Return null after reaching the number of breakpoints specified in the model
  psi_mat <- tryCatch(summary(model)$psi, error = function(e) NULL)

  # Defining the output structure and pre-filling "NA"s
  out <- list(
    bnf_chemical = chemical,
    data_source = source,
    slope1 = NA,
    lci1 = NA,
    uci1 = NA,
    slope2 = NA,
    lci2 = NA,
    uci2 = NA,
    slope3 = NA,
    lci3 = NA,
    uci3 = NA,
    slope4 = NA,
    lci4 = NA,
    uci4 = NA,
    # bp: breakpoint
    bp1 = NA,
    bp2 = NA,
    bp3 = NA
  )

  # Extracting coefficients and placing them in the output list
  for (i in seq_len(nrow(slope_mat))) {
    out[[paste0("slope", i)]] <- slope_mat[i, "Est."]
    out[[paste0("lci", i)]] <- slope_mat[i, "CI(95%).l"]
    out[[paste0("uci", i)]] <- slope_mat[i, "CI(95%).u"]
  }

  # Adding breakpoint month numbers only if the breakpoint exists
  if (!is.null(psi_mat)) {
    for (i in seq_len(nrow(psi_mat))) {
      out[[paste0("bp", i)]] <- psi_mat[i, "Est."]
    }
  }

  as.data.frame(out)
}

# Slope calculation for log models with up to 3 breakpoints
# Same logic as above but exponentiates coefficients
# and interprets slopes and 95% CIs as percent change
extract_exp_slopes <- function(model, chemical, source) {
  # slope_mat <- slope(model, .vcov = NeweyWest(model), lag = mod_lag, prewhite = F)$month_number
  slope_mat <- slope(model)$month_number
  # Return null after reaching the number of breakpoints specified in the model
  psi_mat <- tryCatch(summary(model)$psi, error = function(e) NULL)

  # Defining the output structure and pre-filling "NA"s
  out <- list(
    bnf_chemical = chemical,
    data_source = source,
    pct_change1 = NA,
    pct_lci1 = NA,
    pct_uci1 = NA,
    pct_change2 = NA,
    pct_lci2 = NA,
    pct_uci2 = NA,
    pct_change3 = NA,
    pct_lci3 = NA,
    pct_uci3 = NA,
    pct_change4 = NA,
    pct_lci4 = NA,
    pct_uci4 = NA,
    bp1 = NA,
    bp2 = NA,
    bp3 = NA
  )

  for (i in seq_len(nrow(slope_mat))) {
    out[[paste0("pct_change", i)]] <- (exp(slope_mat[i, "Est."]) - 1) * 100
    out[[paste0("pct_lci", i)]] <- (exp(slope_mat[i, "CI(95%).l"]) - 1) * 100
    out[[paste0("pct_uci", i)]] <- (exp(slope_mat[i, "CI(95%).u"]) - 1) * 100
  }

  if (!is.null(psi_mat)) {
    for (i in seq_len(nrow(psi_mat))) {
      out[[paste0("bp", i)]] <- psi_mat[i, "Est."]
    }
  }

  as.data.frame(out)
}

# The following functions are not used in the script but I have kept them as they might be useful for analysing individual chemicals.

# # Calculate AIC for models with 0, 1, 2, and 3 breakpoints
# # Note that `glm()` is used to create a Poisson model instead
# mod_aic <- function(data, chemical, monthly_measure) {
#   model0 <- glm(
#     reformulate("month_number", response = monthly_measure),
#     family = poisson,
#     data = data,
#     subset = bnf_chemical_name == chemical
#   )

#   # As the lowest AIC is often found at 2 breakpoints, I wanted to also calculate AIC for 3 breakpoints
#   # However when the data doesn't fit 3 breakpoints, AIC calculation can fail
#   # So I'm using another function to return "NULL" when AIC calculation fails
#   safe_seg <- function(...) {
#     tryCatch(
#       segmented::segmented(...),
#       error = function(e) NULL
#     )
#   }
#   seg.model1 <- safe_seg(model0, seg.Z = ~month_number, npsi = 1)
#   seg.model2 <- safe_seg(model0, seg.Z = ~month_number, npsi = 2)
#   seg.model3 <- safe_seg(model0, seg.Z = ~month_number, npsi = 3)

#   get_aic <- function(model) {
#     if (is.null(model)) NA else AIC(model)
#   }

#   aic <- data.frame(
#     num_breaks = c("0", "1", "2", "3"),
#     aic = c(
#       AIC(model0),
#       get_aic(seg.model1),
#       get_aic(seg.model2),
#       get_aic(seg.model3)
#     )
#   )
#   return(aic)
# }

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

# Slope calculation for model with 1 breakpoint
# Same logic as `slope0` but more segments
# slope1 <- function(model, chemical, source) {
#   slope <- slope(model)
#   month <- slope(model)$month_number
#   psi <- summary(model)$psi

#   df.slope <- data.frame(
#     slope1 = month[1, "Est."],
#     lci1 = month[1, "CI(95%).l"],
#     uci1 = month[1, "CI(95%).u"],

#     slope2 = month[2, "Est."],
#     lci2 = month[2, "CI(95%).l"],
#     uci2 = month[2, "CI(95%).u"],

#     change1 = psi[1, "Est."],

#     bnf_chemical = chemical,
#     data_source = source
#   )
# }
