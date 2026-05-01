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
  # Choose number of breakpoints depending on model fit and slope change
  selgmented(
    model0,
    type = "aic",
    refit = T, # improves model selection accuracy
    Kmax = 3, # max number of breakpoints = 3
    check.dslope = T # removes breakpoints without a significant slope change
  )
}

# Predicted values for linear models with any number of changepoints
# Enter any string for chemical and source in quotation marks
# This is just to keep track of things when all predictions are combined as one file
# Provide Newey-West standard errors `nw_se` only when needed
predicted <- function(model, chemical, data, source, nw_se) {
  # Generate predicted values using segmented model
  # `"response"` allows plotting on the scale of the response variable
  pred <- predict(model, se.fit = TRUE, type = "response")

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
    bnf_chemical = chemical,
    data_source = source
  )
  return(df.pred)
}

# Predicted values for log models with any number of changepoints, similar to `predicted` above
# Enter any string for chemical and source in quotation marks
# This is just to keep track of things when all predictions are combined as one file
# Provide Newey-West standard errors `nw_se` only when needed
predicted_exp <- function(model, chemical, data, source, nw_se) {
  # Generate predicted values using segmented model
  # `"response"` not needed for log-linear models
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
# Only provide `nw_variance` if Newey-West corrections needed for slope CIs
extract_lin_slopes <- function(model, chemical, source, nw_variance) {
  # Construct slope matrix using `slope()`
  if (missing(nw_variance)) {
    # When Newey-West variance adjustment is not needed
    slope_mat <- slope(model)$month_number
  } else {
    slope_mat <- slope(model, .vcov = nw_variance)$month_number
  }

  # Return null after reaching the number of breakpoints specified in the model
  psi_mat <- tryCatch(summary(model)$psi, error = function(e) NULL)

  # Defining the output structure and pre-filling "NA"s
  out <- list(
    bnf_chemical = chemical,
    dataset = source
  )

  # Extracting coefficients and placing them in the output list
  for (i in seq_len(nrow(slope_mat))) {
    out[[paste0("slope", i)]] <- as.character(slope_mat[i, "Est."])
    out[[paste0("s", i, "_lci")]] <- as.character(slope_mat[i, "CI(95%).l"])
    out[[paste0("s", i, "_uci")]] <- as.character(slope_mat[i, "CI(95%).u"])
  }

  # Adding breakpoint month numbers and SE only if the breakpoint exists
  if (!is.null(psi_mat)) {
    for (i in seq_len(nrow(psi_mat))) {
      out[[paste0("bp", i)]] <- psi_mat[i, "Est."]
      out[[paste0("bp", i, "_lci")]] <- psi_mat[i, "Est."] -
        (psi_mat[i, "St.Err"] * 1.96)
      out[[paste0("bp", i, "_uci")]] <- psi_mat[i, "Est."] +
        (psi_mat[i, "St.Err"] * 1.96)
    }
  }

  as.data.frame(out)
}

# Slope calculation for log models with up to 3 breakpoints
# Same logic as above but exponentiates coefficients
# and interprets slopes and 95% CIs as percent change
extract_exp_slopes <- function(model, chemical, source, nw_variance) {
  # Construct slope matrix using `slope()`
  if (missing(nw_variance)) {
    # When Newey-West variance adjustment is not needed
    slope_mat <- slope(model)$month_number
  } else {
    slope_mat <- slope(model, .vcov = nw_variance)$month_number
  }

  # Return null after reaching the number of breakpoints specified in the model
  psi_mat <- tryCatch(summary(model)$psi, error = function(e) NULL)

  # Defining the output structure and pre-filling "NA"s
  out <- list(
    bnf_chemical = chemical,
    dataset = source
  )

  for (i in seq_len(nrow(slope_mat))) {
    out[[paste0("slope", i)]] <- paste0(
      round((exp(slope_mat[i, "Est."]) - 1) * 100, digits = 2),
      "%"
    )
    out[[paste0("s", i, "_lci")]] <- paste0(
      round((exp(slope_mat[i, "CI(95%).l"]) - 1) * 100, digits = 2),
      "%"
    )
    out[[paste0("s", i, "_uci")]] <- paste0(
      round((exp(slope_mat[i, "CI(95%).u"]) - 1) * 100, digits = 2),
      "%"
    )
  }

  if (!is.null(psi_mat)) {
    for (i in seq_len(nrow(psi_mat))) {
      out[[paste0("bp", i)]] <- psi_mat[i, "Est."]
      out[[paste0("bp", i, "_lci")]] <- psi_mat[i, "Est."] -
        (psi_mat[i, "St.Err"] * 1.96)
      out[[paste0("bp", i, "_uci")]] <- psi_mat[i, "Est."] +
        (psi_mat[i, "St.Err"] * 1.96)
    }
  }

  as.data.frame(out)
}
