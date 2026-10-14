#' Add missing values under MCAR, MAR, or MNAR
#'
#' Sets some values of a generated dataset to `NA`. The mechanism decides what
#' the missingness depends on, and that choice is recorded in the provenance
#' record.
#'
#' @param data A dataset made by a datagenerator function. Other data frames
#'   are refused: this package does not add missingness to real data.
#' @param prop Target proportion of missing values in each affected column, a
#'   number from 0 up to but not including 1.
#' @param mechanism One of:
#'   * `"MCAR"` (missing completely at random): each value is missing with
#'     probability `prop`, independent of everything.
#'   * `"MAR"` (missing at random): the probability depends on the observed
#'     column named in `driver`.
#'   * `"MNAR"` (missing not at random): the probability depends on the value
#'     itself. Only numeric columns are allowed.
#' @param cols Columns to make partly missing. Defaults to every column for
#'   MCAR, every column except `driver` for MAR, and every numeric column for
#'   MNAR.
#' @param driver For MAR only: the name of a numeric column that is fully
#'   observed. It drives missingness and is never made missing itself.
#' @param strength For MAR and MNAR: how steeply the missing probability rises
#'   with the standardized driver (or value). `0` gives MCAR. Must be at least 0.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return The dataset with missing values inserted. Its provenance record
#'   gains a `missingness` entry with the mechanism, target rate, achieved rate
#'   per column, and the assumption that the mechanism encodes.
#'
#' @details
#' **How the probabilities are set.** For MAR and MNAR, the function standardizes
#' the driver (or value) to `z`, then uses a logistic model
#' `P(missing) = plogis(a + strength * z)`. The intercept `a` is chosen with
#' [stats::uniroot()] so that the average probability equals `prop`. The
#' realized rate varies a little around `prop` in any single draw.
#'
#' **What each mechanism means for analysis.**
#' * MCAR: dropping rows with missing values (complete-case analysis) remains
#'   unbiased, only less precise.
#' * MAR: complete-case analysis is biased unless the driver is adjusted for.
#'   Methods such as multiple imputation that use the driver can recover the
#'   right answer.
#' * MNAR: the observed data cannot reveal the mechanism. Results depend on an
#'   assumption that cannot be checked from the data alone. Use this to test
#'   sensitivity, not as a realistic model of a survey.
#'
#' @export
#' @examples
#' d <- gen_correlated(n = 500, means = c(a = 0, b = 0), sds = c(1, 1),
#'                     cor = diag(2), seed = 1)
#' m <- add_missingness(d, prop = 0.2, mechanism = "MAR", driver = "b",
#'                      cols = "a", strength = 1.5, seed = 2)
#' mean(is.na(m$a))
#' dg_meta(m)$missingness$mechanism
add_missingness <- function(data, prop, mechanism = c("MCAR", "MAR", "MNAR"),
                            cols = NULL, driver = NULL, strength = 1, seed) {
  if (!inherits(data, "dg_dataset")) {
    cli::cli_abort(c(
      "{.arg data} must be a dataset made by a datagenerator function.",
      "i" = "Adding missingness to real data is outside this package's scope."
    ))
  }
  mechanism <- match.arg(mechanism)
  check_number(prop, "prop", min = 0)
  if (prop >= 1) {
    cli::cli_abort("{.arg prop} must be less than 1, not {.val {prop}}.")
  }
  check_number(strength, "strength", min = 0)
  check_seed(seed)

  meta <- dg_meta(data)
  plain <- strip_dg(data)
  n <- nrow(plain)
  all_cols <- names(plain)
  is_num <- vapply(plain, is.numeric, logical(1))

  if (mechanism == "MAR") {
    if (is.null(driver)) {
      cli::cli_abort("{.arg driver} is required for MAR: name the observed column that drives missingness.")
    }
    check_name(driver, "driver")
    if (!driver %in% all_cols) {
      cli::cli_abort("{.arg driver} must be a column of {.arg data}, not {.val {driver}}.")
    }
    if (!is_num[[driver]]) {
      cli::cli_abort("{.arg driver} must be a numeric column, but {.val {driver}} is not.")
    }
  } else if (!is.null(driver)) {
    cli::cli_abort("{.arg driver} is only used with MAR.")
  }

  if (is.null(cols)) {
    cols <- switch(mechanism,
      MCAR = all_cols,
      MAR = setdiff(all_cols, driver),
      MNAR = all_cols[is_num]
    )
  }
  unknown <- setdiff(cols, all_cols)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown column(s) in {.arg cols}: {.val {unknown}}.")
  }
  if (!is.null(driver) && driver %in% cols) {
    cli::cli_abort("{.arg driver} cannot also be in {.arg cols}, because it must stay observed.")
  }
  if (mechanism == "MNAR" && !all(is_num[cols])) {
    non_numeric <- cols[!is_num[cols]]
    cli::cli_abort(
      "MNAR missingness depends on the value itself, so {.arg cols} must be numeric. Not numeric: {.val {non_numeric}}."
    )
  }
  if (length(cols) == 0) {
    cli::cli_abort("There are no columns to make missing.")
  }

  # Probability of missingness for each affected column.
  probs <- lapply(cols, function(col) {
    if (mechanism == "MCAR") {
      return(rep(prop, n))
    }
    src <- if (mechanism == "MNAR") col else driver
    x <- plain[[src]]
    if (anyNA(x)) {
      cli::cli_abort(c(
        "{.field {src}} contains missing values.",
        "i" = "Add missingness to complete data only, then add more with another call if needed."
      ))
    }
    if (stats::sd(x) == 0) {
      cli::cli_abort("{.field {src}} is constant, so it cannot drive missingness.")
    }
    z <- (x - mean(x)) / stats::sd(x)
    a <- calibrate_intercept(z, prop, strength)
    stats::plogis(a + strength * z)
  })
  names(probs) <- cols

  # One seeded block draws every uniform in a fixed order, so the result is
  # reproducible.
  u <- with_dg_seed(seed, matrix(stats::runif(n * length(cols)), nrow = n))

  achieved <- numeric(length(cols))
  for (k in seq_along(cols)) {
    miss <- u[, k] < probs[[k]]
    plain[[cols[k]]][miss] <- NA
    achieved[k] <- mean(miss)
  }

  meta$missingness <- list(
    mechanism = mechanism,
    prop = prop,
    strength = strength,
    driver = driver,
    columns = cols,
    achieved_rate = stats::setNames(achieved, cols),
    seed = seed
  )
  meta$assumptions <- paste(meta$assumptions, missingness_assumption(mechanism))

  as_dg_dataset(plain, meta)
}

# Find the intercept a so that mean(plogis(a + strength * z)) = prop.
# A target of 0 needs no search: a = -Inf gives probability 0 everywhere.
calibrate_intercept <- function(z, prop, strength) {
  if (prop == 0) {
    return(-Inf)
  }
  f <- function(a) mean(stats::plogis(a + strength * z)) - prop
  stats::uniroot(f, interval = c(-50, 50), tol = 1e-8)$root
}

missingness_assumption <- function(mechanism) {
  switch(mechanism,
    MCAR = "Missingness unrelated to any variable (MCAR).",
    MAR = "Missingness depends only on the observed driver column (MAR).",
    MNAR = "Missingness depends on the missing value itself (MNAR); the observed data cannot confirm this."
  )
}
