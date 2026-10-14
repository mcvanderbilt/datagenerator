#' Generate a categorical variable with specified proportions
#'
#' @param n Number of rows: a whole number of at least 1.
#' @param levels Character vector of at least two unique, non-empty labels.
#'   The order given becomes the factor level order.
#' @param probs Probability of each level, non-negative and summing to 1. If
#'   `NULL`, all levels are equally likely.
#' @param name Column name: a single non-empty string.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return A `dg_dataset` (a tibble) with one factor column.
#'
#' @details
#' **Model.** Each row independently takes one level, with probability
#' `probs`. This is a multinomial model with fixed proportions.
#'
#' **Assumptions and pitfalls.** Rows are independent and the proportions do
#' not depend on any other variable. The proportions are targets in
#' expectation: a small `n` gives realized proportions that can differ from
#' `probs` by several percentage points. For example, with `n = 20` a true
#' 50/50 split often comes out 45/55 or worse.
#'
#' @export
#' @examples
#' x <- gen_categorical(
#'   n = 1000,
#'   levels = c("Yes", "No", "Unknown"),
#'   probs = c(0.6, 0.3, 0.1),
#'   name = "response",
#'   seed = 1
#' )
#' table(x$response)
gen_categorical <- function(n, levels, probs = NULL, name = "category", seed) {
  check_count(n)
  check_name(name)

  if (!is.character(levels) || length(levels) < 2 || anyNA(levels) ||
      anyDuplicated(levels) || any(!nzchar(levels))) {
    cli::cli_abort("{.arg levels} must be at least two unique, non-empty strings.")
  }
  if (is.null(probs)) {
    probs <- rep(1 / length(levels), length(levels))
  }
  if (!is.numeric(probs) || length(probs) != length(levels) ||
      anyNA(probs) || any(probs < 0)) {
    cli::cli_abort(
      "{.arg probs} must be non-negative numbers, one per level ({length(levels)} needed)."
    )
  }
  if (abs(sum(probs) - 1) > 1e-8) {
    cli::cli_abort("{.arg probs} must sum to 1, not {.val {sum(probs)}}.")
  }

  values <- with_dg_seed(
    seed,
    sample(levels, size = n, replace = TRUE, prob = probs)
  )

  new_dg_dataset(
    single_column(factor(values, levels = levels), name),
    generator = "gen_categorical",
    params = list(n = n, levels = levels, probs = probs, name = name),
    seed = seed,
    assumptions = paste(
      "Independent multinomial draws with fixed proportions.",
      "Realized proportions vary around probs, most at small n."
    )
  )
}
