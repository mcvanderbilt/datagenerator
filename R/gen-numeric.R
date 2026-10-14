#' Generate a normally distributed numeric variable
#'
#' @param n Number of rows: a whole number of at least 1.
#' @param mean Mean of the distribution: a single finite number.
#' @param sd Standard deviation: a single number of at least 0. `sd = 0`
#'   gives a constant column.
#' @param name Column name: a single non-empty string.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return A `dg_dataset` (a tibble) with one double column.
#'
#' @details
#' **Model.** Each row is an independent draw `x ~ Normal(mean, sd^2)`. The
#' normal distribution is the default because it is fully described by two
#' numbers and is easy to reason about in teaching and simulation.
#'
#' **Assumptions and pitfalls.** The model is symmetric and unbounded.
#' Variables that are skewed (income), bounded (percentages), or counts
#' (visits) will not look like this, and a normal model can produce impossible
#' values such as negative counts. Use it when a symmetric, roughly
#' bell-shaped variable is the goal.
#'
#' @export
#' @examples
#' x <- gen_numeric(n = 5, mean = 100, sd = 15, seed = 1)
#' x
#' dg_meta(x)$params
gen_numeric <- function(n, mean = 0, sd = 1, name = "x", seed) {
  check_count(n)
  check_number(mean, "mean")
  check_number(sd, "sd", min = 0)
  check_name(name)

  values <- with_dg_seed(seed, stats::rnorm(n, mean = mean, sd = sd))

  new_dg_dataset(
    single_column(values, name),
    generator = "gen_numeric",
    params = list(n = n, mean = mean, sd = sd, name = name),
    seed = seed,
    assumptions = paste(
      "Independent Normal(mean, sd^2) draws.",
      "Symmetric, unbounded values; real skewed, bounded, or count data will not match."
    )
  )
}
