#' Generate grouped (hierarchical) data with a random group effect
#'
#' @param n_groups Number of groups: a whole number of at least 1.
#' @param n_per_group Rows per group: one whole number used for every group,
#'   or one number per group for unbalanced designs.
#' @param grand_mean Overall mean of the outcome.
#' @param between_sd Standard deviation of the group effects (`tau`), at least 0.
#' @param within_sd Standard deviation of the rows within a group (`sigma`), at least 0.
#' @param group_name Name of the group column.
#' @param name Name of the outcome column.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return A `dg_dataset` (a tibble) with a factor group column and a double
#'   outcome column. The provenance record also stores `icc`, the implied
#'   intraclass correlation.
#'
#' @details
#' **Model.** A one-level random-intercept model:
#' `y[ij] = grand_mean + u[j] + e[ij]`, with `u[j] ~ Normal(0, between_sd^2)`
#' for each group `j` and `e[ij] ~ Normal(0, within_sd^2)` for each row `i`
#' in group `j`.
#'
#' The intraclass correlation (ICC) is `between_sd^2 / (between_sd^2 + within_sd^2)`.
#' It is the share of total variance that lies between groups. An ICC near 0
#' means group membership tells you little; near 1 means rows within a group
#' are nearly identical.
#'
#' **Assumptions and pitfalls.** Group effects are normal, independent of each
#' other, and independent of group size. Real groups often differ in size in
#' ways related to their outcomes (for example, large clinics may differ from
#' small ones), which this model cannot represent. Setting `within_sd = 0`
#' makes every row in a group identical, which is useful for testing grouped
#' code but is unrealistic.
#'
#' @export
#' @examples
#' x <- gen_grouped(n_groups = 10, n_per_group = 20, between_sd = 2,
#'                  within_sd = 1, seed = 1)
#' dg_meta(x)$params$icc
#' aggregate(y ~ group, data = as.data.frame(x), FUN = mean)
gen_grouped <- function(n_groups, n_per_group, grand_mean = 0, between_sd = 1,
                        within_sd = 1, group_name = "group", name = "y", seed) {
  check_count(n_groups, "n_groups")
  if (!is.numeric(n_per_group) || !(length(n_per_group) %in% c(1, n_groups)) ||
      anyNA(n_per_group) || any(n_per_group < 1) ||
      any(n_per_group != round(n_per_group))) {
    cli::cli_abort(
      "{.arg n_per_group} must be one whole number, or one per group ({n_groups} needed)."
    )
  }
  n_per_group <- rep_len(as.integer(n_per_group), n_groups)
  check_number(grand_mean, "grand_mean")
  check_number(between_sd, "between_sd", min = 0)
  check_number(within_sd, "within_sd", min = 0)
  check_name(group_name, "group_name")
  check_name(name, "name")
  if (identical(group_name, name)) {
    cli::cli_abort("{.arg group_name} and {.arg name} must be different.")
  }

  # Both random draws happen inside one seeded block so the whole dataset is
  # reproducible from a single seed.
  draws <- with_dg_seed(seed, {
    u <- stats::rnorm(n_groups, mean = 0, sd = between_sd)
    e <- stats::rnorm(sum(n_per_group), mean = 0, sd = within_sd)
    list(u = u, e = e)
  })

  group_index <- rep(seq_len(n_groups), times = n_per_group)
  labels <- paste0("G", formatC(seq_len(n_groups), width = nchar(n_groups), flag = "0"))
  y <- grand_mean + draws$u[group_index] + draws$e

  tau2 <- between_sd^2
  sigma2 <- within_sd^2
  icc <- if (tau2 + sigma2 == 0) NA_real_ else tau2 / (tau2 + sigma2)

  out <- stats::setNames(
    list(factor(labels[group_index], levels = labels), y),
    c(group_name, name)
  )

  new_dg_dataset(
    tibble::as_tibble(out),
    generator = "gen_grouped",
    params = list(
      n_groups = n_groups, n_per_group = n_per_group, grand_mean = grand_mean,
      between_sd = between_sd, within_sd = within_sd, icc = icc,
      group_name = group_name, name = name
    ),
    seed = seed,
    assumptions = paste(
      "Random-intercept model y = grand_mean + u_group + e_row.",
      "Normal, independent group effects, unrelated to group size."
    )
  )
}
