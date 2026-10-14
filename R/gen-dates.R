#' Generate dates uniformly over a range
#'
#' @param n Number of rows: a whole number of at least 1.
#' @param start,end First and last date of the range, inclusive. Anything
#'   [as.Date()] reads, such as `"2024-01-01"`. `end` must be on or after `start`.
#' @param name Column name: a single non-empty string.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return A `dg_dataset` (a tibble) with one `Date` column.
#'
#' @details
#' **Model.** Each row is an independent draw of a calendar day, uniform over
#' every day from `start` to `end`.
#'
#' **Assumptions and pitfalls.** Uniform days ignore weekday effects,
#' seasonality, trends, and holidays. Real activity (for example, clinic
#' visits or sales) usually clusters, so this model is suitable for testing
#' date handling and filters, not for realistic timing patterns.
#'
#' @export
#' @examples
#' x <- gen_dates(n = 5, start = "2024-01-01", end = "2024-12-31", seed = 1)
#' x
gen_dates <- function(n, start, end, name = "date", seed) {
  check_count(n)
  check_name(name)
  start <- parse_date_arg(start, "start")
  end <- parse_date_arg(end, "end")
  if (end < start) {
    cli::cli_abort(
      "{.arg end} must be on or after {.arg start}, but {.val {format(end)}} is before {.val {format(start)}}."
    )
  }

  days <- seq(start, end, by = "day")
  values <- with_dg_seed(seed, days[sample.int(length(days), size = n, replace = TRUE)])

  new_dg_dataset(
    single_column(values, name),
    generator = "gen_dates",
    params = list(n = n, start = format(start), end = format(end), name = name),
    seed = seed,
    assumptions = "Independent uniform draws over every calendar day in the range; no weekday or seasonal pattern."
  )
}

parse_date_arg <- function(x, arg) {
  d <- tryCatch(as.Date(x), error = function(e) NA)
  if (length(d) != 1 || is.na(d)) {
    cli::cli_abort("{.arg {arg}} must be a single date such as {.val 2024-01-31}, not {.val {x}}.")
  }
  d
}
