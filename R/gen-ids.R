#' Generate unique identifiers
#'
#' @param n Number of rows: a whole number of at least 1.
#' @param prefix Text placed before each number, such as `"P-"`. May be `""`.
#' @param width Number of digits, zero-padded. Defaults to the number of digits
#'   in `n`, so every ID has the same length.
#' @param name Column name: a single non-empty string.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return A `dg_dataset` (a tibble) with one character column of unique IDs.
#'
#' @details
#' **Model.** The integers `1` to `n` are shuffled at random and then formatted
#' with the prefix. Shuffling means the row order does not reveal how the IDs
#' were assigned.
#'
#' **Assumptions and pitfalls.** IDs are arbitrary labels with no check digit
#' and no link to any real person, record, or system. Do not use them as real
#' identifiers. A generated ID that looks like a real one (such as a medical
#' record number or a Social Security number format) can be mistaken for a real
#' record.
#'
#' @export
#' @examples
#' gen_ids(n = 5, prefix = "P-", seed = 1)
gen_ids <- function(n, prefix = "ID", width = NULL, name = "id", seed) {
  check_count(n)
  check_name(name)
  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix)) {
    cli::cli_abort("{.arg prefix} must be a single string, not {.val {prefix}}.")
  }

  min_width <- nchar(as.character(n))
  if (is.null(width)) {
    width <- min_width
  }
  check_count(width, "width")
  if (width < min_width) {
    cli::cli_abort(
      "{.arg width} must be at least {min_width} so that all {n} IDs fit, not {.val {width}}."
    )
  }

  numbers <- with_dg_seed(seed, sample.int(n))
  values <- paste0(prefix, formatC(numbers, width = width, flag = "0", format = "d"))

  new_dg_dataset(
    single_column(values, name),
    generator = "gen_ids",
    params = list(n = n, prefix = prefix, width = width, name = name),
    seed = seed,
    assumptions = "Unique labels assigned to shuffled integers; carry no real-world meaning."
  )
}
