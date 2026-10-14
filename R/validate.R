# Input checks. Every generator calls these before doing any random work, so
# bad input fails at the call site with a message that names the argument and
# the fix.

check_count <- function(n, arg = "n") {
  ok <- is.numeric(n) && length(n) == 1 && is.finite(n) &&
    n >= 1 && n == round(n)
  if (!ok) {
    cli::cli_abort(
      "{.arg {arg}} must be a single whole number of at least 1, not {.val {n}}."
    )
  }
  invisible(TRUE)
}

check_number <- function(x, arg, min = -Inf) {
  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a single finite number, not {.val {x}}.")
  }
  if (x < min) {
    cli::cli_abort("{.arg {arg}} must be at least {min}, not {.val {x}}.")
  }
  invisible(TRUE)
}

check_name <- function(name, arg = "name") {
  ok <- is.character(name) && length(name) == 1 && !is.na(name) && nzchar(name)
  if (!ok) {
    cli::cli_abort("{.arg {arg}} must be a single non-empty string, not {.val {name}}.")
  }
  invisible(TRUE)
}

check_seed <- function(seed) {
  if (missing(seed)) {
    cli::cli_abort(c(
      "{.arg seed} is required.",
      "i" = "Supply one (for example {.code seed = 42}) so the result can be reproduced."
    ))
  }
  ok <- is.numeric(seed) && length(seed) == 1 && is.finite(seed) &&
    seed == round(seed) && abs(seed) <= .Machine$integer.max
  if (!ok) {
    cli::cli_abort("{.arg seed} must be a single whole number, not {.val {seed}}.")
  }
  invisible(TRUE)
}
