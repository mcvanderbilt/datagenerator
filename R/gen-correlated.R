#' Generate correlated normal variables
#'
#' @param n Number of rows: a whole number of at least 1.
#' @param means Numeric vector of column means. Its names become the column
#'   names; unnamed means are called `x1`, `x2`, and so on.
#' @param sds Positive standard deviations, one per element of `means`.
#' @param cor Correlation matrix: square, symmetric, with 1 on the diagonal,
#'   one row and column per element of `means`, and positive definite.
#' @param seed A single whole number. Required, so the result can be reproduced.
#'
#' @return A `dg_dataset` (a tibble) with one double column per variable.
#'
#' @details
#' **Model.** The rows are draws from a multivariate normal distribution with
#' mean vector `means` and covariance `diag(sds) %*% cor %*% diag(sds)`. The
#' draws are made as `Z %*% chol(Sigma)` for independent standard normals `Z`,
#' which is the standard way to induce a target covariance.
#'
#' **Assumptions and pitfalls.**
#' * Dependence is linear only. The Pearson correlation in `cor` describes
#'   straight-line association and cannot produce nonlinear or tail-dependent
#'   relationships.
#' * Every margin is normal. A copula would allow other margins but is not
#'   part of this version.
#' * If `cor` is not positive definite, no valid joint distribution exists.
#'   Correlations that are individually plausible can still be jointly
#'   impossible, for example three variables each pairwise correlated at
#'   0.9 with one of the pairs at -0.9. The function reports this instead of
#'   returning a distribution that does not exist.
#'
#' @export
#' @examples
#' cor_m <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
#' x <- gen_correlated(
#'   n = 200,
#'   means = c(height = 170, weight = 70),
#'   sds = c(10, 12),
#'   cor = cor_m,
#'   seed = 1
#' )
#' cor(x$height, x$weight)
gen_correlated <- function(n, means, sds, cor, seed) {
  check_count(n)

  if (!is.numeric(means) || length(means) < 1 || any(!is.finite(means))) {
    cli::cli_abort("{.arg means} must be a non-empty set of finite numbers.")
  }
  p <- length(means)

  if (!is.numeric(sds) || length(sds) != p || any(!is.finite(sds)) || any(sds <= 0)) {
    cli::cli_abort(
      "{.arg sds} must be positive numbers, one per element of {.arg means} ({p} needed)."
    )
  }

  if (!is.matrix(cor) || !is.numeric(cor) || nrow(cor) != p || ncol(cor) != p) {
    cli::cli_abort(
      "{.arg cor} must be a numeric {p} x {p} matrix, one row and column per element of {.arg means}."
    )
  }
  if (anyNA(cor) || any(abs(cor) > 1)) {
    cli::cli_abort("{.arg cor} must contain only numbers between -1 and 1.")
  }
  if (any(abs(diag(cor) - 1) > 1e-8)) {
    cli::cli_abort("The diagonal of {.arg cor} must be 1.")
  }
  if (!isSymmetric(cor, tol = 1e-8)) {
    cli::cli_abort("{.arg cor} must be symmetric.")
  }

  var_names <- names(means)
  if (is.null(var_names)) var_names <- paste0("x", seq_len(p))

  # Covariance = D R D, where D holds the standard deviations and R is the
  # correlation matrix. chol() fails exactly when Sigma is not positive definite.
  D <- diag(sds, nrow = p)
  Sigma <- D %*% cor %*% D
  R <- tryCatch(
    chol(Sigma),
    error = function(e) {
      cli::cli_abort(c(
        "{.arg cor} is not positive definite, so no joint distribution has these correlations.",
        "i" = "Check that the pairwise correlations are jointly consistent, for example with eigen(cor)$values, which must all be positive."
      ))
    }
  )

  Z <- with_dg_seed(seed, matrix(stats::rnorm(n * p), nrow = n, ncol = p))
  X <- sweep(Z %*% R, 2, means, "+")
  colnames(X) <- var_names

  new_dg_dataset(
    X,
    generator = "gen_correlated",
    params = list(n = n, means = means, sds = sds, cor = cor),
    seed = seed,
    assumptions = paste(
      "Multivariate Normal with covariance diag(sds) %*% cor %*% diag(sds).",
      "Linear (Pearson) dependence only; every margin is normal."
    )
  )
}
