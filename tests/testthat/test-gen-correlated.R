cor_m <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)

test_that("sample moments and correlation approach the targets", {
  x <- gen_correlated(2e4, means = c(a = 0, b = 5), sds = c(1, 3),
                      cor = cor_m, seed = 1)
  expect_s3_class(x, "dg_dataset")
  expect_named(x, c("a", "b"))
  expect_lt(abs(stats::cor(x$a, x$b) - 0.6), 0.02)
  expect_lt(abs(mean(x$b) - 5), 0.1)
  expect_lt(abs(stats::sd(x$b) - 3), 0.1)
})

test_that("unnamed means get default column names", {
  x <- gen_correlated(5, means = c(0, 0), sds = c(1, 1), cor = diag(2), seed = 1)
  expect_named(x, c("x1", "x2"))
})

test_that("a non-positive-definite correlation matrix is rejected", {
  bad <- matrix(c(1, 0.9, -0.9, 0.9, 1, 0.9, -0.9, 0.9, 1), nrow = 3)
  expect_error(
    gen_correlated(10, means = c(0, 0, 0), sds = c(1, 1, 1), cor = bad, seed = 1),
    "positive definite"
  )
})

test_that("malformed correlation matrices are rejected", {
  expect_error(
    gen_correlated(10, means = c(0, 0), sds = c(1, 1), cor = diag(3), seed = 1),
    "2 x 2 matrix"
  )
  expect_error(
    gen_correlated(10, means = c(0, 0), sds = c(1, 1),
                   cor = matrix(c(1, 0.5, 0.3, 1), 2), seed = 1),
    "symmetric"
  )
  expect_error(
    gen_correlated(10, means = c(0, 0), sds = c(1, 1),
                   cor = matrix(c(0.9, 0.5, 0.5, 0.9), 2), seed = 1),
    "diagonal"
  )
  expect_error(
    gen_correlated(10, means = c(0, 0), sds = c(1, 1),
                   cor = matrix(c(1, 1.2, 1.2, 1), 2), seed = 1),
    "between -1 and 1"
  )
})

test_that("non-positive standard deviations are rejected", {
  expect_error(
    gen_correlated(10, means = c(0, 0), sds = c(1, 0), cor = diag(2), seed = 1),
    "positive"
  )
})
