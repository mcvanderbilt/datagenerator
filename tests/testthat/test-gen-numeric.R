test_that("gen_numeric returns a tagged tibble with the requested shape", {
  x <- gen_numeric(50, mean = 10, sd = 2, name = "score", seed = 1)
  expect_s3_class(x, "dg_dataset")
  expect_s3_class(x, "tbl_df")
  expect_named(x, "score")
  expect_length(x$score, 50)
  expect_type(x$score, "double")
})

test_that("large samples recover the requested mean and sd", {
  x <- gen_numeric(1e5, mean = 10, sd = 2, seed = 1)
  expect_lt(abs(mean(x$x) - 10), 0.05)
  expect_lt(abs(stats::sd(x$x) - 2), 0.05)
})

test_that("sd = 0 gives a constant column", {
  x <- gen_numeric(5, mean = 3, sd = 0, seed = 1)
  expect_true(all(x$x == 3))
})

test_that("invalid inputs fail with actionable messages", {
  expect_error(gen_numeric(0, seed = 1), "single whole number of at least 1")
  expect_error(gen_numeric(2.5, seed = 1), "single whole number")
  expect_error(gen_numeric(Inf, seed = 1), "single whole number")
  expect_error(gen_numeric(5, sd = -1, seed = 1), "at least 0")
  expect_error(gen_numeric(5, mean = NA, seed = 1), "finite number")
  expect_error(gen_numeric(5, name = "", seed = 1), "non-empty")
})
