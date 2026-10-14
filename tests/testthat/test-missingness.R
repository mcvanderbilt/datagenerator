base_data <- gen_correlated(5000, means = c(a = 0, b = 0), sds = c(1, 1),
                            cor = diag(2), seed = 1)

test_that("MCAR hits the target rate and records the mechanism", {
  m <- add_missingness(base_data, prop = 0.2, mechanism = "MCAR", seed = 2)
  expect_s3_class(m, "dg_dataset")
  expect_lt(abs(mean(is.na(m$a)) - 0.2), 0.02)
  expect_lt(abs(mean(is.na(m$b)) - 0.2), 0.02)
  expect_identical(dg_meta(m)$missingness$mechanism, "MCAR")
})

test_that("MAR never makes the driver missing and depends on it", {
  m <- add_missingness(base_data, prop = 0.2, mechanism = "MAR",
                       driver = "b", cols = "a", strength = 2, seed = 2)
  expect_false(anyNA(m$b))
  expect_lt(abs(mean(is.na(m$a)) - 0.2), 0.02)
  # Rows with a missing a have, on average, larger b when strength > 0.
  expect_gt(mean(m$b[is.na(m$a)]), mean(m$b[!is.na(m$a)]))
})

test_that("MNAR depends on the missing value itself", {
  m <- add_missingness(base_data, prop = 0.2, mechanism = "MNAR",
                       cols = "a", strength = 2, seed = 2)
  expect_lt(abs(mean(is.na(m$a)) - 0.2), 0.02)
  # Compare the original values of the rows that became missing.
  expect_gt(mean(base_data$a[is.na(m$a)]), mean(base_data$a[!is.na(m$a)]))
})

test_that("strength = 0 makes MAR a plain MCAR-style rate", {
  m <- add_missingness(base_data, prop = 0.3, mechanism = "MAR",
                       driver = "b", cols = "a", strength = 0, seed = 3)
  expect_lt(abs(mean(is.na(m$a)) - 0.3), 0.02)
})

test_that("prop = 0 inserts no missing values", {
  m <- add_missingness(base_data, prop = 0, mechanism = "MCAR", seed = 1)
  expect_false(anyNA(m))
})

test_that("the same seed reproduces the same missingness", {
  a <- add_missingness(base_data, prop = 0.1, seed = 5)
  b <- add_missingness(base_data, prop = 0.1, seed = 5)
  expect_identical(strip_dg(a), strip_dg(b))
})

test_that("the assumption for each mechanism is recorded", {
  m <- add_missingness(base_data, prop = 0.1, mechanism = "MNAR",
                       cols = "a", seed = 1)
  expect_match(dg_meta(m)$assumptions, "MNAR")
  expect_named(dg_meta(m)$missingness$achieved_rate, "a")
})

test_that("misuse and invalid arguments are refused", {
  expect_error(add_missingness(as.data.frame(base_data), prop = 0.1, seed = 1),
               "datagenerator function")
  expect_error(add_missingness(base_data, prop = 1, seed = 1), "less than 1")
  expect_error(add_missingness(base_data, prop = 0.1, mechanism = "MAR", seed = 1),
               "driver.*required")
  expect_error(add_missingness(base_data, prop = 0.1, mechanism = "MAR",
                               driver = "b", cols = "b", seed = 1),
               "cannot also be in")
  expect_error(add_missingness(base_data, prop = 0.1, mechanism = "MCAR",
                               driver = "b", seed = 1),
               "only used with MAR")
  expect_error(add_missingness(base_data, prop = 0.1, mechanism = "MCAR",
                               cols = "nope", seed = 1),
               "Unknown column")
})

test_that("MNAR refuses non-numeric columns", {
  cat_data <- gen_categorical(100, levels = c("x", "y"), name = "g", seed = 1)
  expect_error(add_missingness(cat_data, prop = 0.1, mechanism = "MNAR",
                               cols = "g", seed = 1),
               "must be numeric")
})
