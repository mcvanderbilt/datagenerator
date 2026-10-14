test_that("realized proportions approach the requested probabilities", {
  x <- gen_categorical(1e5, levels = c("A", "B", "C"),
                       probs = c(0.5, 0.3, 0.2), seed = 1)
  expect_s3_class(x, "dg_dataset")
  expect_true(is.factor(x$category))
  expect_identical(levels(x$category), c("A", "B", "C"))
  p <- as.numeric(prop.table(table(x$category)))
  expect_lt(max(abs(p - c(0.5, 0.3, 0.2))), 0.01)
})

test_that("equal probabilities are the default", {
  x <- gen_categorical(1e5, levels = c("a", "b", "c", "d"), seed = 1)
  p <- as.numeric(prop.table(table(x$category)))
  expect_lt(max(abs(p - 0.25)), 0.01)
})

test_that("invalid levels and probabilities are rejected", {
  expect_error(gen_categorical(5, levels = "only", seed = 1), "at least two")
  expect_error(gen_categorical(5, levels = c("a", "a"), seed = 1), "unique")
  expect_error(gen_categorical(5, levels = c("a", "b"), probs = c(0.5, 0.6), seed = 1),
               "sum to 1")
  expect_error(gen_categorical(5, levels = c("a", "b"), probs = c(1.2, -0.2), seed = 1),
               "non-negative")
  expect_error(gen_categorical(5, levels = c("a", "b", "c"), probs = c(0.5, 0.5), seed = 1),
               "one per level")
})
