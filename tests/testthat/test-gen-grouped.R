test_that("gen_grouped returns the right number of rows and groups", {
  x <- gen_grouped(n_groups = 40, n_per_group = 25, between_sd = 2,
                   within_sd = 1, seed = 1)
  expect_s3_class(x, "dg_dataset")
  expect_equal(nrow(x), 1000)
  expect_equal(nlevels(x$group), 40)
  expect_named(x, c("group", "y"))
})

test_that("the implied ICC is recorded in the provenance record", {
  x <- gen_grouped(40, 25, between_sd = 2, within_sd = 1, seed = 1)
  expect_equal(dg_meta(x)$params$icc, 4 / 5)
})

test_that("with within_sd = 0, every row in a group has the same value", {
  x <- gen_grouped(n_groups = 8, n_per_group = 10, between_sd = 3,
                   within_sd = 0, seed = 1)
  spread <- tapply(x$y, x$group, stats::sd)
  expect_true(all(spread == 0))
  expect_gt(stats::sd(tapply(x$y, x$group, mean)), 0)
})

test_that("unbalanced group sizes are supported", {
  x <- gen_grouped(n_groups = 3, n_per_group = c(2, 5, 9), seed = 1)
  expect_equal(as.integer(table(x$group)), c(2L, 5L, 9L))
})

test_that("invalid group arguments are rejected", {
  expect_error(gen_grouped(3, n_per_group = c(2, 5), seed = 1),
               "one whole number, or one per group")
  expect_error(gen_grouped(3, 5, between_sd = -1, seed = 1), "at least 0")
  expect_error(gen_grouped(3, 5, group_name = "y", name = "y", seed = 1),
               "must be different")
})
