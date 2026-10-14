test_that("gen_dates draws Date values inside the inclusive range", {
  x <- gen_dates(200, start = "2024-01-01", end = "2024-12-31", seed = 1)
  expect_s3_class(x, "dg_dataset")
  expect_s3_class(x$date, "Date")
  expect_true(all(x$date >= as.Date("2024-01-01")))
  expect_true(all(x$date <= as.Date("2024-12-31")))
})

test_that("a one-day range returns that day every time", {
  x <- gen_dates(4, start = "2024-05-05", end = "2024-05-05", seed = 1)
  expect_true(all(x$date == as.Date("2024-05-05")))
})

test_that("invalid date ranges and values are rejected", {
  expect_error(gen_dates(5, start = "2024-02-01", end = "2024-01-01", seed = 1),
               "on or after")
  expect_error(gen_dates(5, start = "not a date", end = "2024-01-01", seed = 1),
               "single date")
})

test_that("gen_ids returns unique, prefixed, equal-width, shuffled labels", {
  x <- gen_ids(500, prefix = "P-", seed = 1)
  expect_equal(anyDuplicated(x$id), 0)
  expect_true(all(startsWith(x$id, "P-")))
  expect_true(all(nchar(x$id) == 5))
  expect_false(identical(x$id, sort(x$id)))
})

test_that("gen_ids enforces a width that fits every ID", {
  expect_error(gen_ids(100, width = 2, seed = 1), "at least 3")
})
