test_that("the same seed reproduces the same values", {
  a <- gen_numeric(10, seed = 7)
  b <- gen_numeric(10, seed = 7)
  expect_identical(strip_dg(a), strip_dg(b))
})

test_that("different seeds give different values", {
  expect_false(identical(
    strip_dg(gen_numeric(10, seed = 1)),
    strip_dg(gen_numeric(10, seed = 2))
  ))
})

test_that("generators leave the caller's global RNG stream untouched", {
  set.seed(123)
  before <- stats::runif(1)
  set.seed(123)
  gen_numeric(10, seed = 99)
  gen_grouped(3, 4, seed = 99)
  after <- stats::runif(1)
  expect_identical(before, after)
})

test_that("a missing seed is refused with a message that says why", {
  expect_error(gen_numeric(5), "seed.*required")
})

test_that("invalid seeds are refused", {
  expect_error(gen_numeric(5, seed = "a"), "single whole number")
  expect_error(gen_numeric(5, seed = 1.5), "single whole number")
  expect_error(gen_numeric(5, seed = c(1, 2)), "single whole number")
})

test_that("printing shows the synthetic banner and the generator", {
  x <- gen_numeric(3, seed = 1)
  expect_output(print(x), "SYNTHETIC DATA")
  expect_output(print(x), "generator: gen_numeric")
})

test_that("the provenance record holds generator, parameters and seed", {
  x <- gen_numeric(3, mean = 5, seed = 42)
  m <- dg_meta(x)
  expect_true(m$synthetic)
  expect_identical(m$generator, "gen_numeric")
  expect_identical(m$seed, 42)
  expect_identical(m$params$mean, 5)
  expect_false(is.null(m$assumptions))
})

test_that("dg_meta refuses objects not made by the package", {
  expect_error(dg_meta(data.frame(a = 1)), "datagenerator function")
})
