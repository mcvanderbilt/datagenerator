# Run `code` with a fixed RNG seed, then restore the caller's RNG state.
#
# withr::with_seed() sets the seed for the duration of `code` and puts the
# global random stream back afterwards. A generator called with seed = 1 can
# therefore never change what the user's next rnorm() call returns.
with_dg_seed <- function(seed, code) {
  check_seed(seed)
  withr::with_seed(as.integer(seed), code)
}
