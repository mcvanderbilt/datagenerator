# datagenerator

Generate synthetic datasets from explicit, validated parameters. Every dataset
is reproducible from a seed, prints a banner that says it is synthetic, and
carries a record of the parameters that produced it.

> **Synthetic data is not real data.** Do not use this package to fabricate
> evidence, records, or research results. See the *Responsible use* vignette.

## Installation

This package is in development and is not on CRAN.

```r
# install.packages("pak")
pak::pak("mcvanderbilt/datagenerator")
```

## Example

```r
library(datagenerator)

x <- gen_correlated(
  n = 100,
  means = c(height = 170, weight = 70),
  sds = c(10, 12),
  cor = matrix(c(1, 0.6, 0.6, 1), nrow = 2),
  seed = 1
)
x
dg_meta(x)$params
```

## Current generators (Stage 1)

| Function | Produces |
|---|---|
| `gen_numeric()` | One normal variable |
| `gen_correlated()` | Several correlated normal variables |
| `gen_categorical()` | A factor with fixed proportions |
| `gen_dates()` | Dates uniform over a range |
| `gen_ids()` | Unique prefixed labels |
| `gen_grouped()` | Grouped data with a random group effect |
| `add_missingness()` | MCAR, MAR, or MNAR missing values |

See `BACKLOG.md` for what is planned next.

## License

MIT. See `LICENSE.md`.
