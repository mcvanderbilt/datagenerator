# datagenerator 0.0.0.9000

* Stage 1 generators: `gen_numeric()`, `gen_correlated()`, `gen_categorical()`,
  `gen_dates()`, `gen_ids()`, `gen_grouped()`, and `add_missingness()` (MCAR,
  MAR, and MNAR).
* Every generated object is a `dg_dataset`: it prints a synthetic-data banner
  and carries a provenance record readable with `dg_meta()`.
* All generators require a `seed` and leave the global random stream unchanged.
