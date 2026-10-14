#' datagenerator: Synthetic Datasets with Provenance
#'
#' Generates synthetic datasets from explicit, validated parameters. Every
#' generator is reproducible from a `seed`, returns a tibble labelled as
#' synthetic, and records the parameters that produced it in a provenance
#' record that [dg_meta()] reads back.
#'
#' @section Synthetic data is not real data:
#' Synthetic data reduces privacy risk but does not remove it, and it can
#' reproduce or amplify bias from the parameters chosen. Do not use this
#' package to fabricate evidence, records, or research results. See the
#' "Responsible use" vignette.
#'
#' @keywords internal
"_PACKAGE"
