# Bootstrap edge stability for a co-occurrence network

Resamples transactions, or user-supplied transaction blocks, from a
fitted
[`cooccurrence`](https://saqr.me/cooccure/reference/cooccurrence.md)
object and recomputes edge weights on the fixed post-`min_occur` item
support. The bootstrap uses the same similarity measure as the original
fit, with no per-replicate thresholding or top-n filtering.

## Usage

``` r
co_bootstrap(
  x,
  R = 1000,
  by = NULL,
  engine = c("classic", "bayes"),
  ci = 0.95,
  consistency_range = c(0.75, 1.25),
  consistency = 0.95,
  seed = NULL
)
```

## Arguments

- x:

  A fitted `cooccurrence` object created with
  `keep_transactions = TRUE`.

- R:

  Integer. Number of bootstrap replicates. Default `1000`.

- by:

  Optional block identifier aligned to the retained transactions, or
  `"block_id"` to use the block metadata stored on `x`. When `NULL`,
  each transaction is resampled independently.

- engine:

  Character. `"classic"` uses multinomial resampling of blocks.
  `"bayes"` uses Rubin's Bayesian bootstrap with exponential block
  weights scaled to sum to the number of blocks.

- ci:

  Numeric. Percentile interval coverage reported in `ci_low`/`ci_high`.
  Default `0.95`.

- consistency_range:

  Numeric length-2 multiplicative band, as in
  [`Nestimate::bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html).
  An edge is `stable` only if its resampled weight stays within
  `consistency_range * weight` (two-sided) in at least `consistency` of
  the resamples. Default `c(0.75, 1.25)`. Two-sided, so weights that
  land consistently far above the estimate are flagged unstable too.

- consistency:

  Numeric in `(0, 1]`. Proportion of resamples that must fall within the
  band for an edge to be `stable`. Default `0.95`.

- seed:

  Optional integer random seed.

## Value

A data frame with class
`c("co_bootstrap", "cooccurrence", "data.frame")` and one row per
original edge. Columns: `from`, `to`, observed `weight`, `boot_mean`,
`boot_se`, percentile bounds `ci_low` / `ci_high`, consistency band
`cr_lower` / `cr_upper`, `prop_within` (share of resamples inside the
band), and the logical `stable` flag (`prop_within >= consistency`).

## Examples

``` r
tx <- list(c("A", "B"), c("A", "B", "C"), c("A", "C"), c("B", "C"))
fit <- cooccurrence(tx, similarity = "jaccard", keep_transactions = TRUE)
co_bootstrap(fit, R = 20, seed = 1)
#> co_bootstrap() is experimental — interpret results with caution.
#> # cooccurrence: 3 nodes, 3 edges (4 transactions) | similarity: jaccard
#>  from to weight boot_mean   boot_se  ci_low ci_high cr_lower cr_upper
#>     A  B    0.5     0.600 0.2615742 0.25000       1    0.375    0.625
#>     A  C    0.5     0.575 0.2821440 0.25000       1    0.375    0.625
#>     B  C    0.5     0.525 0.2913219 0.11875       1    0.375    0.625
#>  prop_within stable
#>         0.40  FALSE
#>         0.15  FALSE
#>         0.15  FALSE
```
