# Convert to cograph network

Creates a `cograph_network` object from a `cooccurrence` edge list,
compatible with
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
and other cograph functions.

Builds a `cograph_network` from a
[`co_bootstrap`](https://saqr.me/cooccure/reference/co_bootstrap.md)
result so cograph can plot it directly (e.g.
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)).
By default only `stable` edges are kept (the credible network) and edges
carry the bootstrap mean weight, so the plotted network is entirely
bootstrap-derived.

## Usage

``` r
as_cograph(x, ...)

# S3 method for class 'cooccurrence'
as_cograph(x, ...)

# S3 method for class 'co_bootstrap'
as_cograph(x, stable_only = TRUE, weight = c("boot_mean", "weight"), ...)
```

## Arguments

- x:

  A `co_bootstrap` result.

- ...:

  Ignored.

- stable_only:

  Logical. Keep only `stable` (credible) edges. Default `TRUE`.

- weight:

  Character. Edge weight to carry: `"boot_mean"` (bootstrap mean,
  default) or `"weight"` (observed).

## Value

A `cograph_network` object.

## Examples

``` r
res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
if (requireNamespace("cograph", quietly = TRUE)) {
  net <- as_cograph(res)
  net$n_nodes
}
#> [1] 3
```
