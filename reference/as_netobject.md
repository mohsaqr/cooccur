# Convert to Nestimate netobject

Creates a `netobject` from a `cooccurrence` edge list, compatible with
`Nestimate::centrality()`,
[`Nestimate::bootstrap_network()`](https://saqr.me/Nestimate/reference/bootstrap_network.html),
etc.

## Usage

``` r
as_netobject(x, ...)

# S3 method for class 'cooccurrence'
as_netobject(x, ...)

# S3 method for class 'co_bootstrap'
as_netobject(x, stable_only = TRUE, weight = c("boot_mean", "weight"), ...)
```

## Arguments

- x:

  A `cooccurrence` data frame.

- ...:

  Ignored.

- stable_only:

  Logical. For a `co_bootstrap` result, keep only `stable` (credible)
  edges. Default `TRUE`.

- weight:

  Character. For a `co_bootstrap` result, edge weight to carry:
  `"boot_mean"` (default) or `"weight"`.

## Value

A `netobject` with class `c("netobject", "cograph_network")`.

## Examples

``` r
res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
if (requireNamespace("Nestimate", quietly = TRUE)) {
  net <- as_netobject(res)
  net$n_nodes
}
#> [1] 3
```
