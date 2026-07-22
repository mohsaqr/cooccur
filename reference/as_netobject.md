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
```

## Arguments

- x:

  A `cooccurrence` data frame.

- ...:

  Ignored.

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
