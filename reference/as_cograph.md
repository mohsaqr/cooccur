# Convert to cograph network

Creates a `cograph_network` object from a `cooccurrence` edge list,
compatible with
[`cograph::splot()`](https://sonsoles.me/cograph/reference/splot.html)
and other cograph functions.

## Usage

``` r
as_cograph(x, ...)

# S3 method for class 'cooccurrence'
as_cograph(x, ...)
```

## Arguments

- x:

  A `cooccurrence` data frame.

- ...:

  Ignored.

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
