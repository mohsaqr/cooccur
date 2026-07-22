# Tidy node or group table from a cooccurrence summary

Returns the summary's node-level metrics (one row per node) or its
group-level metrics (one row per group) as a plain `data.frame`, so
callers never have to reach into the summary object's internals.

## Usage

``` r
# S3 method for class 'summary.cooccurrence'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  what = c("nodes", "groups"),
  ...
)
```

## Arguments

- x:

  A `summary.cooccurrence` object.

- row.names:

  Passed to
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html); unused.

- optional:

  Passed to
  [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html); unused.

- what:

  Character. `"nodes"` (default) for one row per node with degree,
  strength, count strength, and frequency; `"groups"` for one row per
  group when the network was built with `split_by`.

- ...:

  Ignored.

## Value

A base `data.frame`. For `what = "nodes"`, one row per node; for
`what = "groups"`, one row per group (zero rows when the network was not
split).

## Examples

``` r
res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
as.data.frame(summary(res))
#> cooccurrence network
#> ------------------------------
#> Nodes          : 3
#> Edges          : 3
#> Possible edges : 3
#> Density        : 1.0000
#> Mean degree    : 2.0000
#> Isolates       : 0
#> Transactions   : 3
#> Similarity     : none
#> Counting       : full
#> Weight range   : [1, 2]
#> Weight mean    : 1.667
#> Count range    : [1, 2]
#> Count mean     : 1.667
#> Top nodes      : C(2, 4), A(2, 3), B(2, 3)
#>   node degree strength count_strength frequency
#> 1    C      2        4              4         3
#> 2    A      2        3              3         2
#> 3    B      2        3              3         2
```
