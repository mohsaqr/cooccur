# Convert to tidygraph

Creates a `tbl_graph` from a `cooccurrence` edge list.

## Usage

``` r
as_tidygraph(x, ...)

# S3 method for class 'cooccurrence'
as_tidygraph(x, ...)
```

## Arguments

- x:

  A `cooccurrence` data frame.

- ...:

  Ignored.

## Value

A `tbl_graph` object.

## Examples

``` r
res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
if (requireNamespace("tidygraph", quietly = TRUE) &&
    requireNamespace("igraph",    quietly = TRUE)) {
  as_tidygraph(res)
}
#> # A tbl_graph: 3 nodes and 3 edges
#> #
#> # An undirected simple graph with 1 component
#> #
#> # Node Data: 3 × 1 (active)
#>   name 
#>   <chr>
#> 1 A    
#> 2 B    
#> 3 C    
#> #
#> # Edge Data: 3 × 4
#>    from    to weight count
#>   <int> <int>  <dbl> <dbl>
#> 1     1     3      2     2
#> 2     2     3      2     2
#> 3     1     2      1     1
```
