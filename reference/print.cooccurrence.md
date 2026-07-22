# Print a cooccurrence edge list

Prints a compact network diagnostic header followed by an edge preview.

## Usage

``` r
# S3 method for class 'cooccurrence'
print(x, n = 10L, ...)
```

## Arguments

- x:

  A `cooccurrence` data frame.

- n:

  Integer. Number of rows to show. Default 10.

- ...:

  Ignored.

## Value

Invisibly returns `x`.

## Examples

``` r
res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
print(res)
#> # cooccurrence: 3 nodes, 3 edges | density: 1.0000 | mean degree: 2.0000 | 3 transactions
#> # similarity: none | counting: full
#> # possible edges: 3 | isolates: 0
#> # top nodes: C(deg=2,str=4), A(deg=2,str=3), B(deg=2,str=3)
#>  from to weight count
#>     A  C      2     2
#>     B  C      2     2
#>     A  B      1     1
```
