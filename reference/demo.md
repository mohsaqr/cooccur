# Demo actor-movie-genre table

A small hand-crafted dataset of 30 well-known actors across 10 classic
films with genre labels. Designed for quick exploration in the Shiny
app. Use `field = "actor"` with `by = "movie"` or `by = "genre"`.

## Usage

``` r
demo
```

## Format

A data frame with 34 rows and 3 variables:

- movie:

  Movie title.

- actor:

  Actor name.

- genre:

  Primary genre label.

## Examples

``` r
head(demo)
#>           movie          actor genre
#> 1 The Godfather  Marlon Brando Crime
#> 2 The Godfather      Al Pacino Crime
#> 3 The Godfather     James Caan Crime
#> 4 The Godfather  Robert Duvall Crime
#> 5    Goodfellas Robert De Niro Crime
#> 6    Goodfellas     Ray Liotta Crime
cooccurrence(demo, field = "actor", by = "movie", similarity = "jaccard")
#> # cooccurrence: 30 nodes, 43 edges | density: 0.0989 | mean degree: 2.8667 | 10 transactions
#> # similarity: jaccard | counting: full
#> # possible edges: 435 | isolates: 0
#> # top nodes: Al Pacino(deg=6,str=2.833), Robert De Niro(deg=6,str=2.833), Leonardo DiCaprio(deg=5,str=2.333), Michael Caine(deg=4,str=1.833), Bruce Willis(deg=3,str=3)
#>            from                   to weight count
#>       Brad Pitt        Edward Norton      1     1
#>  Christian Bale         Heath Ledger      1     1
#>       Brad Pitt Helena Bonham Carter      1     1
#>   Edward Norton Helena Bonham Carter      1     1
#>    Bruce Willis        John Travolta      1     1
#>   Javier Bardem          Josh Brolin      1     1
#>       Joe Pesci      Lorraine Bracco      1     1
#>  Jack Nicholson        Mark Wahlberg      1     1
#>      James Caan        Marlon Brando      1     1
#>  Jack Nicholson           Matt Damon      1     1
#> # ... 33 more edges
```
