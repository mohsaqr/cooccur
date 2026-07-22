# Demo actor-movie-genre table

A small hand-crafted dataset of 30 well-known actors across 10 classic
films, with each film carrying its full genre list. Designed for quick
exploration in the Shiny app. One row per movie-actor-genre triple, so
every pairing of `field` and `by` yields a network: actors linked by
shared films, genres linked by shared films, or actors linked by shared
genres.

## Usage

``` r
demo
```

## Format

A data frame with 89 rows and 3 variables:

- movie:

  Movie title.

- actor:

  Actor name.

- genre:

  Genre label. Films carry every genre they belong to, so a movie
  contributes several rows per actor.

## Examples

``` r
head(demo)
#>           movie         actor genre
#> 1 The Godfather     Al Pacino Crime
#> 2 The Godfather     Al Pacino Drama
#> 3 The Godfather    James Caan Crime
#> 4 The Godfather    James Caan Drama
#> 5 The Godfather Marlon Brando Crime
#> 6 The Godfather Marlon Brando Drama
# Actors linked by the films they share
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

# Genres linked by the films they share
cooccurrence(demo, field = "genre", by = "movie", similarity = "jaccard")
#> # cooccurrence: 8 nodes, 11 edges | density: 0.3929 | mean degree: 2.7500 | 10 transactions
#> # similarity: jaccard | counting: full
#> # possible edges: 28 | isolates: 0
#> # top nodes: Drama(deg=5,str=1.533), Crime(deg=4,str=1.421), Action(deg=4,str=1.117), Adventure(deg=2,str=1.333), Sci-Fi(deg=2,str=1.333)
#>       from        to    weight count
#>  Adventure    Sci-Fi 1.0000000     1
#>      Crime     Drama 0.7777778     7
#>     Action Adventure 0.3333333     1
#>     Action    Sci-Fi 0.3333333     1
#>      Drama  Thriller 0.3333333     3
#>     Action     Crime 0.2500000     2
#>      Crime  Thriller 0.2500000     2
#>     Action     Drama 0.2000000     2
#>  Biography     Crime 0.1428571     1
#>  Biography     Drama 0.1111111     1
#> # ... 1 more edges

# Actors linked by the genres they share
cooccurrence(demo, field = "actor", by = "genre")
#> # cooccurrence: 30 nodes, 414 edges | density: 0.9517 | mean degree: 27.6000 | 8 transactions
#> # similarity: none | counting: full
#> # possible edges: 435 | isolates: 0
#> # top nodes: Leonardo DiCaprio(deg=29,str=72), Michael Caine(deg=29,str=63), Robert De Niro(deg=29,str=62), Al Pacino(deg=29,str=59), Christian Bale(deg=29,str=59)
#>               from             to weight count
#>  Leonardo DiCaprio  Michael Caine      5     5
#>          Al Pacino Christian Bale      3     3
#>          Al Pacino   Heath Ledger      3     3
#>     Christian Bale   Heath Ledger      3     3
#>     Jack Nicholson  Javier Bardem      3     3
#>          Al Pacino     Jon Voight      3     3
#>     Christian Bale     Jon Voight      3     3
#>       Heath Ledger     Jon Voight      3     3
#>     Jack Nicholson    Josh Brolin      3     3
#>      Javier Bardem    Josh Brolin      3     3
#> # ... 404 more edges
```
