# IMDB movie metadata (1970-2024)

A sample of 1,000 highly-rated IMDB movies (rating \>= 7.0, \>= 1,000
votes) released between 1970 and 2024. The `genres` column is
comma-delimited and suitable for use as the `field` argument to
[`cooccurrence`](https://saqr.me/cooccure/reference/cooccurrence.md).

## Usage

``` r
movies
```

## Format

A data frame with 1,000 rows and 7 variables:

- tconst:

  IMDB title identifier (e.g. `"tt0068646"`).

- primaryTitle:

  Movie title.

- startYear:

  Release year (integer).

- genres:

  Comma-separated genre labels (e.g. `"Crime,Drama"`).

- decade:

  Release decade as a character string (e.g. `"1970s"`).

- averageRating:

  IMDB average user rating.

- numVotes:

  Number of IMDB user votes.

## Source

<https://developer.imdb.com/non-commercial-datasets/>

## Examples

``` r
head(movies)
#>       tconst                          primaryTitle startYear
#> 1  tt0118117                       The War at Home      1996
#> 2 tt10534996                                 Josep      2020
#> 3  tt4686844                   The Death of Stalin      2017
#> 4  tt0089957 Samaya obayatelnaya i privlekatelnaya      1985
#> 5  tt3469964                         Blind Massage      2014
#> 6  tt7647198                       Love and Shukla      2017
#>                      genres decade averageRating numVotes
#> 1                     Drama  1990s           7.0     2733
#> 2 Animation,Biography,Drama  2020s           7.4     2340
#> 3      Comedy,Drama,History  2010s           7.3   126237
#> 4            Comedy,Romance  1980s           7.4     2521
#> 5                     Drama  2010s           7.2     1804
#> 6      Comedy,Drama,Romance  2010s           7.2     1234
cooccurrence(movies, field = "genres", sep = ",", similarity = "jaccard")
#> # cooccurrence: 22 nodes, 129 edges | density: 0.5584 | mean degree: 11.7273 | 1000 transactions
#> # similarity: jaccard | counting: full
#> # possible edges: 231 | isolates: 0
#> # top nodes: Drama(deg=20,str=1.178), Comedy(deg=19,str=0.9227), Action(deg=17,str=1.07), Romance(deg=17,str=0.5551), Adventure(deg=15,str=0.9235)
#>         from          to    weight count
#>    Adventure   Animation 0.2846715    39
#>       Action       Crime 0.2369478    59
#>       Comedy       Drama 0.2111554   159
#>       Action   Adventure 0.2075472    44
#>    Biography Documentary 0.2037037    44
#>        Drama     Romance 0.1975867   131
#>        Crime    Thriller 0.1745283    37
#>       Comedy     Romance 0.1680000    63
#>  Documentary       Music 0.1590909    28
#>    Biography     History 0.1564626    23
#> # ... 119 more edges
```
