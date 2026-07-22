# IMDB actor-movie long table (1970-2024)

Long-format bipartite table linking actors to movies in
[`movies`](https://saqr.me/cooccure/reference/movies.md). Pre-filtered
to the 624 actors who appear in at least two movies, so all similarity
measures compute instantly. Pass `field = "actor"` and `by = "tconst"`
to [`cooccurrence`](https://saqr.me/cooccure/reference/cooccurrence.md)
to build an actor co-appearance network.

## Usage

``` r
actors
```

## Format

A data frame with 1,267 rows and 7 variables:

- actor:

  Actor name.

- tconst:

  IMDB title identifier linking to
  [`movies`](https://saqr.me/cooccure/reference/movies.md).

- primaryTitle:

  Movie title.

- startYear:

  Release year (integer).

- decade:

  Release decade as a character string.

- genres:

  Comma-separated genre labels for the linked movie.

- averageRating:

  IMDB average user rating for the linked movie.

## Source

<https://developer.imdb.com/non-commercial-datasets/>

## Examples

``` r
head(actors)
#>                actor    tconst   primaryTitle startYear decade
#> 24       Phil Harris tt0065421 The Aristocats      1970  1970s
#> 25    Larry Clemmons tt0065421 The Aristocats      1970  1970s
#> 26     Paul Winchell tt0065421 The Aristocats      1970  1970s
#> 34 Sterling Holloway tt0065421 The Aristocats      1970  1970s
#> 37  Scatman Crothers tt0065421 The Aristocats      1970  1970s
#> 43       Vito Scotti tt0065421 The Aristocats      1970  1970s
#>                        genres averageRating
#> 24 Adventure,Animation,Comedy           7.1
#> 25 Adventure,Animation,Comedy           7.1
#> 26 Adventure,Animation,Comedy           7.1
#> 34 Adventure,Animation,Comedy           7.1
#> 37 Adventure,Animation,Comedy           7.1
#> 43 Adventure,Animation,Comedy           7.1
cooccurrence(actors, field = "actor", by = "tconst", similarity = "jaccard")
#> # cooccurrence: 624 nodes, 4353 edges | density: 0.0224 | mean degree: 13.9519 | 401 transactions
#> # similarity: jaccard | counting: full
#> # possible edges: 194376 | isolates: 34
#> # top nodes: Dawn Michelle King(deg=66,str=24.67), Ricki Lander(deg=66,str=24.67), Brad Rea(deg=66,str=24.67), Kevin LaRosa II(deg=66,str=24.67), Michael Runyard(deg=66,str=24.67)
#>                  from                 to weight count
#>          Anson Antony    Antony Varghese      1     2
#>    Catalina Harabagiu Cerasela Iosifescu      1     2
#>    Ben Hernandez Bray    Chris Ingersoll      1     2
#>         Bruce Seifert      Chris Sanders      1     2
#>  Christiant D'Alberto      Chuck Mathews      1     2
#>  Christiant D'Alberto      Claire Koonce      1     2
#>         Chuck Mathews      Claire Koonce      1     2
#>      Akihiko Sugizaki        Daisuke Ryû      1     2
#>       Alicia Aguilera      Dane Anderson      1     2
#>            Adi Granov       David Marten      1     2
#> # ... 4343 more edges
```
