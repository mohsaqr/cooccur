# IMDB actor-genre long table (1970-2024)

Long-format table mapping each of the 624 actors in
[`actors`](https://saqr.me/cooccure/reference/actors.md) to every genre
of every movie they appeared in. Use this to build an actor
co-occurrence network grouped by genre: which actors share the same
genres? Pass `field = "actor"` and `by = "genre"` to
[`cooccurrence`](https://saqr.me/cooccure/reference/cooccurrence.md).

## Usage

``` r
actor_genres
```

## Format

A data frame with 2,502 rows and 2 variables:

- actor:

  Actor name.

- genre:

  Genre label (one row per actor-genre combination).

## Source

<https://developer.imdb.com/non-commercial-datasets/>

## Examples

``` r
head(actor_genres)
#>            actor     genre
#> 1    Phil Harris Adventure
#> 2    Phil Harris Animation
#> 3    Phil Harris    Comedy
#> 4 Larry Clemmons Adventure
#> 5 Larry Clemmons Animation
#> 6 Larry Clemmons    Comedy
cooccurrence(actor_genres, field = "actor", by = "genre", similarity = "jaccard")
#> # cooccurrence: 624 nodes, 172390 edges | density: 0.8869 | mean degree: 552.5321 | 20 transactions
#> # similarity: jaccard | counting: full
#> # possible edges: 194376 | isolates: 0
#> # top nodes: Babu Pathak(deg=623,str=240.1), Sheila Allen(deg=623,str=240.1), Stacey Pickren(deg=623,str=240.1), Armando Pizzuti(deg=623,str=236.2), Shameer Muhammed(deg=623,str=236.2)
#>            from               to weight count
#>      Adam Brown      Adnan Ghani      1     4
#>      Adam Brown   Akimasa Ohmori      1     4
#>     Adnan Ghani   Akimasa Ohmori      1     4
#>     Adam Strick         Aleks Le      1     3
#>       Akash Dev Anil K. Shivaram      1     5
#>        Abhilash     Anson Antony      1     4
#>        Abhilash  Antony Varghese      1     4
#>    Anson Antony  Antony Varghese      1     4
#>  Ashwani Chopra     Asif Ali Beg      1     5
#>          Aamani         Banerjee      1     3
#> # ... 172380 more edges
```
