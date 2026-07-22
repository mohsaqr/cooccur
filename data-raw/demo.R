# Builds data/demo.rda — a small hand-crafted actor / movie / genre table.
#
# One row per (movie, actor, genre) triple. Films carry their full IMDB
# genre list rather than a single "primary" genre, so all three groupings
# produce a network:
#   field = "actor", by = "movie"   co-starring
#   field = "genre", by = "movie"   genre co-occurrence within a film
#   field = "actor", by = "genre"   actors sharing a genre
#
# Run with:  Rscript data-raw/demo.R

cast <- list(
  "The Godfather"          = c("Marlon Brando", "Al Pacino", "James Caan",
                               "Robert Duvall"),
  "Goodfellas"             = c("Robert De Niro", "Ray Liotta", "Joe Pesci",
                               "Lorraine Bracco"),
  "Heat"                   = c("Al Pacino", "Robert De Niro", "Val Kilmer",
                               "Jon Voight"),
  "The Dark Knight"        = c("Christian Bale", "Heath Ledger",
                               "Michael Caine"),
  "Pulp Fiction"           = c("John Travolta", "Samuel L. Jackson",
                               "Uma Thurman", "Bruce Willis"),
  "Fight Club"             = c("Brad Pitt", "Edward Norton",
                               "Helena Bonham Carter"),
  "Inception"              = c("Leonardo DiCaprio", "Joseph Gordon-Levitt",
                               "Michael Caine"),
  "The Departed"           = c("Leonardo DiCaprio", "Matt Damon",
                               "Jack Nicholson", "Mark Wahlberg"),
  "No Country for Old Men" = c("Javier Bardem", "Josh Brolin",
                               "Tommy Lee Jones"),
  "There Will Be Blood"    = c("Daniel Day-Lewis", "Paul Dano")
)

## Full IMDB genre lists, not a single primary label.
genres <- list(
  "The Godfather"          = c("Crime", "Drama"),
  "Goodfellas"             = c("Biography", "Crime", "Drama"),
  "Heat"                   = c("Action", "Crime", "Drama"),
  "The Dark Knight"        = c("Action", "Crime", "Drama"),
  "Pulp Fiction"           = c("Crime", "Drama"),
  "Fight Club"             = c("Drama", "Thriller"),
  "Inception"              = c("Action", "Adventure", "Sci-Fi"),
  "The Departed"           = c("Crime", "Drama", "Thriller"),
  "No Country for Old Men" = c("Crime", "Drama", "Thriller"),
  "There Will Be Blood"    = c("Drama", "History")
)

stopifnot(setequal(names(cast), names(genres)))

demo <- do.call(rbind, lapply(names(cast), function(m) {
  expand.grid(movie = m, actor = cast[[m]], genre = genres[[m]],
              stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
}))
demo <- demo[order(match(demo$movie, names(cast)), demo$actor, demo$genre), ]
rownames(demo) <- NULL
demo <- demo[, c("movie", "actor", "genre")]

save(demo, file = "data/demo.rda", compress = "bzip2", version = 2)

cat(sprintf("demo: %d rows, %d movies, %d actors, %d genres\n",
            nrow(demo), length(unique(demo$movie)),
            length(unique(demo$actor)), length(unique(demo$genre))))
