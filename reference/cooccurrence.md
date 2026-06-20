# Build a co-occurrence network

Constructs an undirected co-occurrence network from various input
formats and returns a tidy edge data frame. Argument names follow the
citenets convention.

## Usage

``` r
cooccurrence(
  data,
  field = NULL,
  by = NULL,
  sep = NULL,
  weight_by = NULL,
  split_by = NULL,
  aggregate_by = NULL,
  aggregate = c("sum", "mean", "min", "max"),
  keep_transactions = TRUE,
  block = NULL,
  window = NULL,
  similarity = c("none", "jaccard", "cosine", "inclusion", "association", "dice",
    "equivalence", "relative"),
  counting = c("full", "fractional", "attention"),
  lambda = 1,
  scale = NULL,
  threshold = 0,
  min_occur = 1L,
  top_n = NULL,
  output = c("default", "gephi", "igraph", "cograph", "matrix"),
  ...
)

co(
  data,
  field = NULL,
  by = NULL,
  sep = NULL,
  weight_by = NULL,
  split_by = NULL,
  aggregate_by = NULL,
  aggregate = c("sum", "mean", "min", "max"),
  keep_transactions = TRUE,
  block = NULL,
  window = NULL,
  similarity = c("none", "jaccard", "cosine", "inclusion", "association", "dice",
    "equivalence", "relative"),
  counting = c("full", "fractional", "attention"),
  lambda = 1,
  scale = NULL,
  threshold = 0,
  min_occur = 1L,
  top_n = NULL,
  output = c("default", "gephi", "igraph", "cograph", "matrix"),
  ...
)
```

## Arguments

- data:

  Input data. Accepts:

  - A `data.frame` with a delimited column (`field` + `sep`).

  - A `data.frame` in long/bipartite format (`field` + `by`).

  - A binary (0/1) `data.frame` or `matrix` (auto-detected).

  - A wide sequence `data.frame` or `matrix` (non-binary).

  - A `list` of character vectors (each element is a transaction).

- field:

  Character. The entity column — determines what the nodes are. For
  delimited format, a single column split by `sep`. For long/bipartite,
  the item column. For multi-column delimited, a vector of column names
  pooled per row. Use `field = "all"` for wide sequence data (e.g.
  TraMineR / tna format) where every column is a time point and cell
  values are the items.

- by:

  Character or `NULL`. Grouping column for long/bipartite format. Each
  unique value defines one transaction.

- sep:

  Character or `NULL`. Separator for splitting delimited fields.

- weight_by:

  Character or `NULL`. Column name containing a numeric association
  strength for each entity-transaction pair. Only accepted for long
  format (`field` + `by`). When supplied, each entity contributes its
  weight rather than 1, so \\C\_{ij} = \sum_d w\_{id} \cdot w\_{jd}\\.
  Typical use: topic-document probability matrices from LDA or similar
  models.

- split_by:

  Character or `NULL`. Column name to split the data by before computing
  co-occurrence. A separate network is computed per group and the
  results are combined into a single data frame with an additional
  `group` column. Only works with data.frame inputs.

- aggregate_by:

  Character or `NULL`. Column name to group the data by before computing
  co-occurrence. For each unique value, the per-group network is
  computed (with the chosen `similarity`, `counting`, `scale`,
  `window`); the per-group edge weights are then combined across groups
  via `aggregate` into ONE final network. Differs from `split_by`, which
  keeps groups separate. Cannot be combined with `split_by`. Only
  applies to data frame inputs.

- aggregate:

  Character. How to combine edge weights across groups when
  `aggregate_by` is used: `"sum"` (default), `"mean"`, `"min"`, or
  `"max"`. The `count` column is always summed. `threshold` and `top_n`
  are applied AFTER aggregation.

- keep_transactions:

  Logical. When `TRUE`, retain the integer-encoded post-`min_occur`
  transaction data as attributes for confirmatory procedures such as
  [`co_bootstrap`](https://saqr.me/cooccure/reference/co_bootstrap.md).

- block:

  Character column name or vector, or `NULL`. Optional block identifier
  aligned to the input rows. Retained as `block_id` on the returned
  object when `keep_transactions = TRUE`.

- window:

  Integer or `NULL`. Sliding-window size for categorical time-series /
  ordered-sequence input. When set to an integer \\w \ge 2\\, every
  window of `w` consecutive positions in a sequence becomes a
  mini-transaction; states inside the same window co-occur. Sequences
  shorter than `w` contribute no transactions. Only applies to ordered
  formats: wide (`field = "all"`) and `list`. Default `NULL` (whole
  sequence treated as one transaction — bag of states).

- similarity:

  Character. Similarity measure:

  `"none"`

  :   Raw co-occurrence counts.

  `"jaccard"`

  :   \\C\_{ij} / (f_i + f_j - C\_{ij})\\.

  `"cosine"`

  :   Salton's cosine: \\C\_{ij} / \sqrt{f_i \cdot f_j}\\.

  `"inclusion"`

  :   Simpson coefficient: \\C\_{ij} / \min(f_i, f_j)\\.

  `"association"`

  :   Association strength: \\C\_{ij} / (f_i \cdot f_j)\\ (van Eck &
      Waltman, 2009).

  `"dice"`

  :   \\2 C\_{ij} / (f_i + f_j)\\.

  `"equivalence"`

  :   Salton's cosine squared: \\C\_{ij}^2 / (f_i \cdot f_j)\\.

  `"relative"`

  :   Row-normalized: each row sums to 1.

- counting:

  Character. Counting method:

  `"full"`

  :   Each co-occurring pair adds 1 regardless of transaction size.
      Default.

  `"fractional"`

  :   Each pair adds \\1 / (n_i - 1)\\ where \\n_i\\ is the number of
      items in transaction \\i\\. Transactions with many items
      contribute less per pair (Perianes-Rodriguez et al., 2016).

  `"attention"`

  :   Each pair within a transaction contributes \\\exp(-\|pos_i -
      pos_j\| / \lambda)\\ — closer positions give a stronger edge,
      distant pairs decay. Requires ordered transactions (list / wide /
      delimited / windowed input). The decay rate \\\lambda\\ is
      controlled by the `lambda` argument.

- lambda:

  Numeric. Decay rate for `counting = "attention"`. Higher `lambda` →
  slower decay → distant pairs still contribute. Default `1.0`, matching
  the tna package. Ignored for other counting methods.

- scale:

  Character or `NULL`. Optional scaling applied to weights after
  similarity normalization:

  `NULL` or `"none"`

  :   No scaling.

  `"minmax"`

  :   Min-max to \\\[0, 1\]\\.

  `"log"`

  :   Natural log: \\\log(1 + w)\\.

  `"log10"`

  :   Log base 10: \\\log\_{10}(1 + w)\\.

  `"binary"`

  :   Binary: 1 if \\w \> 0\\, else 0.

  `"zscore"`

  :   Z-score standardization.

  `"sqrt"`

  :   Square root.

  `"proportion"`

  :   Divide by sum of all weights.

- threshold:

  Numeric. Minimum edge weight to retain. Applied after similarity and
  scaling. Default 0.

- min_occur:

  Integer. Minimum entity frequency. Entities appearing in fewer than
  `min_occur` transactions are dropped. Default 1.

- top_n:

  Integer or `NULL`. Keep only the top `top_n` edges by weight. When
  `split_by` is used, applied per group. Default `NULL` (all edges).

- output:

  Character. Column naming convention for the output:

  `"default"`

  :   `from`, `to`, `weight`, `count`.

  `"gephi"`

  :   `Source`, `Target`, `Weight`, `Type` (= `"Undirected"`). Ready for
      Gephi import.

  `"igraph"`

  :   Returns an `igraph` graph object directly.

  `"cograph"`

  :   Returns a `cograph_network` object directly.

  `"matrix"`

  :   Returns the square co-occurrence matrix.

- ...:

  Currently unused.

## Value

Depends on `output`:

- `"default"`: A `cooccurrence` data frame with columns `from`, `to`,
  `weight`, `count` (and `group` when `split_by` is used).

- `"gephi"`: A data frame with columns `Source`, `Target`, `Weight`,
  `Type`, `Count`. Ready for Gephi CSV import.

- `"igraph"`: An `igraph` graph object.

- `"cograph"`: A `cograph_network` object.

- `"matrix"`: A square numeric co-occurrence matrix.

For the data frame outputs, rows are sorted by weight descending and
attributes store the full matrix, item frequencies, and parameters.

## References

van Eck, N. J., & Waltman, L. (2009). How to normalize co-occurrence
data? An analysis of some well-known similarity measures. *Journal of
the American Society for Information Science and Technology*, 60(8),
1635–1651.

## Examples

``` r
# Delimited keywords
df <- data.frame(
  id = 1:4,
  keywords = c("network; graph", "graph; matrix; network",
               "matrix; algebra", "network; algebra; graph")
)
cooccurrence(df, field = "keywords", sep = ";")
#> # cooccurrence: 4 nodes, 6 edges (4 transactions)
#>     from      to weight count
#>    graph network      3     3
#>  algebra   graph      1     1
#>  algebra  matrix      1     1
#>    graph  matrix      1     1
#>  algebra network      1     1
#>   matrix network      1     1

# Split by a grouping variable
df$year <- c(2020, 2020, 2021, 2021)
cooccurrence(df, field = "keywords", sep = ";", split_by = "year")
#> # cooccurrence: 4 nodes, 7 edges (2 transactions) | split_by: year (2 groups)
#>     from      to weight count group
#>    graph network      2     2  2020
#>    graph  matrix      1     1  2020
#>   matrix network      1     1  2020
#>  algebra   graph      1     1  2021
#>  algebra  matrix      1     1  2021
#>  algebra network      1     1  2021
#>    graph network      1     1  2021

# List of transactions with Jaccard similarity
cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")),
             similarity = "jaccard")
#> # cooccurrence: 3 nodes, 3 edges (3 transactions) | similarity: jaccard
#>  from to    weight count
#>     A  C 0.6666667     2
#>     B  C 0.6666667     2
#>     A  B 0.3333333     1

# Short alias
co(df, field = "keywords", sep = ";", similarity = "cosine")
#> # cooccurrence: 4 nodes, 6 edges (4 transactions) | similarity: cosine
#>     from      to    weight count
#>    graph network 1.0000000     3
#>  algebra  matrix 0.5000000     1
#>  algebra   graph 0.4082483     1
#>    graph  matrix 0.4082483     1
#>  algebra network 0.4082483     1
#>   matrix network 0.4082483     1

# Windowed co-occurrence on a categorical time series. With
# window = 2 only adjacent states co-occur; window = 3 also pairs
# states two positions apart, etc.
seqs <- list(
  c("focus", "focus", "distract", "focus", "confused"),
  c("focus", "distract", "distract", "focus")
)
cooccurrence(seqs, window = 2)
#> # cooccurrence: 3 nodes, 2 edges (7 transactions)
#>      from    to weight count
#>  distract focus      4     4
#>  confused focus      1     1

# Weighted long format (e.g. LDA topic-document probabilities)
theta <- data.frame(
  doc   = c("d1","d1","d1","d2","d2","d3","d3"),
  topic = c("T1","T2","T3","T1","T3","T2","T3"),
  prob  = c(0.6, 0.3, 0.1, 0.4, 0.6, 0.5, 0.5)
)
cooccurrence(theta, field = "topic", by = "doc", weight_by = "prob")
#> # cooccurrence: 3 nodes, 3 edges (3 transactions)
#>  from to weight count
#>    T1 T3   0.30     2
#>    T2 T3   0.28     2
#>    T1 T2   0.18     1
```
