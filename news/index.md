# Changelog

## cooccure 0.4.0

- [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  gains a `window =` parameter for sliding-window co-occurrence on
  ordered sequence input (lists of vectors and wide TraMineR-style data
  frames via `field = "all"`). Each window of `w` consecutive positions
  becomes one transaction; states inside the same window co-occur.
  `window = sequence_length` reduces to the existing bag-of-states
  behavior. TraMineR void markers (`NA`, `""`, `"%"`, `"*"`, `"NaN"`)
  are dropped before windowing. Pure base R via
  [`embed()`](https://rdrr.io/r/stats/embed.html).
- [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  gains `aggregate_by =` and `aggregate =` parameters. `aggregate_by`
  groups the data by a column (e.g. journal id), computes the per-group
  co-occurrence network with whatever `similarity` / `counting` /
  `scale` / `window` was chosen, then combines the per-group edge
  weights into ONE final network.
  `aggregate = c("sum", "mean", "min", "max")` controls the combiner;
  `count` is always summed. Differs from `split_by`, which keeps groups
  separate. Cannot be combined with `split_by`. `threshold` and `top_n`
  apply after aggregation.
- `counting = "attention"` adds positional gap-decay weighting: each
  pair within an ordered transaction contributes
  `exp(-|pos_i - pos_j| / lambda)` to the edge. Closer positions give a
  stronger edge, distant pairs decay. Matches the
  `tna::build_model(type = "attention")` semantics, undirected. The new
  `lambda =` parameter (default `1.0`) controls the decay rate.
- `group =` is now accepted as an alias for `split_by =`.
- [`summary.cooccurrence()`](https://saqr.me/cooccure/reference/summary.cooccurrence.md)
  now returns a structured summary object with network size, density,
  mean degree, isolates, node-level degree and strength, and optional
  group-level summaries.
- [`print.cooccurrence()`](https://saqr.me/cooccure/reference/print.cooccurrence.md)
  now shows a compact diagnostic header before the edge preview,
  including density, mean degree, isolates, method metadata, and top
  nodes.
- `plot.cooccurrence(type = "degree")` adds a base R degree-distribution
  plot.
- [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  accepts a raw event log directly via `action =` (the event column),
  with optional `actor =`, `time =`, `session =`, `order =`, and
  `time_threshold =` (default 900 seconds). The log is sessionized into
  ordered sequences and each session becomes one transaction, so
  `window =` and `counting = "attention"` apply. Sessionization —
  timestamp parsing and gap splitting — is delegated to
  [`Nestimate::prepare()`](https://saqr.me/Nestimate/reference/prepare.html)
  rather than reimplemented, so `Nestimate` is required for this input
  only.
- [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  gains `vars =`, naming the indicator (one-hot) columns directly. The
  specification is resolved like `select` in base
  [`subset()`](https://rdrr.io/r/base/subset.html), so a bare range
  (`vars = A:D`), bare names (`vars = c(A, B, C)`), positions
  (`vars = 2:5`), negative selection (`vars = -c(id, note)`), a logical
  mask, and a plain character vector all work. Other columns — ids,
  timestamps, metadata — are ignored, so a one-hot table no longer has
  to be stripped down to a bare matrix first. Cell values are read as
  presence, so count tables such as document-term matrices work
  alongside `0`/`1` and `TRUE`/`FALSE`.
- Logical (`TRUE`/`FALSE`) indicator tables are now auto-detected;
  previously only numeric `0`/`1` was recognized.
- `NA` in an indicator table is treated as absent. It previously reached
  [`Matrix::sparseMatrix()`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  as an `NA` index and failed with an internal error.
- [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  accepts a `nestimate_data` object from
  [`Nestimate::prepare()`](https://saqr.me/Nestimate/reference/prepare.html)
  and uses its `sequence_data`, so event logs can be sessionized by
  `prepare()` (time gaps, timestamp parsing) and networked here without
  duplicating that logic. Passing one used to fall into the
  list-of-transactions branch and silently produce items built from the
  object’s internal components.
- Bug fix: unknown arguments were silently swallowed by `...`, so a typo
  such as `simliarity = "jaccard"` returned raw counts with no warning.
  Unknown or unnamed arguments now raise an error.
- [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  `summary.cooccurrence` object returns tidy node-level metrics
  (`what = "nodes"`, the default) or group-level metrics
  (`what = "groups"`), so callers no longer reach into the summary
  object’s internals.
- `similarity = "relative"` is asymmetric, so each direction is now
  thresholded on its own value. Filtering by the undirected pair could
  retain a direction whose weight was below `threshold`.
- Added a cross-implementation test suite pinning
  [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  to
  [`Nestimate::cooccurrence()`](https://saqr.me/Nestimate/reference/cooccurrence.html)
  across all eight similarity measures, all five shared input formats,
  `min_occur`, `threshold`, `top_n`, their combinations, degenerate
  inputs, and randomized networks. The two agree exactly once
  conventions are aligned: Nestimate stores item frequency on the matrix
  diagonal (`diagonal = TRUE`) where cooccure zeroes it, and Nestimate
  keeps every edge tied at the `top_n` cut where cooccure truncates
  strictly at N.
- Bug fix: `weight_by` combined with `min_occur > 1` errored with
  “‘dims’ must contain all (i,j) pairs”. The binary companion matrix was
  built from unfiltered column indices against filtered dimensions.
- Bug fix: `min_occur` support for weighted input counted rows rather
  than distinct documents, so duplicate rows for one document could keep
  an item that appears in only that document.
- Bug fix: `threshold` and `top_n` filtered the edge list but not the
  stored matrix, so
  [`as_matrix()`](https://saqr.me/cooccure/reference/as_matrix.md), the
  heatmap, and the cograph and netobject converters disagreed with the
  printed edges (`top_n = 1` returned one edge but a three-edge matrix).
  The matrix is now rebuilt from the surviving edges.
- Bug fix: with `counting = "attention"`, a window covering the whole
  sequence did not reproduce the unwindowed result.
  [`embed()`](https://rdrr.io/r/stats/embed.html) emits each window
  most-recent-first, and attention reads positional gaps off the
  transaction, so the decay was mirrored. Windows are restored to
  reading order before deduplication; set-based counting is unaffected.
- [`as_igraph()`](https://saqr.me/cooccure/reference/as_igraph.md) and
  [`as_matrix()`](https://saqr.me/cooccure/reference/as_matrix.md) now
  accept `output = "gephi"` objects, whose columns are named
  `Source`/`Target`/`Weight`/`Count`. They previously failed with
  “undefined columns selected”.
- A group that fails during `split_by` now warns and names the group.
  Genuinely edgeless groups are still dropped quietly.
- `threshold = 0` is documented as meaning no filtering rather than
  “drop negative weights”, so centering scalings such as
  `scale = "zscore"` keep their negative half.
- Bug fix: the argument order now reproduces the CRAN 0.1.1 signature
  for its first thirteen arguments, so positional calls written against
  0.1.1 keep their meaning. Arguments added in 0.1.2 (`vars`, the
  event-log arguments, `group`, `aggregate_by`, `aggregate`, `window`,
  `lambda`) follow them.
- Bug fix: a column repeated in `vars` was counted twice by the sparse
  builder, inflating weight and count for every pair involving it.
  Selected columns are now deduplicated.
- Bug fix: a `split_by` result inherited the FIRST group’s `matrix`,
  `items`, `frequencies`, and `n_transactions` attributes from
  [`rbind()`](https://rdrr.io/r/base/cbind.html), so
  [`as_matrix()`](https://saqr.me/cooccure/reference/as_matrix.md) and
  the other converters silently described only that group. Those
  attributes are now dropped from split results, and
  [`as_matrix()`](https://saqr.me/cooccure/reference/as_matrix.md)
  rebuilds from the full edge list. Per-group support is recorded in the
  new `group_items` and `group_transactions` attributes.
- Bug fix: the `groups` attribute of a `split_by` result listed every
  input level, including groups that produced no edges and were dropped
  from the data. It now lists only the groups present.
- Bug fix: per-group density in
  [`summary()`](https://rdrr.io/r/base/summary.html) counted only nodes
  that appear in an edge, so a group containing an isolated node
  reported a density above its true value (a group with edge `A-B` plus
  isolated `C` reported density 1 instead of 1/3).
- Bug fix: [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`print()`](https://rdrr.io/r/base/print.html) reported pooled
  simple-graph metrics for `split_by` results, where a node pair can
  occur once per group. Density could exceed 1 and node degree could
  exceed `n_nodes - 1`. `density`, `mean_degree`, and `possible_edges`
  are now `NA` for split results, and the per-group table reports the
  correct values.

## cooccure 0.1.1

CRAN release: 2026-04-24

- First CRAN release. Package renamed from `cooccur` (GitHub-only
  development name) to `cooccure` to avoid collision with an unrelated
  archived CRAN package of the same name.
- Internal engine rewritten on top of
  [`Matrix::sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  /
  [`Matrix::crossprod`](https://rdrr.io/pkg/Matrix/man/matmult-methods.html).
  The previous implementation allocated dense `n x k` and `k x k`
  matrices, which hit R’s vector memory limit on corpora with many
  documents and items (e.g. citation networks \> ~100k unique
  references). The sparse rewrite stays in triplet form end-to-end and
  scales linearly with the number of non-zero co-occurrences.
- `attr(x, "matrix")` and `attr(x, "raw_matrix")` are now sparse Matrix
  objects.
  [`as_matrix()`](https://saqr.me/cooccure/reference/as_matrix.md)
  densifies them on demand so existing downstream code keeps working.
- Added `Matrix` to `Imports`.
- Delimited parsers (`.co_parse_delimited`, `.co_parse_multi_delimited`)
  vectorized: [`trimws()`](https://rdrr.io/r/base/trimws.html), NA/empty
  filtering, and per-row deduplication now run as single C-level calls
  over the flattened token vector rather than as per-row R calls. Cuts
  overall runtime on a 166k-row x 20-items-per-row citation corpus from
  ~6.2 s to ~3.4 s (~1.8x faster).

## cooccure 0.1.0

- Initial development release (distributed as `cooccur` on GitHub).
- [`cooccurrence()`](https://saqr.me/cooccure/reference/cooccurrence.md)
  (alias [`co()`](https://saqr.me/cooccure/reference/cooccurrence.md))
  builds co-occurrence networks from six input formats: delimited
  fields, multi-column delimited, long/bipartite, binary matrices, wide
  sequences (`field = "all"`), and lists of character vectors.
- Eight similarity measures: `none`, `jaccard`, `cosine`, `inclusion`,
  `association`, `dice`, `equivalence`, `relative`.
- Eight scaling methods: `minmax`, `log`, `log10`, `binary`, `zscore`,
  `sqrt`, `proportion`, `none`.
- `counting = "fractional"` implements the Perianes-Rodriguez et al.
  2016. weighted pair count.
- `weight_by` parameter supports weighted long-format input (e.g. LDA
  topic-document probabilities).
- `split_by` computes a separate network per group and returns a
  combined edge data frame.
- `output` argument returns edges in default, Gephi, `igraph`,
  `cograph`, or matrix form.
- S3 converters:
  [`as_matrix()`](https://saqr.me/cooccure/reference/as_matrix.md),
  [`as_igraph()`](https://saqr.me/cooccure/reference/as_igraph.md),
  [`as_tidygraph()`](https://saqr.me/cooccure/reference/as_tidygraph.md),
  [`as_cograph()`](https://saqr.me/cooccure/reference/as_cograph.md),
  [`as_netobject()`](https://saqr.me/cooccure/reference/as_netobject.md).
- Shiny app accessible via
  [`launch_app()`](https://saqr.me/cooccure/reference/launch_app.md).
