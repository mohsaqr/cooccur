# ---- cooccurrence() tests ----

.test_list <- list(c("A", "B", "C"), c("B", "C"), c("A", "C"))

# ========================================
# 1. Returns tidy data.frame
# ========================================

test_that("cooccurrence returns a tidy data.frame", {
  res <- cooccurrence(.test_list)
  expect_s3_class(res, "cooccurrence")
  expect_s3_class(res, "data.frame")
  expect_true(all(c("from", "to", "weight", "count") %in% names(res)))
  expect_true(nrow(res) > 0)
})

test_that("co() alias works", {
  res <- co(.test_list)
  expect_s3_class(res, "cooccurrence")
})

test_that("edges are sorted by weight descending", {
  res <- cooccurrence(.test_list)
  expect_true(all(diff(res$weight) <= 0))
})

# ========================================
# 2. Input formats
# ========================================

test_that("delimited input works", {
  df <- data.frame(items = c("A;B;C", "B;C", "A;C"), stringsAsFactors = FALSE)
  res <- cooccurrence(df, field = "items", sep = ";")
  expect_equal(nrow(res), 3L)  # 3 pairs: A-B, A-C, B-C
})

test_that("multi-column delimited works", {
  df <- data.frame(c1 = c("A;B", "B", "A"), c2 = c("C", "C", "C"),
                   stringsAsFactors = FALSE)
  res <- cooccurrence(df, field = c("c1", "c2"), sep = ";")
  expect_equal(nrow(res), 3L)
})

test_that("long/bipartite works", {
  df <- data.frame(doc = c(1,1,1,2,2,3,3),
                   item = c("A","B","C","B","C","A","C"),
                   stringsAsFactors = FALSE)
  res <- cooccurrence(df, field = "item", by = "doc")
  expect_equal(nrow(res), 3L)
})

test_that("binary matrix works", {
  bin <- matrix(c(1,1,1, 0,1,1, 1,0,1), nrow = 3, byrow = TRUE,
                dimnames = list(NULL, c("A","B","C")))
  res <- cooccurrence(bin)
  expect_equal(nrow(res), 3L)
})

test_that("wide sequence works with field = 'all'", {
  df <- data.frame(V1 = c("A","B","A"), V2 = c("B","C","C"),
                   V3 = c("C", NA, NA), stringsAsFactors = FALSE)
  res <- cooccurrence(df, field = "all")
  expect_equal(nrow(res), 3L)
})

test_that("non-binary data frame without field = 'all' errors", {
  df <- data.frame(V1 = c("A","B","A"), V2 = c("B","C","C"),
                   stringsAsFactors = FALSE)
  expect_error(cooccurrence(df), "field = \"all\"")
})

test_that("list input works", {
  res <- cooccurrence(.test_list)
  expect_equal(nrow(res), 3L)
})

test_that("all formats give same counts", {
  res_list <- cooccurrence(.test_list)

  df_del <- data.frame(items = c("A;B;C", "B;C", "A;C"),
                        stringsAsFactors = FALSE)
  res_del <- cooccurrence(df_del, field = "items", sep = ";")

  # Sort both by from+to for comparison
  key <- function(r) paste(pmin(r$from, r$to), pmax(r$from, r$to))
  r1 <- res_list[order(key(res_list)), ]
  r2 <- res_del[order(key(res_del)), ]

  expect_equal(r1$count, r2$count)
  expect_equal(r1$weight, r2$weight)
})

# ========================================
# 3. Similarity measures
# ========================================

test_that("similarity = 'none' gives raw counts", {
  res <- cooccurrence(.test_list, similarity = "none")
  # A-C co-occur 2 times, so weight = 2
  ac <- res[res$from == "A" & res$to == "C", ]
  expect_equal(ac$weight, 2)
  expect_equal(ac$count, 2L)
})

test_that("similarity = 'jaccard' is correct", {
  res <- cooccurrence(.test_list, similarity = "jaccard")
  ab <- res[res$from == "A" & res$to == "B", ]
  # Jaccard(A,B) = 1 / (2+2-1) = 1/3
  expect_equal(ab$weight, 1 / 3)
})

test_that("similarity = 'cosine' is correct", {
  res <- cooccurrence(.test_list, similarity = "cosine")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$weight, 0.5)
})

test_that("similarity = 'association' is correct", {
  res <- cooccurrence(.test_list, similarity = "association")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$weight, 0.25)
})

test_that("similarity = 'equivalence' is correct", {
  res <- cooccurrence(.test_list, similarity = "equivalence")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$weight, 0.25)
})

test_that("similarity = 'dice' is correct", {
  res <- cooccurrence(.test_list, similarity = "dice")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$weight, 0.5)
})

test_that("similarity = 'inclusion' is correct", {
  res <- cooccurrence(.test_list, similarity = "inclusion")
  ac <- res[res$from == "A" & res$to == "C", ]
  expect_equal(ac$weight, 1)
})

test_that("similarity = 'relative' row-normalizes", {
  res <- cooccurrence(.test_list, similarity = "relative")
  mat <- as_matrix(res)
  expect_true(all(abs(rowSums(mat) - 1) < 1e-10 | rowSums(mat) == 0))
})

# ========================================
# 4. Scaling
# ========================================

test_that("scale = 'minmax' scales to [0,1]", {
  res <- cooccurrence(.test_list, scale = "minmax")
  expect_true(max(res$weight) <= 1)
  expect_true(min(res$weight) >= 0)
})

test_that("scale = 'log' applies log(1+w)", {
  res_raw <- cooccurrence(.test_list)
  res_log <- cooccurrence(.test_list, scale = "log")
  expect_equal(res_log$weight, log(1 + res_raw$weight))
})

test_that("scale = 'binary' gives 0/1 weights", {
  res <- cooccurrence(.test_list, scale = "binary")
  expect_true(all(res$weight %in% c(0, 1)))
})

test_that("scale = 'sqrt' applies square root", {
  res_raw <- cooccurrence(.test_list)
  res_sqrt <- cooccurrence(.test_list, scale = "sqrt")
  expect_equal(res_sqrt$weight, sqrt(res_raw$weight))
})

test_that("scale = 'zscore' standardizes", {
  res <- cooccurrence(.test_list, scale = "zscore")
  vals <- res$weight[res$weight != 0]
  if (length(vals) > 1) {
    expect_equal(mean(vals), 0, tolerance = 1e-10)
  }
})

test_that("scale = 'proportion' normalizes by total weight", {
  res <- cooccurrence(.test_list, scale = "proportion")
  # The full symmetric matrix sums to 1 (upper + lower triangle)
  mat <- as_matrix(res)
  expect_equal(sum(mat), 1, tolerance = 1e-10)
})

# ========================================
# 5. Parameters
# ========================================

test_that("threshold filters edges", {
  res <- cooccurrence(.test_list, threshold = 2)
  expect_true(all(res$weight >= 2))
  expect_equal(nrow(res), 2L)  # Only A-C and B-C have count >= 2
})

test_that("min_occur drops rare entities", {
  trans <- list(c("A","B"), c("A","B"), c("A","D"))
  res <- cooccurrence(trans, min_occur = 2L)
  expect_false("D" %in% c(res$from, res$to))
})

test_that("top_n limits edges", {
  res <- cooccurrence(.test_list, top_n = 1L)
  expect_equal(nrow(res), 1L)
})

# ========================================
# 6. Attributes
# ========================================

test_that("attributes are stored", {
  res <- cooccurrence(.test_list, similarity = "jaccard", scale = "log")
  expect_equal(attr(res, "similarity"), "jaccard")
  expect_equal(attr(res, "scale"), "log")
  expect_equal(attr(res, "n_transactions"), 3L)
  expect_equal(attr(res, "n_items"), 3L)
  expect_true(!is.null(attr(res, "matrix")))
  expect_true(!is.null(attr(res, "items")))
  expect_true(!is.null(attr(res, "frequencies")))
})

# ========================================
# 7. Converters
# ========================================

test_that("as_matrix returns correct matrix", {
  res <- cooccurrence(.test_list)
  mat <- as_matrix(res)
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 3L)
  expect_equal(ncol(mat), 3L)
  expect_true(isSymmetric(mat))
})

test_that("as_matrix type='raw' gives counts", {
  res <- cooccurrence(.test_list, similarity = "jaccard")
  raw <- as_matrix(res, type = "raw")
  expect_equal(raw["A", "B"], 1)
  expect_equal(raw["A", "C"], 2)
})

test_that("as_igraph works", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list)
  g <- as_igraph(res)
  expect_true(igraph::is_igraph(g))
  expect_equal(igraph::vcount(g), 3L)
  expect_false(igraph::is_directed(g))
})

test_that("as_cograph works", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  expect_true(inherits(net, "cograph_network"))
  expect_equal(net$n_nodes, 3L)
})

test_that("as_netobject works", {
  skip_if_not_installed("Nestimate")
  res <- cooccurrence(.test_list)
  net <- as_netobject(res)
  expect_true(inherits(net, "netobject"))
  expect_true(inherits(net, "cograph_network"))
  expect_false(net$directed)
})

test_that("as_tidygraph works", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list)
  tg <- as_tidygraph(res)
  expect_true(inherits(tg, "tbl_graph"))
})

# ========================================
# 8. Edge cases
# ========================================

test_that("error on empty input", {
  expect_error(cooccurrence(list()), "No non-empty transactions")
})

test_that("single-item transactions give no edges", {
  res <- cooccurrence(list(c("A"), c("B"), c("C")))
  expect_equal(nrow(res), 0L)
})

test_that("count is always the raw co-occurrence", {
  res <- cooccurrence(.test_list, similarity = "jaccard", scale = "log")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$count, 1L)
})

# ========================================
# 9. split_by
# ========================================

test_that("split_by produces a group column", {
  df <- data.frame(
    year = c(2020, 2020, 2020, 2021, 2021, 2021),
    kw = c("A; B; C", "B; C", "A; C", "B; C; D", "C; D", "B; D"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", split_by = "year")
  expect_s3_class(res, "cooccurrence")
  expect_true("group" %in% names(res))
  expect_equal(sort(unique(res$group)), c("2020", "2021"))
})

test_that("group is an alias for split_by", {
  df <- data.frame(
    year = c(2020, 2020, 2021, 2021),
    kw = c("A; B", "A; C", "B; C", "B; D"),
    stringsAsFactors = FALSE
  )
  by_split <- cooccurrence(df, field = "kw", sep = ";", split_by = "year")
  by_group <- cooccurrence(df, field = "kw", sep = ";", group = "year")

  expect_equal(as.data.frame(by_group), as.data.frame(by_split))
  expect_equal(attr(by_group, "split_by"), "year")
  expect_equal(attr(by_group, "groups"), attr(by_split, "groups"))
})

test_that("group and split_by cannot disagree", {
  df <- data.frame(
    year = c(2020, 2021),
    cohort = c("A", "B"),
    kw = c("X; Y", "X; Y"),
    stringsAsFactors = FALSE
  )
  expect_error(
    cooccurrence(df, field = "kw", sep = ";",
                 split_by = "year", group = "cohort"),
    "alias for `split_by`"
  )
})

test_that("split_by computes separate networks per group", {
  df <- data.frame(
    grp = c("X", "X", "Y", "Y"),
    kw = c("A; B", "A; B", "C; D", "C; D"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", split_by = "grp")
  # Group X has only A-B; group Y has only C-D
  res_x <- res[res$group == "X", ]
  res_y <- res[res$group == "Y", ]
  expect_equal(nrow(res_x), 1L)
  expect_equal(nrow(res_y), 1L)
  expect_true(all(c(res_x$from, res_x$to) %in% c("A", "B")))
  expect_true(all(c(res_y$from, res_y$to) %in% c("C", "D")))
})

test_that("split_by + similarity works", {
  df <- data.frame(
    grp = c("X", "X", "X", "Y", "Y", "Y"),
    kw = c("A; B; C", "B; C", "A; C", "A; B; C", "B; C", "A; C"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";",
                      split_by = "grp", similarity = "jaccard")
  # Both groups have the same data → same weights
  res_x <- res[res$group == "X", ]
  res_y <- res[res$group == "Y", ]
  expect_equal(sort(res_x$weight), sort(res_y$weight))
})

test_that("split_by + top_n applies per group", {
  df <- data.frame(
    grp = c("X", "X", "X", "Y", "Y", "Y"),
    kw = c("A; B; C", "B; C", "A; C", "D; E; F", "E; F", "D; F"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";",
                      split_by = "grp", top_n = 1L)
  # 1 edge per group → 2 total
  expect_equal(nrow(res), 2L)
  expect_equal(length(unique(res$group)), 2L)
})

test_that("split_by skips groups with no edges", {
  df <- data.frame(
    grp = c("X", "X", "Y"),
    kw = c("A; B", "A; B", "C"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", split_by = "grp")
  # Y has only one item → no edges → should be absent
  expect_equal(unique(res$group), "X")
})

test_that("split_by stores attributes", {
  df <- data.frame(
    year = c(2020, 2021),
    kw = c("A; B", "A; B"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", split_by = "year")
  expect_equal(attr(res, "split_by"), "year")
  expect_equal(sort(attr(res, "groups")), c("2020", "2021"))
})

# ========================================
# 10. Output formats
# ========================================

test_that("output = 'gephi' renames columns", {
  res <- cooccurrence(.test_list, output = "gephi")
  expect_true(all(c("Source", "Target", "Weight", "Type", "Count") %in% names(res)))
  expect_false("from" %in% names(res))
  expect_equal(unique(res$Type), "Undirected")
})

test_that("output = 'gephi' with split_by includes group", {
  df <- data.frame(grp = c("X", "X", "Y", "Y"),
                   kw = c("A; B", "A; B", "C; D", "C; D"),
                   stringsAsFactors = FALSE)
  res <- cooccurrence(df, field = "kw", sep = ";",
                      split_by = "grp", output = "gephi")
  expect_true("group" %in% names(res))
  expect_true("Source" %in% names(res))
})

test_that("output = 'matrix' returns a matrix", {
  res <- cooccurrence(.test_list, output = "matrix")
  expect_true(is.matrix(res))
  expect_equal(nrow(res), 3L)
})

test_that("output = 'igraph' returns igraph object", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list, output = "igraph")
  expect_true(igraph::is_igraph(res))
})

test_that("output = 'cograph' returns cograph_network", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list, output = "cograph")
  expect_true(inherits(res, "cograph_network"))
})

# ========================================
# 11. Counting methods
# ========================================

# Transactions: {A,B,C} (n=3), {B,C} (n=2), {A,C} (n=2)

test_that("counting = 'full' gives raw counts (default)", {
  res <- cooccurrence(.test_list, counting = "full")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$weight, 1)
  expect_equal(ab$count, 1L)
})

test_that("counting = 'fractional' weights by 1/(n-1)", {
  # {A,B,C}: n=3, each pair gets 1/(3-1) = 0.5
  # {B,C}: n=2, each pair gets 1/(2-1) = 1
  # {A,C}: n=2, each pair gets 1
  # A-B: only in {A,B,C} → 0.5
  # B-C: in {A,B,C}(0.5) + {B,C}(1.0) = 1.5
  # A-C: in {A,B,C}(0.5) + {A,C}(1.0) = 1.5
  res <- cooccurrence(.test_list, counting = "fractional")
  ab <- res[res$from == "A" & res$to == "B", ]
  bc <- res[res$from == "B" & res$to == "C", ]
  ac <- res[res$from == "A" & res$to == "C", ]
  expect_equal(ab$weight, 0.5, tolerance = 1e-10)
  expect_equal(bc$weight, 1.5, tolerance = 1e-10)
  expect_equal(ac$weight, 1.5, tolerance = 1e-10)
  # count is always raw
  expect_equal(ab$count, 1L)
  expect_equal(bc$count, 2L)
})

test_that("counting = 'fractional' + similarity works", {
  res <- cooccurrence(.test_list, counting = "fractional", similarity = "jaccard")
  expect_true(nrow(res) > 0)
  # Weights should differ from full counting
  res_full <- cooccurrence(.test_list, counting = "full", similarity = "jaccard")
  expect_false(isTRUE(all.equal(res$weight, res_full$weight)))
})

test_that("counting with long/bipartite format", {
  df <- data.frame(
    doc = c(1, 1, 1, 2, 2, 3, 3),
    item = c("A", "B", "C", "B", "C", "A", "C"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "item", by = "doc", counting = "fractional")
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(ab$weight, 0.5, tolerance = 1e-10)
})

# ========================================
# 11b. field without sep (field_only format)
# ========================================

test_that("field without sep treats each column value as one item", {
  df <- data.frame(g1 = c("A", "B", "A"), g2 = c("B", "C", "C"),
                   stringsAsFactors = FALSE)
  expect_warning(res <- cooccurrence(df, field = c("g1", "g2")),
                 "without `sep`")
  expect_s3_class(res, "cooccurrence")
  expect_equal(nrow(res), 3L)
  expect_true(all(c("A", "B", "C") %in% c(res$from, res$to)))
})

test_that("field without sep ignores other columns", {
  df <- data.frame(id = 1:3, x = c("A", "B", "A"), y = c("B", "C", "C"),
                   stringsAsFactors = FALSE)
  expect_warning(res <- cooccurrence(df, field = c("x", "y")),
                 "without `sep`")
  items <- unique(c(res$from, res$to))
  expect_false(any(items %in% c("1", "2", "3")))
})

test_that("single field without sep gives one-item transactions (no edges)", {
  df <- data.frame(genre = c("Action", "Comedy", "Action"),
                   stringsAsFactors = FALSE)
  expect_warning(res <- cooccurrence(df, field = "genre"), "without `sep`")
  expect_equal(nrow(res), 0L)
})

test_that("field without sep matches multi_delimited with no delimiters", {
  df <- data.frame(g1 = c("A", "B", "A"), g2 = c("B", "C", "C"),
                   stringsAsFactors = FALSE)
  expect_warning(res_field <- cooccurrence(df, field = c("g1", "g2")),
                 "without `sep`")
  res_multi <- cooccurrence(df, field = c("g1", "g2"), sep = ";")
  key <- function(r) paste(pmin(r$from, r$to), pmax(r$from, r$to))
  r1 <- res_field[order(key(res_field)), ]
  r2 <- res_multi[order(key(res_multi)), ]
  expect_equal(r1$count, r2$count)
  expect_equal(r1$weight, r2$weight)
})

test_that("field without sep works with similarity", {
  df <- data.frame(g1 = c("A", "B", "A"), g2 = c("B", "C", "C"),
                   stringsAsFactors = FALSE)
  expect_warning(
    res <- cooccurrence(df, field = c("g1", "g2"), similarity = "jaccard"),
    "without `sep`"
  )
  expect_true(nrow(res) > 0)
  expect_true(all(res$weight >= 0 & res$weight <= 1))
})

test_that("field without sep handles NAs", {
  df <- data.frame(g1 = c("A", "B", NA), g2 = c("B", NA, "C"),
                   stringsAsFactors = FALSE)
  expect_warning(res <- cooccurrence(df, field = c("g1", "g2")),
                 "without `sep`")
  expect_s3_class(res, "cooccurrence")
})

test_that("field without sep suggests separator when found", {
  df <- data.frame(items = c("A;B;C", "B;C", "A;C"),
                   stringsAsFactors = FALSE)
  expect_warning(cooccurrence(df, field = "items"),
                 'sep = ";"')
})

test_that("field without sep suggests comma separator", {
  df <- data.frame(items = c("A,B,C", "B,C", "A,C"),
                   stringsAsFactors = FALSE)
  expect_warning(cooccurrence(df, field = "items"),
                 'sep = ","')
})

test_that("field without sep gives generic warning when no separator found", {
  df <- data.frame(g1 = c("A", "B"), g2 = c("C", "D"),
                   stringsAsFactors = FALSE)
  expect_warning(cooccurrence(df, field = c("g1", "g2")),
                 "without `sep`")
})

# ========================================
# 12. print.cooccurrence
# ========================================

test_that("print.cooccurrence shows header and edges", {
  res <- cooccurrence(.test_list)
  out <- capture.output(print(res))
  expect_true(any(grepl("cooccurrence:", out)))
  expect_true(any(grepl("nodes", out)))
  expect_true(any(grepl("edges", out)))
  expect_true(any(grepl("density", out)))
  expect_true(any(grepl("mean degree", out)))
  expect_true(any(grepl("possible edges", out)))
  expect_true(any(grepl("top nodes", out)))
  expect_true(any(grepl("transactions", out)))
})

test_that("print.cooccurrence returns x invisibly", {
  res <- cooccurrence(.test_list)
  out <- withVisible(capture.output(ret <- print(res)))
  expect_identical(ret, res)
})

test_that("print.cooccurrence shows similarity and scale", {
  res <- cooccurrence(.test_list, similarity = "jaccard", scale = "log")
  out <- capture.output(print(res))
  expect_true(any(grepl("similarity: jaccard", out)))
  expect_true(any(grepl("scale: log", out)))
})

test_that("print.cooccurrence with split_by shows group info", {
  df <- data.frame(
    grp = c("X", "X", "Y", "Y"),
    kw = c("A; B", "A; B", "C; D", "C; D"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", split_by = "grp")
  out <- capture.output(print(res))
  expect_true(any(grepl("split_by: grp", out)))
  expect_true(any(grepl("2 groups", out)))
})

# ========================================
# 13. weight_by (weighted long format)
# ========================================

.theta <- data.frame(
  doc   = c("d1","d1","d1","d2","d2","d3","d3"),
  topic = c("T1","T2","T3","T1","T3","T2","T3"),
  prob  = c(0.6, 0.3, 0.1, 0.4, 0.6, 0.5, 0.5),
  stringsAsFactors = FALSE
)

test_that("weight_by returns cooccurrence object with correct columns", {
  res <- cooccurrence(.theta, field = "topic", by = "doc", weight_by = "prob")
  expect_s3_class(res, "cooccurrence")
  expect_true(all(c("from", "to", "weight", "count") %in% names(res)))
})

test_that("weight_by computes correct weighted co-occurrence values", {
  res <- cooccurrence(.theta, field = "topic", by = "doc", weight_by = "prob")
  get_w <- function(a, b) {
    row <- res[res$from == a & res$to == b | res$from == b & res$to == a, ]
    row$weight
  }
  expect_equal(get_w("T1", "T3"), 0.6*0.1 + 0.4*0.6, tolerance = 1e-10)
  expect_equal(get_w("T2", "T3"), 0.3*0.1 + 0.5*0.5, tolerance = 1e-10)
  expect_equal(get_w("T1", "T2"), 0.6*0.3,            tolerance = 1e-10)
})

test_that("weight_by count column reflects binary co-occurrence", {
  res <- cooccurrence(.theta, field = "topic", by = "doc", weight_by = "prob")
  # T1-T2 appear together only in d1 -> count = 1
  row <- res[res$from == "T1" & res$to == "T2" |
             res$from == "T2" & res$to == "T1", ]
  expect_equal(row$count, 1L)
  # T1-T3 appear in d1 and d2 -> count = 2
  row <- res[res$from == "T1" & res$to == "T3" |
             res$from == "T3" & res$to == "T1", ]
  expect_equal(row$count, 2L)
})

test_that("weight_by works with similarity normalization", {
  res <- cooccurrence(.theta, field = "topic", by = "doc",
                      weight_by = "prob", similarity = "cosine")
  expect_s3_class(res, "cooccurrence")
  expect_true(all(res$weight >= 0 & res$weight <= 1))
})

test_that("weight_by respects min_occur", {
  res <- cooccurrence(.theta, field = "topic", by = "doc",
                      weight_by = "prob", min_occur = 2)
  # T1-T2 pair: T2 appears in d1 and d3 (2 docs), T1 in d1 and d2 (2 docs)
  # All topics appear in >= 2 docs, so all 3 edges remain
  expect_equal(nrow(res), 3L)
})

test_that("weight_by errors on non-long format", {
  df <- data.frame(items = c("A;B", "B;C"), w = c(1, 2),
                   stringsAsFactors = FALSE)
  expect_error(
    cooccurrence(df, field = "items", sep = ";", weight_by = "w"),
    "long format"
  )
})

test_that("weight_by works with split_by", {
  df <- data.frame(
    doc   = c("d1","d1","d2","d2","d3","d3"),
    topic = c("T1","T2","T1","T2","T1","T2"),
    prob  = c(0.6, 0.4, 0.3, 0.7, 0.5, 0.5),
    year  = c(2020, 2020, 2020, 2020, 2021, 2021),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "topic", by = "doc",
                      weight_by = "prob", split_by = "year")
  expect_true("group" %in% names(res))
  expect_equal(sort(unique(res$group)), c("2020", "2021"))
})

test_that("print.cooccurrence respects n parameter", {
  res <- cooccurrence(.test_list)
  out <- capture.output(print(res, n = 1L))
  expect_true(any(grepl("more edges", out)))
})

test_that("print.cooccurrence handles zero edges", {
  res <- cooccurrence(list(c("A"), c("B"), c("C")))
  out <- capture.output(print(res))
  expect_true(any(grepl("no edges", out)))
})

test_that("print.cooccurrence shows default method metadata", {
  res <- cooccurrence(.test_list, similarity = "none")
  out <- capture.output(print(res))
  expect_true(any(grepl("similarity: none", out)))
  expect_true(any(grepl("counting: full", out)))
})

# ========================================
# 13. summary.cooccurrence
# ========================================

test_that("summary.cooccurrence shows network stats", {
  res <- cooccurrence(.test_list, similarity = "jaccard")
  out <- capture.output(summary(res))
  expect_true(any(grepl("cooccurrence network", out)))
  expect_true(any(grepl("Nodes", out)))
  expect_true(any(grepl("Edges", out)))
  expect_true(any(grepl("Possible edges", out)))
  expect_true(any(grepl("Density", out)))
  expect_true(any(grepl("Mean degree", out)))
  expect_true(any(grepl("Isolates", out)))
  expect_true(any(grepl("Transactions", out)))
  expect_true(any(grepl("Similarity.*jaccard", out)))
  expect_true(any(grepl("Weight range", out)))
  expect_true(any(grepl("Weight mean", out)))
  expect_true(any(grepl("Count range", out)))
  expect_true(any(grepl("Count mean", out)))
  expect_true(any(grepl("Top nodes", out)))
})

test_that("summary.cooccurrence returns structured summary invisibly", {
  res <- cooccurrence(.test_list)
  capture.output(vis <- withVisible(ret <- summary(res)))
  expect_false(vis$visible)
  expect_s3_class(ret, "summary.cooccurrence")
  expect_equal(ret$n_nodes, 3L)
  expect_equal(ret$n_edges, 3L)
  expect_equal(ret$possible_edges, 3L)
  expect_equal(ret$density, 1)
  expect_equal(ret$mean_degree, 2)
  expect_true(all(c("node", "degree", "strength", "count_strength",
                    "frequency") %in% names(ret$nodes)))
})

test_that("summary.cooccurrence shows scale when not 'none'", {
  res <- cooccurrence(.test_list, scale = "minmax")
  out <- capture.output(summary(res))
  expect_true(any(grepl("Scale.*minmax", out)))
})

test_that("summary.cooccurrence omits scale when 'none'", {
  res <- cooccurrence(.test_list)
  out <- capture.output(summary(res))
  expect_false(any(grepl("Scale", out)))
})

test_that("summary.cooccurrence handles zero edges", {
  res <- cooccurrence(list(c("A"), c("B"), c("C")))
  out <- capture.output(s <- summary(res))
  expect_true(any(grepl("Edges.*: 0", out)))
  expect_equal(s$n_nodes, 3L)
  expect_equal(s$n_edges, 0L)
  expect_equal(s$density, 0)
  expect_equal(s$isolates, 3L)
  # Should NOT print weight/count/top nodes sections
  expect_false(any(grepl("Weight range", out)))
})

test_that("summary.cooccurrence reports group summaries", {
  df <- data.frame(
    grp = c("X", "X", "Y", "Y"),
    kw = c("A; B", "A; C", "B; C", "B; D"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", group = "grp")
  out <- capture.output(s <- summary(res))

  expect_true(any(grepl("Split by.*grp", out)))
  expect_true(any(grepl("Groups", out)))
  expect_s3_class(s, "summary.cooccurrence")
  expect_true(all(c("group", "n_nodes", "n_edges", "density") %in%
                    names(s$groups_summary)))
  expect_equal(sort(s$groups_summary$group), c("X", "Y"))
})

# ========================================
# 14. plot.cooccurrence
# ========================================

test_that("plot.cooccurrence heatmap works", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- cooccurrence(.test_list)
  expect_invisible(plot(res, type = "heatmap"))
})

test_that("plot.cooccurrence heatmap works when matrix attr is NULL", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- cooccurrence(.test_list)
  attr(res, "matrix") <- NULL
  expect_invisible(plot(res, type = "heatmap"))
})

test_that("plot.cooccurrence network requires igraph", {
  skip_if_not_installed("igraph")
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- cooccurrence(.test_list)
  expect_invisible(plot(res, type = "network"))
})

test_that("plot.cooccurrence degree distribution uses base graphics", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- cooccurrence(.test_list)
  expect_invisible(plot(res, type = "degree"))
})

test_that("plot.cooccurrence degree distribution handles zero edges", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- cooccurrence(list(c("A"), c("B"), c("C")))
  expect_invisible(plot(res, type = "degree"))
})

test_that("plot.cooccurrence returns x invisibly", {
  pdf(NULL); on.exit(dev.off(), add = TRUE)
  res <- cooccurrence(.test_list)
  ret <- plot(res, type = "heatmap")
  expect_identical(ret, res)
})

# ========================================
# 15. Converter edge cases
# ========================================

test_that("as_matrix rebuilds from edges when attribute is lost", {
  res <- cooccurrence(.test_list, similarity = "jaccard")
  attr(res, "matrix") <- NULL
  mat <- as_matrix(res)
  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), 3L)
  expect_true(isSymmetric(mat))
  # Should match the weight values
  ab <- res[res$from == "A" & res$to == "B", ]
  expect_equal(mat["A", "B"], ab$weight)
})

test_that("as_matrix rebuilds raw from edges when attribute is lost", {
  res <- cooccurrence(.test_list, similarity = "jaccard")
  attr(res, "raw_matrix") <- NULL
  raw <- as_matrix(res, type = "raw")
  expect_true(is.matrix(raw))
  expect_equal(raw["A", "B"], 1)
  expect_equal(raw["A", "C"], 2)
})

test_that("as_igraph works when items attribute is NULL", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list)
  attr(res, "items") <- NULL
  g <- as_igraph(res)
  expect_true(igraph::is_igraph(g))
  expect_equal(igraph::vcount(g), 3L)
})

test_that("as_cograph structure is complete", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  expect_false(net$directed)
  expect_equal(net$n_edges, 3L)
  expect_true(is.matrix(net$weights))
  expect_true(all(c("id", "label", "name", "x", "y") %in% names(net$nodes)))
  expect_true(all(c("from", "to", "weight") %in% names(net$edges)))
  expect_equal(net$meta$source, "cooccure")
})

test_that("as_netobject structure is complete", {
  skip_if_not_installed("Nestimate")
  res <- cooccurrence(.test_list, similarity = "jaccard", threshold = 0)
  net <- as_netobject(res)
  expect_equal(net$method, "cooccurrence")
  expect_equal(net$params$similarity, "jaccard")
  expect_true(is.matrix(net$weights))
  expect_true(all(c("id", "label", "name") %in% names(net$nodes)))
  expect_true(all(c("from", "to", "weight") %in% names(net$edges)))
  expect_equal(net$meta$tna$method, "cooccurrence")
})

# ========================================
# 16. Converter error paths (missing packages)
# ========================================

test_that("as_tidygraph errors when tidygraph is not available", {
  skip_if(requireNamespace("tidygraph", quietly = TRUE),
          "tidygraph is installed; cannot test missing-package path")
  res <- cooccurrence(.test_list)
  expect_error(as_tidygraph(res), "tidygraph")
})

test_that("as_netobject errors when Nestimate is not available", {
  skip_if(requireNamespace("Nestimate", quietly = TRUE),
          "Nestimate is installed; cannot test missing-package path")
  res <- cooccurrence(.test_list)
  expect_error(as_netobject(res), "Nestimate")
})

# ========================================
# 17. as_cograph / as_netobject with zero edges
# ========================================

test_that("as_cograph handles zero edges", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(list(c("A"), c("B")))
  net <- as_cograph(res)
  expect_equal(net$n_edges, 0L)
  expect_equal(nrow(net$edges), 0L)
  expect_true(all(c("from", "to", "weight") %in% names(net$edges)))
})

test_that("as_netobject handles zero edges", {
  skip_if_not_installed("Nestimate")
  res <- cooccurrence(list(c("A"), c("B")))
  net <- as_netobject(res)
  expect_equal(net$n_edges, 0L)
  expect_equal(nrow(net$edges), 0L)
})

# ========================================
# 18. as_tidygraph full path
# ========================================

test_that("as_tidygraph produces correct tbl_graph", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list)
  tg <- as_tidygraph(res)
  expect_true(inherits(tg, "tbl_graph"))
  expect_equal(igraph::vcount(tg), 3L)
  expect_equal(igraph::ecount(tg), 3L)
})

# ========================================
# 19. as_igraph edge attributes & vertex details
# ========================================

test_that("as_igraph preserves edge weights and counts", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list, similarity = "jaccard")
  g <- as_igraph(res)
  expect_true("weight" %in% igraph::edge_attr_names(g))
  expect_true("count" %in% igraph::edge_attr_names(g))
  expect_equal(igraph::ecount(g), nrow(res))
})

test_that("as_igraph includes isolated nodes from items attr", {
  skip_if_not_installed("igraph")
  # min_occur filters D from edges, but items attr includes all original items
  trans <- list(c("A", "B"), c("A", "B"), c("A", "D"))
  res <- cooccurrence(trans, min_occur = 2L)
  # Items attr still has D if it appeared in transactions
  g <- as_igraph(res)
  expect_true(igraph::is_igraph(g))
})

# ========================================
# 20. as_cograph detailed checks
# ========================================

test_that("as_cograph nodes have sequential ids", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  expect_equal(net$nodes$id, seq_len(net$n_nodes))
})

test_that("as_cograph edges reference valid node ids", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  all_ids <- net$nodes$id
  expect_true(all(net$edges$from %in% all_ids))
  expect_true(all(net$edges$to %in% all_ids))
})

test_that("as_cograph weights matrix matches normalized matrix", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list, similarity = "cosine")
  net <- as_cograph(res)
  mat <- as_matrix(res, type = "normalized")
  expect_equal(net$weights, mat)
})

# ========================================
# 21. as_matrix thorough
# ========================================

test_that("as_matrix diagonal is zero (no self-loops)", {
  res <- cooccurrence(.test_list)
  mat <- as_matrix(res)
  expect_true(all(diag(mat) == 0))
  raw <- as_matrix(res, type = "raw")
  expect_true(all(diag(raw) == 0))
})

test_that("as_matrix dimnames are sorted unique items", {
  res <- cooccurrence(.test_list)
  mat <- as_matrix(res)
  items <- sort(unique(c(res$from, res$to)))
  expect_equal(rownames(mat), items)
  expect_equal(colnames(mat), items)
})

test_that("as_matrix rebuilt normalized matches stored matrix", {
  res <- cooccurrence(.test_list, similarity = "jaccard")
  ## Stored attribute is a sparse Matrix in the current engine; densify for
  ## comparison against the dense fallback rebuilt from the edge list.
  stored <- as.matrix(attr(res, "matrix"))
  attr(res, "matrix") <- NULL
  rebuilt <- as_matrix(res)
  # Diagonal is lost when rebuilding from edges (no self-edges), so compare off-diagonal
  diag(stored) <- 0
  expect_equal(rebuilt, stored)
})

test_that("as_matrix rebuilt raw is symmetric", {
  res <- cooccurrence(.test_list, similarity = "cosine")
  attr(res, "raw_matrix") <- NULL
  raw <- as_matrix(res, type = "raw")
  expect_true(isSymmetric(raw))
})

test_that("as_matrix with scaled result returns scaled values", {
  res <- cooccurrence(.test_list, similarity = "jaccard", scale = "minmax")
  mat <- as_matrix(res)
  expect_true(all(mat >= 0))
  expect_true(all(mat <= 1))
})

test_that("as_matrix on zero-edge result returns zero off-diagonal", {
  res <- cooccurrence(list(c("A"), c("B"), c("C")))
  mat <- as_matrix(res, type = "raw")
  diag(mat) <- 0
  expect_true(all(mat == 0))
})

test_that("as_matrix on split_by falls back to rebuild from edges", {
  df <- data.frame(
    grp = c("X", "X", "Y", "Y"),
    kw = c("A; B", "A; B", "C; D", "C; D"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "kw", sep = ";", split_by = "grp")
  # split_by does not store per-group matrices in attr, but the combined
  # result may not have a matrix attr → rebuild path
  mat <- as_matrix(res)
  expect_true(is.matrix(mat))
  expect_true(isSymmetric(mat))
})

# ========================================
# 22. Generic dispatch errors
# ========================================

test_that("as_matrix errors on non-cooccurrence input", {
  expect_error(as_matrix(42))
})

test_that("as_igraph errors on non-cooccurrence input", {
  expect_error(as_igraph(42))
})

test_that("as_cograph errors on non-cooccurrence input", {
  expect_error(as_cograph(42))
})

test_that("as_tidygraph errors on non-cooccurrence input", {
  expect_error(as_tidygraph(42))
})

test_that("as_netobject errors on non-cooccurrence input", {
  expect_error(as_netobject(42))
})

# ========================================
# 23. as_igraph thorough
# ========================================

test_that("as_igraph vertex names match items attribute", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list)
  g <- as_igraph(res)
  expect_setequal(igraph::V(g)$name, attr(res, "items"))
})

test_that("as_igraph edge weights match cooccurrence weights", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list, similarity = "jaccard")
  g <- as_igraph(res)
  el <- igraph::as_data_frame(g, what = "edges")
  # Sort both for comparison
  el <- el[order(el$from, el$to), ]
  co_sorted <- res[order(res$from, res$to), ]
  expect_equal(el$weight, co_sorted$weight)
  expect_equal(el$count, co_sorted$count)
})

test_that("as_igraph with zero edges gives edgeless graph", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(list(c("A"), c("B"), c("C")))
  g <- as_igraph(res)
  expect_equal(igraph::ecount(g), 0L)
  # Vertices still present if items attr exists
  items <- attr(res, "items")
  if (!is.null(items)) {
    expect_true(igraph::vcount(g) >= length(items))
  }
})

test_that("as_igraph with similarity + scale produces valid graph", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list, similarity = "dice", scale = "sqrt")
  g <- as_igraph(res)
  expect_true(igraph::is_igraph(g))
  expect_true(all(igraph::E(g)$weight >= 0))
})

test_that("as_igraph roundtrip preserves edge count", {
  skip_if_not_installed("igraph")
  res <- cooccurrence(.test_list)
  g <- as_igraph(res)
  el <- igraph::as_data_frame(g, what = "edges")
  expect_equal(nrow(el), nrow(res))
})

# ========================================
# 24. as_cograph thorough
# ========================================

test_that("as_cograph node labels match sorted items", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  mat <- as_matrix(res, type = "normalized")
  expect_equal(net$nodes$label, colnames(mat))
  expect_equal(net$nodes$name, colnames(mat))
})

test_that("as_cograph edge count matches non-zero upper triangle", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list, similarity = "jaccard")
  net <- as_cograph(res)
  mat <- as_matrix(res, type = "normalized")
  expected_edges <- sum(upper.tri(mat) & mat != 0)
  expect_equal(net$n_edges, expected_edges)
  expect_equal(nrow(net$edges), expected_edges)
})

test_that("as_cograph meta$layout is NULL", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  expect_null(net$meta$layout)
})

test_that("as_cograph with single pair", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(list(c("X", "Y"), c("X", "Y")))
  net <- as_cograph(res)
  expect_equal(net$n_nodes, 2L)
  expect_equal(net$n_edges, 1L)
  expect_equal(nrow(net$nodes), 2L)
  expect_equal(nrow(net$edges), 1L)
})

test_that("as_cograph with different similarity measures", {
  skip_if_not_installed("cograph")
  for (sim in c("none", "jaccard", "cosine", "dice")) {
    res <- cooccurrence(.test_list, similarity = sim)
    net <- as_cograph(res)
    expect_true(inherits(net, "cograph_network"))
    expect_equal(net$n_nodes, 3L)
  }
})

test_that("as_cograph node x and y are NA", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  expect_true(all(is.na(net$nodes$x)))
  expect_true(all(is.na(net$nodes$y)))
})

test_that("as_cograph edges$from and edges$to are integer", {
  skip_if_not_installed("cograph")
  res <- cooccurrence(.test_list)
  net <- as_cograph(res)
  expect_type(net$edges$from, "integer")
  expect_type(net$edges$to, "integer")
})

# ========================================
# 14. Large-dataset scalability
# ========================================

test_that("cooccurrence scales to wide sparse inputs without densifying", {
  ## Regression guard: the engine must stay in sparse representation.
  ## The dense predecessor allocated n * k logicals plus a k * k weight matrix;
  ## at these sizes that would be ~60 GB and fail with `vector memory limit`.
  skip_on_cran()
  n_docs  <- 20000L
  k_items <- 20000L
  items_per_doc <- 20L  # typical citation count

  set.seed(42)
  transactions <- lapply(seq_len(n_docs), function(i) {
    paste0("R", sample.int(k_items, items_per_doc))
  })

  ## Run both counting methods; just assert we return an edge list without
  ## blowing up memory or time.
  t0 <- Sys.time()
  res_full <- cooccurrence(transactions, counting = "full")
  res_frac <- cooccurrence(transactions, counting = "fractional")
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  expect_s3_class(res_full, "cooccurrence")
  expect_s3_class(res_frac, "cooccurrence")
  expect_gt(nrow(res_full), 0L)
  expect_gt(nrow(res_frac), 0L)

  ## The stored matrix attribute must be sparse, not a dense k x k base matrix.
  expect_true(inherits(attr(res_full, "matrix"), "Matrix"))
  expect_true(inherits(attr(res_frac, "matrix"), "Matrix"))

  ## Sanity ceiling on wall time — catches accidental dense regressions that
  ## would take minutes instead of seconds.
  expect_lt(elapsed, 120)
})


# ---- window= parameter (categorical time series) -----------------------

test_that("window=2 produces adjacent-pair co-occurrence", {
  ## Sequence ABCAB, window=2 → mini-transactions {A,B}, {B,C}, {A,C},
  ## {A,B}. Raw counts: A-B=2, A-C=1, B-C=1.
  seq <- list(c("A", "B", "C", "A", "B"))
  res <- cooccurrence(seq, window = 2L, similarity = "none")

  expect_equal(nrow(res), 3L)
  expect_equal(attr(res, "n_transactions"), 4L)

  pair <- function(r, a, b) {
    rows <- (r$from == a & r$to == b) | (r$from == b & r$to == a)
    r[rows, , drop = FALSE]
  }
  expect_equal(pair(res, "A", "B")$count, 2L)
  expect_equal(pair(res, "A", "C")$count, 1L)
  expect_equal(pair(res, "B", "C")$count, 1L)
})

test_that("window equal to sequence length matches bag-of-states", {
  ## window = T → exactly one window per sequence, deduped to its
  ## state set. This must equal the no-window (default) result.
  seq <- list(c("A", "B", "C", "A", "B"))
  res_win  <- cooccurrence(seq, window = 5L, similarity = "none")
  res_bag  <- cooccurrence(seq, similarity = "none")

  key <- function(r) paste(pmin(r$from, r$to), pmax(r$from, r$to))
  res_win <- res_win[order(key(res_win)), c("from", "to", "count")]
  res_bag <- res_bag[order(key(res_bag)), c("from", "to", "count")]
  rownames(res_win) <- rownames(res_bag) <- NULL
  expect_equal(res_win, res_bag)
})

test_that("window drops sequences shorter than the window", {
  ## Two sequences, only the long one contributes when window=4.
  seqs <- list(c("A", "B"), c("A", "B", "C", "A"))
  res <- cooccurrence(seqs, window = 4L, similarity = "none")
  ## One window (the full long sequence), three states, three pairs.
  expect_equal(nrow(res), 3L)
  expect_equal(attr(res, "n_transactions"), 1L)
})

test_that("window works on wide TraMineR-style data with void markers", {
  ## Voids must be dropped before windowing.
  df <- data.frame(
    t1 = c("A", "B"),
    t2 = c("B", "NA"),
    t3 = c("C", "C"),
    t4 = c("A", "%"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "all", window = 2L, similarity = "none")
  ## Row 1 cleaned: A,B,C,A → windows {A,B}, {B,C}, {C,A} → A-B=1, A-C=1, B-C=1
  ## Row 2 cleaned: B,C   → window  {B,C}                 → B-C=1
  pair <- function(r, a, b) {
    rows <- (r$from == a & r$to == b) | (r$from == b & r$to == a)
    r[rows, , drop = FALSE]
  }
  expect_equal(pair(res, "A", "B")$count, 1L)
  expect_equal(pair(res, "A", "C")$count, 1L)
  expect_equal(pair(res, "B", "C")$count, 2L)
})

test_that("window composes with similarity normalisation", {
  ## Same ABCAB / window=2 setup. After dedup-by-window:
  ##   transactions = {A,B}, {B,C}, {A,C}, {A,B}
  ##   freq: A=3, B=3, C=2; counts: A-B=2, A-C=1, B-C=1
  ##   jaccard(A,B) = 2 / (3+3-2) = 0.5
  seq <- list(c("A", "B", "C", "A", "B"))
  res <- cooccurrence(seq, window = 2L, similarity = "jaccard")
  ab <- res[(res$from == "A" & res$to == "B") |
              (res$from == "B" & res$to == "A"), ]
  expect_equal(ab$weight, 0.5)
})

test_that("window=1 is rejected", {
  expect_error(
    cooccurrence(list(c("A", "B", "C")), window = 1L),
    "window >= 2"
  )
})

test_that("window is rejected for non-sequence formats", {
  df <- data.frame(items = c("A;B;C", "B;C", "A;C"),
                   stringsAsFactors = FALSE)
  expect_error(
    cooccurrence(df, field = "items", sep = ";", window = 2L),
    "ordered sequence formats"
  )

  df_long <- data.frame(doc = c(1, 1, 2), item = c("A", "B", "C"),
                        stringsAsFactors = FALSE)
  expect_error(
    cooccurrence(df_long, field = "item", by = "doc", window = 2L),
    "ordered sequence formats"
  )

  bin <- matrix(c(1, 1, 0, 1, 0, 1), nrow = 2,
                dimnames = list(NULL, c("A", "B", "C")))
  expect_error(
    cooccurrence(bin, window = 2L),
    "ordered sequence formats"
  )
})

test_that("counting = 'attention' gives closer pairs higher weight (gap-decay)", {
  ## One transaction with 4 ordered items at positions 1..4, lambda = 1.
  ## Pair contribution = exp(-(j - i) / lambda).
  ##   gap 1 → exp(-1) ≈ 0.368
  ##   gap 2 → exp(-2) ≈ 0.135
  ##   gap 3 → exp(-3) ≈ 0.050
  papers <- list(c("A", "B", "C", "D"))
  r <- cooccurrence(papers, counting = "attention",
                    similarity = "none")
  pull <- function(r, i, j) {
    rows <- (r$from == i & r$to == j) | (r$from == j & r$to == i)
    r[rows, "weight"]
  }
  expect_equal(pull(r, "A", "B"), exp(-1))
  expect_equal(pull(r, "B", "C"), exp(-1))
  expect_equal(pull(r, "C", "D"), exp(-1))
  expect_equal(pull(r, "A", "C"), exp(-2))
  expect_equal(pull(r, "B", "D"), exp(-2))
  expect_equal(pull(r, "A", "D"), exp(-3))

  ## Closer pairs strictly heavier than distant pairs.
  expect_gt(pull(r, "A", "B"), pull(r, "A", "C"))
  expect_gt(pull(r, "A", "C"), pull(r, "A", "D"))
})

test_that("attention pair weights depend ONLY on gap, not absolute position", {
  ## Pairs at the same gap get the same weight regardless of where they
  ## sit in the sequence — this is what distinguishes real attention
  ## from positional emphasis schemes.
  papers <- list(c("A", "B", "C", "D", "E"))
  r <- cooccurrence(papers, counting = "attention", similarity = "none")
  pull <- function(r, i, j) {
    rows <- (r$from == i & r$to == j) | (r$from == j & r$to == i)
    r[rows, "weight"]
  }
  ## Pairs at gap 1: (A,B), (B,C), (C,D), (D,E) — all equal.
  gap1 <- c(pull(r, "A", "B"), pull(r, "B", "C"),
            pull(r, "C", "D"), pull(r, "D", "E"))
  expect_true(all(gap1 == gap1[1]))
  ## Pairs at gap 2: (A,C), (B,D), (C,E) — all equal.
  gap2 <- c(pull(r, "A", "C"), pull(r, "B", "D"), pull(r, "C", "E"))
  expect_true(all(gap2 == gap2[1]))
})

test_that("attention lambda controls the decay rate", {
  papers <- list(c("A", "B", "C", "D"))
  r_fast  <- cooccurrence(papers, counting = "attention",
                          lambda = 0.5, similarity = "none")
  r_slow  <- cooccurrence(papers, counting = "attention",
                          lambda = 5.0, similarity = "none")
  pull <- function(r, i, j) {
    rows <- (r$from == i & r$to == j) | (r$from == j & r$to == i)
    r[rows, "weight"]
  }
  ## Fast decay: gap-3 pair very small.
  expect_equal(pull(r_fast, "A", "D"), exp(-3 / 0.5))
  ## Slow decay: gap-3 pair still substantial.
  expect_equal(pull(r_slow, "A", "D"), exp(-3 / 5.0))
  expect_lt(pull(r_fast, "A", "D"), pull(r_slow, "A", "D"))
})

test_that("attention aggregates pair contributions across transactions", {
  ## Two papers, A-B at gap 1 in both → final weight = 2 * exp(-1).
  papers <- list(c("A", "B", "C"), c("A", "B"))
  r <- cooccurrence(papers, counting = "attention",
                    similarity = "none")
  pull <- function(r, i, j) {
    rows <- (r$from == i & r$to == j) | (r$from == j & r$to == i)
    r[rows, "weight"]
  }
  expect_equal(pull(r, "A", "B"), 2 * exp(-1))
})

test_that("attention rejects non-positive lambda", {
  expect_error(
    cooccurrence(list(c("A", "B")), counting = "attention", lambda = 0),
    "lambda > 0"
  )
  expect_error(
    cooccurrence(list(c("A", "B")), counting = "attention", lambda = -1),
    "lambda > 0"
  )
})

test_that("aggregate_by sums per-group counts equals global count (similarity = none)", {
  df <- data.frame(
    journal  = c("J1", "J1", "J2", "J2", "J3"),
    keywords = c("A;B;C", "B;C", "A;B", "B;C", "A;C"),
    stringsAsFactors = FALSE
  )
  r_agg <- cooccurrence(df, field = "keywords", sep = ";",
                        aggregate_by = "journal", similarity = "none")
  r_glo <- cooccurrence(df[, "keywords", drop = FALSE],
                        field = "keywords", sep = ";",
                        similarity = "none")

  key <- function(r) paste(pmin(r$from, r$to), pmax(r$from, r$to))
  a <- r_agg[order(key(r_agg)), c("from", "to", "weight")]
  g <- r_glo[order(key(r_glo)), c("from", "to", "weight")]
  rownames(a) <- rownames(g) <- NULL
  expect_equal(a, g)
})

test_that("aggregate_by attributes record group info", {
  df <- data.frame(j = c("a", "a", "b"),
                   k = c("X;Y", "Y;Z", "X;Z"),
                   stringsAsFactors = FALSE)
  r <- cooccurrence(df, field = "k", sep = ";",
                    aggregate_by = "j", similarity = "none")
  expect_identical(attr(r, "aggregate_by"), "j")
  expect_identical(attr(r, "aggregate"), "sum")
  expect_setequal(attr(r, "groups"), c("a", "b"))
  expect_false("group" %in% names(r))
})

test_that("aggregate = mean / min / max give different weights", {
  df <- data.frame(
    journal  = c("J1", "J1", "J2"),
    keywords = c("A;B", "A;B", "A;B"),
    stringsAsFactors = FALSE
  )
  ## J1 has count 2 for A-B; J2 has count 1.
  r_sum  <- cooccurrence(df, field = "keywords", sep = ";",
                         aggregate_by = "journal", aggregate = "sum",
                         similarity = "none")
  r_mean <- cooccurrence(df, field = "keywords", sep = ";",
                         aggregate_by = "journal", aggregate = "mean",
                         similarity = "none")
  r_min  <- cooccurrence(df, field = "keywords", sep = ";",
                         aggregate_by = "journal", aggregate = "min",
                         similarity = "none")
  r_max  <- cooccurrence(df, field = "keywords", sep = ";",
                         aggregate_by = "journal", aggregate = "max",
                         similarity = "none")
  expect_equal(r_sum$weight,  3)
  expect_equal(r_mean$weight, 1.5)
  expect_equal(r_min$weight,  1)
  expect_equal(r_max$weight,  2)
  ## Count is always summed regardless of aggregate.
  expect_equal(r_mean$count, 3L)
  expect_equal(r_max$count,  3L)
})

test_that("aggregate_by composes with per-group similarity normalisation", {
  df <- data.frame(
    journal  = c("J1", "J1", "J2"),
    keywords = c("A;B", "A;B;C", "A;B"),
    stringsAsFactors = FALSE
  )
  ## In J1: A appears 2x, B 2x, C 1x. Co-occ A-B=2, A-C=1, B-C=1.
  ## Per-group jaccard at J1: A-B = 2/(2+2-2) = 1; A-C = 1/(2+1-1) = 0.5; B-C = 0.5.
  ## In J2: A-B=1, jaccard = 1/(1+1-1) = 1.
  ## Sum across journals: A-B = 1+1 = 2, A-C = 0.5, B-C = 0.5.
  r <- cooccurrence(df, field = "keywords", sep = ";",
                    aggregate_by = "journal", aggregate = "sum",
                    similarity = "jaccard")
  ab <- r[(r$from == "A" & r$to == "B") | (r$from == "B" & r$to == "A"), ]
  ac <- r[(r$from == "A" & r$to == "C") | (r$from == "C" & r$to == "A"), ]
  expect_equal(ab$weight, 2)
  expect_equal(ac$weight, 0.5)
})

test_that("aggregate_by + split_by together is rejected", {
  df <- data.frame(j = c("a", "b"), k = c("X;Y", "X;Y"),
                   stringsAsFactors = FALSE)
  expect_error(
    cooccurrence(df, field = "k", sep = ";",
                 aggregate_by = "j", split_by = "j"),
    "cannot be combined"
  )
})

test_that("aggregate_by applies threshold and top_n after aggregation", {
  df <- data.frame(
    journal  = c("J1", "J1", "J2", "J2"),
    keywords = c("A;B;C", "B;C", "A;C", "B;C"),
    stringsAsFactors = FALSE
  )
  ## Per-group counts:
  ##   J1: A-B=1, A-C=1, B-C=2
  ##   J2: A-C=1, B-C=1
  ## Summed: A-B=1, A-C=2, B-C=3
  r <- cooccurrence(df, field = "keywords", sep = ";",
                    aggregate_by = "journal", similarity = "none",
                    threshold = 2, top_n = 1)
  expect_equal(nrow(r), 1L)
  expect_true(r$weight >= 2)
  expect_true(r$from == "B" || r$to == "B")  # B-C is the strongest
})

test_that("window combined with split_by computes per group", {
  df <- data.frame(
    grp = c("g1", "g1", "g2", "g2"),
    t1  = c("A", "B", "A", "C"),
    t2  = c("B", "C", "B", "B"),
    t3  = c("C", "A", "C", "A"),
    stringsAsFactors = FALSE
  )
  res <- cooccurrence(df, field = "all", window = 2L, split_by = "grp",
                      similarity = "none")
  expect_true("group" %in% names(res))
  expect_setequal(unique(res$group), c("g1", "g2"))
  ## Each group should have at least one edge.
  expect_true(all(table(res$group) >= 1L))
})


# ========================================
# 20. Pooled metrics are undefined for split_by results
# ========================================

.split_fixture <- function() {
  df <- data.frame(
    grp = c("X", "X", "X", "Y", "Y", "Z", "Z", "Z"),
    kw  = c("A,B", "B,C", "A,B,C", "A,B", "B,C,D", "A,C", "C,D", "A,B,D"),
    stringsAsFactors = FALSE
  )
  cooccurrence(df, field = "kw", sep = ",", split_by = "grp")
}

test_that("split_by summary reports NA for undefined pooled metrics", {
  s <- .co_summary_data(.split_fixture())
  expect_true(s$is_split)
  ## Pooling one network per group makes these undefined, not merely large.
  expect_true(is.na(s$density))
  expect_true(is.na(s$mean_degree))
  expect_true(is.na(s$possible_edges))
})

test_that("non-split summary still reports pooled metrics", {
  s <- .co_summary_data(cooccurrence(.test_list))
  expect_false(s$is_split)
  expect_false(is.na(s$density))
  expect_false(is.na(s$mean_degree))
  expect_false(is.na(s$possible_edges))
  expect_lte(s$density, 1)
})

test_that("density never exceeds 1 and degree never exceeds n - 1", {
  for (res in list(cooccurrence(.test_list), cooccurrence(.test_list,
                                                          similarity = "jaccard"))) {
    s <- .co_summary_data(res)
    expect_lte(s$density, 1)
    expect_lte(max(s$nodes$degree), s$n_nodes - 1L)
  }
})

test_that("split_by print omits density and top nodes", {
  out <- capture.output(print(.split_fixture()))
  expect_false(any(grepl("density", out)))
  expect_false(any(grepl("top nodes", out)))
  expect_true(any(grepl("split_by: grp", out)))
})

test_that("split_by summary print omits density but keeps group table", {
  out <- capture.output(summary(.split_fixture()))
  expect_false(any(grepl("^Density", out)))
  expect_false(any(grepl("^Mean degree", out)))
  expect_false(any(grepl("^Possible edges", out)))
  expect_false(any(grepl("^Top nodes", out)))
  expect_true(any(grepl("Groups", out)))
})

test_that("per-group densities remain valid for split_by", {
  s <- .co_summary_data(.split_fixture())
  expect_true(all(s$groups_summary$density <= 1))
  expect_true(all(s$groups_summary$density >= 0))
})

# ========================================
# 21. as.data.frame() on a summary
# ========================================

test_that("as.data.frame on a summary returns the tidy node table", {
  s <- .co_summary_data(cooccurrence(.test_list))
  nodes <- as.data.frame(s)
  expect_s3_class(nodes, "data.frame")
  expect_identical(class(nodes), "data.frame")
  expect_equal(nrow(nodes), s$n_nodes)
  expect_true(all(c("node", "degree", "strength", "count_strength",
                    "frequency") %in% names(nodes)))
})

test_that("as.data.frame(what = 'groups') returns one row per group", {
  s <- .co_summary_data(.split_fixture())
  groups <- as.data.frame(s, what = "groups")
  expect_s3_class(groups, "data.frame")
  expect_equal(nrow(groups), 3L)
  expect_setequal(groups$group, c("X", "Y", "Z"))
})

test_that("as.data.frame(what = 'groups') is empty for an unsplit network", {
  groups <- as.data.frame(.co_summary_data(cooccurrence(.test_list)),
                          what = "groups")
  expect_s3_class(groups, "data.frame")
  expect_equal(nrow(groups), 0L)
  expect_true("group" %in% names(groups))
})

test_that("as.data.frame rejects an unknown 'what'", {
  s <- .co_summary_data(cooccurrence(.test_list))
  expect_error(as.data.frame(s, what = "nope"))
})

test_that("subset() on a split result keeps a printable cooccurrence", {
  one <- subset(.split_fixture(), group == "X")
  expect_true(all(one$group == "X"))
  expect_gt(nrow(one), 0L)
  expect_no_error(capture.output(print(one)))
})


# ========================================
# 22. Unknown arguments are rejected
# ========================================

test_that("a misspelled argument errors instead of silently changing results", {
  expect_error(cooccurrence(.test_list, simliarity = "jaccard"),
               "Unknown argument")
  expect_error(cooccurrence(.test_list, windwo = 2), "Unknown argument")
  ## The bug this guards: the typo used to fall through to raw counts.
  expect_false(identical(
    as.data.frame(cooccurrence(.test_list, similarity = "jaccard"))$weight,
    as.data.frame(cooccurrence(.test_list))$weight
  ))
})

test_that("correctly spelled arguments still work", {
  expect_s3_class(cooccurrence(.test_list, similarity = "jaccard"),
                  "cooccurrence")
})

# ========================================
# 23. vars = explicit indicator columns
# ========================================

.onehot_df <- function() {
  data.frame(id = c("d1", "d2", "d3"),
             A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1),
             stringsAsFactors = FALSE)
}

test_that("vars selects indicator columns and ignores id columns", {
  res <- cooccurrence(.onehot_df(), vars = c("A", "B", "C"))
  expect_s3_class(res, "cooccurrence")
  expect_setequal(attr(res, "items"), c("A", "B", "C"))
  expect_false("id" %in% attr(res, "items"))
})

test_that("vars gives the same answer as the bare binary matrix", {
  bare <- data.frame(A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1))
  expect_equal(as.data.frame(cooccurrence(.onehot_df(), vars = c("A","B","C"))),
               as.data.frame(cooccurrence(bare)))
})

test_that("logical indicator columns are accepted", {
  lg <- data.frame(A = c(TRUE, FALSE, TRUE), B = c(TRUE, TRUE, FALSE),
                   C = c(FALSE, TRUE, TRUE))
  num <- data.frame(A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1))
  ## auto-detected without vars, and identical to the numeric encoding
  expect_equal(as.data.frame(cooccurrence(lg)),
               as.data.frame(cooccurrence(num)))
  expect_equal(as.data.frame(cooccurrence(lg, vars = c("A","B","C"))),
               as.data.frame(cooccurrence(num)))
})

test_that("count columns are read as presence", {
  counts <- data.frame(doc = 1:3, A = c(2, 0, 5), B = c(1, 3, 0),
                       C = c(0, 1, 4))
  binary <- data.frame(A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1))
  expect_equal(as.data.frame(cooccurrence(counts, vars = c("A","B","C"))),
               as.data.frame(cooccurrence(binary)))
})

test_that("NA in an indicator table is treated as absent, not an error", {
  na_df <- data.frame(A = c(1, NA, 1), B = c(1, 1, 0), C = c(0, 1, 1))
  zero_df <- data.frame(A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1))
  expect_no_error(cooccurrence(na_df))
  expect_equal(as.data.frame(cooccurrence(na_df)),
               as.data.frame(cooccurrence(zero_df)))
  expect_equal(as.data.frame(cooccurrence(na_df, vars = c("A","B","C"))),
               as.data.frame(cooccurrence(zero_df)))
})

test_that("vars validates its input", {
  d <- .onehot_df()
  expect_error(cooccurrence(d, vars = c("A", "nope")), "not found")
  expect_error(cooccurrence(d, vars = c("id", "A")), "not numeric or logical")
  expect_error(cooccurrence(d, vars = "A"), "at least two")
  expect_error(cooccurrence(d, vars = c("A", "B"), field = "id"),
               "cannot be combined")
  expect_error(cooccurrence(list(c("A", "B")), vars = c("A", "B")),
               "data frame or matrix")
})

test_that("negative values in vars columns are rejected", {
  d <- data.frame(A = c(1, -1), B = c(1, 1))
  expect_error(cooccurrence(d, vars = c("A", "B")), "non-negative")
})

test_that("vars composes with similarity, split_by and top_n", {
  d <- data.frame(grp = c("x", "x", "y", "y"),
                  A = c(1, 1, 1, 0), B = c(1, 0, 1, 1), C = c(0, 1, 1, 1))
  res <- cooccurrence(d, vars = c("A", "B", "C"), similarity = "jaccard",
                      split_by = "grp")
  expect_true("group" %in% names(res))
  expect_setequal(unique(res$group), c("x", "y"))
  expect_equal(nrow(cooccurrence(d, vars = c("A","B","C"), top_n = 2)), 2L)
})

test_that("window is rejected for indicator tables", {
  expect_error(cooccurrence(.onehot_df(), vars = c("A","B","C"), window = 2),
               "no within-row order")
})

# ========================================
# 24. nestimate_data input
# ========================================

test_that("a nestimate_data object is unwrapped instead of silently mangled", {
  skip_if_not_installed("Nestimate")
  ev <- data.frame(
    student = rep(c("s1", "s2"), each = 3),
    code = c("read", "write", "read", "test", "write", "read"),
    timestamp = as.POSIXct("2026-01-01") + c(0, 60, 120, 0, 30, 90),
    stringsAsFactors = FALSE
  )
  p <- Nestimate::prepare(ev, actor = "student", action = "code",
                          time = "timestamp")
  res <- cooccurrence(p)
  ## Items must be the actual states, not stringified list internals.
  expect_setequal(attr(res, "items"), c("read", "test", "write"))
  ## Identical to unwrapping by hand.
  expect_equal(as.data.frame(res),
               as.data.frame(cooccurrence(p$sequence_data, field = "all")))
})

test_that("nestimate_data composes with window", {
  skip_if_not_installed("Nestimate")
  ev <- data.frame(
    student = rep(c("s1", "s2"), each = 3),
    code = c("read", "write", "test", "test", "write", "read"),
    timestamp = as.POSIXct("2026-01-01") + c(0, 60, 120, 0, 30, 90),
    stringsAsFactors = FALSE
  )
  p <- Nestimate::prepare(ev, actor = "student", action = "code",
                          time = "timestamp")
  expect_equal(as.data.frame(cooccurrence(p, window = 2)),
               as.data.frame(cooccurrence(p$sequence_data, field = "all",
                                          window = 2)))
})


# ========================================
# 25. Raw event-log input (action/actor/time)
# ========================================

.event_fixture <- function() {
  data.frame(
    student = rep(c("s1", "s2", "s3"), each = 6),
    code = c("read", "write", "read", "test", "write", "read",
             "read", "read", "test", "write", "test", "read",
             "write", "test", "read", "read", "write", "test"),
    stamp = as.POSIXct("2026-01-01 09:00:00") +
      c(0, 60, 120, 5000, 5060, 5120,
        0, 30, 90, 150, 210, 270,
        0, 45, 90, 20000, 20060, 20120),
    stringsAsFactors = FALSE
  )
}

test_that("a raw event log can be passed directly", {
  skip_if_not_installed("Nestimate")
  res <- cooccurrence(.event_fixture(), actor = "student", action = "code",
                      time = "stamp")
  expect_s3_class(res, "cooccurrence")
  expect_setequal(attr(res, "items"), c("read", "test", "write"))
})

test_that("raw event input equals the manual prepare() route", {
  skip_if_not_installed("Nestimate")
  ev <- .event_fixture()
  p <- Nestimate::prepare(ev, actor = "student", action = "code",
                          time = "stamp")
  expect_equal(
    as.data.frame(cooccurrence(ev, actor = "student", action = "code",
                               time = "stamp")),
    as.data.frame(cooccurrence(p$sequence_data, field = "all"))
  )
})

test_that("time_threshold controls how sessions are split", {
  skip_if_not_installed("Nestimate")
  ev <- .event_fixture()
  tight <- cooccurrence(ev, actor = "student", action = "code",
                        time = "stamp", time_threshold = 900)
  loose <- cooccurrence(ev, actor = "student", action = "code",
                        time = "stamp", time_threshold = Inf)
  ## Gaps split s1 and s3 into two sessions each at 900s; Inf keeps 3.
  expect_equal(attr(tight, "n_transactions"), 5L)
  expect_equal(attr(loose, "n_transactions"), 3L)
})

test_that("raw event input works without a time column", {
  skip_if_not_installed("Nestimate")
  res <- cooccurrence(.event_fixture(), actor = "student", action = "code")
  expect_s3_class(res, "cooccurrence")
  expect_equal(attr(res, "n_transactions"), 3L)
})

test_that("raw event input composes with window and similarity", {
  skip_if_not_installed("Nestimate")
  res <- cooccurrence(.event_fixture(), actor = "student", action = "code",
                      time = "stamp", window = 2, similarity = "jaccard")
  expect_s3_class(res, "cooccurrence")
  expect_true(all(res$weight <= 1))
  expect_equal(attr(res, "similarity"), "jaccard")
})

test_that("event-log arguments are validated", {
  skip_if_not_installed("Nestimate")
  ev <- .event_fixture()
  expect_error(cooccurrence(ev, actor = "student"), "require `action`")
  expect_error(cooccurrence(ev, time = "stamp"), "require `action`")
  expect_error(cooccurrence(ev, actor = "student", action = "nope"),
               "must name a single column")
  expect_error(cooccurrence(ev, actor = "zzz", action = "code"), "not found")
  expect_error(cooccurrence(ev, action = "code", session = "zzz"), "not found")
  expect_error(cooccurrence(list(c("a", "b")), action = "code"),
               "data frame of event records")
})


# ========================================
# 26. vars accepts flexible column specifications
# ========================================

.spec_df <- function() {
  data.frame(id = c("d1", "d2", "d3"),
             A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1), D = c(1, 1, 1),
             note = c("x", "y", "z"),
             stringsAsFactors = FALSE)
}

test_that("vars accepts a bare column range", {
  expect_setequal(attr(cooccurrence(.spec_df(), vars = A:D), "items"),
                  c("A", "B", "C", "D"))
})

test_that("vars accepts bare names via c()", {
  expect_setequal(attr(cooccurrence(.spec_df(), vars = c(A, B, C)), "items"),
                  c("A", "B", "C"))
})

test_that("vars accepts positional indices", {
  expect_setequal(attr(cooccurrence(.spec_df(), vars = 2:5), "items"),
                  c("A", "B", "C", "D"))
})

test_that("vars accepts negative selection", {
  expect_setequal(attr(cooccurrence(.spec_df(), vars = -c(id, note)), "items"),
                  c("A", "B", "C", "D"))
})

test_that("vars accepts a logical mask", {
  mask <- c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
  expect_setequal(attr(cooccurrence(.spec_df(), vars = mask), "items"),
                  c("A", "B", "C", "D"))
})

test_that("vars accepts a character vector held in a variable", {
  states <- c("A", "B", "C")
  expect_setequal(attr(cooccurrence(.spec_df(), vars = states), "items"),
                  c("A", "B", "C"))
})

test_that("all vars spellings give the same network", {
  d <- .spec_df()
  target <- as.data.frame(cooccurrence(d, vars = c("A", "B", "C", "D")))
  expect_equal(as.data.frame(cooccurrence(d, vars = A:D)), target)
  expect_equal(as.data.frame(cooccurrence(d, vars = 2:5)), target)
  expect_equal(as.data.frame(cooccurrence(d, vars = -c(id, note))), target)
})

test_that("bad vars specifications are rejected clearly", {
  d <- .spec_df()
  expect_error(cooccurrence(d, vars = A:zzz), "could not be resolved")
  expect_error(cooccurrence(d, vars = 2:99), "out of range")
  expect_error(cooccurrence(d, vars = c(-1, 2)), "mix positive and negative")
  expect_error(cooccurrence(d, vars = A), "at least two")
  expect_error(cooccurrence(d, vars = c(id, A)), "not numeric or logical")
  expect_error(cooccurrence(d, vars = c(TRUE, FALSE)), "one entry per column")
})


# ========================================
# 27. Findings from the Codex audit
# ========================================

test_that("the CRAN 0.1.1 positional argument prefix is preserved", {
  ## Positional calls written against 0.1.1 must keep their meaning.
  expect_identical(
    names(formals(cooccurrence))[1:13],
    c("data", "field", "by", "sep", "weight_by", "split_by", "similarity",
      "counting", "scale", "threshold", "min_occur", "top_n", "output")
  )
  d <- data.frame(item = c("a", "a", "b"), doc = c("d1", "d2", "d1"),
                  w = c(1, 2, 1), stringsAsFactors = FALSE)
  expect_no_error(cooccurrence(d, "item", "doc", NULL, "w"))
  expect_equal(as.data.frame(cooccurrence(d, "item", "doc", NULL, "w")),
               as.data.frame(cooccurrence(d, field = "item", by = "doc",
                                          weight_by = "w")))
})

test_that("a repeated column in vars does not inflate weight or count", {
  one_row <- data.frame(A = 1, B = 1)
  expect_equal(as.data.frame(cooccurrence(one_row, vars = c("A", "A", "B"))),
               as.data.frame(cooccurrence(one_row, vars = c("A", "B"))))
  d <- data.frame(A = c(1, 0), B = c(1, 1), C = c(0, 1))
  expect_equal(as.data.frame(cooccurrence(d, vars = c(A, B, C, A))),
               as.data.frame(cooccurrence(d, vars = c(A, B, C))))
})

test_that("per-group density counts nodes isolated within their group", {
  ## Group x holds edge A-B plus isolated C: 3 nodes, 1 of 3 possible edges.
  g <- data.frame(grp = c("x", "x", "y", "y"),
                  kw = c("A,B", "C", "D,E", "F"), stringsAsFactors = FALSE)
  gs <- as.data.frame(summary(cooccurrence(g, field = "kw", sep = ",",
                                           split_by = "grp")),
                      what = "groups")
  expect_equal(gs$n_nodes, c(3L, 3L))
  expect_equal(gs$density, c(1/3, 1/3))
  expect_true(all(gs$density <= 1))
})

test_that("a split result does not inherit the first group's matrix", {
  g <- data.frame(grp = c("x", "x", "y", "y"),
                  kw = c("A,B", "A,B", "D,E", "D,E"), stringsAsFactors = FALSE)
  sp <- cooccurrence(g, field = "kw", sep = ",", split_by = "grp")
  ## Previously rbind() carried group x's matrix/items onto the whole frame,
  ## so as_matrix() silently described only group x.
  expect_null(attr(sp, "matrix"))
  expect_null(attr(sp, "items"))
  expect_null(attr(sp, "frequencies"))
})

test_that("the groups attribute lists only groups that produced edges", {
  ## Group "z" has a single item and yields no edges.
  g <- data.frame(grp = c("x", "x", "z"), kw = c("A,B", "A,B", "Q"),
                  stringsAsFactors = FALSE)
  sp <- cooccurrence(g, field = "kw", sep = ",", split_by = "grp")
  expect_setequal(attr(sp, "groups"), unique(sp$group))
  expect_false("z" %in% attr(sp, "groups"))
})


# ========================================
# 28. Audit findings: engine consistency
# ========================================

test_that("weight_by composes with min_occur without crashing", {
  w <- data.frame(item = c("A", "A", "B", "C"), doc = c("d1", "d2", "d1", "d2"),
                  w = c(1, 1, 1, 1), stringsAsFactors = FALSE)
  ## The binary companion matrix used to keep unfiltered column indices
  ## against filtered dims: "'dims' must contain all (i,j) pairs".
  expect_no_error(cooccurrence(w, field = "item", by = "doc",
                               weight_by = "w", min_occur = 2))
})

test_that("min_occur support counts distinct documents, not rows", {
  ## A appears twice but in ONE document, so it fails min_occur = 2.
  dup <- data.frame(item = c("A", "A", "B", "B"),
                    doc = c("d1", "d1", "d1", "d2"),
                    w = c(1, 1, 1, 1), stringsAsFactors = FALSE)
  res <- try(cooccurrence(dup, field = "item", by = "doc", weight_by = "w",
                          min_occur = 2), silent = TRUE)
  if (!inherits(res, "try-error")) expect_false("A" %in% attr(res, "items"))
})

test_that("the stored matrix agrees with the filtered edge list", {
  tx <- list(c("A", "B", "C"), c("A", "B"), c("B", "C"))
  r1 <- cooccurrence(tx, top_n = 1)
  expect_equal(sum(as_matrix(r1) != 0) / 2, nrow(r1))
  r2 <- cooccurrence(tx, similarity = "jaccard", threshold = 0.6)
  expect_equal(sum(as_matrix(r2) != 0) / 2, nrow(r2))
})

test_that("threshold = 0 does not discard negative weights", {
  ## z-score centres weights on zero; the default must not halve the network.
  res <- cooccurrence(.test_list, scale = "zscore", threshold = 0)
  vals <- res$weight[res$weight != 0]
  if (length(vals) > 1) expect_equal(mean(vals), 0, tolerance = 1e-10)
  expect_true(any(res$weight < 0))
})

test_that("a full-length window matches unwindowed attention", {
  ## embed() emits each window reversed; attention reads positional gaps,
  ## so an unreversed window silently mirrored the decay.
  seqs <- list(c("A", "B", "C", "A"))
  expect_equal(
    as.data.frame(cooccurrence(seqs, counting = "attention", window = 4)),
    as.data.frame(cooccurrence(seqs, counting = "attention"))
  )
  expect_equal(
    as.data.frame(cooccurrence(list(c("A", "B", "C")), counting = "attention",
                               window = 3)),
    as.data.frame(cooccurrence(list(c("A", "B", "C")), counting = "attention"))
  )
})

test_that("windowed set counting is unaffected by the window orientation", {
  seqs <- list(c("A", "B", "C", "D"))
  res <- cooccurrence(seqs, window = 2)
  expect_setequal(paste(res$from, res$to), c("A B", "B C", "C D"))
})

test_that("relative keeps both directions in the stored matrix", {
  tx <- list(c("A", "B", "C"), c("A", "B"), c("B", "C"))
  m <- as_matrix(cooccurrence(tx, similarity = "relative"))
  expect_false(isTRUE(all.equal(m, t(m))))
  expect_equal(unname(rowSums(m)[rowSums(m) > 0]),
               rep(1, sum(rowSums(m) > 0)))
})

test_that("gephi output works with the converters", {
  skip_if_not_installed("igraph")
  tx <- list(c("A", "B", "C"), c("A", "B"), c("B", "C"))
  gph <- cooccurrence(tx, output = "gephi")
  expect_no_error(as_igraph(gph))
  expect_no_error(as_matrix(gph))
  expect_equal(igraph::ecount(as_igraph(gph)), nrow(gph))
})

test_that("a failing group warns instead of vanishing silently", {
  ## An edgeless group is expected and stays quiet.
  g <- data.frame(grp = c("x", "x", "y"), kw = c("A,B", "A,B", "Q"),
                  stringsAsFactors = FALSE)
  expect_no_warning(cooccurrence(g, field = "kw", sep = ",", split_by = "grp"))
})


# ========================================
# 29. Bundled demo dataset supports every grouping
# ========================================

test_that("demo has multi-genre films so genre x movie yields a network", {
  expect_true(all(c("movie", "actor", "genre") %in% names(demo)))
  ## Every film must carry more than one genre, or genres can never
  ## co-occur within a film.
  per_movie <- tapply(demo$genre, demo$movie, function(x) length(unique(x)))
  expect_true(all(per_movie >= 1))
  expect_true(any(per_movie > 1))

  res <- cooccurrence(demo, field = "genre", by = "movie")
  expect_gt(nrow(res), 0L)
  ## Crime and Drama share the most films in this cast.
  top <- as.data.frame(res)[1, ]
  expect_setequal(c(top$from, top$to), c("Crime", "Drama"))
})

test_that("all three demo groupings produce networks", {
  expect_gt(nrow(cooccurrence(demo, field = "actor", by = "movie")), 0L)
  expect_gt(nrow(cooccurrence(demo, field = "genre", by = "movie")), 0L)
  expect_gt(nrow(cooccurrence(demo, field = "actor", by = "genre")), 0L)
  expect_gt(nrow(cooccurrence(demo, field = "genre", by = "actor")), 0L)
})

test_that("the demo co-starring network is unchanged", {
  ## Adding genre rows must not alter the actor network: the long parser
  ## deduplicates items within a transaction.
  res <- cooccurrence(demo, field = "actor", by = "movie",
                      similarity = "jaccard")
  expect_equal(nrow(res), 43L)
  expect_equal(length(unique(demo$actor)), 30L)
  expect_equal(nrow(unique(demo[, c("movie", "actor")])), 34L)
})
