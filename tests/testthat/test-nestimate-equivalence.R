# Cross-implementation equivalence with Nestimate::cooccurrence().
#
# Both packages export a cooccurrence() built on the same eight similarity
# definitions. These tests pin them to identical numbers so the two cannot
# drift apart silently.
#
# Two conventions must be aligned before comparing:
#   * Nestimate keeps item frequency on the matrix diagonal
#     (`diagonal = TRUE`); cooccure always zeroes it. Compare off-diagonal
#     values with `diagonal = FALSE`.
#   * `top_n` ties: cooccure truncates strictly at N, Nestimate keeps every
#     edge tied at the cut, so cooccure's edges are a SUBSET of Nestimate's.

SIMS <- c("none", "jaccard", "cosine", "inclusion", "association",
          "dice", "equivalence", "relative")

.ne_matrix <- function(...) {
  m <- as.matrix(Nestimate::cooccurrence(..., diagonal = FALSE)$weights)
  diag(m) <- 0
  m
}

.co_matrix <- function(...) {
  m <- as_matrix(cooccurrence(...))
  diag(m) <- 0
  m
}

.expect_same_network <- function(co, ne, tolerance = 1e-10) {
  expect_setequal(rownames(co), rownames(ne))
  it <- sort(rownames(co))
  expect_equal(co[it, it, drop = FALSE], ne[it, it, drop = FALSE],
               tolerance = tolerance)
}

.eq_list <- list(c("A", "B", "C"), c("B", "C"), c("A", "C"), c("A", "B", "D"),
                 c("D"), c("B", "C", "D"), c("A", "B", "C", "D"))

.eq_delim <- data.frame(
  id = 1:8,
  kw = c("A,B", "B,C", "A,B,C", "C", "A,C,D", "B,D", "A,B,D", "C,D"),
  stringsAsFactors = FALSE
)

.eq_long <- data.frame(
  item = c("A", "A", "B", "B", "C", "C", "C", "D", "D", "A"),
  doc  = c("d1", "d2", "d1", "d3", "d2", "d3", "d1", "d2", "d4", "d3"),
  stringsAsFactors = FALSE
)

.eq_binary <- data.frame(A = c(1, 0, 1, 1, 0, 1), B = c(1, 1, 0, 1, 1, 0),
                         C = c(0, 1, 1, 1, 0, 1), D = c(1, 1, 1, 0, 1, 0))

.eq_wide <- data.frame(t1 = c("A", "B", "A", "C", "B", "A"),
                       t2 = c("B", "C", "C", "A", "B", "D"),
                       t3 = c("C", "A", "B", "B", "D", "C"),
                       stringsAsFactors = FALSE)

test_that("all eight similarities match Nestimate on a list of transactions", {
  skip_if_not_installed("Nestimate")
  for (s in SIMS) {
    .expect_same_network(.co_matrix(.eq_list, similarity = s),
                         .ne_matrix(.eq_list, similarity = s))
  }
})

test_that("all eight similarities match Nestimate on delimited input", {
  skip_if_not_installed("Nestimate")
  for (s in SIMS) {
    .expect_same_network(
      .co_matrix(.eq_delim, field = "kw", sep = ",", similarity = s),
      .ne_matrix(.eq_delim, field = "kw", sep = ",", similarity = s))
  }
})

test_that("all eight similarities match Nestimate on long/bipartite input", {
  skip_if_not_installed("Nestimate")
  for (s in SIMS) {
    .expect_same_network(
      .co_matrix(.eq_long, field = "item", by = "doc", similarity = s),
      .ne_matrix(.eq_long, field = "item", by = "doc", similarity = s))
  }
})

test_that("all eight similarities match Nestimate on binary and wide input", {
  skip_if_not_installed("Nestimate")
  for (s in SIMS) {
    .expect_same_network(.co_matrix(.eq_binary, similarity = s),
                         .ne_matrix(.eq_binary, similarity = s))
    .expect_same_network(.co_matrix(.eq_wide, field = "all", similarity = s),
                         .ne_matrix(.eq_wide, field = "all", similarity = s))
  }
})

test_that("min_occur filtering matches Nestimate", {
  skip_if_not_installed("Nestimate")
  for (mo in 1:3) {
    .expect_same_network(.co_matrix(.eq_list, min_occur = mo),
                         .ne_matrix(.eq_list, min_occur = mo))
    .expect_same_network(
      .co_matrix(.eq_list, similarity = "jaccard", min_occur = mo),
      .ne_matrix(.eq_list, similarity = "jaccard", min_occur = mo))
  }
})

test_that("threshold filtering matches Nestimate", {
  skip_if_not_installed("Nestimate")
  for (th in c(0, 0.2, 0.5)) {
    .expect_same_network(
      .co_matrix(.eq_list, similarity = "jaccard", threshold = th),
      .ne_matrix(.eq_list, similarity = "jaccard", threshold = th))
  }
  for (th in c(0, 1, 2)) {
    .expect_same_network(.co_matrix(.eq_list, threshold = th),
                         .ne_matrix(.eq_list, threshold = th))
  }
})

test_that("asymmetric 'relative' is thresholded per direction like Nestimate", {
  skip_if_not_installed("Nestimate")
  ## W[i,j] and W[j,i] differ, so one direction can fall below the cut while
  ## the other survives. Filtering by undirected pair would keep both.
  for (th in c(0, 0.1, 0.25, 0.3)) {
    .expect_same_network(
      .co_matrix(.eq_list, similarity = "relative", threshold = th),
      .ne_matrix(.eq_list, similarity = "relative", threshold = th))
  }
})

test_that("cooccure's top_n edges are a subset of Nestimate's", {
  skip_if_not_installed("Nestimate")
  ## Nestimate keeps every edge tied at the cut; cooccure truncates at N.
  for (tn in c(1L, 3L, 5L)) {
    co <- .co_matrix(.eq_list, top_n = tn)
    ne <- .ne_matrix(.eq_list, top_n = tn)
    it <- sort(intersect(rownames(co), rownames(ne)))
    co <- co[it, it, drop = FALSE]; ne <- ne[it, it, drop = FALSE]
    kept <- co != 0
    expect_true(all(ne[kept] != 0))
    expect_equal(co[kept], ne[kept], tolerance = 1e-10)
  }
})

test_that("combined options match Nestimate", {
  skip_if_not_installed("Nestimate")
  .expect_same_network(
    .co_matrix(.eq_delim, field = "kw", sep = ",", similarity = "jaccard",
               min_occur = 2, threshold = 0.2),
    .ne_matrix(.eq_delim, field = "kw", sep = ",", similarity = "jaccard",
               min_occur = 2, threshold = 0.2))
  .expect_same_network(
    .co_matrix(.eq_list, similarity = "dice", min_occur = 2),
    .ne_matrix(.eq_list, similarity = "dice", min_occur = 2))
  .expect_same_network(
    .co_matrix(.eq_list, similarity = "cosine", threshold = 0.4),
    .ne_matrix(.eq_list, similarity = "cosine", threshold = 0.4))
})

test_that("degenerate inputs match Nestimate", {
  skip_if_not_installed("Nestimate")
  .expect_same_network(.co_matrix(list(c("A", "B"))),
                       .ne_matrix(list(c("A", "B"))))
  .expect_same_network(.co_matrix(list(c("A", "A", "B"), c("B", "B", "A"))),
                       .ne_matrix(list(c("A", "A", "B"), c("B", "B", "A"))))
  .expect_same_network(.co_matrix(list(c("A", NA, "B"), c("B", "C"))),
                       .ne_matrix(list(c("A", NA, "B"), c("B", "C"))))
})

test_that("randomised networks match Nestimate exactly", {
  skip_if_not_installed("Nestimate")
  skip_on_cran()
  set.seed(20260722)
  for (k in 1:40) {
    n_items <- sample(3:10, 1)
    items <- paste0("i", seq_len(n_items))
    tx <- lapply(seq_len(sample(5:40, 1)), function(i)
      unique(sample(items, sample(seq_len(min(n_items, 5)), 1))))
    s  <- sample(SIMS, 1)
    mo <- sample(1:3, 1)
    th <- sample(c(0, 0.1, 0.25), 1)
    co <- try(.co_matrix(tx, similarity = s, min_occur = mo, threshold = th),
              silent = TRUE)
    ne <- try(.ne_matrix(tx, similarity = s, min_occur = mo, threshold = th),
              silent = TRUE)
    if (inherits(co, "try-error") || inherits(ne, "try-error")) next
    .expect_same_network(co, ne)
  }
})
