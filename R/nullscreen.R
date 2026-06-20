# ---- Fixed-margin null-model edge screen ----

.co_nullscreen_trades_per_nnz <- 5L

#' Fixed-margin null-model screen for co-occurrence edges
#'
#' @noRd
#' @export
co_nullscreen <- function(x, R = 1000, alpha = 0.05, p_adjust = "BH",
                          seed = NULL) {
  .co_nullscreen_validate(x, R, alpha, p_adjust)
  R <- as.integer(R)

  if (!is.null(seed)) {
    stopifnot(is.numeric(seed), length(seed) == 1L, !is.na(seed))
    set.seed(seed)
  }

  transactions <- .co_nullscreen_transactions(x)
  items <- attr(x, "items")
  similarity <- attr(x, "similarity")
  counting <- attr(x, "counting")
  lambda <- attr(x, "lambda")
  if (is.null(lambda)) lambda <- 1.0

  n_items <- length(items)
  n_edges <- nrow(x)
  edge_i <- match(x$from, items)
  edge_j <- match(x$to, items)
  observed <- x$weight

  null_sum <- numeric(n_edges)
  exceed <- integer(n_edges)
  nnz <- sum(lengths(transactions))
  n_trades <- as.integer(max(1L, .co_nullscreen_trades_per_nnz * nnz))

  for (iter in seq_len(R)) {
    tx_r <- .co_curveball_randomize(transactions, n_items = n_items,
                                    trades = n_trades)
    fit_r <- .co_nullscreen_fit_once(tx_r, items, similarity, counting,
                                     lambda = lambda)
    W <- attr(fit_r, "matrix")
    null_w <- as.numeric(W[cbind(edge_i, edge_j)])
    null_w[is.na(null_w)] <- 0
    null_sum <- null_sum + null_w
    exceed <- exceed + as.integer(null_w >= observed)
  }

  p_value <- (1 + exceed) / (R + 1)
  p_adj <- stats::p.adjust(p_value, method = p_adjust)

  out <- data.frame(
    from = x$from,
    to = x$to,
    weight = observed,
    null_mean = null_sum / R,
    p_value = p_value,
    p_adj = p_adj,
    significant = p_adj < alpha,
    stringsAsFactors = FALSE
  )

  class(out) <- c("co_nullscreen", "cooccurrence", "data.frame")
  attr(out, "items") <- items
  attr(out, "similarity") <- similarity
  attr(out, "counting") <- counting
  attr(out, "n_transactions") <- attr(x, "n_transactions")
  attr(out, "R") <- R
  attr(out, "alpha") <- alpha
  attr(out, "p_adjust") <- p_adjust
  out
}

#' @noRd
.co_nullscreen_validate <- function(x, R, alpha, p_adjust) {
  if (!inherits(x, "cooccurrence"))
    stop("`x` must be a cooccurrence object.", call. = FALSE)
  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R < 2)
    stop("`R` must be at least 2.", call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1)
    stop("`alpha` must be a number between 0 and 1.", call. = FALSE)
  if (!is.character(p_adjust) || length(p_adjust) != 1L ||
      !p_adjust %in% stats::p.adjust.methods) {
    stop("`p_adjust` must be one of `stats::p.adjust.methods`.",
         call. = FALSE)
  }

  if (is.null(attr(x, "transactions"))) {
    stop("`x` does not carry transaction data; refit with ",
         "`keep_transactions = TRUE`.", call. = FALSE)
  }

  if (identical(attr(x, "similarity"), "relative")) {
    stop("`co_nullscreen()` does not support `similarity = \"relative\"` ",
         "in this version because it is asymmetric.", call. = FALSE)
  }

  scale_method <- attr(x, "scale")
  if (!is.null(scale_method) && !identical(scale_method, "none")) {
    stop("`co_nullscreen()` requires unscaled output; refit with ",
         "`scale = NULL` or `scale = \"none\"`.", call. = FALSE)
  }

  invisible(TRUE)
}

#' @noRd
.co_nullscreen_transactions <- function(x) {
  transactions <- attr(x, "transactions")
  items <- attr(x, "items")
  n_items <- length(items)

  if (!is.list(transactions)) {
    stop("`x` has malformed transaction data; refit with ",
         "`keep_transactions = TRUE`.", call. = FALSE)
  }

  lapply(transactions, function(tx) {
    tx <- as.integer(tx)
    if (anyNA(tx) || any(tx < 1L | tx > n_items)) {
      stop("`x` has malformed transaction indices; refit with ",
           "`keep_transactions = TRUE`.", call. = FALSE)
    }
    sort.int(unique(tx))
  })
}

#' @noRd
.co_curveball_randomize <- function(transactions, n_items, trades) {
  n_rows <- length(transactions)
  out <- lapply(transactions, function(tx) sort.int(unique(as.integer(tx))))

  if (n_rows < 2L || trades <= 0L || sum(lengths(out)) == 0L)
    return(out)

  trades <- as.integer(trades)
  for (trade in seq_len(trades)) {
    rows <- sample.int(n_rows, 2L)
    a <- rows[1L]
    b <- rows[2L]
    tx_a <- out[[a]]
    tx_b <- out[[b]]

    only_a <- setdiff(tx_a, tx_b)
    only_b <- setdiff(tx_b, tx_a)
    if (length(only_a) == 0L || length(only_b) == 0L)
      next

    common <- intersect(tx_a, tx_b)
    pool <- c(only_a, only_b)
    new_a_only <- sample(pool, length(only_a), replace = FALSE)
    new_b_only <- setdiff(pool, new_a_only)

    out[[a]] <- sort.int(c(common, new_a_only))
    out[[b]] <- sort.int(c(common, new_b_only))
  }

  out
}

#' @noRd
.co_nullscreen_fit_once <- function(transactions, items, similarity, counting,
                                    lambda = 1.0) {
  n_trans <- length(transactions)
  n_items <- length(items)
  lens <- lengths(transactions)
  row_idx <- rep.int(seq_len(n_trans), lens)
  col_idx <- as.integer(unlist(transactions, use.names = FALSE))

  B_bin <- Matrix::sparseMatrix(
    i = row_idx, j = col_idx, x = rep(1.0, length(row_idx)),
    dims = c(n_trans, n_items),
    dimnames = list(NULL, items)
  )

  C <- if (identical(counting, "attention")) {
    .co_nullscreen_attention_pairs(transactions, n_items, lambda = lambda)
  } else {
    x <- if (identical(counting, "fractional")) {
      row_weight <- ifelse(lens > 1L, 1 / (lens - 1L), 1)
      sqrt(row_weight[row_idx])
    } else {
      rep(1.0, length(row_idx))
    }
    B <- Matrix::sparseMatrix(
      i = row_idx, j = col_idx, x = x,
      dims = c(n_trans, n_items),
      dimnames = list(NULL, items)
    )
    Matrix::crossprod(B)
  }

  C_raw <- Matrix::crossprod(B_bin)
  freq <- as.numeric(Matrix::colSums(B_bin))
  names(freq) <- items

  Matrix::diag(C) <- 0
  Matrix::diag(C_raw) <- 0
  C <- Matrix::drop0(C)
  C_raw <- Matrix::drop0(C_raw)

  .co_finalize(C, C_raw, freq, n_trans, n_items, items,
               similarity = similarity, scale_method = "none",
               threshold = 0, min_occur = 1L, top_n = NULL)
}

#' @noRd
.co_nullscreen_attention_pairs <- function(transactions, n_items, lambda = 1.0) {
  triplets <- lapply(transactions, function(tx) {
    n <- length(tx)
    if (n < 2L) return(NULL)
    pos_i <- rep.int(seq_len(n - 1L), (n - 1L):1L)
    pos_j <- sequence((n - 1L):1L, 2:n)
    list(i = tx[pos_i], j = tx[pos_j],
         w = exp(-(pos_j - pos_i) / lambda))
  })
  triplets <- triplets[!vapply(triplets, is.null, logical(1))]
  if (length(triplets) == 0L)
    return(.co_sym_sparse(integer(0), integer(0), numeric(0),
                          seq_len(n_items)))

  ii <- unlist(lapply(triplets, `[[`, "i"), use.names = FALSE)
  jj <- unlist(lapply(triplets, `[[`, "j"), use.names = FALSE)
  ww <- unlist(lapply(triplets, `[[`, "w"), use.names = FALSE)
  .co_sym_sparse(ii, jj, ww, seq_len(n_items))
}

#' @noRd
#' @export
print.co_nullscreen <- function(x, n = 10L, ...) {
  nodes <- length(unique(c(x$from, x$to)))
  edges <- nrow(x)
  n_sig <- sum(x$significant, na.rm = TRUE)
  sim <- attr(x, "similarity")
  counting <- attr(x, "counting")
  n_trans <- attr(x, "n_transactions")
  R <- attr(x, "R")
  alpha <- attr(x, "alpha")
  p_adjust <- attr(x, "p_adjust")

  cat(sprintf("# co_nullscreen: %d nodes, %d edges", nodes, edges))
  if (!is.null(n_trans)) cat(sprintf(" (%d transactions)", n_trans))
  cat(sprintf(" | significant: %d", n_sig))
  if (!is.null(R)) cat(sprintf(" | R: %d", R))
  if (!is.null(alpha)) cat(sprintf(" | alpha: %.3g", alpha))
  if (!is.null(p_adjust)) cat(sprintf(" | p_adjust: %s", p_adjust))
  if (!is.null(sim) && sim != "none") cat(sprintf(" | similarity: %s", sim))
  if (!is.null(counting) && counting != "full")
    cat(sprintf(" | counting: %s", counting))
  cat("\n")

  show <- min(n, edges)
  if (show > 0L) {
    print(as.data.frame(x)[seq_len(show), ], row.names = FALSE)
    if (edges > show)
      cat(sprintf("# ... %d more edges\n", edges - show))
  } else {
    cat("# (no edges)\n")
  }
  invisible(x)
}
