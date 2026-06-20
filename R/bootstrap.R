# ---- Bootstrap confirmatory layer ----

# Session state for one-time notices.
.co_pkg_state <- new.env(parent = emptyenv())

# Emit the experimental-feature caution once per session (suppressible with
# suppressMessages() or options(cooccure.quiet = TRUE)).
.co_bootstrap_notice <- function() {
  if (isTRUE(getOption("cooccure.quiet"))) return(invisible())
  if (isTRUE(.co_pkg_state$boot_notice_shown)) return(invisible())
  .co_pkg_state$boot_notice_shown <- TRUE
  message("co_bootstrap() is experimental — interpret results with caution.")
}

#' Bootstrap edge stability for a co-occurrence network
#'
#' Resamples transactions, or user-supplied transaction blocks, from a fitted
#' \code{\link{cooccurrence}} object and recomputes edge weights on the fixed
#' post-\code{min_occur} item support. The bootstrap uses the same similarity
#' measure as the original fit, with no per-replicate thresholding or top-n
#' filtering.
#'
#' @param x A fitted \code{cooccurrence} object created with
#'   \code{keep_transactions = TRUE}.
#' @param R Integer. Number of bootstrap replicates. Default \code{1000}.
#' @param by Optional block identifier aligned to the retained transactions, or
#'   \code{"block_id"} to use the block metadata stored on \code{x}. When
#'   \code{NULL}, each transaction is resampled independently.
#' @param engine Character. \code{"classic"} uses multinomial resampling of
#'   blocks. \code{"bayes"} uses Rubin's Bayesian bootstrap with exponential
#'   block weights scaled to sum to the number of blocks.
#' @param ci Numeric. Percentile interval coverage reported in
#'   \code{ci_low}/\code{ci_high}. Default \code{0.95}.
#' @param consistency_range Numeric length-2 multiplicative band, as in
#'   \code{Nestimate::bootstrap_network()}. An edge is \code{stable} only if its
#'   resampled weight stays within \code{consistency_range * weight} (two-sided)
#'   in at least \code{consistency} of the resamples. Default \code{c(0.75,
#'   1.25)}. Two-sided, so weights that land consistently far above the estimate
#'   are flagged unstable too.
#' @param consistency Numeric in \code{(0, 1]}. Proportion of resamples that must
#'   fall within the band for an edge to be \code{stable}. Default \code{0.95}.
#' @param seed Optional integer random seed.
#'
#' @return A data frame with class
#'   \code{c("co_bootstrap", "cooccurrence", "data.frame")} and one row per
#'   original edge. Columns: \code{from}, \code{to}, observed \code{weight},
#'   \code{boot_mean}, \code{boot_se}, percentile bounds \code{ci_low} /
#'   \code{ci_high}, consistency band \code{cr_lower} / \code{cr_upper},
#'   \code{prop_within} (share of resamples inside the band), and the logical
#'   \code{stable} flag (\code{prop_within >= consistency}).
#'
#' @examples
#' tx <- list(c("A", "B"), c("A", "B", "C"), c("A", "C"), c("B", "C"))
#' fit <- cooccurrence(tx, similarity = "jaccard", keep_transactions = TRUE)
#' co_bootstrap(fit, R = 20, seed = 1)
#'
#' @export
co_bootstrap <- function(x, R = 1000, by = NULL,
                         engine = c("classic", "bayes"),
                         ci = 0.95, consistency_range = c(0.75, 1.25),
                         consistency = 0.95, seed = NULL) {
  engine <- match.arg(engine)
  .co_bootstrap_notice()
  .co_bootstrap_validate(x, R, ci)
  stopifnot(is.numeric(consistency_range), length(consistency_range) == 2L,
            all(consistency_range > 0),
            is.numeric(consistency), length(consistency) == 1L,
            consistency > 0, consistency <= 1)
  R <- as.integer(R)

  if (!is.null(seed)) {
    stopifnot(is.numeric(seed), length(seed) == 1L, !is.na(seed))
    set.seed(seed)
  }

  transactions <- attr(x, "transactions")
  items <- attr(x, "items")
  similarity <- attr(x, "similarity")
  counting <- attr(x, "counting")
  if (is.null(counting)) counting <- "full"
  n_trans <- length(transactions)
  n_items <- length(items)

  block_id <- .co_bootstrap_block_id(x, by, n_trans)
  block_factor <- factor(block_id, levels = unique(block_id))
  block_index <- as.integer(block_factor)
  n_blocks <- nlevels(block_factor)

  stats <- .co_precompute_bootstrap_stats(transactions, n_items, counting)
  edge_i <- match(x$from, items)
  edge_j <- match(x$to, items)
  n_edges <- nrow(x)

  boot <- vapply(seq_len(R), function(iter) {
    block_weights <- if (engine == "classic") {
      tabulate(sample.int(n_blocks, n_blocks, replace = TRUE),
               nbins = n_blocks)
    } else {
      w <- stats::rexp(n_blocks)
      w / sum(w) * n_blocks
    }

    tx_weights <- as.numeric(block_weights[block_index])
    fit_r <- .co_bootstrap_fit_once(stats, tx_weights, items, similarity)
    W <- attr(fit_r, "matrix")
    as.numeric(W[cbind(edge_i, edge_j)])
  }, numeric(n_edges))

  alpha <- 1 - ci
  ci_low <- apply(boot, 1L, stats::quantile, probs = alpha / 2,
                  names = FALSE, na.rm = TRUE)
  ci_high <- apply(boot, 1L, stats::quantile, probs = 1 - alpha / 2,
                   names = FALSE, na.rm = TRUE)

  # Consistency-range stability (matching Nestimate::bootstrap_network): an edge
  # is `stable` when its resampled weight stays WITHIN a two-sided multiplicative
  # band [cr_lower, cr_upper] of the observed weight in at least `consistency` of
  # the resamples. Two-sided, so a weight that consistently lands far ABOVE the
  # estimate is also flagged unstable, not just one that shrinks. `prop_within`
  # is the share of resamples inside the band.
  cr_lower <- pmin(consistency_range[1L], consistency_range[2L]) * x$weight
  cr_upper <- pmax(consistency_range[1L], consistency_range[2L]) * x$weight
  prop_within <- rowMeans(boot >= cr_lower & boot <= cr_upper, na.rm = TRUE)

  out <- data.frame(
    from = x$from,
    to = x$to,
    weight = x$weight,
    boot_mean = rowMeans(boot, na.rm = TRUE),
    boot_se = apply(boot, 1L, stats::sd, na.rm = TRUE),
    ci_low = ci_low,
    ci_high = ci_high,
    cr_lower = cr_lower,
    cr_upper = cr_upper,
    prop_within = prop_within,
    stable = prop_within >= consistency,
    stringsAsFactors = FALSE
  )

  class(out) <- c("co_bootstrap", "cooccurrence", "data.frame")
  for (a in c("similarity", "scale", "threshold", "min_occur",
              "n_transactions", "n_items", "items", "counting",
              "transaction_id", "block_id")) {
    attr(out, a) <- attr(x, a)
  }
  attr(out, "R") <- R
  attr(out, "engine") <- engine
  attr(out, "ci") <- ci
  attr(out, "consistency_range") <- consistency_range
  attr(out, "consistency") <- consistency
  out
}

#' @noRd
.co_bootstrap_validate <- function(x, R, ci) {
  if (!inherits(x, "cooccurrence"))
    stop("`x` must be a cooccurrence object.", call. = FALSE)
  if (!is.numeric(R) || length(R) != 1L || is.na(R) || R < 2)
    stop("`R` must be at least 2.", call. = FALSE)
  if (!is.numeric(ci) || length(ci) != 1L || is.na(ci) ||
      ci <= 0 || ci >= 1)
    stop("`ci` must be a number between 0 and 1.", call. = FALSE)

  if (is.null(attr(x, "transactions"))) {
    stop("`x` does not carry transaction data; refit with ",
         "`keep_transactions = TRUE`.", call. = FALSE)
  }

  counting <- attr(x, "counting")
  if (!is.null(counting) && !counting %in% c("full", "fractional")) {
    stop("`co_bootstrap()` supports `counting = \"full\"` or ",
         "\"fractional\"; `", counting, "` (position-dependent) is not ",
         "resamplable as a per-transaction sufficient statistic.",
         call. = FALSE)
  }

  scale_method <- attr(x, "scale")
  if (!is.null(scale_method) && !identical(scale_method, "none")) {
    stop("`co_bootstrap()` requires unscaled output; refit with ",
         "`scale = NULL` or `scale = \"none\"`.", call. = FALSE)
  }

  if (identical(attr(x, "similarity"), "relative")) {
    stop("`co_bootstrap()` does not support `similarity = \"relative\"` ",
         "in this version because it is asymmetric.", call. = FALSE)
  }

  invisible(TRUE)
}

#' @noRd
.co_bootstrap_block_id <- function(x, by, n_trans) {
  stored <- attr(x, "block_id")

  if (is.null(by)) {
    if (!is.null(stored)) {
      warning("`x` carries `block_id`, but `by = NULL`; bootstrapping ",
              "transactions independently.", call. = FALSE)
    }
    return(seq_len(n_trans))
  }

  if (is.character(by) && length(by) == 1L && by %in% c("block_id", "block")) {
    if (is.null(stored)) {
      stop("`by = \"block_id\"` was requested, but `x` has no stored ",
           "`block_id`.", call. = FALSE)
    }
    by <- stored
  }

  if (length(by) != n_trans) {
    stop("`by` must cover all retained transactions in `x`.",
         call. = FALSE)
  }
  if (anyNA(by))
    stop("`by` must not contain missing values.", call. = FALSE)
  as.character(by)
}

#' @noRd
.co_precompute_bootstrap_stats <- function(transactions, n_items,
                                           counting = "full") {
  item_lengths <- lengths(transactions)
  item_index <- as.integer(unlist(transactions, use.names = FALSE))
  item_tx <- rep.int(seq_along(transactions), item_lengths)

  pairs <- lapply(transactions, function(tx) {
    n <- length(tx)
    if (n < 2L) {
      return(list(i = integer(0), j = integer(0)))
    }
    pos_i <- rep.int(seq_len(n - 1L), (n - 1L):1L)
    pos_j <- sequence((n - 1L):1L, 2:n)
    i <- tx[pos_i]
    j <- tx[pos_j]
    swap <- i > j
    if (any(swap)) {
      tmp <- i[swap]; i[swap] <- j[swap]; j[swap] <- tmp
    }
    list(i = as.integer(i), j = as.integer(j))
  })

  pair_lengths <- vapply(pairs, function(p) length(p$i), integer(1))
  pair_tx <- rep.int(seq_along(transactions), pair_lengths)

  # Per-pair counting weight, matching cooccurrence(): "full" adds 1 per pair;
  # "fractional" adds 1/(n-1) per pair (n = transaction size), so a co-author on
  # a large team contributes proportionally less to each pairwise tie. Item
  # frequencies (margins) stay binary in both modes, as in the estimator.
  pair_weight <- if (identical(counting, "fractional")) {
    1 / (item_lengths[pair_tx] - 1)
  } else {
    rep.int(1, length(pair_tx))
  }

  list(
    n_items = n_items,
    item_index = item_index,
    item_tx = item_tx,
    pair_i = as.integer(unlist(lapply(pairs, `[[`, "i"), use.names = FALSE)),
    pair_j = as.integer(unlist(lapply(pairs, `[[`, "j"), use.names = FALSE)),
    pair_tx = pair_tx,
    pair_weight = pair_weight
  )
}

#' @noRd
.co_bootstrap_fit_once <- function(stats, tx_weights, items, similarity) {
  n_items <- stats$n_items

  freq <- numeric(n_items)
  if (length(stats$item_index) > 0L) {
    freq_sum <- rowsum(tx_weights[stats$item_tx], stats$item_index,
                       reorder = FALSE)
    freq[as.integer(rownames(freq_sum))] <- as.numeric(freq_sum[, 1L])
  }
  names(freq) <- items

  if (length(stats$pair_i) == 0L) {
    C <- .co_sym_sparse(integer(0), integer(0), numeric(0), items)
  } else {
    pair_weights <- tx_weights[stats$pair_tx] * stats$pair_weight
    keep <- pair_weights != 0
    C <- .co_sym_sparse(stats$pair_i[keep], stats$pair_j[keep],
                        pair_weights[keep], items)
  }
  Matrix::diag(C) <- 0
  C <- Matrix::drop0(C)

  .co_finalize(C, C, freq, length(tx_weights), n_items, items,
               similarity = similarity, scale_method = "none",
               threshold = 0, min_occur = 1L, top_n = NULL)
}
