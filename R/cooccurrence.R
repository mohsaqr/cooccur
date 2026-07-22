# ---- Co-occurrence Network Construction ----

# TraMineR/tna void markers
.void_markers <- c("%", "*", "", "NA", "NaN")

#' Build a co-occurrence network
#'
#' Constructs an undirected co-occurrence network from various input formats
#' and returns a tidy edge data frame. Argument names follow the citenets
#' convention.
#'
#' @param data Input data. Accepts:
#'   \itemize{
#'     \item A \code{data.frame} with a delimited column (\code{field} + \code{sep}).
#'     \item A \code{data.frame} in long/bipartite format (\code{field} + \code{by}).
#'     \item A binary (0/1 or \code{TRUE}/\code{FALSE}) \code{data.frame} or
#'       \code{matrix} (auto-detected).
#'     \item A one-hot or count table with \code{vars} naming the indicator
#'       columns; other columns (ids, metadata) are ignored.
#'     \item A wide sequence \code{data.frame} or \code{matrix} (non-binary).
#'     \item A \code{list} of character vectors (each element is a transaction).
#'     \item A \code{nestimate_data} object from
#'       \code{Nestimate::prepare()}; its \code{sequence_data} is used, so
#'       event logs can be sessionized there and networked here.
#'   }
#' @param field Character. The entity column --- determines what the nodes are.
#'   For delimited format, a single column split by \code{sep}. For
#'   long/bipartite, the item column. For multi-column delimited, a vector
#'   of column names pooled per row. Use \code{field = "all"} for wide
#'   sequence data (e.g. TraMineR / tna format) where every column is a
#'   time point and cell values are the items.
#' @param by Character or \code{NULL}. Grouping column for long/bipartite
#'   format. Each unique value defines one transaction.
#' @param weight_by Character or \code{NULL}. Column name containing a numeric
#'   association strength for each entity-transaction pair. Only accepted for
#'   long format (\code{field} + \code{by}). When supplied, each entity
#'   contributes its weight rather than 1, so
#'   \eqn{C_{ij} = \sum_d w_{id} \cdot w_{jd}}.
#'   Typical use: topic-document probability matrices from LDA or similar
#'   models.
#' @param sep Character or \code{NULL}. Separator for splitting delimited
#'   fields.
#' @param action Character or \code{NULL}. Column holding the event/state
#'   for raw event-log input. When supplied, the log is sessionized into
#'   ordered sequences first and each session becomes one transaction, so
#'   \code{window} and \code{counting = "attention"} apply. Requires the
#'   \pkg{Nestimate} package, which performs the sessionization.
#' @param actor Character vector or \code{NULL}. Column(s) identifying who
#'   performed the action. When \code{NULL}, all rows are treated as one
#'   actor. Only used with \code{action}.
#' @param time Character or \code{NULL}. Timestamp column. Accepts ISO8601,
#'   Unix time, and common date/time formats. When \code{NULL}, row order
#'   defines the sequence. Only used with \code{action}.
#' @param session Character vector or \code{NULL}. Column(s) giving an
#'   explicit session grouping. Combined with \code{time}, sessions are
#'   further split on time gaps. Only used with \code{action}.
#' @param order Character or \code{NULL}. Column used to break ties when
#'   timestamps are identical. Only used with \code{action}.
#' @param time_threshold Numeric. Maximum gap in seconds between consecutive
#'   events before a new session starts. Use \code{Inf} to keep each actor
#'   as a single sequence regardless of gaps. Default 900 (15 minutes).
#'   Only used with \code{action} and \code{time}.
#' @param vars Column specification or \code{NULL}. The indicator (one-hot)
#'   columns, one column per item. Resolved like \code{select} in
#'   \code{\link[base]{subset}}, so all of these work: a bare range
#'   (\code{vars = A:D}), bare names (\code{vars = c(A, B, C)}), positions
#'   (\code{vars = 2:5}), negative selection (\code{vars = -c(id, note)}),
#'   a logical mask, or a character vector. Every other column --- ids,
#'   timestamps, metadata --- is ignored, so a one-hot table that also
#'   carries identifier columns needs no pre-processing. Cell values are
#'   read as presence: any non-zero, non-\code{NA} value marks the item
#'   present, so count tables such as document-term matrices work as well
#'   as \code{0}/\code{1} and \code{TRUE}/\code{FALSE}. \code{NA} counts as
#'   absent. Cannot be combined with \code{field}, \code{by}, or
#'   \code{sep}. A purely binary table with no extra columns is still
#'   auto-detected without \code{vars}.
#' @param split_by Character or \code{NULL}. Column name to split the data
#'   by before computing co-occurrence. A separate network is computed per
#'   group and the results are combined into a single data frame with an
#'   additional \code{group} column. Only works with data.frame inputs.
#' @param group Character or \code{NULL}. Alias for \code{split_by}.
#' @param similarity Character. Similarity measure:
#'   \describe{
#'     \item{\code{"none"}}{Raw co-occurrence counts.}
#'     \item{\code{"jaccard"}}{\eqn{C_{ij} / (f_i + f_j - C_{ij})}.}
#'     \item{\code{"cosine"}}{Salton's cosine:
#'       \eqn{C_{ij} / \sqrt{f_i \cdot f_j}}.}
#'     \item{\code{"inclusion"}}{Simpson coefficient:
#'       \eqn{C_{ij} / \min(f_i, f_j)}.}
#'     \item{\code{"association"}}{Association strength:
#'       \eqn{C_{ij} / (f_i \cdot f_j)}
#'       (van Eck & Waltman, 2009).}
#'     \item{\code{"dice"}}{\eqn{2 C_{ij} / (f_i + f_j)}.}
#'     \item{\code{"equivalence"}}{Salton's cosine squared:
#'       \eqn{C_{ij}^2 / (f_i \cdot f_j)}.}
#'     \item{\code{"relative"}}{Row-normalized: each row sums to 1.}
#'   }
#' @param scale Character or \code{NULL}. Optional scaling applied to weights
#'   after similarity normalization:
#'   \describe{
#'     \item{\code{NULL} or \code{"none"}}{No scaling.}
#'     \item{\code{"minmax"}}{Min-max to \eqn{[0, 1]}.}
#'     \item{\code{"log"}}{Natural log: \eqn{\log(1 + w)}.}
#'     \item{\code{"log10"}}{Log base 10: \eqn{\log_{10}(1 + w)}.}
#'     \item{\code{"binary"}}{Binary: 1 if \eqn{w > 0}, else 0.}
#'     \item{\code{"zscore"}}{Z-score standardization.}
#'     \item{\code{"sqrt"}}{Square root.}
#'     \item{\code{"proportion"}}{Divide by sum of all weights.}
#'   }
#' @param counting Character. Counting method:
#'   \describe{
#'     \item{\code{"full"}}{Each co-occurring pair adds 1 regardless of
#'       transaction size. Default.}
#'     \item{\code{"fractional"}}{Each pair adds \eqn{1 / (n_i - 1)}
#'       where \eqn{n_i} is the number of items in transaction \eqn{i}.
#'       Transactions with many items contribute less per pair
#'       (Perianes-Rodriguez et al., 2016).}
#'     \item{\code{"attention"}}{Each pair within a transaction
#'       contributes \eqn{\exp(-|pos_i - pos_j| / \lambda)} — closer
#'       positions give a stronger edge, distant pairs decay. Requires
#'       ordered transactions (list / wide / delimited / windowed
#'       input). The decay rate \eqn{\lambda} is controlled by the
#'       \code{lambda} argument.}
#'   }
#' @param lambda Numeric. Decay rate for \code{counting = "attention"}.
#'   Higher \code{lambda} → slower decay → distant pairs still
#'   contribute. Default \code{1.0}, matching the \pkg{tna} package.
#'   Ignored for other counting methods.
#' @param threshold Numeric. Minimum edge weight to retain, applied after
#'   similarity and scaling. The default \code{0} means no filtering rather
#'   than "drop negative weights", so centering scalings such as
#'   \code{scale = "zscore"} keep their negative half. Pass a positive
#'   value to filter.
#' @param min_occur Integer. Minimum entity frequency. Entities appearing in
#'   fewer than \code{min_occur} transactions are dropped. Default 1.
#' @param top_n Integer or \code{NULL}. Keep only the top \code{top_n} edges
#'   by weight. When \code{split_by} is used, applied per group.
#'   Default \code{NULL} (all edges).
#' @param aggregate_by Character or \code{NULL}. Column name to group
#'   the data by before computing co-occurrence. For each unique value,
#'   the per-group network is computed (with the chosen
#'   \code{similarity}, \code{counting}, \code{scale}, \code{window});
#'   the per-group edge weights are then combined across groups via
#'   \code{aggregate} into ONE final network. Differs from
#'   \code{split_by}, which keeps groups separate. Cannot be combined
#'   with \code{split_by}. Only applies to data frame inputs.
#' @param aggregate Character. How to combine edge weights across
#'   groups when \code{aggregate_by} is used: \code{"sum"} (default),
#'   \code{"mean"}, \code{"min"}, or \code{"max"}. The \code{count}
#'   column is always summed. \code{threshold} and \code{top_n} are
#'   applied AFTER aggregation.
#' @param window Integer or \code{NULL}. Sliding-window size for
#'   categorical time-series / ordered-sequence input. When set to an
#'   integer \eqn{w \ge 2}, every window of \code{w} consecutive
#'   positions in a sequence becomes a mini-transaction; states inside
#'   the same window co-occur. Sequences shorter than \code{w}
#'   contribute no transactions. Only applies to ordered formats:
#'   wide (\code{field = "all"}) and \code{list}. Default \code{NULL}
#'   (whole sequence treated as one transaction --- bag of states).
#' @param output Character. Column naming convention for the output:
#'   \describe{
#'     \item{\code{"default"}}{\code{from}, \code{to}, \code{weight}, \code{count}.}
#'     \item{\code{"gephi"}}{\code{Source}, \code{Target}, \code{Weight},
#'       \code{Type} (= \code{"Undirected"}). Ready for Gephi import.}
#'     \item{\code{"igraph"}}{Returns an \code{igraph} graph object directly.}
#'     \item{\code{"cograph"}}{Returns a \code{cograph_network} object directly.}
#'     \item{\code{"matrix"}}{Returns the square co-occurrence matrix.}
#'   }
#' @param ... Currently unused.
#'
#' @return Depends on \code{output}:
#'   \itemize{
#'     \item \code{"default"}: A \code{cooccurrence} data frame with columns
#'       \code{from}, \code{to}, \code{weight}, \code{count} (and \code{group}
#'       when \code{split_by} is used).
#'     \item \code{"gephi"}: A data frame with columns \code{Source},
#'       \code{Target}, \code{Weight}, \code{Type}, \code{Count}. Ready for
#'       Gephi CSV import.
#'     \item \code{"igraph"}: An \code{igraph} graph object.
#'     \item \code{"cograph"}: A \code{cograph_network} object.
#'     \item \code{"matrix"}: A square numeric co-occurrence matrix.
#'   }
#'   For the data frame outputs, rows are sorted by weight descending and
#'   attributes store the full matrix, item frequencies, and parameters.
#'
#' @references
#' van Eck, N. J., & Waltman, L. (2009). How to normalize co-occurrence
#' data? An analysis of some well-known similarity measures. \emph{Journal of
#' the American Society for Information Science and Technology}, 60(8),
#' 1635--1651.
#'
#' @examples
#' # Delimited keywords
#' df <- data.frame(
#'   id = 1:4,
#'   keywords = c("network; graph", "graph; matrix; network",
#'                "matrix; algebra", "network; algebra; graph")
#' )
#' cooccurrence(df, field = "keywords", sep = ";")
#'
#' # Split by a grouping variable
#' df$year <- c(2020, 2020, 2021, 2021)
#' cooccurrence(df, field = "keywords", sep = ";", split_by = "year")
#' cooccurrence(df, field = "keywords", sep = ";", group = "year")
#'
#' # List of transactions with Jaccard similarity
#' cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")),
#'              similarity = "jaccard")
#'
#' # Short alias
#' co(df, field = "keywords", sep = ";", similarity = "cosine")
#'
#' # Windowed co-occurrence on a categorical time series. With
#' # window = 2 only adjacent states co-occur; window = 3 also pairs
#' # states two positions apart, etc.
#' seqs <- list(
#'   c("focus", "focus", "distract", "focus", "confused"),
#'   c("focus", "distract", "distract", "focus")
#' )
#' cooccurrence(seqs, window = 2)
#'
#' # Weighted long format (e.g. LDA topic-document probabilities)
#' theta <- data.frame(
#'   doc   = c("d1","d1","d1","d2","d2","d3","d3"),
#'   topic = c("T1","T2","T3","T1","T3","T2","T3"),
#'   prob  = c(0.6, 0.3, 0.1, 0.4, 0.6, 0.5, 0.5)
#' )
#' cooccurrence(theta, field = "topic", by = "doc", weight_by = "prob")
#'
#' # One-hot / indicator table: name the indicator columns, ignore the rest
#' onehot <- data.frame(
#'   doc = c("d1", "d2", "d3"),
#'   A = c(1, 0, 1), B = c(1, 1, 0), C = c(0, 1, 1)
#' )
#' cooccurrence(onehot, vars = c("A", "B", "C"))
#'
#' # Raw event log: sessionized on a 15-minute gap, then windowed
#' \donttest{
#' if (requireNamespace("Nestimate", quietly = TRUE)) {
#'   events <- data.frame(
#'     student = rep(c("s1", "s2"), each = 3),
#'     code    = c("read", "write", "read", "test", "write", "read"),
#'     stamp   = as.POSIXct("2026-01-01 09:00:00") + c(0, 60, 120, 0, 30, 90)
#'   )
#'   cooccurrence(events, actor = "student", action = "code", time = "stamp")
#' }
#' }
#'
#' @export
## The first thirteen arguments reproduce the CRAN 0.1.1 signature exactly,
## so positional calls written against 0.1.1 keep their meaning. Every
## argument added since is keyword-only in practice, appended after them.
cooccurrence <- function(data, field = NULL, by = NULL, sep = NULL,
                         weight_by = NULL,
                         split_by = NULL,
                         similarity = c("none", "jaccard", "cosine",
                                        "inclusion", "association",
                                        "dice", "equivalence", "relative"),
                         counting = c("full", "fractional", "attention"),
                         scale = NULL,
                         threshold = 0, min_occur = 1L,
                         top_n = NULL,
                         output = c("default", "gephi", "igraph",
                                    "cograph", "matrix"),
                         vars = NULL,
                         actor = NULL, action = NULL, time = NULL,
                         session = NULL, order = NULL, time_threshold = 900,
                         group = NULL,
                         aggregate_by = NULL,
                         aggregate = c("sum", "mean", "min", "max"),
                         window = NULL,
                         lambda = 1.0, ...) {
  ## An unknown argument used to be swallowed silently by `...`, so a typo
  ## such as `simliarity = "jaccard"` returned raw counts with no warning.
  .co_check_dots(...)
  similarity <- match.arg(similarity)
  counting <- match.arg(counting)
  output <- match.arg(output)
  aggregate <- match.arg(aggregate)
  threshold <- as.numeric(threshold)
  min_occur <- as.integer(min_occur)
  stopifnot(threshold >= 0, min_occur >= 1L)
  if (!is.null(group)) {
    if (!is.null(split_by) && !identical(split_by, group)) {
      stop("`group` is an alias for `split_by`; supply only one value.",
           call. = FALSE)
    }
    split_by <- group
  }
  if (!is.null(window)) {
    stopifnot(is.numeric(window), length(window) == 1L, window >= 2L)
    window <- as.integer(window)
  }
  if (!is.null(aggregate_by) && !is.null(split_by))
    stop("`aggregate_by` and `split_by` cannot be combined.", call. = FALSE)
  if (!is.null(weight_by) && !is.null(window))
    stop("`window` is not compatible with `weight_by`.", call. = FALSE)
  stopifnot(is.numeric(lambda), length(lambda) == 1L, lambda > 0)
  ## `vars` is resolved like base R's subset(select = ): bare ranges
  ## (a:c), positions (1:8), negatives (-id), and character vectors all work.
  vars <- .co_resolve_vars(substitute(vars), data, parent.frame())
  if (!is.null(vars)) {
    if (length(vars) < 2L)
      stop("`vars` must name at least two indicator columns.", call. = FALSE)
    if (!is.null(field) || !is.null(by) || !is.null(sep))
      stop("`vars` names indicator columns directly and cannot be combined ",
           "with `field`, `by`, or `sep`.", call. = FALSE)
  }

  ## Raw event-log input: sessionize first, then continue down the ordinary
  ## wide-sequence path. Sessionization (timestamp parsing, gap splitting)
  ## is delegated to Nestimate::prepare() rather than reimplemented here.
  if (!is.null(action)) {
    data <- .co_prepare_events(data, actor = actor, action = action,
                               time = time, session = session, order = order,
                               time_threshold = time_threshold)
    field <- "all"
  } else if (!is.null(actor) || !is.null(time) || !is.null(session)) {
    stop("`actor`, `time`, and `session` describe raw event data and ",
         "require `action` naming the event column.", call. = FALSE)
  }

  if (is.null(scale) || identical(scale, "none")) {
    scale_method <- "none"
  } else {
    scale_method <- match.arg(scale, c("none", "minmax", "log", "log10",
                                       "binary", "zscore", "sqrt",
                                       "proportion"))
  }

  # ---- split_by: compute per group, combine ----
  if (!is.null(split_by)) {
    stopifnot(is.data.frame(data), split_by %in% names(data))
    groups <- split(data, data[[split_by]])
    parts <- lapply(names(groups), function(g) {
      sub <- groups[[g]]
      # Drop the split_by column so it doesn't interfere with format detection
      sub[[split_by]] <- NULL
      ## A group that genuinely yields no edges is expected and dropped
      ## quietly; a group that FAILS is not, so its error is surfaced as a
      ## warning naming the group rather than vanishing into a NULL.
      edges <- tryCatch(
        .co_core(sub, field = field, by = by, sep = sep, vars = vars,
                 weight_by = weight_by,
                 similarity = similarity, counting = counting,
                 scale_method = scale_method,
                 threshold = threshold, min_occur = min_occur,
                 top_n = top_n, window = window, lambda = lambda),
        error = function(e) {
          if (!grepl("No non-empty transactions|No items remain",
                     conditionMessage(e))) {
            warning("Group '", g, "' produced no network: ",
                    conditionMessage(e), call. = FALSE)
          }
          NULL
        }
      )
      if (is.null(edges) || nrow(edges) == 0L) return(NULL)
      edges$group <- g
      edges
    })
    kept <- !vapply(parts, is.null, logical(1))
    ## Per-group node support, captured before the parts are stacked. A node
    ## that is isolated within its group appears in no edge, so it cannot be
    ## recovered from the combined frame afterwards.
    group_items <- lapply(parts[kept], function(p) attr(p, "items"))
    names(group_items) <- names(groups)[kept]
    group_tx <- vapply(parts[kept],
                       function(p) as.integer(attr(p, "n_transactions") %||% NA),
                       integer(1))
    names(group_tx) <- names(groups)[kept]

    parts <- parts[kept]
    if (length(parts) == 0L)
      stop("No groups produced any edges.", call. = FALSE)
    edges <- do.call(rbind, parts)
    rownames(edges) <- NULL

    ## rbind() carries the FIRST part's attributes onto the stacked frame,
    ## so the combined object would otherwise advertise group 1's matrix,
    ## items, and frequencies as if they described the whole result.
    for (a in c("matrix", "raw_matrix", "items", "frequencies",
                "n_transactions", "n_items", "counting", "lambda"))
      attr(edges, a) <- NULL

    class(edges) <- c("cooccurrence", "data.frame")
    attr(edges, "similarity") <- similarity
    attr(edges, "scale") <- scale_method
    attr(edges, "threshold") <- threshold
    attr(edges, "min_occur") <- min_occur
    attr(edges, "counting") <- counting
    attr(edges, "split_by") <- split_by
    ## Only groups that actually contributed edges, not every input level.
    attr(edges, "groups") <- names(groups)[kept]
    attr(edges, "group_items") <- group_items
    attr(edges, "group_transactions") <- group_tx
    return(.co_format_output(edges, output))
  }

  # ---- aggregate_by: per-group compute, then combine into one network ----
  if (!is.null(aggregate_by)) {
    stopifnot(is.data.frame(data), aggregate_by %in% names(data))
    groups <- split(data, data[[aggregate_by]])
    parts <- lapply(names(groups), function(g) {
      sub <- groups[[g]]
      sub[[aggregate_by]] <- NULL
      tryCatch(
        ## Defer threshold/top_n to after aggregation; per-group
        ## filtering would distort the global combine.
        .co_core(sub, field = field, by = by, sep = sep, vars = vars,
                 weight_by = weight_by,
                 similarity = similarity, counting = counting,
                 scale_method = scale_method,
                 threshold = 0, min_occur = min_occur,
                 top_n = NULL, window = window, lambda = lambda),
        error = function(e) NULL
      )
    })
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (length(parts) == 0L)
      stop("No groups produced any edges.", call. = FALSE)

    edges <- .co_aggregate_parts(parts, aggregate, threshold, top_n)

    items <- sort(unique(c(edges$from, edges$to)))
    n_items <- length(items)
    M <- .co_edges_to_sparse(edges, items)

    n_trans <- sum(vapply(parts, function(p) {
      v <- attr(p, "n_transactions")
      if (is.null(v) || is.na(v)) 0L else as.integer(v)
    }, integer(1)))

    class(edges) <- c("cooccurrence", "data.frame")
    attr(edges, "matrix")         <- M
    attr(edges, "raw_matrix")     <- M
    attr(edges, "items")          <- items
    attr(edges, "frequencies")    <- NULL
    attr(edges, "similarity")     <- similarity
    attr(edges, "scale")          <- scale_method
    attr(edges, "threshold")      <- threshold
    attr(edges, "min_occur")      <- min_occur
    attr(edges, "n_transactions") <- n_trans
    attr(edges, "n_items")        <- n_items
    attr(edges, "aggregate_by")   <- aggregate_by
    attr(edges, "aggregate")      <- aggregate
    attr(edges, "groups")         <- names(groups)
    return(.co_format_output(edges, output))
  }

  # ---- Single-group path ----
  result <- .co_core(data, field = field, by = by, sep = sep, vars = vars,
                     weight_by = weight_by,
                     similarity = similarity, counting = counting,
                     scale_method = scale_method,
                     threshold = threshold, min_occur = min_occur,
                     top_n = top_n, window = window, lambda = lambda)

  .co_format_output(result, output)
}


#' @rdname cooccurrence
#' @export
co <- cooccurrence


# ---- Core pipeline (used by both single and split_by paths) ----

#' @noRd
.co_core <- function(data, field, by, sep, vars = NULL, weight_by = NULL, similarity,
                     counting, scale_method, threshold, min_occur, top_n,
                     window = NULL, lambda = 1.0) {
  # Parse input
  fmt <- .co_detect_format(data, field, by, sep, vars)

  # Weighted path — long format only
  if (!is.null(weight_by)) {
    if (fmt != "long")
      stop("`weight_by` is only supported for long format (field + by).",
           call. = FALSE)
    return(.co_core_weighted(data, field, by, weight_by, similarity,
                             scale_method, threshold, min_occur, top_n))
  }

  transactions <- .co_parse_transactions(data, fmt, field, by, sep, window,
                                         vars)

  # Drop empty transactions
  keep_tx <- vapply(transactions, length, integer(1)) > 0L
  transactions <- transactions[keep_tx]
  if (length(transactions) == 0L)
    stop("No non-empty transactions found in the input data.", call. = FALSE)

  # min_occur filter
  if (min_occur > 1L) {
    freq_table <- table(unlist(transactions))
    keep <- names(freq_table[freq_table >= min_occur])
    transactions <- lapply(transactions, function(t) t[t %in% keep])
    keep_tx <- vapply(transactions, length, integer(1)) > 0L
    transactions <- transactions[keep_tx]
    if (length(transactions) == 0L)
      stop("No transactions remain after min_occur filtering.", call. = FALSE)
  }

  # Build sparse bipartite matrix (works x items) with counting weights baked in
  sp <- .co_build_sparse(transactions, counting, lambda = lambda)
  n_trans <- sp$n
  n_items <- sp$k
  items <- sp$items

  # Counting-weighted co-occurrence (sparse k x k). For "attention" the
  # pair contribution depends on the positional GAP, not on a per-item
  # weight, so we cannot factor it through B + crossprod — `.co_build_sparse`
  # builds C directly in that case.
  C <- if (counting == "attention") sp$C else Matrix::crossprod(sp$B)
  # Raw binary co-occurrence (sparse k x k) for the count column and frequencies
  C_raw <- Matrix::crossprod(sp$B_bin)
  freq <- as.numeric(Matrix::colSums(sp$B_bin))
  names(freq) <- items

  # Zero diagonals (self co-occurrence is not an edge)
  Matrix::diag(C) <- 0
  Matrix::diag(C_raw) <- 0
  C <- Matrix::drop0(C)
  C_raw <- Matrix::drop0(C_raw)

  result <- .co_finalize(C, C_raw, freq, n_trans, n_items, items,
                         similarity, scale_method, threshold, min_occur,
                         top_n)
  attr(result, "counting") <- counting
  attr(result, "lambda") <- lambda

  result
}


# ---- Shared finalization: normalize -> scale -> threshold -> edges -> stamp ----

#' Finalize edges from sparse co-occurrence matrices.
#'
#' Operates on triplets throughout — never materializes a dense k x k matrix.
#' For symmetric similarities, the attribute `matrix` is stored as a symmetric
#' sparse `dsCMatrix`; for `similarity = "relative"` it is a general
#' `dgCMatrix` holding both triangles.
#'
#' @param C Sparse counting-weighted co-occurrence matrix (k x k, diag = 0).
#' @param C_raw Sparse raw (binary) co-occurrence matrix (k x k, diag = 0).
#' @param freq Named numeric vector of item frequencies.
#' @param items Character vector of item names (k).
#' @noRd
.co_finalize <- function(C, C_raw, freq, n_trans, n_items, items,
                         similarity, scale_method, threshold, min_occur, top_n) {
  ## Upper-triangle triplets of C (counting-weighted) — these carry x values.
  C_upper_T <- methods::as(Matrix::triu(C, k = 1L), "TsparseMatrix")
  i <- C_upper_T@i + 1L
  j <- C_upper_T@j + 1L
  c_vals <- C_upper_T@x

  if (length(i) == 0L) {
    edges <- data.frame(from = character(0), to = character(0),
                        weight = numeric(0), count = integer(0),
                        stringsAsFactors = FALSE)
    W_sparse <- Matrix::sparseMatrix(
      i = integer(0), j = integer(0), x = numeric(0),
      dims = c(n_items, n_items), symmetric = TRUE,
      dimnames = list(items, items)
    )
  } else {
    ## Raw counts at the same positions. Non-zero pattern of C and C_raw is
    ## identical (both are crossprods of binaries with the same support), so
    ## [cbind(i, j)] indexing returns counts aligned with c_vals.
    raw_vals <- as.integer(C_raw[cbind(i, j)])

    ## Similarity normalization on triplets.
    if (similarity == "none") {
      W_x <- c_vals
      W_sparse <- Matrix::sparseMatrix(
        i = i, j = j, x = W_x,
        dims = c(n_items, n_items), symmetric = TRUE,
        dimnames = list(items, items)
      )
    } else if (similarity == "relative") {
      ## Asymmetric: W[i,j] = C[i,j] / rowSums(C)[i]. Needs both triangles.
      ## C is a symmetric dsCMatrix whose TsparseMatrix form stores only the
      ## upper triangle, so we mirror (i, j, c_vals) to build the full matrix
      ## explicitly rather than relying on the symmetric packing.
      rs <- as.numeric(Matrix::rowSums(C))
      rs[rs == 0] <- 1
      W_x <- c_vals / rs[i]

      ii <- c(i, j)
      jj <- c(j, i)
      xx <- c(c_vals, c_vals)
      W_full_x <- xx / rs[ii]
    } else {
      denom <- switch(similarity,
        jaccard     = freq[i] + freq[j] - c_vals,
        cosine      = sqrt(freq[i] * freq[j]),
        inclusion   = pmin(freq[i], freq[j]),
        association = freq[i] * freq[j],
        dice        = freq[i] + freq[j],
        equivalence = freq[i] * freq[j]
      )
      denom[denom == 0] <- 1
      numer <- switch(similarity,
        dice        = 2 * c_vals,
        equivalence = c_vals^2,
        c_vals
      )
      W_x <- as.numeric(numer / denom)
      W_sparse <- Matrix::sparseMatrix(
        i = i, j = j, x = W_x,
        dims = c(n_items, n_items), symmetric = TRUE,
        dimnames = list(items, items)
      )
    }

    ## Scaling operates on the full non-zero population of the stored matrix.
    ## For symmetric W, that population is c(W_x, W_x); for relative W, it is
    ## the asymmetric entries in both triangles.
    if (scale_method != "none") {
      if (similarity == "relative") {
        population <- W_full_x
        W_full_x <- .co_scale_values(W_full_x, population, scale_method)
        W_x <- .co_scale_values(W_x, population, scale_method)
      } else {
        population <- c(W_x, W_x)
        W_x <- .co_scale_values(W_x, population, scale_method)
        W_sparse <- Matrix::sparseMatrix(
          i = i, j = j, x = W_x,
          dims = c(n_items, n_items), symmetric = TRUE,
          dimnames = list(items, items)
        )
      }
    }

    ## Build the asymmetric W_sparse for 'relative' (post-scaling).
    if (similarity == "relative") {
      W_sparse <- Matrix::sparseMatrix(
        i = ii, j = jj, x = W_full_x,
        dims = c(n_items, n_items),
        dimnames = list(items, items)
      )
    }

    ## Threshold filter on edge weights. `threshold = 0` is the default and
    ## means "no filtering", NOT "drop negative weights": scalings such as
    ## z-score legitimately centre weights on zero, and filtering at the
    ## default would silently discard half of that network.
    if (threshold > 0) {
      keep <- W_x >= threshold
      W_x <- W_x[keep]; i <- i[keep]; j <- j[keep]; raw_vals <- raw_vals[keep]
    }

    edges <- data.frame(
      from   = items[i],
      to     = items[j],
      weight = W_x,
      count  = raw_vals,
      stringsAsFactors = FALSE
    )
    edges <- edges[order(-edges$weight), ]

    if (!is.null(top_n)) {
      stopifnot(is.numeric(top_n), top_n > 0)
      top_n <- as.integer(top_n)
      if (nrow(edges) > top_n) edges <- edges[seq_len(top_n), ]
    }

    ## Rebuild the stored matrix from the surviving edges so that
    ## as_matrix(), the heatmap, and the cograph/netobject converters agree
    ## with the edge list. Previously W_sparse was built pre-filter, so
    ## `top_n = 1` printed one edge while as_matrix() still held all of them.
    if (similarity != "relative") {
      ki <- match(edges$from, items)
      kj <- match(edges$to, items)
      W_sparse <- .co_sym_sparse(ki, kj, edges$weight, items)
    } else {
      ## 'relative' is asymmetric, so each DIRECTION is thresholded on its
      ## own value: W[i,j] and W[j,i] differ and one can fall below the cut
      ## while the other survives. Filtering by the undirected pair instead
      ## would retain a direction that is below threshold.
      keep_full <- if (threshold > 0) W_full_x >= threshold else
        rep(TRUE, length(W_full_x))
      if (!is.null(top_n)) {
        ## top_n is decided on the undirected edge list; restrict the
        ## stored directions to pairs that survived it.
        pair_kept <- paste(pmin(match(edges$from, items),
                                match(edges$to, items)),
                           pmax(match(edges$from, items),
                                match(edges$to, items)))
        keep_full <- keep_full &
          paste(pmin(ii, jj), pmax(ii, jj)) %in% pair_kept
      }
      W_sparse <- Matrix::sparseMatrix(
        i = ii[keep_full], j = jj[keep_full], x = W_full_x[keep_full],
        dims = c(n_items, n_items), dimnames = list(items, items)
      )
    }
  }

  rownames(edges) <- NULL
  class(edges) <- c("cooccurrence", "data.frame")
  attr(edges, "matrix")         <- W_sparse
  attr(edges, "raw_matrix")     <- C_raw
  attr(edges, "items")          <- items
  attr(edges, "frequencies")    <- freq
  attr(edges, "similarity")     <- similarity
  attr(edges, "scale")          <- scale_method
  attr(edges, "threshold")      <- threshold
  attr(edges, "min_occur")      <- min_occur
  attr(edges, "n_transactions") <- n_trans
  attr(edges, "n_items")        <- n_items

  edges
}


# ---- Weighted core (long format with per-entity weights) ----

#' @noRd
.co_core_weighted <- function(data, field, by, weight_by, similarity,
                               scale_method, threshold, min_occur, top_n) {
  stopifnot(
    is.data.frame(data),
    field %in% names(data), by %in% names(data), weight_by %in% names(data)
  )

  docs      <- as.character(data[[by]])
  items_col <- as.character(data[[field]])
  weights   <- as.numeric(data[[weight_by]])

  ## Drop NA / zero-weight rows up front.
  keep <- !is.na(docs) & !is.na(items_col) & !is.na(weights) & weights != 0
  docs <- docs[keep]
  items_col <- items_col[keep]
  weights <- weights[keep]

  all_docs  <- unique(docs)
  all_items <- sort(unique(items_col))
  doc_idx   <- match(docs, all_docs)
  item_idx  <- match(items_col, all_items)

  ## Sparse weighted matrix: docs x items. Duplicate (doc, item) rows sum.
  W <- Matrix::sparseMatrix(
    i = doc_idx, j = item_idx, x = weights,
    dims = c(length(all_docs), length(all_items)),
    dimnames = list(NULL, all_items)
  )

  ## Binary companion for raw counts, built over the FULL item set so its
  ## column indices stay valid; it is filtered alongside W below. Building
  ## it after filtering would leave `item_idx` pointing past the new dims.
  B_bin <- Matrix::sparseMatrix(
    i = doc_idx, j = item_idx, x = 1,
    dims = c(length(all_docs), length(all_items)),
    dimnames = list(NULL, all_items)
  )
  ## sparseMatrix sums duplicates — clip to {0, 1} for the binary matrix.
  B_bin <- Matrix::drop0(sign(B_bin))

  if (min_occur > 1L) {
    ## Support is the number of distinct documents an item appears in, not
    ## the number of rows mentioning it: duplicate (doc, item) rows must not
    ## inflate it. The clipped binary matrix gives exactly that.
    n_docs_per_item <- as.numeric(Matrix::colSums(B_bin))
    keep_items <- n_docs_per_item >= min_occur
    W <- W[, keep_items, drop = FALSE]
    B_bin <- B_bin[, keep_items, drop = FALSE]
    all_items <- all_items[keep_items]
  }

  if (ncol(W) == 0L)
    stop("No items remain after min_occur filtering.", call. = FALSE)

  n_trans <- nrow(W)
  n_items <- ncol(W)
  freq <- as.numeric(Matrix::colSums(W))
  names(freq) <- all_items

  C_raw <- Matrix::crossprod(B_bin)
  C     <- Matrix::crossprod(W)
  Matrix::diag(C) <- 0
  Matrix::diag(C_raw) <- 0
  C     <- Matrix::drop0(C)
  C_raw <- Matrix::drop0(C_raw)

  result <- .co_finalize(C, C_raw, freq, n_trans, n_items, all_items,
                         similarity, scale_method, threshold, min_occur,
                         top_n)
  attr(result, "counting") <- "weighted"
  result
}


# ---- Scaling (triplet-based) ----

#' Apply a post-normalization scaling to edge weights.
#'
#' @param vals Numeric vector to scale (the edge weights returned to the user).
#' @param population Numeric vector of all non-zero values that would appear in
#'   the conceptual full k x k matrix. Used to compute statistics (min/max,
#'   mean/sd, sum) so results match the original dense implementation.
#' @param method Scaling method.
#' @noRd
.co_scale_values <- function(vals, population, method) {
  nz <- population[population != 0]
  if (length(nz) == 0L) return(vals)

  nonzero <- vals != 0

  switch(method,
    minmax = {
      mn <- min(nz); mx <- max(nz)
      out <- vals
      out[nonzero] <- if (mx > mn) (vals[nonzero] - mn) / (mx - mn) else 1
      out
    },
    log = {
      out <- vals
      out[nonzero] <- log(1 + vals[nonzero])
      out
    },
    log10 = {
      out <- vals
      out[nonzero] <- log10(1 + vals[nonzero])
      out
    },
    binary = {
      out <- vals
      out[nonzero] <- 1
      out
    },
    zscore = {
      mu <- mean(nz); s <- stats::sd(nz)
      out <- vals
      if (s > 0) out[nonzero] <- (vals[nonzero] - mu) / s
      out
    },
    sqrt = {
      out <- vals
      out[nonzero] <- sqrt(vals[nonzero])
      out
    },
    proportion = {
      s <- sum(nz)
      out <- vals
      if (s > 0) out[nonzero] <- vals[nonzero] / s
      out
    }
  )
}


# ---- Format detection ----

#' @noRd
.co_detect_format <- function(data, field, by, sep, vars = NULL) {
  ## `vars` names the indicator columns explicitly, so no detection is needed
  ## and columns that are not indicators (ids, timestamps) are simply ignored.
  if (!is.null(vars)) {
    if (!is.data.frame(data) && !is.matrix(data))
      stop("`vars` requires a data frame or matrix input.", call. = FALSE)
    missing_vars <- setdiff(vars, colnames(data))
    if (length(missing_vars))
      stop("`vars` column(s) not found in the data: ",
           paste(missing_vars, collapse = ", "), ".", call. = FALSE)
    return("indicator")
  }

  ## A `nestimate_data` object from Nestimate::prepare() is a list, so it
  ## would otherwise fall into the "list of transactions" branch below and
  ## silently produce nonsense items from its internal components.
  if (inherits(data, "nestimate_data"))
    return("nestimate")

  if (is.list(data) && !is.data.frame(data) && !is.matrix(data))
    return("list")

  if (!is.null(field) && length(field) == 1L && identical(field, "all"))
    return("wide")

  if (!is.null(sep) && !is.null(field) && length(field) > 1L)
    return("multi_delimited")

  if (!is.null(sep) && !is.null(field))
    return("delimited")

  if (!is.null(field) && !is.null(by))
    return("long")

  if (!is.null(field) && is.null(sep) && is.null(by) && is.data.frame(data))
    return("field_only")

  if (is.matrix(data) || is.data.frame(data)) {
    ## Logical columns are indicators too, but as.matrix() keeps them
    ## logical and is.numeric() is FALSE for a logical matrix.
    if (is.data.frame(data) && all(vapply(data, is.logical, logical(1))))
      return("binary")
    mat <- if (is.data.frame(data)) as.matrix(data) else data
    if (is.logical(mat)) return("binary")
    if (is.numeric(mat) && all(mat[!is.na(mat)] %in% c(0, 1)))
      return("binary")
    stop("Cannot detect input format. Use `field = \"all\"` for wide ",
         "sequence data, or `vars = c(...)` to name the indicator columns ",
         "of a one-hot / count table (needed when the table also holds id ",
         "or metadata columns, or when cell values are counts rather than ",
         "0/1).", call. = FALSE)
  }

  stop("Cannot detect input format. Provide field/by/sep arguments or a ",
       "recognized data structure (data.frame, matrix, list).", call. = FALSE)
}


#' @noRd
.co_warn_missing_sep <- function(data, field) {
  candidates <- c(";", ",", "|", "/", "\t")
  vals <- unlist(lapply(field, function(f) as.character(data[[f]])))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) return(invisible())

  counts <- vapply(candidates, function(s) sum(grepl(s, vals, fixed = TRUE)),
                   integer(1))
  best <- which.max(counts)

  if (counts[best] > 0L) {
    sep_label <- if (candidates[best] == "\t") "\\t" else candidates[best]
    pct <- round(100 * counts[best] / length(vals))
    warning(
      "`field` was provided without `sep`. Each value is treated as a single item. ",
      sprintf(
        "Found '%s' in %d%% of values. Did you mean: sep = \"%s\"?",
        sep_label, pct, sep_label
      ),
      call. = FALSE
    )
  } else {
    warning(
      "`field` was provided without `sep`. Each value is treated as a single item.",
      call. = FALSE
    )
  }
}

# ---- Parsers ----

#' Parse transactions for the detected input format.
#' @noRd
.co_parse_transactions <- function(data, fmt, field, by, sep, window = NULL,
                                   vars = NULL) {
  ## Nestimate::prepare() already produced sessionized wide sequences, so
  ## unwrap to its $sequence_data and continue down the ordinary wide path
  ## rather than duplicating its sessionization here.
  if (fmt == "nestimate") {
    seqs <- data$sequence_data
    if (is.null(seqs))
      stop("This looks like a `nestimate_data` object but it has no ",
           "`sequence_data` component.", call. = FALSE)
    return(.co_parse_transactions(seqs, "wide", field, by, sep, window, NULL))
  }

  if (!is.null(window)) {
    if (fmt == "indicator")
      stop("`window` needs ordered sequence input; an indicator table has ",
           "no within-row order.", call. = FALSE)
    if (!fmt %in% c("wide", "list"))
      stop("`window` only applies to ordered sequence formats: a list of ",
           "vectors, or a wide data frame with field = \"all\".",
           call. = FALSE)
    return(if (fmt == "wide") {
      .co_parse_wide_windowed(data, window)
    } else {
      .co_parse_list_windowed(data, window)
    })
  }

  if (fmt == "field_only")
    .co_warn_missing_sep(data, field)

  switch(fmt,
    delimited       = .co_parse_delimited(data, field, sep),
    multi_delimited = .co_parse_multi_delimited(data, field, sep),
    field_only      = .co_parse_field_only(data, field),
    long            = .co_parse_long(data, field, by),
    binary          = .co_parse_binary(data),
    indicator       = .co_parse_indicator(data, vars),
    wide            = .co_parse_wide(data),
    list            = .co_parse_list(data)
  )
}


#' Transactions from explicitly named indicator columns.
#'
#' `vars` names the one-hot / count columns; every other column (ids,
#' timestamps, metadata) is ignored. Values are read as presence: any
#' non-zero, non-NA value marks the item present, so count tables such as
#' document-term matrices work as well as 0/1 and TRUE/FALSE. `NA` is
#' treated as absent, consistent with the void handling in the sequence
#' parsers.
#' @noRd
.co_parse_indicator <- function(data, vars) {
  sub <- if (is.data.frame(data)) data[, vars, drop = FALSE] else
    data[, vars, drop = FALSE]
  bad <- vars[!vapply(as.data.frame(sub), function(v)
    is.numeric(v) || is.logical(v), logical(1))]
  if (length(bad))
    stop("`vars` column(s) are not numeric or logical: ",
         paste(bad, collapse = ", "),
         ". Indicator columns must hold 0/1, counts, or TRUE/FALSE.",
         call. = FALSE)
  mat <- as.matrix(as.data.frame(sub))
  if (any(!is.na(mat) & mat < 0))
    stop("`vars` columns contain negative values; indicator columns must ",
         "be non-negative.", call. = FALSE)
  present <- !is.na(mat) & mat != 0
  lapply(seq_len(nrow(present)), function(i) vars[present[i, ]])
}

#' Rebuild a list of per-row character vectors from a flat token vector.
#'
#' Splits `flat` (already trimmed and NA-cleaned) back into a list of length
#' `n`, honouring the original row lengths via the accompanying `row_idx`.
#' Rows whose tokens were all dropped appear as `character(0)`. Per-row
#' deduplication is done globally via `duplicated()` on a two-column frame,
#' which is faster than calling `unique()` once per row.
#'
#' @noRd
.co_relist_unique <- function(flat, row_idx, n) {
  if (length(flat) == 0L) return(rep(list(character(0)), n))

  ## Global dedup: at most one (row, item) pair.
  dup <- duplicated(data.frame(row_idx, flat, stringsAsFactors = FALSE))
  flat <- flat[!dup]
  row_idx <- row_idx[!dup]

  ## split() by a factor with the full level range preserves empty rows.
  out <- split(flat, factor(row_idx, levels = seq_len(n)))
  names(out) <- NULL
  out
}

#' @noRd
.co_parse_delimited <- function(data, field, sep) {
  stopifnot(is.data.frame(data), length(field) == 1L, field %in% names(data))
  vals <- as.character(data[[field]])
  n <- length(vals)

  ## strsplit returns a list; flatten once so `trimws` and filtering run as
  ## vectorized C loops over the full token population rather than 166k
  ## per-row R calls (that per-row path was ~48% of total runtime).
  splits <- strsplit(vals, sep, fixed = TRUE)
  lens <- lengths(splits)
  if (sum(lens) == 0L) return(rep(list(character(0)), n))

  flat <- trimws(unlist(splits, use.names = FALSE))
  row_idx <- rep.int(seq_len(n), lens)
  keep <- !is.na(flat) & nzchar(flat)
  flat <- flat[keep]; row_idx <- row_idx[keep]

  .co_relist_unique(flat, row_idx, n)
}

#' @noRd
.co_parse_multi_delimited <- function(data, field, sep) {
  stopifnot(is.data.frame(data), all(field %in% names(data)))
  n <- nrow(data)

  ## Split each column independently, concatenate with aligned row indices,
  ## then run the same flat trim + dedup as `.co_parse_delimited`.
  parts <- lapply(field, function(f) {
    splits <- strsplit(as.character(data[[f]]), sep, fixed = TRUE)
    lens <- lengths(splits)
    list(
      row_idx = rep.int(seq_len(n), lens),
      flat    = unlist(splits, use.names = FALSE)
    )
  })
  row_idx <- unlist(lapply(parts, `[[`, "row_idx"), use.names = FALSE)
  flat    <- unlist(lapply(parts, `[[`, "flat"),    use.names = FALSE)
  if (length(flat) == 0L) return(rep(list(character(0)), n))

  flat <- trimws(flat)
  keep <- !is.na(flat) & nzchar(flat)
  flat <- flat[keep]; row_idx <- row_idx[keep]

  .co_relist_unique(flat, row_idx, n)
}

#' @noRd
.co_parse_field_only <- function(data, field) {
  stopifnot(is.data.frame(data), all(field %in% names(data)))
  lapply(seq_len(nrow(data)), function(i) {
    vals <- as.character(unlist(data[i, field, drop = TRUE]))
    vals <- trimws(vals)
    vals <- vals[!is.na(vals) & nzchar(vals)]
    unique(vals)
  })
}

#' @noRd
.co_parse_long <- function(data, field, by) {
  stopifnot(is.data.frame(data), field %in% names(data), by %in% names(data))
  groups <- split(as.character(data[[field]]), data[[by]])
  lapply(groups, function(items) {
    items <- items[nzchar(items) & !is.na(items)]
    unique(items)
  })
}

#' @noRd
.co_parse_binary <- function(data) {
  mat <- if (is.data.frame(data)) as.matrix(data) else data
  if (is.null(colnames(mat)))
    colnames(mat) <- paste0("V", seq_len(ncol(mat)))
  cn <- colnames(mat)
  ## `mat[i, ] == 1` yields NA for missing cells, which used to reach
  ## Matrix::sparseMatrix() as an NA index and fail with an internal error.
  ## NA means "not present", matching the void handling in the sequence
  ## parsers.
  present <- !is.na(mat) & mat == 1
  lapply(seq_len(nrow(present)), function(i) cn[present[i, ]])
}


#' Resolve a `vars` column specification to column names.
#'
#' Uses the same mechanism as base R's `subset(select = )`: the expression is
#' evaluated in a frame that maps each column name to its position, so bare
#' ranges (`a:c`), positions (`1:8`), negative selection (`-id`), `c(a, b)`,
#' and plain character vectors all resolve. Falls back to the caller's
#' environment, so a pre-built character vector variable works too.
#' @noRd
.co_resolve_vars <- function(vars_expr, data, env) {
  if (is.null(vars_expr)) return(NULL)
  ## Checked here rather than in .co_detect_format() because resolution
  ## needs column names, so it must fail first and with the right message.
  if (!is.data.frame(data) && !is.matrix(data))
    stop("`vars` requires a data frame or matrix input.", call. = FALSE)
  nm <- colnames(data)
  if (is.null(nm))
    stop("`vars` requires input with column names.", call. = FALSE)
  ## Evaluate with column names bound to positions; unbound symbols fall
  ## through to the caller, so `vars = my_states` still works.
  pos <- as.list(seq_along(nm))
  names(pos) <- nm
  idx <- tryCatch(eval(vars_expr, pos, env),
                  error = function(e)
                    stop("`vars` could not be resolved to columns: ",
                         conditionMessage(e), call. = FALSE))
  if (is.null(idx)) return(NULL)
  if (is.logical(idx)) {
    if (length(idx) != length(nm))
      stop("A logical `vars` must have one entry per column (", length(nm),
           ").", call. = FALSE)
    return(nm[idx])
  }
  if (is.numeric(idx)) {
    if (any(idx < 0) && any(idx > 0))
      stop("`vars` cannot mix positive and negative positions.",
           call. = FALSE)
    out <- nm[idx]
    if (anyNA(out)) {
      bad <- idx[is.na(out)]
      stop("`vars` position(s) out of range (data has ", length(nm),
           " columns): ", paste(utils::head(bad, 5L), collapse = ", "),
           if (length(bad) > 5L) paste0(", ... (", length(bad), " total)"),
           ".", call. = FALSE)
    }
    return(unique(out))
  }
  if (is.character(idx)) {
    missing_vars <- setdiff(idx, nm)
    if (length(missing_vars))
      stop("`vars` column(s) not found in the data: ",
           paste(missing_vars, collapse = ", "), ".", call. = FALSE)
    ## A repeated column would be counted twice by the sparse builder,
    ## inflating both weight and count for every pair involving it.
    return(unique(idx))
  }
  stop("`vars` must resolve to column names, positions, or a logical mask.",
       call. = FALSE)
}


#' Sessionize a raw event log into wide sequence data.
#'
#' Delegates to `Nestimate::prepare()`, which owns timestamp parsing (ISO8601,
#' Unix, 40+ formats), tie-breaking, and gap-based session splitting. Returns
#' the wide `sequence_data` frame, which the ordinary `field = "all"` path
#' then consumes.
#' @noRd
.co_prepare_events <- function(data, actor, action, time, session, order,
                               time_threshold) {
  if (!requireNamespace("Nestimate", quietly = TRUE))
    stop("Package 'Nestimate' is required for raw event-log input ",
         "(`action = `). Install it, or sessionize the data yourself and ",
         "pass wide sequences with `field = \"all\"`.", call. = FALSE)
  if (!is.data.frame(data))
    stop("`action` requires a data frame of event records.", call. = FALSE)
  if (!is.character(action) || length(action) != 1L || !action %in% names(data))
    stop("`action` must name a single column of `data`.", call. = FALSE)
  for (arg in list(c("actor", actor), c("time", time), c("session", session),
                   c("order", order))) {
    val <- arg[-1]
    if (length(val) && !all(val %in% names(data)))
      stop("`", arg[1], "` column(s) not found in the data: ",
           paste(setdiff(val, names(data)), collapse = ", "), ".",
           call. = FALSE)
  }

  args <- list(data = data, action = action, time = time, session = session,
               order = order, time_threshold = time_threshold)
  ## prepare() treats a missing `actor` as "all rows are one actor".
  if (!is.null(actor)) args$actor <- actor
  prepared <- do.call(Nestimate::prepare, args)
  prepared$sequence_data
}


#' Reject unknown arguments passed through `...`.
#'
#' `cooccurrence()` keeps `...` for signature stability, but silently
#' swallowing unknown names turned a typo such as `simliarity = "jaccard"`
#' into a raw-count network with no warning.
#' @noRd
.co_check_dots <- function(...) {
  extra <- names(list(...))
  if (length(list(...)) == 0L) return(invisible(NULL))
  if (is.null(extra) || any(!nzchar(extra)))
    stop("Unnamed arguments passed to `cooccurrence()`. All optional ",
         "arguments must be named.", call. = FALSE)
  stop("Unknown argument(s) passed to `cooccurrence()`: ",
       paste(extra, collapse = ", "),
       ". Check for a misspelled argument name.", call. = FALSE)
}

#' @noRd
.co_parse_wide <- function(data) {
  mat <- if (is.data.frame(data)) as.matrix(data) else data
  lapply(seq_len(nrow(mat)), function(i) {
    vals <- as.character(mat[i, ])
    vals <- vals[!is.na(vals) & nzchar(vals) & !(vals %in% .void_markers)]
    unique(vals)
  })
}

#' @noRd
.co_parse_list <- function(data) {
  lapply(data, function(items) {
    items <- as.character(items)
    items <- items[!is.na(items) & nzchar(items)]
    unique(items)
  })
}


# ---- Windowed parsers (ordered sequences) ----

#' Sliding-window transactions for one ordered sequence.
#'
#' Drops void markers (NA, "", "%", "*", "NaN") before windowing — voids
#' are not real states. Returns one transaction per window of length
#' \code{window}, deduped within each window. Empty list when the
#' cleaned sequence is shorter than \code{window}.
#' @noRd
.co_window_one <- function(seq, window) {
  seq <- as.character(seq)
  seq <- seq[!is.na(seq) & nzchar(seq) & !(seq %in% .void_markers)]
  if (length(seq) < window) return(list())
  ## embed() returns each window most-recent-first, i.e. REVERSED. That is
  ## harmless for set-based counting but not for `counting = "attention"`,
  ## which reads positional gaps off the transaction, so the row is flipped
  ## back into reading order before dedup.
  W <- stats::embed(seq, window)
  lapply(seq_len(nrow(W)), function(r) unique(rev(W[r, ])))
}

#' @noRd
.co_parse_wide_windowed <- function(data, window) {
  mat <- if (is.data.frame(data)) as.matrix(data) else data
  per_row <- lapply(seq_len(nrow(mat)),
                    function(i) .co_window_one(mat[i, ], window))
  do.call(c, per_row)
}

#' @noRd
.co_parse_list_windowed <- function(data, window) {
  per_seq <- lapply(data, .co_window_one, window = window)
  do.call(c, per_seq)
}


# ---- Sparse bipartite matrix builder ----

#' Build sparse works-by-items incidence matrices from a list of transactions.
#'
#' Returns both the counting-weighted matrix `B` (used for the weighted
#' crossprod) and the binary matrix `B_bin` (used for item frequencies and
#' raw counts). Staying in sparse representation is what lets the pipeline
#' scale to hundreds of thousands of items.
#'
#' Counting method `"fractional"` (Perianes-Rodriguez et al., 2016) bakes
#' `sqrt(1/(n_r - 1))` into each row's non-zero entries, so that
#' `crossprod(B)[i, j] = sum over rows with both i and j of 1/(n_r - 1)`.
#'
#' @noRd
.co_build_sparse <- function(transactions, counting, lambda = 1.0) {
  all_items <- sort(unique(unlist(transactions, use.names = FALSE)))
  n <- length(transactions)
  k <- length(all_items)
  lens <- vapply(transactions, length, integer(1))
  row_idx <- rep.int(seq_len(n), lens)
  col_idx <- match(unlist(transactions, use.names = FALSE), all_items)

  ## Binary bipartite matrix — needed for raw counts and item frequencies
  ## in every counting mode.
  B_bin <- Matrix::sparseMatrix(
    i = row_idx, j = col_idx, x = rep(1.0, length(row_idx)),
    dims = c(n, k),
    dimnames = list(NULL, all_items)
  )

  ## "attention" can't be expressed as a per-row weighted bipartite + crossprod:
  ## the pair contribution is exp(-|pos_i - pos_j| / lambda), a function of
  ## positional GAP, not a product of per-item weights. So we build the
  ## symmetric pair matrix C directly and skip B.
  if (counting == "attention") {
    C <- .co_attention_pairs(transactions, all_items, lambda = lambda)
    return(list(B = NULL, B_bin = B_bin, C = C,
                items = all_items, n = n, k = k))
  }

  if (counting == "fractional") {
    row_weight <- ifelse(lens > 1L, 1 / (lens - 1L), 1)
    x <- sqrt(row_weight[row_idx])
  } else {
    x <- rep(1.0, length(row_idx))
  }

  B <- Matrix::sparseMatrix(
    i = row_idx, j = col_idx, x = x,
    dims = c(n, k),
    dimnames = list(NULL, all_items)
  )

  list(B = B, B_bin = B_bin, items = all_items, n = n, k = k)
}


# ---- Attention pair-decay matrix ----

#' Build the symmetric pair matrix for `counting = "attention"`.
#'
#' For every transaction with n >= 2 items, generate all pair triplets
#' (item_i, item_j, weight) where weight = exp(-|pos_i - pos_j| / lambda).
#' Triplets are concatenated across transactions and fed to
#' `Matrix::sparseMatrix(symmetric = TRUE)`, which sums duplicate (i, j)
#' entries to give the aggregated pair weights.
#'
#' Closer positions → larger weight; distant positions → exponential
#' decay. Matches the `tna::build_model(type = "attention")` semantics
#' but is undirected (we always sum the contribution into the
#' upper-triangle entry; cooccure networks are symmetric by
#' construction).
#'
#' @noRd
.co_attention_pairs <- function(transactions, items, lambda = 1.0) {
  n_items <- length(items)

  ## Per transaction, generate triplets via vectorized triangle-index
  ## construction (avoids utils::combn(), which is recursive R and
  ## materializes a 2 x C(n,2) matrix per call).
  triplets_list <- lapply(transactions, function(t) {
    n <- length(t)
    if (n < 2L) return(NULL)
    pos_i <- rep.int(seq_len(n - 1L), (n - 1L):1L)
    pos_j <- sequence((n - 1L):1L, 2:n)
    weights <- exp(-(pos_j - pos_i) / lambda)
    list(i = match(t[pos_i], items),
         j = match(t[pos_j], items),
         w = weights)
  })
  triplets_list <- triplets_list[!vapply(triplets_list, is.null, logical(1))]

  if (length(triplets_list) == 0L)
    return(.co_sym_sparse(integer(0), integer(0), numeric(0), items))

  ii <- unlist(lapply(triplets_list, `[[`, "i"), use.names = FALSE)
  jj <- unlist(lapply(triplets_list, `[[`, "j"), use.names = FALSE)
  ww <- unlist(lapply(triplets_list, `[[`, "w"), use.names = FALSE)
  .co_sym_sparse(ii, jj, ww, items)
}


# ---- Shared sparse-matrix builder ----

#' Build a symmetric sparse matrix from triplets, normalizing i <= j.
#'
#' `Matrix::sparseMatrix(symmetric = TRUE)` requires entries in one
#' triangle. Callers that arrive with mixed i,j must swap; this helper
#' centralizes the swap and the empty-triplets shortcut so the
#' three places that build symmetric pair matrices
#' (`.co_attention_pairs`, `.co_edges_to_sparse`, plus the empty
#' fallbacks) read as one line.
#' @noRd
.co_sym_sparse <- function(i, j, x, items) {
  n_items <- length(items)
  if (length(i) == 0L) {
    return(Matrix::sparseMatrix(
      i = integer(0), j = integer(0), x = numeric(0),
      dims = c(n_items, n_items), symmetric = TRUE,
      dimnames = list(items, items)
    ))
  }
  swap <- i > j
  if (any(swap)) {
    tmp <- i[swap]; i[swap] <- j[swap]; j[swap] <- tmp
  }
  Matrix::sparseMatrix(
    i = as.integer(i), j = as.integer(j), x = as.numeric(x),
    dims = c(n_items, n_items), symmetric = TRUE,
    dimnames = list(items, items)
  )
}



# ---- aggregate_by helpers ----

#' Combine per-group edge tables into one aggregated table.
#'
#' Stacks the (from, to, weight, count) rows from each group's
#' `cooccurrence` data frame, groups by unordered pair, and reduces
#' the weight column with the chosen aggregator. The count column is
#' always summed (it is a count, not a weight). Threshold and top_n
#' are applied AFTER aggregation so per-group filtering doesn't
#' distort the global combine.
#' @noRd
.co_aggregate_parts <- function(parts, aggregate, threshold, top_n) {
  all_edges <- do.call(rbind, lapply(parts, function(p) {
    p[, c("from", "to", "weight", "count"), drop = FALSE]
  }))
  rownames(all_edges) <- NULL

  if (nrow(all_edges) == 0L) {
    return(data.frame(from = character(0), to = character(0),
                      weight = numeric(0), count = integer(0),
                      stringsAsFactors = FALSE))
  }

  agg_fn <- switch(aggregate,
    sum  = sum, mean = mean, min  = min, max  = max
  )

  ## Group by ordered pair (from is already < to within each part).
  key <- paste(all_edges$from, all_edges$to, sep = "\037")
  weight_by_pair <- vapply(split(all_edges$weight, key),
                           agg_fn, numeric(1))
  count_by_pair  <- vapply(split(all_edges$count, key),
                           function(v) as.integer(sum(v)), integer(1))

  uk <- names(weight_by_pair)
  fr_to <- do.call(rbind, strsplit(uk, "\037", fixed = TRUE))
  edges <- data.frame(
    from   = fr_to[, 1],
    to     = fr_to[, 2],
    weight = unname(weight_by_pair),
    count  = unname(count_by_pair),
    stringsAsFactors = FALSE
  )

  if (threshold > 0) edges <- edges[edges$weight >= threshold, ]
  edges <- edges[order(-edges$weight), ]
  if (!is.null(top_n)) {
    top_n <- as.integer(top_n)
    if (nrow(edges) > top_n) edges <- edges[seq_len(top_n), ]
  }
  rownames(edges) <- NULL
  edges
}

#' Build a symmetric sparse matrix from an edge data frame.
#' @noRd
.co_edges_to_sparse <- function(edges, items) {
  if (nrow(edges) == 0L)
    return(.co_sym_sparse(integer(0), integer(0), numeric(0), items))
  .co_sym_sparse(match(edges$from, items),
                 match(edges$to,   items),
                 edges$weight, items)
}


# ---- Output format conversion ----

#' @noRd
.co_format_output <- function(result, output) {
  if (output == "default") return(result)

  if (output == "gephi") {
    out <- result
    has_group <- "group" %in% names(out)
    names(out)[names(out) == "from"] <- "Source"
    names(out)[names(out) == "to"] <- "Target"
    names(out)[names(out) == "weight"] <- "Weight"
    names(out)[names(out) == "count"] <- "Count"
    out$Type <- "Undirected"
    # Reorder: Source, Target, Weight, Type, Count, [group]
    cols <- c("Source", "Target", "Weight", "Type", "Count")
    if (has_group) cols <- c(cols, "group")
    out <- out[, cols]
    class(out) <- c("cooccurrence", "data.frame")
    # Copy attributes
    for (a in c("matrix", "raw_matrix", "items", "frequencies",
                "similarity", "scale", "threshold", "min_occur",
                "n_transactions", "n_items", "split_by", "groups",
                "counting", "lambda")) {
      attr(out, a) <- attr(result, a)
    }
    return(out)
  }

  if (output == "igraph") return(as_igraph(result))
  if (output == "cograph") return(as_cograph(result))
  if (output == "matrix") return(as_matrix(result))

  result
}
