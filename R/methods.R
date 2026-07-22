# ---- S3 methods for cooccurrence ----

#' Print a cooccurrence edge list
#'
#' Prints a compact network diagnostic header followed by an edge preview.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param n Integer. Number of rows to show. Default 10.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' print(res)
#' @export
print.cooccurrence <- function(x, n = 10L, ...) {
  s <- .co_summary_data(x)

  cat(sprintf("# cooccurrence: %d nodes, %d edges", s$n_nodes, s$n_edges))
  if (!s$is_split) {
    cat(sprintf(" | density: %.4f | mean degree: %.4f",
                s$density, s$mean_degree))
  }
  if (!is.null(s$n_transactions)) {
    cat(sprintf(" | %d transactions", s$n_transactions))
  }
  cat("\n")
  if (!is.null(s$split_by)) {
    cat(sprintf("# split_by: %s (%d groups)\n", s$split_by, length(s$groups)))
  }
  if (!is.null(s$aggregate_by)) {
    cat(sprintf("# aggregate_by: %s (%d groups, %s)\n",
                s$aggregate_by, length(s$groups), s$aggregate))
  }
  cat(sprintf("# similarity: %s | counting: %s",
              s$similarity %||% "none", s$counting %||% "full"))
  if (!is.null(s$scale) && s$scale != "none") {
    cat(sprintf(" | scale: %s", s$scale))
  }
  cat("\n")
  if (s$is_split) {
    cat(sprintf("# isolates: %d | per-group metrics: summary()\n", s$isolates))
  } else {
    cat(sprintf("# possible edges: %d | isolates: %d\n",
                s$possible_edges, s$isolates))
    if (s$n_edges > 0L) {
      top <- utils::head(s$nodes[s$nodes$degree > 0L, ], 5L)
      cat(sprintf("# top nodes: %s\n",
                  paste(sprintf("%s(deg=%d,str=%.4g)", top$node, top$degree,
                                top$strength),
                        collapse = ", ")))
    }
  }

  show <- min(n, s$n_edges)
  if (show > 0L) {
    print(as.data.frame(x)[seq_len(show), ], row.names = FALSE)
    if (s$n_edges > show)
      cat(sprintf("# ... %d more edges\n", s$n_edges - show))
  } else {
    cat("# (no edges)\n")
  }
  invisible(x)
}


.co_summary_data <- function(object) {
  cols <- .co_edge_columns(object)
  items <- attr(object, "items")
  edge_items <- sort(unique(c(cols$from, cols$to)))
  if (is.null(items)) {
    items <- edge_items
  } else {
    items <- sort(unique(c(items, edge_items)))
  }
  n_nodes <- length(items)
  n_edges <- nrow(object)

  ## A split_by result stacks one network per group into a single frame, so
  ## the same node pair can appear once per group. Pooled simple-graph
  ## metrics are undefined there (density would exceed 1, degree would
  ## exceed n - 1), so they are reported as NA and the per-group table in
  ## `groups_summary` carries the real numbers instead.
  is_split <- "group" %in% names(object)

  possible_edges <- if (is_split) NA_real_ else n_nodes * (n_nodes - 1) / 2
  density <- if (is_split) {
    NA_real_
  } else if (possible_edges > 0) {
    n_edges / possible_edges
  } else {
    NA_real_
  }

  degree <- stats::setNames(integer(n_nodes), items)
  strength <- stats::setNames(numeric(n_nodes), items)
  count_strength <- stats::setNames(numeric(n_nodes), items)
  if (n_edges > 0L) {
    edge_nodes <- c(cols$from, cols$to)
    deg_tab <- table(edge_nodes)
    degree[names(deg_tab)] <- as.integer(deg_tab)

    edge_weight <- rep(cols$weight, 2L)
    edge_count <- rep(cols$count, 2L)
    strength_tab <- rowsum(edge_weight, edge_nodes, reorder = FALSE)
    count_tab <- rowsum(edge_count, edge_nodes, reorder = FALSE)
    strength[rownames(strength_tab)] <- as.numeric(strength_tab[, 1])
    count_strength[rownames(count_tab)] <- as.numeric(count_tab[, 1])
  }

  frequencies <- attr(object, "frequencies")
  frequency <- if (!is.null(frequencies)) {
    out <- stats::setNames(rep(NA_real_, n_nodes), items)
    out[names(frequencies)] <- as.numeric(frequencies)
    out
  } else {
    stats::setNames(rep(NA_real_, n_nodes), items)
  }

  node_stats <- data.frame(
    node = items,
    degree = as.integer(degree),
    strength = as.numeric(strength),
    count_strength = as.numeric(count_strength),
    frequency = as.numeric(frequency),
    stringsAsFactors = FALSE
  )
  node_stats <- node_stats[order(-node_stats$degree, -node_stats$strength,
                                 node_stats$node), ]
  rownames(node_stats) <- NULL

  weight_summary <- if (n_edges > 0L) {
    c(
      min = min(cols$weight),
      q1 = unname(stats::quantile(cols$weight, 0.25, names = FALSE)),
      median = stats::median(cols$weight),
      mean = mean(cols$weight),
      q3 = unname(stats::quantile(cols$weight, 0.75, names = FALSE)),
      max = max(cols$weight)
    )
  } else {
    c(min = NA_real_, q1 = NA_real_, median = NA_real_,
      mean = NA_real_, q3 = NA_real_, max = NA_real_)
  }

  count_summary <- if (n_edges > 0L) {
    c(
      min = min(cols$count),
      q1 = unname(stats::quantile(cols$count, 0.25, names = FALSE)),
      median = stats::median(cols$count),
      mean = mean(cols$count),
      q3 = unname(stats::quantile(cols$count, 0.75, names = FALSE)),
      max = max(cols$count)
    )
  } else {
    c(min = NA_real_, q1 = NA_real_, median = NA_real_,
      mean = NA_real_, q3 = NA_real_, max = NA_real_)
  }

  group_stats <- NULL
  if ("group" %in% names(object)) {
    ## Per-group support recorded at split time; a node isolated inside its
    ## group appears in no edge and cannot be recovered from the rows alone.
    group_items <- attr(object, "group_items")
    group_stats <- do.call(rbind, lapply(split(object, object$group), function(x) {
      group_cols <- .co_edge_columns(x)
      grp <- as.character(x$group[1L])
      nodes <- if (!is.null(group_items[[grp]])) {
        group_items[[grp]]
      } else {
        sort(unique(c(group_cols$from, group_cols$to)))
      }
      n_group_nodes <- length(nodes)
      n_group_edges <- nrow(x)
      group_possible <- n_group_nodes * (n_group_nodes - 1L) / 2L
      data.frame(
        group = as.character(x$group[1L]),
        n_nodes = n_group_nodes,
        n_edges = n_group_edges,
        possible_edges = group_possible,
        density = if (group_possible > 0) n_group_edges / group_possible else NA_real_,
        weight_mean = if (n_group_edges > 0L) mean(group_cols$weight) else NA_real_,
        count_sum = if (n_group_edges > 0L) sum(group_cols$count) else 0,
        stringsAsFactors = FALSE
      )
    }))
    rownames(group_stats) <- NULL
  }

  out <- list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    possible_edges = possible_edges,
    density = density,
    mean_degree = if (is_split || n_nodes == 0L) {
      NA_real_
    } else {
      2 * n_edges / n_nodes
    },
    isolates = sum(degree == 0L),
    is_split = is_split,
    n_transactions = attr(object, "n_transactions"),
    similarity = attr(object, "similarity"),
    counting = attr(object, "counting"),
    scale = attr(object, "scale"),
    threshold = attr(object, "threshold"),
    min_occur = attr(object, "min_occur"),
    split_by = attr(object, "split_by"),
    aggregate_by = attr(object, "aggregate_by"),
    aggregate = attr(object, "aggregate"),
    groups = attr(object, "groups"),
    weight = weight_summary,
    count = count_summary,
    nodes = node_stats,
    groups_summary = group_stats
  )
  class(out) <- "summary.cooccurrence"
  out
}


.co_edge_columns <- function(x) {
  if (all(c("from", "to", "weight", "count") %in% names(x))) {
    return(list(from = x$from, to = x$to,
                weight = as.numeric(x$weight), count = as.numeric(x$count)))
  }
  if (all(c("Source", "Target", "Weight", "Count") %in% names(x))) {
    return(list(from = x$Source, to = x$Target,
                weight = as.numeric(x$Weight), count = as.numeric(x$Count)))
  }
  stop("Cannot identify edge columns in this cooccurrence object.",
       call. = FALSE)
}


`%||%` <- function(x, y) if (is.null(x)) y else x


#' Summarise a cooccurrence network
#'
#' Node- and group-level metrics are available as tidy data frames via
#' \code{\link[base]{as.data.frame}}.
#'
#' For a network built with \code{split_by}, the returned frame stacks one
#' network per group, so a node pair can occur once per group. Pooled
#' simple-graph metrics (\code{density}, \code{mean_degree},
#' \code{possible_edges}) are undefined in that case and reported as
#' \code{NA}; the per-group table carries the correct values.
#'
#' @param object A \code{cooccurrence} data frame.
#' @param ... Ignored.
#' @return Invisibly returns a \code{summary.cooccurrence} object with network,
#'   node, and group-level metrics.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' summary(res)
#'
#' # Node-level metrics as a tidy data frame
#' as.data.frame(summary(res))
#' @export
summary.cooccurrence <- function(object, ...) {
  out <- .co_summary_data(object)
  print(out)
  invisible(out)
}


#' Print a cooccurrence summary
#'
#' @param x A \code{summary.cooccurrence} object.
#' @param n Integer. Number of top nodes/groups to show. Default 5.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.summary.cooccurrence <- function(x, n = 5L, ...) {
  cat("cooccurrence network\n")
  cat(rep("-", 30), "\n", sep = "")
  cat(sprintf("Nodes          : %d\n", x$n_nodes))
  cat(sprintf("Edges          : %d%s\n", x$n_edges,
              if (x$is_split) " (pooled across groups)" else ""))
  ## Pooled simple-graph metrics are undefined for a split_by result; the
  ## per-group block below reports them correctly instead.
  if (!x$is_split) {
    cat(sprintf("Possible edges : %d\n", x$possible_edges))
    cat(sprintf("Density        : %.4f\n", x$density))
    cat(sprintf("Mean degree    : %.4f\n", x$mean_degree))
  }
  cat(sprintf("Isolates       : %d\n", x$isolates))
  if (!is.null(x$n_transactions)) {
    cat(sprintf("Transactions   : %d\n", x$n_transactions))
  }
  if (!is.null(x$split_by)) {
    cat(sprintf("Split by       : %s (%d groups)\n",
                x$split_by, length(x$groups)))
  }
  if (!is.null(x$aggregate_by)) {
    cat(sprintf("Aggregate by   : %s (%d groups, %s)\n",
                x$aggregate_by, length(x$groups), x$aggregate))
  }
  if (!is.null(x$similarity)) {
    cat(sprintf("Similarity     : %s\n", x$similarity))
  }
  if (!is.null(x$counting)) {
    cat(sprintf("Counting       : %s\n", x$counting))
  }
  if (!is.null(x$scale) && x$scale != "none") {
    cat(sprintf("Scale          : %s\n", x$scale))
  }

  if (x$n_edges > 0L) {
    cat(sprintf("Weight range   : [%.4g, %.4g]\n",
                x$weight[["min"]], x$weight[["max"]]))
    cat(sprintf("Weight mean    : %.4g\n", x$weight[["mean"]]))
    cat(sprintf("Count range    : [%.4g, %.4g]\n",
                x$count[["min"]], x$count[["max"]]))
    cat(sprintf("Count mean     : %.4g\n", x$count[["mean"]]))

    ## Node degree pools across groups when split_by is used, so it is not a
    ## simple-graph degree; suppress rather than report a misleading number.
    if (!x$is_split) {
      top <- utils::head(x$nodes[x$nodes$degree > 0L, ], as.integer(n))
      if (nrow(top) > 0L) {
        cat(sprintf("Top nodes      : %s\n",
                    paste(sprintf("%s(%d, %.4g)", top$node, top$degree,
                                  top$strength),
                          collapse = ", ")))
      }
    }
  }

  if (!is.null(x$groups_summary) && nrow(x$groups_summary) > 0L) {
    show <- utils::head(x$groups_summary[order(-x$groups_summary$n_edges), ],
                        as.integer(n))
    cat("Groups         : ")
    cat(paste(sprintf("%s(%d nodes, %d edges, %.4f density)",
                      show$group, show$n_nodes, show$n_edges, show$density),
              collapse = ", "))
    cat("\n")
  }

  invisible(x)
}


#' Tidy node or group table from a cooccurrence summary
#'
#' Returns the summary's node-level metrics (one row per node) or its
#' group-level metrics (one row per group) as a plain \code{data.frame},
#' so callers never have to reach into the summary object's internals.
#'
#' @param x A \code{summary.cooccurrence} object.
#' @param row.names Passed to \code{\link[base]{as.data.frame}}; unused.
#' @param optional Passed to \code{\link[base]{as.data.frame}}; unused.
#' @param what Character. \code{"nodes"} (default) for one row per node with
#'   degree, strength, count strength, and frequency; \code{"groups"} for one
#'   row per group when the network was built with \code{split_by}.
#' @param ... Ignored.
#' @return A base \code{data.frame}. For \code{what = "nodes"}, one row per
#'   node; for \code{what = "groups"}, one row per group (zero rows when the
#'   network was not split).
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' as.data.frame(summary(res))
#' @export
as.data.frame.summary.cooccurrence <- function(x, row.names = NULL,
                                               optional = FALSE,
                                               what = c("nodes", "groups"),
                                               ...) {
  what <- match.arg(what)
  if (what == "nodes") return(x$nodes)
  if (is.null(x$groups_summary)) {
    return(data.frame(group = character(0), n_nodes = integer(0),
                      n_edges = integer(0), possible_edges = numeric(0),
                      density = numeric(0), weight_mean = numeric(0),
                      count_sum = numeric(0), stringsAsFactors = FALSE))
  }
  x$groups_summary
}


#' Plot a cooccurrence network
#'
#' Plots the co-occurrence matrix as a heatmap, an \pkg{igraph} network, or a
#' base-R degree distribution.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param type Character. \code{"heatmap"} (default), \code{"network"}
#'   (requires \pkg{igraph}), or \code{"degree"} for a base-R degree
#'   distribution bar plot.
#' @param ... Passed to the plotting function.
#' @return Invisibly returns \code{x}.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' plot(res)
#' @export
plot.cooccurrence <- function(x, type = c("heatmap", "network", "degree"), ...) {
  type <- match.arg(type)

  if (type == "network") {
    if (!requireNamespace("igraph", quietly = TRUE))
      stop("Package 'igraph' is required for network plots.", call. = FALSE)
    g <- as_igraph(x)
    plot(g, ...)
  } else if (type == "degree") {
    s <- .co_summary_data(x)
    degree_counts <- table(factor(s$nodes$degree,
                                  levels = seq.int(0L, max(s$nodes$degree))))
    args <- list(
      height = as.numeric(degree_counts),
      names.arg = names(degree_counts),
      xlab = "Degree",
      ylab = "Number of nodes",
      main = "Degree distribution",
      col = "grey70",
      border = "grey30"
    )
    dots <- list(...)
    args[names(dots)] <- dots
    do.call(graphics::barplot, args)
  } else {
    ## Go through as_matrix() so sparse attributes get densified consistently.
    mat <- as_matrix(x)
    n <- nrow(mat)
    graphics::image(
      seq_len(n), seq_len(n), t(mat[n:1, ]),
      xlab = "", ylab = "", axes = FALSE, ...
    )
    graphics::axis(1, at = seq_len(n), labels = colnames(mat),
                   las = 2, cex.axis = 0.7)
    graphics::axis(2, at = seq_len(n), labels = rev(rownames(mat)),
                   las = 2, cex.axis = 0.7)
  }

  invisible(x)
}
