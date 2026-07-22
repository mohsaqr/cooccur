# ---- Converters for cooccurrence objects ----

#' Extract the co-occurrence matrix
#'
#' Returns the full square co-occurrence matrix (normalized + scaled).
#' Use \code{type = "raw"} for the raw count matrix.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param type Character. \code{"normalized"} (default) or \code{"raw"}.
#' @param ... Ignored.
#' @return A numeric matrix.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' as_matrix(res)
#' as_matrix(res, type = "raw")
#' @export
as_matrix <- function(x, ...) UseMethod("as_matrix")

#' @rdname as_matrix
#' @export
as_matrix.cooccurrence <- function(x, type = c("normalized", "raw"), ...) {
  type <- match.arg(type)
  mat <- if (type == "raw") attr(x, "raw_matrix") else attr(x, "matrix")
  if (!is.null(mat)) {
    ## Attributes are stored as sparse Matrix objects in the current engine.
    ## Densify on demand so downstream code gets a regular base matrix. For
    ## very large networks, the caller should avoid as_matrix() and work with
    ## the edge list directly.
    if (inherits(mat, "Matrix")) return(as.matrix(mat))
    return(mat)
  }

  ## Fallback: rebuild from the edge list. Used when the matrix attribute is
  ## absent, which includes split_by results (where a single matrix cannot
  ## describe several stacked networks). Columns are resolved so that
  ## Gephi-named output works too.
  cols <- .co_edge_columns(x)
  items <- attr(x, "items")
  if (is.null(items)) items <- sort(unique(c(cols$from, cols$to)))
  k <- length(items)
  vals <- if (type == "raw") cols$count else cols$weight
  if (nrow(x) == 0L) {
    return(matrix(0, k, k, dimnames = list(items, items)))
  }
  i <- match(cols$from, items)
  j <- match(cols$to, items)
  M <- matrix(0, k, k, dimnames = list(items, items))
  M[cbind(i, j)] <- vals
  M[cbind(j, i)] <- vals
  M
}


#' Convert to igraph
#'
#' Creates an undirected, weighted \code{igraph} graph from a
#' \code{cooccurrence} edge list.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param ... Passed to \code{igraph::graph_from_data_frame}.
#' @return An \code{igraph} object.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   g <- as_igraph(res)
#'   igraph::vcount(g)
#' }
#' @export
as_igraph <- function(x, ...) UseMethod("as_igraph")

#' @rdname as_igraph
#' @export
as_igraph.cooccurrence <- function(x, ...) {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("Package 'igraph' is required.", call. = FALSE)

  items <- attr(x, "items")
  vertices <- if (!is.null(items)) {
    data.frame(name = items, stringsAsFactors = FALSE)
  } else {
    NULL
  }

  ## Resolve edge columns rather than assuming the default names, so that
  ## output = "gephi" objects (Source/Target/Weight/Count) also convert.
  cols <- .co_edge_columns(x)
  edge_df <- data.frame(from = cols$from, to = cols$to,
                        weight = cols$weight, count = cols$count,
                        stringsAsFactors = FALSE)
  if (is.null(vertices)) {
    vertices <- data.frame(name = sort(unique(c(cols$from, cols$to))),
                           stringsAsFactors = FALSE)
  }

  igraph::graph_from_data_frame(
    edge_df,
    directed = FALSE,
    vertices = vertices,
    ...
  )
}


#' Convert to tidygraph
#'
#' Creates a \code{tbl_graph} from a \code{cooccurrence} edge list.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param ... Ignored.
#' @return A \code{tbl_graph} object.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' if (requireNamespace("tidygraph", quietly = TRUE) &&
#'     requireNamespace("igraph",    quietly = TRUE)) {
#'   as_tidygraph(res)
#' }
#' @export
as_tidygraph <- function(x, ...) UseMethod("as_tidygraph")

#' @rdname as_tidygraph
#' @export
as_tidygraph.cooccurrence <- function(x, ...) {
  if (!requireNamespace("tidygraph", quietly = TRUE))
    stop("Package 'tidygraph' is required.", call. = FALSE)
  tidygraph::as_tbl_graph(as_igraph(x, ...))
}


#' Convert to cograph network
#'
#' Creates a \code{cograph_network} object from a \code{cooccurrence}
#' edge list, compatible with \code{cograph::splot()} and other cograph
#' functions.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param ... Ignored.
#' @return A \code{cograph_network} object.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' if (requireNamespace("cograph", quietly = TRUE)) {
#'   net <- as_cograph(res)
#'   net$n_nodes
#' }
#' @export
as_cograph <- function(x, ...) UseMethod("as_cograph")

#' @rdname as_cograph
#' @export
as_cograph.cooccurrence <- function(x, ...) {
  if (!requireNamespace("cograph", quietly = TRUE))
    stop("Package 'cograph' is required.", call. = FALSE)

  mat <- as_matrix(x, type = "normalized")
  items <- colnames(mat)

  nodes_df <- data.frame(
    id = seq_along(items), label = items, name = items,
    x = NA_real_, y = NA_real_, stringsAsFactors = FALSE
  )

  # Edge list from upper triangle
  idx <- which(upper.tri(mat) & mat != 0, arr.ind = TRUE)
  if (nrow(idx) > 0L) {
    edges_df <- data.frame(
      from = as.integer(idx[, 1]),
      to = as.integer(idx[, 2]),
      weight = mat[idx],
      stringsAsFactors = FALSE
    )
  } else {
    edges_df <- data.frame(from = integer(0), to = integer(0),
                           weight = numeric(0), stringsAsFactors = FALSE)
  }

  structure(
    list(
      weights = mat,
      nodes = nodes_df,
      edges = edges_df,
      directed = FALSE,
      n_nodes = length(items),
      n_edges = nrow(edges_df),
      meta = list(source = "cooccure", layout = NULL)
    ),
    class = "cograph_network"
  )
}


#' Convert to Nestimate netobject
#'
#' Creates a \code{netobject} from a \code{cooccurrence} edge list,
#' compatible with \code{Nestimate::centrality()},
#' \code{Nestimate::bootstrap_network()}, etc.
#'
#' @param x A \code{cooccurrence} data frame.
#' @param ... Ignored.
#' @return A \code{netobject} with class \code{c("netobject", "cograph_network")}.
#' @examples
#' res <- cooccurrence(list(c("A","B","C"), c("B","C"), c("A","C")))
#' if (requireNamespace("Nestimate", quietly = TRUE)) {
#'   net <- as_netobject(res)
#'   net$n_nodes
#' }
#' @export
as_netobject <- function(x, ...) UseMethod("as_netobject")

#' @rdname as_netobject
#' @export
as_netobject.cooccurrence <- function(x, ...) {
  if (!requireNamespace("Nestimate", quietly = TRUE))
    stop("Package 'Nestimate' is required.", call. = FALSE)

  mat <- as_matrix(x, type = "normalized")
  items <- colnames(mat)

  nodes_df <- data.frame(
    id = seq_along(items), label = items, name = items,
    x = NA_real_, y = NA_real_, stringsAsFactors = FALSE
  )

  idx <- which(upper.tri(mat) & mat != 0, arr.ind = TRUE)
  if (nrow(idx) > 0L) {
    edges_df <- data.frame(
      from = as.integer(idx[, 1]),
      to = as.integer(idx[, 2]),
      weight = mat[idx],
      stringsAsFactors = FALSE
    )
  } else {
    edges_df <- data.frame(from = integer(0), to = integer(0),
                           weight = numeric(0), stringsAsFactors = FALSE)
  }

  structure(
    list(
      data = NULL,
      weights = mat,
      nodes = nodes_df,
      edges = edges_df,
      directed = FALSE,
      method = "cooccurrence",
      params = list(
        similarity = attr(x, "similarity"),
        scale = attr(x, "scale"),
        threshold = attr(x, "threshold"),
        n_transactions = attr(x, "n_transactions")
      ),
      scaling = NULL,
      threshold = attr(x, "threshold"),
      n_nodes = length(items),
      n_edges = nrow(edges_df),
      level = NULL,
      meta = list(source = "cooccure", layout = NULL,
                  tna = list(method = "cooccurrence")),
      node_groups = NULL
    ),
    class = c("netobject", "cograph_network")
  )
}
