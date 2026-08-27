# PURPOSE ----
# Provide native R spectral initialization and force-directed graph layout.

# FUNCTIONS ----

#' Build an adjacency list and connect disconnected components.
#'
#' @param node_count Number of nodes.
#' @param indexed_edges Two-column integer matrix of zero-free R indexes.
#'
#' @return List of integer neighbor vectors.
build_adjacency <- function(node_count, indexed_edges) {
  adjacency <- replicate(node_count, integer(0), simplify = FALSE)
  if (nrow(indexed_edges) > 0) {
    for (row in seq_len(nrow(indexed_edges))) {
      source <- indexed_edges[row, 1]
      target <- indexed_edges[row, 2]
      adjacency[[source]] <- unique(c(adjacency[[source]], target))
      adjacency[[target]] <- unique(c(adjacency[[target]], source))
    }
  }

  visited <- rep(FALSE, node_count)
  roots <- integer(0)
  for (start in seq_len(node_count)) {
    if (visited[start]) {
      next
    }
    roots <- c(roots, start)
    queue <- start
    visited[start] <- TRUE
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      for (neighbor in adjacency[[current]]) {
        if (!visited[neighbor]) {
          visited[neighbor] <- TRUE
          queue <- c(queue, neighbor)
        }
      }
    }
  }
  if (length(roots) > 1) {
    for (index in seq_len(length(roots) - 1)) {
      first <- roots[index]
      second <- roots[index + 1]
      adjacency[[first]] <- c(adjacency[[first]], second)
      adjacency[[second]] <- c(adjacency[[second]], first)
    }
  }
  adjacency
}

#' Compute graph-distance spectral coordinates.
#'
#' @param node_count Number of nodes.
#' @param indexed_edges Two-column integer edge matrix.
#' @param separation Distance assigned to one graph hop.
#'
#' @return Numeric matrix with x and y columns.
#' @export
spectral_positions <- function(node_count, indexed_edges, separation = 75) {
  if (node_count <= 1) {
    return(matrix(0, nrow = node_count, ncol = 2))
  }
  adjacency <- build_adjacency(node_count, indexed_edges)
  distances <- matrix(0, nrow = node_count, ncol = node_count)
  for (start in seq_len(node_count)) {
    discovered <- rep(Inf, node_count)
    discovered[start] <- 0
    queue <- start
    while (length(queue) > 0) {
      current <- queue[1]
      queue <- queue[-1]
      for (neighbor in adjacency[[current]]) {
        if (!is.finite(discovered[neighbor])) {
          discovered[neighbor] <- discovered[current] + 1
          queue <- c(queue, neighbor)
        }
      }
    }
    distances[start, ] <- discovered * separation
  }
  centering <- diag(node_count) - matrix(1 / node_count, node_count, node_count)
  gram <- -0.5 * centering %*% (distances^2) %*% centering
  decomposition <- eigen(gram, symmetric = TRUE)
  dimensions <- min(2, length(decomposition$values))
  coordinates <- decomposition$vectors[, seq_len(dimensions), drop = FALSE] %*%
    diag(sqrt(pmax(decomposition$values[seq_len(dimensions)], 0)), dimensions)
  if (ncol(coordinates) == 1) {
    coordinates <- cbind(coordinates[, 1], rep(0, node_count))
  }
  coordinates
}

#' Project a relative placement constraint.
#'
#' @param positions Position matrix.
#' @param ids Node IDs.
#' @param constraint Named list with first, second, axis, and gap.
#' @param fixed Logical vector of fixed nodes.
#'
#' @return Updated position matrix.
project_relative <- function(positions, ids, constraint, fixed) {
  first <- match(constraint$first, ids)
  second <- match(constraint$second, ids)
  axis <- if (constraint$axis == "horizontal") 1 else 2
  violation <- positions[first, axis] + constraint$gap - positions[second, axis]
  if (violation <= 0) {
    return(positions)
  }
  if (fixed[first]) {
    positions[second, axis] <- positions[second, axis] + violation
  } else if (fixed[second]) {
    positions[first, axis] <- positions[first, axis] - violation
  } else {
    positions[first, axis] <- positions[first, axis] - violation / 2
    positions[second, axis] <- positions[second, axis] + violation / 2
  }
  positions
}

#' Lay out graph nodes with native R spectral and force-directed stages.
#'
#' @param nodes Data frame with id, x, y, width, and height columns.
#' @param edges Data frame with source and target columns.
#' @param fixed_ids IDs whose coordinates must remain fixed.
#' @param relative Optional relative placement constraint.
#' @param randomize Whether to use spectral initialization.
#' @param iterations Maximum force iterations.
#' @param node_separation Graph-hop separation for spectral layout.
#' @param node_repulsion Pairwise repulsion constant.
#' @param ideal_edge_length Desired edge length.
#' @param edge_elasticity Spring elasticity.
#' @param gravity Gravity strength.
#' @param initial_energy Initial cooling multiplier.
#'
#' @return Input node data frame with updated x and y columns.
#' @export
layout_graph <- function(
  nodes,
  edges,
  fixed_ids = character(0),
  relative = NULL,
  randomize = FALSE,
  iterations = 600,
  node_separation = 75,
  node_repulsion = 4500,
  ideal_edge_length = 50,
  edge_elasticity = 0.45,
  gravity = 0.25,
  initial_energy = 0.3
) {
  if (anyDuplicated(nodes$id)) {
    stop("node IDs must be unique")
  }
  indexes <- stats::setNames(seq_len(nrow(nodes)), nodes$id)
  valid_edges <- edges$source %in% nodes$id &
    edges$target %in% nodes$id & edges$source != edges$target
  indexed_edges <- if (any(valid_edges)) {
    cbind(
      indexes[edges$source[valid_edges]],
      indexes[edges$target[valid_edges]]
    )
  } else {
    matrix(integer(0), nrow = 0, ncol = 2)
  }
  positions <- as.matrix(nodes[, c("x", "y")])
  storage.mode(positions) <- "double"
  if (randomize) {
    positions <- spectral_positions(nrow(nodes), indexed_edges, node_separation)
  }
  fixed <- nodes$id %in% fixed_ids
  fixed_positions <- positions
  stable <- 0

  for (iteration in seq_len(max(0, iterations))) {
    forces <- matrix(0, nrow = nrow(nodes), ncol = 2)
    movable <- which(!fixed)
    for (first in movable) {
      for (second in seq_len(nrow(nodes))) {
        if (first == second) {
          next
        }
        delta <- positions[first, ] - positions[second, ]
        distance <- sqrt(sum(delta^2))
        if (distance < 1e-6) {
          delta <- c(1, 0.25)
          distance <- sqrt(sum(delta^2))
        }
        forces[first, ] <- forces[first, ] +
          delta * node_repulsion / distance^3
        overlap_x <- (nodes$width[first] + nodes$width[second]) / 2 -
          abs(delta[1])
        overlap_y <- (nodes$height[first] + nodes$height[second]) / 2 -
          abs(delta[2])
        if (overlap_x > 0 && overlap_y > 0) {
          if (overlap_x < overlap_y) {
            forces[first, 1] <- forces[first, 1] +
              sign(ifelse(delta[1] == 0, 1, delta[1])) * 2 * overlap_x
          } else {
            forces[first, 2] <- forces[first, 2] +
              sign(ifelse(delta[2] == 0, 1, delta[2])) * 2 * overlap_y
          }
        }
      }
    }
    if (nrow(indexed_edges) > 0) {
      for (edge_index in seq_len(nrow(indexed_edges))) {
        source <- indexed_edges[edge_index, 1]
        target <- indexed_edges[edge_index, 2]
        delta <- positions[target, ] - positions[source, ]
        distance <- max(sqrt(sum(delta^2)), 1e-6)
        force <- delta * edge_elasticity *
          (distance - ideal_edge_length) / distance
        if (!fixed[source]) {
          forces[source, ] <- forces[source, ] + force
        }
        if (!fixed[target]) {
          forces[target, ] <- forces[target, ] - force
        }
      }
    }
    center <- colMeans(positions)
    for (index in movable) {
      forces[index, ] <- forces[index, ] +
        (center - positions[index, ]) * gravity / max(ideal_edge_length, 1)
    }
    progress <- (iteration - 1) / max(iterations, 1)
    max_step <- max(0.01, ideal_edge_length * initial_energy * (1 - progress))
    total_step <- 0
    for (index in movable) {
      force_length <- sqrt(sum(forces[index, ]^2))
      step <- forces[index, ]
      if (force_length > max_step) {
        step <- step * max_step / force_length
      }
      positions[index, ] <- positions[index, ] + step
      total_step <- total_step + sqrt(sum(step^2))
    }
    positions[fixed, ] <- fixed_positions[fixed, ]
    if (!is.null(relative)) {
      positions <- project_relative(positions, nodes$id, relative, fixed)
    }
    if (total_step / nrow(nodes) < 0.02) {
      stable <- stable + 1
      if (stable >= 20) {
        break
      }
    } else {
      stable <- 0
    }
  }
  nodes$x <- positions[, 1]
  nodes$y <- positions[, 2]
  nodes
}
