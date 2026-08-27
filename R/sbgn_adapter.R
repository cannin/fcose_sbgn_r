# PURPOSE ----
# Add a connected SBGN glyph, lay it out in native R, and render the result.

# FUNCTIONS ----

#' Read one numeric XML attribute.
#'
#' @param node xml2 node.
#' @param name Attribute name.
#'
#' @return Numeric attribute value.
xml_number <- function(node, name) {
  value <- xml_attr(node, name)
  if (is.na(value)) {
    stop(sprintf("<%s> is missing %s", xml_name(node), name))
  }
  as.numeric(value)
}

#' Format an SBGN coordinate.
#'
#' @param value Numeric coordinate.
#'
#' @return Compact character representation.
format_coordinate <- function(value) {
  sub("\\.?0+$", "", sprintf("%.3f", value))
}

#' Extract top-level glyph geometry.
#'
#' @param map_node SBGN map node.
#'
#' @return Data frame of glyph geometry.
extract_top_level_glyphs <- function(map_node) {
  glyph_nodes <- xml_find_all(map_node, "./*[local-name()='glyph']")
  records <- lapply(glyph_nodes, function(glyph) {
    bbox <- xml_find_first(glyph, "./*[local-name()='bbox']")
    width <- xml_number(bbox, "w")
    height <- xml_number(bbox, "h")
    x <- xml_number(bbox, "x")
    y <- xml_number(bbox, "y")
    data.frame(
      id = xml_attr(glyph, "id"),
      x = x + width / 2,
      y = y + height / 2,
      width = width,
      height = height,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, records)
}

#' Add an ID to an embedded render style.
#'
#' @param document SBGN XML document.
#' @param style_id Style identifier.
#' @param item_id Glyph or arc identifier.
#'
#' @return Invisibly returns NULL.
append_style_id <- function(document, style_id, item_id) {
  xpath <- sprintf(".//*[local-name()='style' and @id='%s']", style_id)
  style <- xml_find_first(document, xpath)
  if (inherits(style, "xml_missing")) {
    return(invisible(NULL))
  }
  ids <- strsplit(xml_attr(style, "idList"), "\\s+")[[1]]
  ids <- ids[ids != ""]
  if (!(item_id %in% ids)) {
    xml_set_attr(style, "idList", paste(c(ids, item_id), collapse = " "))
  }
  invisible(NULL)
}

#' Find a rectangular boundary point toward another center.
#'
#' @param box Named numeric vector with x, y, width, and height.
#' @param toward Numeric vector containing the opposite center.
#'
#' @return Numeric x/y vector.
boundary_point <- function(box, toward) {
  center <- c(box["x"] + box["width"] / 2, box["y"] + box["height"] / 2)
  delta <- toward - center
  horizontal <- if (abs(delta[1]) < 1e-12) {
    Inf
  } else {
    box["width"] / 2 / abs(delta[1])
  }
  vertical <- if (abs(delta[2]) < 1e-12) {
    Inf
  } else {
    box["height"] / 2 / abs(delta[2])
  }
  center + delta * min(horizontal, vertical)
}

#' Add and lay out one connection in an SBGN document.
#'
#' @param input_path Source SBGN file.
#' @param output_path Destination SBGN file.
#' @param node_id New glyph ID.
#' @param label New glyph label.
#' @param target_id Existing target glyph ID.
#' @param arc_id New arc ID.
#' @param arc_class SBGN arc class.
#' @param direction Either new-to-existing or existing-to-new.
#' @param placement One of left, right, above, or below.
#' @param database_name Optional database identifier.
#' @param database_link Optional database URL.
#' @param width Width of the new glyph.
#' @param height Height of the new glyph.
#' @param iterations Maximum force iterations.
#'
#' @return Output path invisibly.
#' @export
add_connection_and_layout <- function(
  input_path,
  output_path,
  node_id,
  label,
  target_id,
  arc_id,
  arc_class = "stimulation",
  direction = "new-to-existing",
  placement = "left",
  database_name = NULL,
  database_link = NULL,
  width = 46,
  height = 17,
  iterations = 600
) {
  document <- read_xml(input_path)
  map_node <- xml_find_first(document, ".//*[local-name()='map']")
  if (inherits(map_node, "xml_missing")) {
    stop("SBGN document has no map")
  }
  nodes <- extract_top_level_glyphs(map_node)
  if (node_id %in% nodes$id) {
    stop(sprintf("glyph ID '%s' already exists", node_id))
  }
  target_index <- match(target_id, nodes$id)
  if (is.na(target_index)) {
    stop(sprintf("target glyph '%s' does not exist", target_id))
  }
  target <- nodes[target_index, ]
  if (width <= 0 || height <= 0) {
    stop("width and height must be positive")
  }
  horizontal_gap <- 50 + target$width / 2 + width / 2
  vertical_gap <- 50 + target$height / 2 + height / 2
  initial <- switch(
    placement,
    left = c(target$x - horizontal_gap, target$y),
    right = c(target$x + horizontal_gap, target$y),
    above = c(target$x, target$y - vertical_gap),
    below = c(target$x, target$y + vertical_gap),
    stop("placement must be left, right, above, or below")
  )

  first_arc <- xml_find_first(map_node, "./*[local-name()='arc']")
  glyph <- if (inherits(first_arc, "xml_missing")) {
    xml_add_child(
      map_node,
      "glyph",
      id = node_id,
      class = "biological activity"
    )
  } else {
    xml_add_sibling(
      first_arc,
      "glyph",
      .where = "before",
      id = node_id,
      class = "biological activity"
    )
  }
  if (!is.null(database_name) || !is.null(database_link)) {
    extension <- xml_add_child(glyph, "extension")
    entry <- xml_add_child(extension, "kgml:entry", id = node_id, type = "gene")
    if (!is.null(database_name)) {
      xml_set_attr(entry, "name", database_name)
    }
    if (!is.null(database_link)) {
      xml_set_attr(entry, "link", database_link)
    }
  }
  xml_add_child(glyph, "label", text = label)
  xml_add_child(
    glyph,
    "bbox",
    x = format_coordinate(initial[1] - width / 2),
    y = format_coordinate(initial[2] - height / 2),
    w = format_coordinate(width),
    h = format_coordinate(height)
  )
  endpoints <- if (direction == "new-to-existing") {
    c(node_id, target_id)
  } else if (direction == "existing-to-new") {
    c(target_id, node_id)
  } else {
    stop("direction must be new-to-existing or existing-to-new")
  }
  arc <- xml_add_child(
    map_node,
    "arc",
    id = arc_id,
    class = arc_class,
    source = endpoints[1],
    target = endpoints[2]
  )
  append_style_id(document, "style_gene", node_id)
  append_style_id(document, "style_arcs", arc_id)

  existing_ids <- nodes$id
  nodes <- rbind(
    nodes,
    data.frame(
      id = node_id,
      x = initial[1],
      y = initial[2],
      width = width,
      height = height,
      stringsAsFactors = FALSE
    )
  )
  arc_nodes <- xml_find_all(map_node, "./*[local-name()='arc']")
  edges <- data.frame(
    source = xml_attr(arc_nodes, "source"),
    target = xml_attr(arc_nodes, "target"),
    stringsAsFactors = FALSE
  )
  relative <- switch(
    placement,
    left = list(
      first = node_id, second = target_id,
      axis = "horizontal", gap = horizontal_gap
    ),
    right = list(
      first = target_id, second = node_id,
      axis = "horizontal", gap = horizontal_gap
    ),
    above = list(
      first = node_id, second = target_id,
      axis = "vertical", gap = vertical_gap
    ),
    below = list(
      first = target_id, second = node_id,
      axis = "vertical", gap = vertical_gap
    )
  )
  laid_out <- layout_graph( # nolint: object_usage_linter.
    nodes,
    edges,
    fixed_ids = existing_ids,
    relative = relative,
    randomize = FALSE,
    iterations = iterations
  )

  glyph_nodes <- xml_find_all(map_node, "./*[local-name()='glyph']")
  boxes <- list()
  for (glyph_node in glyph_nodes) {
    glyph_id <- xml_attr(glyph_node, "id")
    row <- laid_out[laid_out$id == glyph_id, ]
    bbox <- xml_find_first(glyph_node, "./*[local-name()='bbox']")
    x <- row$x - row$width / 2
    y <- row$y - row$height / 2
    xml_set_attr(bbox, "x", format_coordinate(x))
    xml_set_attr(bbox, "y", format_coordinate(y))
    boxes[[glyph_id]] <- c(x = x, y = y, width = row$width, height = row$height)
  }
  source_row <- laid_out[laid_out$id == endpoints[1], ]
  target_row <- laid_out[laid_out$id == endpoints[2], ]
  start <- boundary_point(boxes[[endpoints[1]]], c(target_row$x, target_row$y))
  end <- boundary_point(boxes[[endpoints[2]]], c(source_row$x, source_row$y))
  xml_add_child(
    arc,
    "start",
    x = format_coordinate(start[1]),
    y = format_coordinate(start[2])
  )
  xml_add_child(
    arc,
    "end",
    x = format_coordinate(end[1]),
    y = format_coordinate(end[2])
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_xml(document, output_path, options = "format")
  invisible(output_path)
}

#' Render an SBGN file with the installed native R renderer.
#'
#' @param sbgn_path Input SBGN path.
#' @param png_path Output PNG path.
#' @param svg_path Output SVG path.
#'
#' @return NULL.
#' @export
render_outputs <- function(sbgn_path, png_path, svg_path) {
  renderSbgnR::draw_sbgnml(sbgn_path, png_path, padding = 10)
  renderSbgnR::draw_sbgnml(sbgn_path, svg_path, padding = 10)
}
