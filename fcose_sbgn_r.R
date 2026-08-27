#!/usr/bin/env Rscript

# PURPOSE ----
# Command-line interface for the native R fCoSE-style SBGN adapter.

# FUNCTIONS ----

#' Return the directory containing this script.
#'
#' @return Absolute script directory.
script_directory <- function() {
  arguments <- commandArgs(trailingOnly = FALSE)
  file_argument <- arguments[grepl("^--file=", arguments)]
  if (length(file_argument) == 0) {
    return(normalizePath(".", mustWork = TRUE))
  }
  dirname(normalizePath(sub("^--file=", "", file_argument[1]), mustWork = TRUE))
}

#' Parse a simple flag-value command line.
#'
#' @param arguments Trailing command-line arguments.
#'
#' @return Named list of adapter configuration.
parse_cli_arguments <- function(arguments) {
  configuration <- list(
    input = NULL,
    output = NULL,
    node_id = NULL,
    label = NULL,
    target_id = NULL,
    arc_id = NULL,
    arc_class = "stimulation",
    direction = "new-to-existing",
    placement = "left",
    database_name = NULL,
    database_link = NULL,
    width = 46,
    height = 17,
    iterations = 600,
    render = TRUE
  )
  value_options <- c(
    "--output" = "output",
    "--node-id" = "node_id",
    "--label" = "label",
    "--target-id" = "target_id",
    "--arc-id" = "arc_id",
    "--arc-class" = "arc_class",
    "--direction" = "direction",
    "--placement" = "placement",
    "--database-name" = "database_name",
    "--database-link" = "database_link",
    "--width" = "width",
    "--height" = "height",
    "--iterations" = "iterations"
  )
  index <- 1
  while (index <= length(arguments)) {
    argument <- arguments[index]
    if (argument == "--no-render") {
      configuration$render <- FALSE
      index <- index + 1
      next
    }
    if (argument %in% names(value_options)) {
      if (index == length(arguments)) {
        stop(sprintf("missing value after %s", argument))
      }
      name <- value_options[[argument]]
      configuration[[name]] <- arguments[index + 1]
      index <- index + 2
      next
    }
    if (startsWith(argument, "--")) {
      stop(sprintf("unknown option %s", argument))
    }
    if (!is.null(configuration$input)) {
      stop("only one input SBGN path may be supplied")
    }
    configuration$input <- argument
    index <- index + 1
  }
  required <- c("input", "output", "node_id", "label", "target_id", "arc_id")
  missing <- required[vapply(configuration[required], is.null, logical(1))]
  if (length(missing) > 0) {
    missing_names <- paste(missing, collapse = ", ")
    message <- paste("missing required options:", missing_names)
    stop(message)
  }
  configuration$width <- as.numeric(configuration$width)
  configuration$height <- as.numeric(configuration$height)
  configuration$iterations <- as.integer(configuration$iterations)
  configuration
}

#' Print command-line usage.
#'
#' @return NULL invisibly.
print_usage <- function() {
  cat(
    paste(
      "Usage: Rscript fcose_sbgn_r.R INPUT --output OUTPUT",
      "--node-id ID --label LABEL --target-id ID --arc-id ID [options]",
      "",
      "Options:",
      "  --arc-class CLASS       SBGN interaction class (default: stimulation)",
      "  --direction DIRECTION   new-to-existing or existing-to-new",
      "  --placement POSITION    left, right, above, or below",
      "  --width NUMBER          New glyph width (default: 46)",
      "  --height NUMBER         New glyph height (default: 17)",
      "  --iterations NUMBER     Force-layout iterations (default: 600)",
      "  --database-name NAME    Optional database identifier",
      "  --database-link URL     Optional database URL",
      "  --no-render             Write SBGN without PNG and SVG",
      sep = "\n"
    )
  )
  invisible(NULL)
}

# LOAD DATA ----

script_dir <- script_directory()
if (!requireNamespace("xml2", quietly = TRUE)) {
  stop("Install the xml2 package before running this script")
}
if (!requireNamespace("renderSbgnR", quietly = TRUE)) {
  stop("Install cannin/render_sbgn_r before running this script")
}
suppressPackageStartupMessages(library(xml2))
source(file.path(script_dir, "R", "fcose_layout.R"))
source(file.path(script_dir, "R", "sbgn_adapter.R"))
arguments <- commandArgs(trailingOnly = TRUE)
if (any(arguments %in% c("-h", "--help"))) {
  print_usage()
  quit(save = "no", status = 0)
}
configuration <- parse_cli_arguments(arguments)

# ANALYSIS ----

add_connection_and_layout(
  input_path = configuration$input,
  output_path = configuration$output,
  node_id = configuration$node_id,
  label = configuration$label,
  target_id = configuration$target_id,
  arc_id = configuration$arc_id,
  arc_class = configuration$arc_class,
  direction = configuration$direction,
  placement = configuration$placement,
  database_name = configuration$database_name,
  database_link = configuration$database_link,
  width = configuration$width,
  height = configuration$height,
  iterations = configuration$iterations
)

# PLOT ----

if (configuration$render) {
  output_stem <- tools::file_path_sans_ext(configuration$output)
  render_outputs(
    configuration$output,
    paste0(output_stem, ".png"),
    paste0(output_stem, ".svg")
  )
}
