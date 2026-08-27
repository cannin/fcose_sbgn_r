#!/usr/bin/env Rscript

# PURPOSE ----
# Run the PARP1-to-CASP3 SBGN adapter and render the generated pathway.

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

input_path <- file.path(script_dir, "inst", "extdata", "hsa05210_2018.sbgn")
output_dir <- file.path(script_dir, "output")
output_sbgn <- file.path(output_dir, "hsa05210_2018_parp1_casp3_r.sbgn")
output_png <- file.path(output_dir, "hsa05210_2018_parp1_casp3_r.png")
output_svg <- file.path(output_dir, "hsa05210_2018_parp1_casp3_r.svg")

# ANALYSIS ----

add_connection_and_layout(
  input_path = input_path,
  output_path = output_sbgn,
  node_id = "glyph_parp1",
  label = "PARP1",
  target_id = "glyph_2",
  arc_id = "arc_parp1_casp3",
  arc_class = "stimulation",
  direction = "new-to-existing",
  placement = "left",
  database_name = "hsa:142",
  database_link = "https://www.kegg.jp/entry/hsa:142",
  iterations = 100
)

# PLOT ----

render_outputs(
  output_sbgn,
  output_png,
  output_svg
)

message("Wrote ", output_sbgn)
message("Wrote ", output_png)
message("Wrote ", output_svg)
