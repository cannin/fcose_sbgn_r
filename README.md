# fcose_sbgn_r

> [!IMPORTANT]
> This repository is archived. Active development has moved to
> [cannin/render_sbgn](https://github.com/cannin/fcose_sbgn). The R
> implementation is now maintained in the monorepo's
> [r directory](https://github.com/cannin/fcose_sbgn/tree/main/r).


Native R SBGN-ML adapter with spectral initialization and incremental
force-directed layout. It preserves existing glyph positions while adding and
placing a new connected glyph. It does not call the Rust, Go, or Python
implementations.

The GitHub repository uses `fcose_sbgn_r`; the installed R package uses
`fcoseSbgnR` because R package names cannot contain underscores.

## Installation

Install the package and its R renderer dependency directly from GitHub:

```r
install.packages("remotes")
remotes::install_github(
  "cannin/fcose_sbgn_r",
  upgrade = "never"
)
```

## Quick start

Clone the repository to use its bundled colorectal-cancer pathway:

```sh
git clone https://github.com/cannin/fcose_sbgn_r.git
cd fcose_sbgn_r
Rscript -e 'install.packages("remotes"); remotes::install_deps(dependencies = NA)'
Rscript fcose_sbgn_r.R \
  inst/extdata/hsa05210_2018.sbgn \
  --output output/hsa05210_2018_parp1_casp3_r.sbgn \
  --node-id glyph_parp1 \
  --label PARP1 \
  --target-id glyph_2 \
  --arc-id arc_parp1_casp3 \
  --arc-class stimulation \
  --direction new-to-existing \
  --placement left \
  --database-name hsa:142 \
  --database-link https://www.kegg.jp/entry/hsa:142
```

The command writes SBGN, PNG, and SVG files under `output/`. In the bundled
input, `glyph_2` is CASP3. All 86 existing glyphs remain fixed; only the new
PARP1 glyph is positioned by the incremental layout.

## R API

The installed package includes the example input, so this example does not
depend on another checkout:

```r
library(fcoseSbgnR)

source_path <- system.file(
  "extdata",
  "hsa05210_2018.sbgn",
  package = "fcoseSbgnR"
)
output_path <- file.path("output", "with_parp1.sbgn")

add_connection_and_layout(
  input_path = source_path,
  output_path = output_path,
  node_id = "glyph_parp1",
  label = "PARP1",
  target_id = "glyph_2", # Existing CASP3
  arc_id = "arc_parp1_casp3",
  arc_class = "stimulation",
  placement = "left",
  iterations = 100
)

render_outputs(
  output_path,
  sub("\\.sbgn$", ".png", output_path),
  sub("\\.sbgn$", ".svg", output_path)
)
```

`arc_class` accepts SBGN interaction classes such as `stimulation`,
`inhibition`, `positive influence`, `negative influence`, `catalysis`, and
`modulation`. Set `direction = "existing-to-new"` to reverse the interaction.

## Dependencies

- R 4.2 or newer
- `xml2` for SBGN-ML parsing and editing
- [`renderSbgnR`](https://github.com/cannin/render_sbgn_r) for PNG and SVG
  rendering

The dependencies are all R packages; no Rust, Go, or Python runtime is used.

## Development

```sh
Rscript -e 'devtools::test()'
Rscript -e 'lintr::lint_package()'
R CMD build .
```
