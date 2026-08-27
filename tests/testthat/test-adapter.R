# PURPOSE ----
# Test the native R SBGN adapter.

test_that("adapter preserves CASP3 and adds PARP1 stimulation", {
  input_path <- tempfile(fileext = ".sbgn")
  output_path <- tempfile(fileext = ".sbgn")
  writeLines(
    c(
      '<?xml version="1.0"?>',
      '<sbgn xmlns="http://sbgn.org/libsbgn/0.3">',
      '<map language="activity flow">',
      '<bbox x="0" y="0" w="300" h="200"/>',
      '<glyph id="casp3" class="biological activity">',
      '<label text="CASP3"/><bbox x="100" y="80" w="46" h="17"/>',
      "</glyph></map></sbgn>"
    ),
    input_path
  )

  add_connection_and_layout(
    input_path,
    output_path,
    node_id = "parp1",
    label = "PARP1",
    target_id = "casp3",
    arc_id = "parp1_casp3",
    arc_class = "stimulation",
    iterations = 100
  )

  document <- xml2::read_xml(output_path)
  arc <- xml2::xml_find_first(
    document,
    ".//*[local-name()='arc' and @id='parp1_casp3']"
  )
  casp3_bbox <- xml2::xml_find_first(
    document,
    ".//*[local-name()='glyph' and @id='casp3']/*[local-name()='bbox']"
  )
  expect_equal(xml2::xml_attr(casp3_bbox, "x"), "100")
  expect_equal(xml2::xml_attr(casp3_bbox, "y"), "80")
  expect_equal(xml2::xml_attr(arc, "class"), "stimulation")
  expect_equal(xml2::xml_attr(arc, "source"), "parp1")
  expect_equal(xml2::xml_attr(arc, "target"), "casp3")
  expect_length(
    xml2::xml_find_all(arc, "./*[local-name()='start']"),
    1
  )
  expect_length(
    xml2::xml_find_all(arc, "./*[local-name()='end']"),
    1
  )
})

test_that("interaction class is configurable", {
  input_path <- tempfile(fileext = ".sbgn")
  output_path <- tempfile(fileext = ".sbgn")
  writeLines(
    c(
      '<?xml version="1.0"?>',
      '<sbgn xmlns="http://sbgn.org/libsbgn/0.3">',
      '<map language="activity flow">',
      '<glyph id="target" class="biological activity">',
      '<bbox x="100" y="80" w="46" h="17"/>',
      "</glyph></map></sbgn>"
    ),
    input_path
  )

  add_connection_and_layout(
    input_path,
    output_path,
    node_id = "source",
    label = "Source",
    target_id = "target",
    arc_id = "inhibition_arc",
    arc_class = "inhibition",
    iterations = 10
  )

  document <- xml2::read_xml(output_path)
  arc <- xml2::xml_find_first(
    document,
    ".//*[local-name()='arc' and @id='inhibition_arc']"
  )
  expect_equal(xml2::xml_attr(arc, "class"), "inhibition")
})

test_that("full layout uses native spectral initialization", {
  nodes <- data.frame(
    id = as.character(0:3),
    x = 0,
    y = 0,
    width = 30,
    height = 30
  )
  edges <- data.frame(
    source = as.character(0:2),
    target = as.character(1:3)
  )

  result <- layout_graph(
    nodes,
    edges,
    randomize = TRUE,
    iterations = 50
  )

  expect_length(unique(paste(result$x, result$y)), 4)
  expect_true(all(is.finite(result$x)))
  expect_true(all(is.finite(result$y)))
})
