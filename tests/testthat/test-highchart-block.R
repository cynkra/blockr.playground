# Tests for new_highchart_block

test_that("highchart block constructor", {
  # Test basic constructor
  blk <- new_highchart_block()
  expect_s3_class(blk, c("highchart_block", "block"))

  # Test constructor with parameters
  blk <- new_highchart_block(type = "column", x = "cyl", y = "mpg")
  expect_s3_class(blk, c("highchart_block", "block"))

  # Test constructor with all chart types
  chart_types <- c("scatter", "line", "column", "area", "pie")
  for (ct in chart_types) {
    blk <- new_highchart_block(type = ct, x = "cyl")
    expect_s3_class(blk, c("highchart_block", "block"))
  }
})

test_that("highchart block with optional aesthetics", {
  # Test with color
  blk <- new_highchart_block(
    type = "scatter",
    x = "wt",
    y = "mpg",
    color = "cyl"
  )
  expect_s3_class(blk, c("highchart_block", "block"))

  # Test with size (scatter only)
  blk <- new_highchart_block(
    type = "scatter",
    x = "wt",
    y = "mpg",
    size = "hp"
  )
  expect_s3_class(blk, c("highchart_block", "block"))

  # Test with all aesthetics
  blk <- new_highchart_block(
    type = "scatter",
    x = "wt",
    y = "mpg",
    color = "cyl",
    size = "hp"
  )
  expect_s3_class(blk, c("highchart_block", "block"))
})

test_that("highchart block types have correct class hierarchy", {
  blk <- new_highchart_block()
  classes <- class(blk)

  # Should have highchart_block class

  expect_true("highchart_block" %in% classes)
  # Should have block class
  expect_true("block" %in% classes)
})
