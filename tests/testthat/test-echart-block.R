# Tests for new_echart_block

test_that("echart block constructor", {
  # Test basic constructor
  blk <- new_echart_block()
  expect_s3_class(blk, c("echart_block", "block"))

  # Test constructor with parameters
  blk <- new_echart_block(type = "bar", x = "cyl", y = "mpg")
  expect_s3_class(blk, c("echart_block", "block"))

  # Test constructor with all chart types
  chart_types <- c("scatter", "line", "bar", "area", "pie")
  for (ct in chart_types) {
    blk <- new_echart_block(type = ct, x = "cyl")
    expect_s3_class(blk, c("echart_block", "block"))
  }
})

test_that("echart block with optional aesthetics", {
  # Test with color
  blk <- new_echart_block(
    type = "scatter",
    x = "wt",
    y = "mpg",
    color = "cyl"
  )
  expect_s3_class(blk, c("echart_block", "block"))

  # Test with size (scatter only)
  blk <- new_echart_block(
    type = "scatter",
    x = "wt",
    y = "mpg",
    size = "hp"
  )
  expect_s3_class(blk, c("echart_block", "block"))

  # Test with all aesthetics
  blk <- new_echart_block(
    type = "scatter",
    x = "wt",
    y = "mpg",
    color = "cyl",
    size = "hp"
  )
  expect_s3_class(blk, c("echart_block", "block"))
})

test_that("echart block types have correct class hierarchy", {
  blk <- new_echart_block()
  classes <- class(blk)

  # Should have echart_block class
  expect_true("echart_block" %in% classes)
  # Should have block class
  expect_true("block" %in% classes)
})
