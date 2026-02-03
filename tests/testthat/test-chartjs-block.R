# Tests for new_chartjs_block

test_that("chartjs block constructor", {
  # Test basic constructor
  blk <- new_chartjs_block()
  expect_s3_class(blk, c("chartjs_block", "block"))

  # Test constructor with parameters
  blk <- new_chartjs_block(type = "bar", x = "cyl", y = "mpg")
  expect_s3_class(blk, c("chartjs_block", "block"))

  # Test constructor with all chart types
  chart_types <- c("bar", "horizontalBar", "line", "pie", "doughnut", "radar", "polar")
  for (ct in chart_types) {
    blk <- new_chartjs_block(type = ct, x = "cyl", y = "mpg")
    expect_s3_class(blk, c("chartjs_block", "block"))
  }
})

test_that("chartjs block types have correct class hierarchy", {
  blk <- new_chartjs_block()
  classes <- class(blk)

  # Should have chartjs_block class
  expect_true("chartjs_block" %in% classes)
  # Should have block class
  expect_true("block" %in% classes)
})
