# Debug script - test just the Sankey block with blockr

library(blockr)
pkgload::load_all()

# Simple sankey data
sankey_df <- data.frame(
  source = c("A", "A", "B"),
  target = c("B", "C", "C"),
  value = c(10, 20, 15)
)

# Minimal test - just one data block and one chart block
run_app(
  blocks = c(
    data = new_static_block(data = sankey_df),
    chart = new_echart_sankey_block(
      source = "source",
      target = "target",
      value = "value"
    )
  ),
  links = c(
    new_link("data", "chart", "data")
  )
)
