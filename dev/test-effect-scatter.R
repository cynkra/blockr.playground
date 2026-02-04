# Test effect scatter animation
library(blockr)
pkgload::load_all()

run_app(
  blocks = c(
    data = new_dataset_block(dataset = "iris"),
    chart = new_echart_block(
      type = "effect_scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      ripple = TRUE,
      title = "Effect Scatter Test"
    )
  ),
  links = c(
    new_link("data", "chart", "data")
  )
)
