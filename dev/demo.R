# Demo workflow for blockr.playground
# Shows all Highchart block types with appropriate data

library(blockr)
pkgload::load_all()

run_app(
  blocks = c(
    # === SCATTER PLOT ===
    # Iris data - perfect for scatter plots
    iris_data = new_dataset_block(dataset = "iris"),

    scatter = new_highchart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species"
    ),

    # === LINE CHART ===
    # Orange tree growth - age vs circumference
    orange_data = new_dataset_block(dataset = "Orange"),

    line = new_highchart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === AREA CHART ===
    # Orange tree growth - same data, different view
    area = new_highchart_block(
      type = "area",
      x = "age",
      y = "circumference"
    ),

    # === COLUMN CHART ===
    # Use mtcars, aggregate by cylinders
    mtcars_data = new_dataset_block(dataset = "mtcars"),

    mtcars_summary = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = "cyl"
    ),

    column = new_highchart_block(
      type = "column",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === PIE CHART ===
    # Count by species for pie
    iris_count = new_summarize_block(
      summaries = list(
        count = list(func = "dplyr::n", col = "")
      ),
      by = "Species"
    ),

    pie = new_highchart_block(
      type = "pie",
      x = "Species",
      y = "count"
    )
  ),
  links = c(
    # Scatter from iris
    new_link("iris_data", "scatter", "data"),

    # Line and area from Orange
    new_link("orange_data", "line", "data"),
    new_link("orange_data", "area", "data"),

    # Column from mtcars summary
    new_link("mtcars_data", "mtcars_summary", "data"),
    new_link("mtcars_summary", "column", "data"),

    # Pie from iris count
    new_link("iris_data", "iris_count", "data"),
    new_link("iris_count", "pie", "data")
  )
)
