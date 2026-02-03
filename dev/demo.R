# Demo workflow for blockr.playground
# Shows Highchart and EChart block types with appropriate data

library(blockr)
pkgload::load_all()

run_app(
  blocks = c(
    # ============================================================
    # HIGHCHARTS EXAMPLES
    # ============================================================

    # === SCATTER PLOT (Highcharts) ===
    # Iris data - perfect for scatter plots
    iris_data = new_dataset_block(dataset = "iris"),

    hc_scatter = new_highchart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species"
    ),

    # === LINE CHART (Highcharts) ===
    # Orange tree growth - age vs circumference
    orange_data = new_dataset_block(dataset = "Orange"),

    hc_line = new_highchart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === AREA CHART (Highcharts) ===
    # Orange tree growth - same data, different view
    hc_area = new_highchart_block(
      type = "area",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === COLUMN CHART (Highcharts) ===
    # Use mtcars, aggregate by cylinders
    mtcars_data = new_dataset_block(dataset = "mtcars"),

    mtcars_summary = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = "cyl"
    ),

    hc_column = new_highchart_block(
      type = "column",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === PIE CHART (Highcharts) ===
    # Count by species for pie
    iris_count = new_summarize_block(
      summaries = list(
        count = list(func = "dplyr::n", col = "")
      ),
      by = "Species"
    ),

    hc_pie = new_highchart_block(
      type = "pie",
      x = "Species",
      y = "count"
    ),

    # ============================================================
    # ECHARTS EXAMPLES
    # ============================================================

    # === SCATTER PLOT (ECharts) ===
    ec_scatter = new_echart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species"
    ),

    # === LINE CHART (ECharts) ===
    ec_line = new_echart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === AREA CHART (ECharts) ===
    ec_area = new_echart_block(
      type = "area",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === BAR CHART (ECharts) ===
    ec_bar = new_echart_block(
      type = "bar",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === PIE CHART (ECharts) ===
    ec_pie = new_echart_block(
      type = "pie",
      x = "Species",
      y = "count"
    )
  ),
  links = c(
    # Highcharts scatter from iris
    new_link("iris_data", "hc_scatter", "data"),

    # Highcharts line and area from Orange
    new_link("orange_data", "hc_line", "data"),
    new_link("orange_data", "hc_area", "data"),

    # Highcharts column from mtcars summary
    new_link("mtcars_data", "mtcars_summary", "data"),
    new_link("mtcars_summary", "hc_column", "data"),

    # Highcharts pie from iris count
    new_link("iris_data", "iris_count", "data"),
    new_link("iris_count", "hc_pie", "data"),

    # ECharts scatter from iris
    new_link("iris_data", "ec_scatter", "data"),

    # ECharts line and area from Orange
    new_link("orange_data", "ec_line", "data"),
    new_link("orange_data", "ec_area", "data"),

    # ECharts bar from mtcars summary
    new_link("mtcars_summary", "ec_bar", "data"),

    # ECharts pie from iris count
    new_link("iris_count", "ec_pie", "data")
  )
)
