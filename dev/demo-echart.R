# Demo workflow for ECharts capabilities in blockr.playground
# Showcases all chart types, themes, and advanced options

library(blockr)
pkgload::load_all()

# Set default theme for all echart blocks (optional)
# Options: "default", "blockr", "infographic", "vintage", "dark", etc.
# "blockr" uses a colorblind-friendly palette
options(blockr.echart_theme = "blockr")

run_app(
  blocks = c(
    # ============================================================
    # DATA SOURCES
    # ============================================================

    # Iris - 3 species (shows first 3 colors of filled palette)
    iris_data = new_dataset_block(dataset = "iris"),

    # Orange - 5 trees (shows first 5 colors of line palette)
    orange_data = new_dataset_block(dataset = "Orange"),

    # mtcars - for aggregations
    mtcars_data = new_dataset_block(dataset = "mtcars"),

    # ============================================================
    # SCATTER PLOT - with color and size mapping
    # ============================================================
    # Shows: Blue, Vermilion, Yellow (filled palette)

    ec_scatter = new_echart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      size = "Petal.Length",
      title = "Iris Scatter Plot",
      subtitle = "Sepal dimensions by species"
    ),

    # ============================================================
    # LINE CHART - multiple series
    # ============================================================
    # Shows: Blue, Vermilion, Black, Green, Sky Blue (line palette)

    ec_line = new_echart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree",
      title = "Orange Tree Growth",
      legend_position = "right"
    ),

    # ============================================================
    # AREA CHART - multiple series
    # ============================================================
    # Shows: Blue, Vermilion, Black, Green, Sky Blue (line palette)

    ec_area = new_echart_block(
      type = "area",
      x = "age",
      y = "circumference",
      color = "Tree",
      theme = "walden"
    ),

    # ============================================================
    # BAR CHART - grouped bars
    # ============================================================
    # Aggregate mtcars by cyl and gear for grouped bars
    # Shows: Blue, Vermilion, Yellow (filled palette)

    mtcars_grouped = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = c("cyl", "gear")
    ),

    ec_bar_grouped = new_echart_block(
      type = "bar",
      x = "cyl",
      y = "avg_mpg",
      color = "gear",
      title = "Average MPG by Cylinders",
      y_scale = "zero"
    ),

    # Simple bar chart (single color - no legend)
    mtcars_simple = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = "cyl"
    ),

    ec_bar_simple = new_echart_block(
      type = "bar",
      x = "cyl",
      y = "avg_mpg",
      title = "Simple Bar (No Legend)",
      theme = "vintage"
    ),

    # ============================================================
    # PIE CHART - multiple slices
    # ============================================================
    # Shows: Blue, Vermilion, Yellow (filled palette)

    iris_count = new_summarize_block(
      summaries = list(
        count = list(func = "dplyr::n", col = "")
      ),
      by = "Species"
    ),

    ec_pie = new_echart_block(
      type = "pie",
      x = "Species",
      y = "count",
      title = "Iris Species Distribution",
      legend_position = "right"
    ),

    # ============================================================
    # BOXPLOT - statistical distribution
    # ============================================================

    ec_boxplot = new_echart_block(
      type = "boxplot",
      x = "Species",
      y = "Sepal.Length",
      title = "Sepal Length Distribution",
      subtitle = "By species"
    ),

    # ============================================================
    # HISTOGRAM - frequency distribution
    # ============================================================

    ec_histogram = new_echart_block(
      type = "histogram",
      x = "mpg",
      bins = 15,
      title = "MPG Distribution",
      theme = "infographic"
    ),

    # ============================================================
    # HEATMAP - using mtcars aggregated data (specialized block)
    # ============================================================

    heatmap_agg = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = c("cyl", "gear")
    ),

    ec_heatmap = new_echart_heatmap_block(
      x = "cyl",
      y = "gear",
      value = "avg_mpg",
      title = "MPG Heatmap (Cyl x Gear)"
    ),

    # ============================================================
    # RADAR CHART - comparing species across metrics (specialized block)
    # ============================================================
    # Radar block aggregates data internally

    ec_radar = new_echart_radar_block(
      group = "Species",
      metrics = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
      title = "Iris Measurements by Species"
    ),

    # ============================================================
    # THEMED EXAMPLES
    # ============================================================

    # Dark theme example
    ec_dark = new_echart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      title = "Dark Theme Example",
      theme = "dark"
    ),

    # Westeros theme
    ec_westeros = new_echart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree",
      title = "Westeros Theme",
      theme = "westeros"
    )
  ),
  links = c(
    # Scatter from iris
    new_link("iris_data", "ec_scatter", "data"),

    # Line and area from Orange (5 trees = 5 colors)
    new_link("orange_data", "ec_line", "data"),
    new_link("orange_data", "ec_area", "data"),

    # Grouped bar from mtcars
    new_link("mtcars_data", "mtcars_grouped", "data"),
    new_link("mtcars_grouped", "ec_bar_grouped", "data"),

    # Simple bar from mtcars
    new_link("mtcars_data", "mtcars_simple", "data"),
    new_link("mtcars_simple", "ec_bar_simple", "data"),

    # Pie from iris species count
    new_link("iris_data", "iris_count", "data"),
    new_link("iris_count", "ec_pie", "data"),

    # Boxplot from iris
    new_link("iris_data", "ec_boxplot", "data"),

    # Histogram from mtcars
    new_link("mtcars_data", "ec_histogram", "data"),

    # Heatmap from aggregated mtcars
    new_link("mtcars_data", "heatmap_agg", "data"),
    new_link("heatmap_agg", "ec_heatmap", "data"),

    # Radar directly from iris (radar block does its own aggregation)
    new_link("iris_data", "ec_radar", "data"),

    # Themed examples
    new_link("iris_data", "ec_dark", "data"),
    new_link("orange_data", "ec_westeros", "data")
  )
)
