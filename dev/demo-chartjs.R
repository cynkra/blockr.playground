# Demo workflow for Chart.js blocks
# Shows all Chart.js chart types

library(blockr)
pkgload::load_all()

run_app(
  blocks = c(
    # Data sources
    mtcars_data = new_dataset_block(dataset = "mtcars"),

    mtcars_summary = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = "cyl"
    ),

    orange_data = new_dataset_block(dataset = "Orange"),

    iris_data = new_dataset_block(dataset = "iris"),

    iris_count = new_summarize_block(
      summaries = list(
        count = list(func = "dplyr::n", col = "")
      ),
      by = "Species"
    ),

    # ============================================================
    # CHART.JS EXAMPLES
    # ============================================================

    # === BAR CHART (Chart.js) ===
    cjs_bar = new_chartjs_block(
      type = "bar",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === HORIZONTAL BAR CHART (Chart.js) ===
    cjs_hbar = new_chartjs_block(
      type = "horizontalBar",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === LINE CHART (Chart.js) ===
    cjs_line = new_chartjs_block(
      type = "line",
      x = "age",
      y = "circumference"
    ),

    # === PIE CHART (Chart.js) ===
    cjs_pie = new_chartjs_block(
      type = "pie",
      x = "Species",
      y = "count"
    ),

    # === DOUGHNUT CHART (Chart.js) ===
    cjs_doughnut = new_chartjs_block(
      type = "doughnut",
      x = "Species",
      y = "count"
    ),

    # === RADAR CHART (Chart.js) ===
    cjs_radar = new_chartjs_block(
      type = "radar",
      x = "Species",
      y = "count"
    ),

    # === POLAR CHART (Chart.js) ===
    cjs_polar = new_chartjs_block(
      type = "polar",
      x = "Species",
      y = "count"
    )
  ),
  links = c(
    # mtcars summary chain
    new_link("mtcars_data", "mtcars_summary", "data"),

    # iris count chain
    new_link("iris_data", "iris_count", "data"),

    # Chart.js bar charts from mtcars summary
    new_link("mtcars_summary", "cjs_bar", "data"),
    new_link("mtcars_summary", "cjs_hbar", "data"),

    # Chart.js line from Orange
    new_link("orange_data", "cjs_line", "data"),

    # Chart.js pie/doughnut/radar/polar from iris count
    new_link("iris_count", "cjs_pie", "data"),
    new_link("iris_count", "cjs_doughnut", "data"),
    new_link("iris_count", "cjs_radar", "data"),
    new_link("iris_count", "cjs_polar", "data")
  )
)
