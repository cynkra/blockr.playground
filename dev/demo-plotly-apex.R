# Demo workflow for Plotly and ApexCharts blocks
# Shows all chart types from both libraries

library(blockr)
pkgload::load_all()

run_app(
  blocks = c(
    # ============================================================
    # DATA SOURCES
    # ============================================================

    # Iris data - good for scatter plots
    iris_data = new_dataset_block(dataset = "iris"),

    # Orange tree growth - good for line/area charts
    orange_data = new_dataset_block(dataset = "Orange"),

    # mtcars with summary - good for bar charts
    mtcars_data = new_dataset_block(dataset = "mtcars"),

    mtcars_summary = new_summarize_block(
      summaries = list(
        avg_mpg = list(func = "mean", col = "mpg")
      ),
      by = "cyl"
    ),

    # Iris count - good for pie/donut charts
    iris_count = new_summarize_block(
      summaries = list(
        count = list(func = "dplyr::n", col = "")
      ),
      by = "Species"
    ),

    # ============================================================
    # PLOTLY EXAMPLES
    # ============================================================

    # === SCATTER PLOT (Plotly) ===
    plotly_scatter = new_plotly_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species"
    ),

    # === LINE CHART (Plotly) ===
    plotly_line = new_plotly_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === BAR CHART (Plotly) ===
    plotly_bar = new_plotly_block(
      type = "bar",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === AREA CHART (Plotly) ===
    plotly_area = new_plotly_block(
      type = "area",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === PIE CHART (Plotly) ===
    plotly_pie = new_plotly_block(
      type = "pie",
      x = "Species",
      y = "count"
    ),

    # ============================================================
    # APEXCHARTS EXAMPLES
    # ============================================================

    # === SCATTER PLOT (ApexCharts) ===
    apex_scatter = new_apexchart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species"
    ),

    # === LINE CHART (ApexCharts) ===
    apex_line = new_apexchart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === BAR CHART (ApexCharts) ===
    apex_bar = new_apexchart_block(
      type = "bar",
      x = "cyl",
      y = "avg_mpg"
    ),

    # === AREA CHART (ApexCharts) ===
    apex_area = new_apexchart_block(
      type = "area",
      x = "age",
      y = "circumference",
      color = "Tree"
    ),

    # === PIE CHART (ApexCharts) ===
    apex_pie = new_apexchart_block(
      type = "pie",
      x = "Species",
      y = "count"
    ),

    # === DONUT CHART (ApexCharts) ===
    apex_donut = new_apexchart_block(
      type = "donut",
      x = "Species",
      y = "count"
    ),

    # === RADAR CHART (ApexCharts) ===
    apex_radar = new_apexchart_block(
      type = "radar",
      x = "Species",
      y = "count"
    )
  ),
  links = c(
    # Data pipeline links
    new_link("mtcars_data", "mtcars_summary", "data"),
    new_link("iris_data", "iris_count", "data"),

    # Plotly scatter from iris
    new_link("iris_data", "plotly_scatter", "data"),

    # Plotly line and area from Orange
    new_link("orange_data", "plotly_line", "data"),
    new_link("orange_data", "plotly_area", "data"),

    # Plotly bar from mtcars summary
    new_link("mtcars_summary", "plotly_bar", "data"),

    # Plotly pie from iris count
    new_link("iris_count", "plotly_pie", "data"),

    # ApexCharts scatter from iris
    new_link("iris_data", "apex_scatter", "data"),

    # ApexCharts line and area from Orange
    new_link("orange_data", "apex_line", "data"),
    new_link("orange_data", "apex_area", "data"),

    # ApexCharts bar from mtcars summary
    new_link("mtcars_summary", "apex_bar", "data"),

    # ApexCharts pie/donut/radar from iris count
    new_link("iris_count", "apex_pie", "data"),
    new_link("iris_count", "apex_donut", "data"),
    new_link("iris_count", "apex_radar", "data")
  )
)
