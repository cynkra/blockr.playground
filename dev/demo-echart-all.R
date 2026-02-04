# Demo: All ECharts capabilities in blockr.playground

library(blockr)
pkgload::load_all()

# ============================================================
# DATA
# ============================================================

# Sankey - user flow
sankey_df <- data.frame(
  source = c("Visit", "Visit", "Browse", "Browse", "Cart", "Cart"),
  target = c("Browse", "Exit", "Cart", "Exit", "Purchase", "Abandon"),
  value = c(1000, 200, 600, 200, 400, 200)
)

# Gauge - KPI
gauge_df <- data.frame(score = 78)

# Calendar - daily activity
set.seed(42)
calendar_df <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
  commits = sample(0:12, 91, replace = TRUE)
)

# Treemap - sales
treemap_df <- data.frame(
  category = c("Electronics", "Clothing", "Food", "Books", "Home", "Sports"),
  sales = c(12000, 8000, 6000, 4000, 5000, 3000)
)

# Heatmap - aggregated mtcars
heatmap_df <- mtcars |>
  dplyr::group_by(cyl, gear) |>
  dplyr::summarize(avg_mpg = mean(mpg), .groups = "drop")

# Funnel data
funnel_df <- data.frame(
  stage = c("Visits", "Signups", "Trials", "Paid"),
  count = c(1000, 400, 200, 80)
)

# ============================================================
# RUN APP
# ============================================================

run_app(
  blocks = c(
    # === DATA SOURCES ===
    iris_data = new_dataset_block(dataset = "iris"),
    mtcars_data = new_dataset_block(dataset = "mtcars"),
    orange_data = new_dataset_block(dataset = "Orange"),
    funnel_data = new_static_block(data = funnel_df),
    sankey_data = new_static_block(data = sankey_df),
    gauge_data = new_static_block(data = gauge_df),
    calendar_data = new_static_block(data = calendar_df),
    treemap_data = new_static_block(data = treemap_df),
    heatmap_data = new_static_block(data = heatmap_df),

    # === BASIC CHART TYPES ===

    # Scatter
    scatter = new_echart_block(
      type = "scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      title = "Scatter Plot"
    ),

    # Line
    line = new_echart_block(
      type = "line",
      x = "age",
      y = "circumference",
      color = "Tree",
      title = "Line Chart"
    ),

    # Bar
    bar = new_echart_block(
      type = "bar",
      x = "cyl",
      y = "mpg",
      title = "Bar Chart"
    ),

    # Area
    area = new_echart_block(
      type = "area",
      x = "age",
      y = "circumference",
      color = "Tree",
      title = "Area Chart"
    ),

    # Pie
    pie = new_echart_block(
      type = "pie",
      x = "stage",
      y = "count",
      title = "Pie Chart"
    ),

    # Boxplot
    boxplot = new_echart_block(
      type = "boxplot",
      x = "Species",
      y = "Sepal.Length",
      title = "Box Plot"
    ),

    # Histogram
    histogram = new_echart_block(
      type = "histogram",
      x = "mpg",
      bins = 15,
      title = "Histogram"
    ),

    # Density
    density = new_echart_block(
      type = "density",
      x = "mpg",
      title = "Density Plot"
    ),

    # Funnel
    funnel = new_echart_block(
      type = "funnel",
      x = "stage",
      y = "count",
      title = "Funnel Chart"
    ),

    # Step
    step = new_echart_block(
      type = "step",
      x = "age",
      y = "circumference",
      color = "Tree",
      title = "Step Chart"
    ),

    # Effect scatter (animated)
    effect = new_echart_block(
      type = "effect_scatter",
      x = "Sepal.Length",
      y = "Sepal.Width",
      color = "Species",
      title = "Effect Scatter"
    ),

    # === SPECIALIZED BLOCKS ===

    # Sankey - flow visualization
    sankey = new_echart_sankey_block(
      source = "source",
      target = "target",
      value = "value",
      title = "User Journey"
    ),

    # Gauge - single KPI
    gauge = new_echart_gauge_block(
      value = "score",
      name = "NPS Score",
      min = 0,
      max = 100
    ),

    # Calendar - activity heatmap
    calendar = new_echart_calendar_block(
      date = "date",
      value = "commits",
      title = "Daily Commits"
    ),

    # Treemap - hierarchical
    treemap = new_echart_treemap_block(
      name = "category",
      value = "sales",
      title = "Sales by Category"
    ),

    # Heatmap - matrix
    heatmap = new_echart_heatmap_block(
      x = "cyl",
      y = "gear",
      value = "avg_mpg",
      title = "MPG Heatmap"
    ),

    # Radar - multi-metric comparison
    radar = new_echart_radar_block(
      group = "Species",
      metrics = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
      title = "Iris Comparison"
    )
  ),
  links = c(
    # Basic - iris
    new_link("iris_data", "scatter", "data"),
    new_link("iris_data", "boxplot", "data"),
    new_link("iris_data", "effect", "data"),

    # Basic - mtcars
    new_link("mtcars_data", "bar", "data"),
    new_link("mtcars_data", "histogram", "data"),
    new_link("mtcars_data", "density", "data"),

    # Basic - Orange
    new_link("orange_data", "line", "data"),
    new_link("orange_data", "area", "data"),
    new_link("orange_data", "step", "data"),

    # Basic - funnel
    new_link("funnel_data", "pie", "data"),
    new_link("funnel_data", "funnel", "data"),

    # Specialized
    new_link("sankey_data", "sankey", "data"),
    new_link("gauge_data", "gauge", "data"),
    new_link("calendar_data", "calendar", "data"),
    new_link("treemap_data", "treemap", "data"),
    new_link("heatmap_data", "heatmap", "data"),
    new_link("iris_data", "radar", "data")
  )
)
