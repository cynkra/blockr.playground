# Demo: Specialized ECharts blocks in blockr.playground

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

# ============================================================
# RUN APP
# ============================================================

run_app(
  blocks = c(
    # Data sources
    sankey_data = new_static_block(data = sankey_df),
    gauge_data = new_static_block(data = gauge_df),
    calendar_data = new_static_block(data = calendar_df),
    treemap_data = new_static_block(data = treemap_df),
    heatmap_data = new_static_block(data = heatmap_df),
    iris_data = new_dataset_block(dataset = "iris"),

    # Sankey
    sankey = new_echart_sankey_block(
      source = "source",
      target = "target",
      value = "value",
      title = "User Journey"
    ),

    # Gauge
    gauge = new_echart_gauge_block(
      value = "score",
      name = "NPS Score",
      min = 0,
      max = 100
    ),

    # Calendar
    calendar = new_echart_calendar_block(
      date = "date",
      value = "commits",
      title = "Daily Commits"
    ),

    # Treemap
    treemap = new_echart_treemap_block(
      name = "category",
      value = "sales",
      title = "Sales by Category"
    ),

    # Heatmap
    heatmap = new_echart_heatmap_block(
      x = "cyl",
      y = "gear",
      value = "avg_mpg",
      title = "MPG by Cyl & Gear"
    ),

    # Radar
    radar = new_echart_radar_block(
      group = "Species",
      metrics = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
      title = "Iris Comparison"
    )
  ),
  links = c(
    new_link("sankey_data", "sankey", "data"),
    new_link("gauge_data", "gauge", "data"),
    new_link("calendar_data", "calendar", "data"),
    new_link("treemap_data", "treemap", "data"),
    new_link("heatmap_data", "heatmap", "data"),
    new_link("iris_data", "radar", "data")
  )
)
