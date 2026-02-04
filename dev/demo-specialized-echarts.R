# Demo workflow for specialized ECharts blocks in blockr.playground
# Showcases Sankey, Gauge, Candlestick, Calendar, and Treemap blocks

library(blockr)
pkgload::load_all()

# ============================================================
# PREPARE DATA SOURCES
# ============================================================

# Sankey data
sankey_df <- data.frame(
  source = c("Visit", "Visit", "Browse", "Browse", "Cart", "Cart"),
  target = c("Browse", "Exit", "Cart", "Exit", "Purchase", "Abandon"),
  value = c(1000, 200, 600, 200, 400, 200)
)

# Gauge data
gauge_df <- data.frame(
  metric = "Satisfaction",
  score = 85
)

# Calendar data
set.seed(42)
calendar_df <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
  commits = sample(0:10, 91, replace = TRUE)
)

# Treemap data
treemap_df <- data.frame(
  category = c("Electronics", "Clothing", "Food", "Books"),
  sales = c(12000, 8000, 6000, 4000)
)

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

    # Charts
    sankey = new_echart_sankey_block(
      source = "source",
      target = "target",
      value = "value",
      title = "User Journey Flow"
    ),
    gauge = new_echart_gauge_block(
      value = "score",
      name = "Satisfaction",
      min = 0,
      max = 100,
      title = "Customer Satisfaction"
    ),
    calendar = new_echart_calendar_block(
      date = "date",
      value = "commits",
      title = "Daily Activity"
    ),
    treemap = new_echart_treemap_block(
      name = "category",
      value = "sales",
      title = "Sales by Category"
    )
  ),
  links = c(
    new_link("sankey_data", "sankey", "data"),
    new_link("gauge_data", "gauge", "data"),
    new_link("calendar_data", "calendar", "data"),
    new_link("treemap_data", "treemap", "data")
  )
)
