# Debug script - test each specialized block individually
# Uncomment the block you want to test

library(blockr)
pkgload::load_all()

# ============================================================
# DATA
# ============================================================

sankey_df <- data.frame(
  source = c("A", "A", "B"),
  target = c("B", "C", "C"),
  value = c(10, 20, 15)
)

gauge_df <- data.frame(score = 85)

candlestick_df <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-01-10"), by = "day"),
  open = c(100, 102, 101, 103, 102, 104, 103, 105, 104, 106),
  close = c(102, 101, 103, 102, 104, 103, 105, 104, 106, 105),
  low = c(99, 100, 100, 101, 101, 102, 102, 103, 103, 104),
  high = c(103, 103, 104, 104, 105, 105, 106, 106, 107, 107)
)

set.seed(42)
calendar_df <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-03-31"), by = "day"),
  commits = sample(0:10, 91, replace = TRUE)
)

treemap_df <- data.frame(
  category = c("Electronics", "Clothing", "Food", "Books"),
  sales = c(12000, 8000, 6000, 4000)
)

# ============================================================
# TEST BLOCKS - uncomment one at a time
# ============================================================

# 1. SANKEY
# run_app(
#   blocks = c(
#     data = new_static_block(data = sankey_df),
#     chart = new_echart_sankey_block(source = "source", target = "target", value = "value")
#   ),
#   links = c(new_link("data", "chart", "data"))
# )

# run_app(
  # blocks = c(
    # data = new_static_block(data = gauge_df),
    # chart = new_echart_gauge_block(value = "score", name = "Score", min = 0, max = 100)
  # ),
  # links = c(new_link("data", "chart", "data"))
# )

# 3. CANDLESTICK
# run_app(
#   blocks = c(
#     data = new_static_block(data = candlestick_df),
#     chart = new_echart_candlestick_block(date = "date", open = "open", close = "close", low = "low", high = "high")
#   ),
#   links = c(new_link("data", "chart", "data"))
# )

# 4. CALENDAR
# run_app(
#   blocks = c(
#     data = new_static_block(data = calendar_df),
#     chart = new_echart_calendar_block(date = "date", value = "commits")
#   ),
#   links = c(new_link("data", "chart", "data"))
# )

# 5. TREEMAP
# run_app(
#   blocks = c(
#     data = new_static_block(data = treemap_df),
#     chart = new_echart_treemap_block(name = "category", value = "sales")
#   ),
#   links = c(new_link("data", "chart", "data"))
# )
