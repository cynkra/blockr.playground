# Debug script - test candlestick block

library(blockr)
pkgload::load_all()

candlestick_df <- data.frame(
  date = seq(as.Date("2024-01-01"), as.Date("2024-01-10"), by = "day"),
  open = c(100, 102, 101, 103, 102, 104, 103, 105, 104, 106),
  close = c(102, 101, 103, 102, 104, 103, 105, 104, 106, 105),
  low = c(99, 100, 100, 101, 101, 102, 102, 103, 103, 104),
  high = c(103, 103, 104, 104, 105, 105, 106, 106, 107, 107)
)

run_app(
  blocks = c(
    data = new_static_block(data = candlestick_df),
    chart = new_echart_candlestick_block(
      date = "date",
      open = "open",
      close = "close",
      low = "low",
      high = "high"
    )
  ),
  links = c(new_link("data", "chart", "data"))
)
