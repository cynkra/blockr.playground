# Demo: Hierarchy Filter Block
# Shows a sunburst/treemap/tree chart that filters downstream data

options(shiny.port = 7860L, shiny.host = "0.0.0.0")
library(blockr.core)
library(blockr.dplyr)
pkgload::load_all("blockr.playground")

# Financial hierarchy data
accounts <- data.frame(
  asset_class = c(
    "Liquid", "Liquid", "Liquid",
    "Illiquid", "Illiquid", "Illiquid"
  ),
  asset_type = c(
    "Bank", "Bank", "Cash",
    "Real Estate", "Private Equity", "Private Equity"
  ),
  account = c(
    "Checking", "Savings", "Wallet",
    "Property A", "Fund X", "Fund Y"
  ),
  value = c(50000, 120000, 5000, 450000, 200000, 150000)
)

serve(
  new_board(
    blocks = c(
      data = new_static_block(data = accounts),

      hierarchy = new_echart_hierarchy_block(
        levels = c("asset_class", "asset_type", "account"),
        value = "value",
        chart_type = "sunburst",
        title = "Portfolio Hierarchy"
      ),

      # Downstream head block — shows filtered rows
      result = new_head_block()
    ),
    links = c(
      new_link("data", "hierarchy", "data"),
      new_link("hierarchy", "result", "data")
    )
  ),
)
