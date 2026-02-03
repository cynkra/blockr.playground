#' Register playground blocks
#'
#' Registers all blocks from the blockr.playground package.
#'
#' @return NULL (invisibly)
#' @export
register_playground_blocks <- function() {
  # nocov start
  register_blocks(
    "new_highchart_block",
    name = "Highchart",
    description = paste0(
      "Create interactive visualizations including scatter plots, ",
      "line charts, column charts, area charts, and pie charts"
    ),
    category = "plot",
    icon = "bar-chart-line",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_block",
    name = "EChart",
    description = "Create interactive visualizations with Apache ECharts",
    category = "plot",
    icon = "bar-chart-line",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_chartjs_block",
    name = "Chart.js",
    description = "Create interactive visualizations with Chart.js",
    category = "plot",
    icon = "bar-chart-line",
    package = utils::packageName(),
    overwrite = TRUE
  )
} # nocov end
