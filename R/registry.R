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
  register_blocks(
    "new_plotly_block",
    name = "Plotly",
    description = "Create interactive scientific visualizations with Plotly",
    category = "plot",
    icon = "bar-chart-line",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_apexchart_block",
    name = "ApexCharts",
    description = "Create modern animated visualizations with ApexCharts",
    category = "plot",
    icon = "bar-chart-line",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_heatmap_block",
    name = "EChart Heatmap",
    description = "Create heatmap visualizations with Apache ECharts",
    category = "plot",
    icon = "grid-3x3",
    package = utils::packageName(),
    overwrite = TRUE
  )
  register_blocks(
    "new_echart_radar_block",
    name = "EChart Radar",
    description = "Create radar chart visualizations with Apache ECharts",
    category = "plot",
    icon = "bullseye",
    package = utils::packageName(),
    overwrite = TRUE
  )
} # nocov end
