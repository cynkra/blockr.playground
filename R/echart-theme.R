# Environment to track theme registration state
.theme_env <- new.env(parent = emptyenv())
.theme_env$registered <- FALSE

#' Register blockr ECharts theme
#'
#' Registers a colorblind-friendly theme for use with ECharts blocks.
#' This is called automatically when any echart block is rendered.
#'
#' To set a default theme for all echart blocks, use:
#' \code{options(blockr.echart_theme = "blockr")}
#'
#' @return A Shiny tagList containing the theme registration script,
#'   or NULL if already registered this session.
#'
#' @keywords internal
echart_theme_blockr <- function() {
  if (.theme_env$registered) {
    return(NULL)
  }
  .theme_env$registered <- TRUE

  echarts4r::e_theme_register(
    name = "blockr",
    theme = '{
      "color": ["#0072B2", "#D55E00", "#F0E442", "#009E73", "#56B4E9", "#E69F00", "#CC79A7"],
      "backgroundColor": "#ffffff",
      "textStyle": {"color": "#333333", "fontFamily": "Open Sans"}
    }'
  )
}
