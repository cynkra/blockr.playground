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

#' Setup reactive board theme sync
#'
#' Returns a reactive that tracks the board's echart_theme.
#' Call this in the block's server function to get a reactive that updates
#' when the sidebar theme changes.
#'
#' @param session Shiny session (ignored, uses blockr.core::get_session())
#' @return A reactive containing the current board theme
#' @export
setup_board_theme_sync <- function(session = NULL) {
  reactive({
    # Use blockr.core::get_session() to get the correct session with board options
    sess <- blockr.core::get_session()
    blockr.core::get_board_option_or_null("echart_theme", sess) %||% "default"
  })
}

#' ECharts theme board option
#'
#' Creates a board option for selecting the default ECharts theme.
#' This theme applies to all ECharts blocks on the board unless
#' overridden by individual block settings.
#'
#' @param value Default theme value
#' @param category Category for settings sidebar grouping
#' @param ... Additional arguments passed to new_board_option
#'
#' @export
new_echart_theme_option <- function(value = "default",
                                    category = "Chart options", ...) {
  blockr.core::new_board_option(
    id = "echart_theme",
    default = value,
    ui = function(id) {
      selectInput(
        NS(id, "echart_theme"),
        "ECharts Theme",
        choices = c(
          "Default" = "default",
          "Blockr" = "blockr",
          "Dark" = "dark",
          "Vintage" = "vintage",
          "Westeros" = "westeros",
          "Essos" = "essos",
          "Wonderland" = "wonderland",
          "Walden" = "walden",
          "Chalk" = "chalk",
          "Infographic" = "infographic",
          "Macarons" = "macarons",
          "Roma" = "roma",
          "Shine" = "shine",
          "Purple Passion" = "purple-passion"
        ),
        selected = value
      )
    },
    server = function(..., session) {
      observeEvent(
        blockr.core::get_board_option_or_null("echart_theme", session),
        {
          theme_val <- blockr.core::get_board_option_value("echart_theme", session)
          updateSelectInput(session, "echart_theme", selected = theme_val)
        }
      )
    },
    category = category,
    ...
  )
}
