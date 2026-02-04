#' ECharts Heatmap Block
#'
#' A specialized block for creating heatmap visualizations with ECharts.
#' Requires pre-aggregated data with categorical x/y columns and a numeric value.
#'
#' @param x Column for x-axis categories (rows)
#' @param y Column for y-axis categories (columns)
#' @param value Column for numeric values (determines color intensity)
#' @param title Chart title
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_heatmap_block`.
#'
#' @examples
#' # Create a heatmap block
#' new_echart_heatmap_block(x = "cyl", y = "gear", value = "avg_mpg")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   library(dplyr)
#'   agg_data <- mtcars |>
#'     group_by(cyl, gear) |>
#'     summarize(avg_mpg = mean(mpg), .groups = "drop")
#'   serve(new_echart_heatmap_block(), list(data = agg_data))
#' }
#'
#' @export
new_echart_heatmap_block <- function(
    x = character(),
    y = character(),
    value = character(),
    title = character(),
    theme = "default",
    ...
) {
  # Normalize aesthetic values - empty/NULL/NA becomes "(none)"
  normalize_aes <- function(val) {
    if (!isTruthy(val)) "(none)" else val
  }

  # Normalize text values - empty/NULL/NA becomes ""
  normalize_text <- function(val) {
    if (!isTruthy(val) || length(val) == 0) "" else val
  }

  # Available themes
  available_themes <- c(
    "default", "blockr", "dark", "vintage", "westeros", "essos",
    "wonderland", "walden", "chalk", "infographic",
    "macarons", "roma", "shine", "purple-passion"
  )

  blockr.core::new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))
          numeric_cols <- reactive({
            d <- data()
            names(d)[sapply(d, is.numeric)]
          })

          # Initialize reactive values
          r_x <- reactiveVal(x)
          r_y <- reactiveVal(normalize_aes(y))
          r_value <- reactiveVal(normalize_aes(value))
          r_title <- reactiveVal(normalize_text(title))
          r_theme <- reactiveVal(theme)

          # Observe input changes
          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(normalize_aes(input$y)))
          observeEvent(input$value, r_value(normalize_aes(input$value)))
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column-dependent inputs
          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "x",
                choices = cols(),
                selected = r_x()
              )
              updateSelectInput(
                session,
                inputId = "y",
                choices = c("(none)", cols()),
                selected = r_y()
              )
              updateSelectInput(
                session,
                inputId = "value",
                choices = c("(none)", numeric_cols()),
                selected = r_value()
              )
            }
          )

          list(
            expr = reactive({
              # Get current values with safe defaults
              x_val <- r_x()
              y_val <- r_y()
              value_val <- r_value()
              title_val <- r_title()
              theme_val <- r_theme()

              # Validate required fields (use isTRUE for safe comparison)
              if (!isTruthy(x_val) || length(x_val) == 0) {
                return(quote(NULL))
              }
              if (!isTruthy(y_val) || isTRUE(y_val == "(none)")) {
                return(quote(NULL))
              }
              if (!isTruthy(value_val) || isTRUE(value_val == "(none)")) {
                return(quote(NULL))
              }

              x_col <- x_val
              y_col <- y_val
              value_col_bt <- backtick_if_needed(value_val)

              # Build echarts4r expression for heatmap using local() to handle intermediate values
              # ECharts heatmaps need numeric 0-based indices for x/y positions
              has_title <- isTruthy(title_val) && nchar(title_val) > 0
              grid_top <- if (has_title) 60 else 40

              # Build the expression as a local block
              title_part <- if (has_title) {
                glue::glue(" |>\n  echarts4r::e_title(\"{title_val}\")")
              } else {
                ""
              }

              # Determine effective theme: block setting takes priority, then board option
              if (isTRUE(theme_val == "default")) {
                global_theme <- blockr.core::get_board_option_or_null("echart_theme", session) %||% "default"
                if (global_theme != "default") {
                  theme_val <- global_theme
                }
              }

              theme_part <- if (isTruthy(theme_val) && !isTRUE(theme_val == "default")) {
                glue::glue(" |>\n  echarts4r::e_theme(\"{theme_val}\")")
              } else {
                ""
              }

              expr_text <- glue::glue("
local({{
  .x_cats <- sort(unique(data[['{x_col}']]))
  .y_cats <- sort(unique(data[['{y_col}']]))
  data |>
    dplyr::mutate(
      .x_idx = match({backtick_if_needed(x_col)}, .x_cats) - 1,
      .y_idx = match({backtick_if_needed(y_col)}, .y_cats) - 1
    ) |>
    echarts4r::e_charts(.x_idx) |>
    echarts4r::e_heatmap(.y_idx, {value_col_bt}) |>
    echarts4r::e_visual_map({value_col_bt}) |>
    echarts4r::e_grid(left = 60, right = 80, top = {grid_top}, bottom = 60) |>
    echarts4r::e_x_axis(type = 'category', data = as.character(.x_cats), axisLabel = list(color = '#666')) |>
    echarts4r::e_y_axis(type = 'category', data = as.character(.y_cats), axisLabel = list(color = '#666')) |>
    echarts4r::e_legend(show = FALSE){title_part}{theme_part} |>
    echarts4r::e_text_style(fontFamily = 'Open Sans') |>
    echarts4r::e_tooltip()
}})
")

              parse(text = expr_text)[[1]]
            }),
            state = list(
              x = r_x,
              y = r_y,
              value = r_value,
              title = r_title,
              theme = r_theme
            )
          )
        }
      )
    },
    ui = function(id) {
      ns <- NS(id)

      tagList(
        shinyjs::useShinyjs(),

        div(
          class = "block-container",

          # Add responsive CSS
          block_responsive_css(),

          # Set container query context
          block_container_script(),

          # Form inputs
          div(
            class = "block-form-grid",

            # Heatmap Mappings Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Heatmap Mappings",
                tags$small(
                  tags$span("*", style = "color: #dc3545; font-weight: bold;"),
                  " Required field",
                  style = paste(
                    "font-size: 0.7em; color: #6c757d;",
                    "font-weight: normal;"
                  )
                )
              ),
              div(
                class = "block-section-grid",
                # X-axis (rows)
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("x"),
                    label = tags$span(
                      tags$strong("X Category"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = x,
                    selected = x,
                    width = "100%"
                  )
                ),
                # Y-axis (columns)
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("y"),
                    label = tags$span(
                      tags$strong("Y Category"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", y),
                    selected = normalize_aes(y),
                    width = "100%"
                  )
                ),
                # Value (color)
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("value"),
                    label = tags$span(
                      tags$strong("Value"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", value),
                    selected = normalize_aes(value),
                    width = "100%"
                  )
                )
              )
            ),

            # Options Section
            div(
              class = "block-section",
              tags$h4("Options"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  textInput(
                    inputId = ns("title"),
                    label = "Title",
                    value = normalize_text(title),
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("theme"),
                    label = "Theme",
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
                    selected = theme,
                    width = "100%"
                  )
                )
              )
            )
          )
        )
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    class = "echart_heatmap_block",
    allow_empty_state = c("y", "value", "title"),
    ...
  )
}

#' @rdname new_echart_heatmap_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_heatmap_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_heatmap_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_heatmap_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_heatmap_block
#' @export
board_options.echart_heatmap_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}
