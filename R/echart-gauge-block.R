#' ECharts Gauge Block
#'
#' A specialized block for creating gauge chart visualizations with ECharts.
#' Displays a single KPI value as a gauge meter (uses first row of data).
#'
#' @param value Column for numeric value to display
#' @param name Label text for the gauge (optional, can use a column or text)
#' @param min Minimum value for gauge scale (default 0)
#' @param max Maximum value for gauge scale (default 100)
#' @param title Chart title
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_gauge_block`.
#'
#' @examples
#' # Create a gauge block
#' new_echart_gauge_block(value = "score", name = "Satisfaction")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   kpi_data <- data.frame(score = 85, label = "Score")
#'   serve(new_echart_gauge_block(), list(data = kpi_data))
#' }
#'
#' @export
new_echart_gauge_block <- function(
    value = character(),
    name = character(),
    min = 0,
    max = 100,
    title = character(),
    theme = "default",
    ...
) {
  # Normalize text values - empty/NULL/NA becomes ""
  normalize_text <- function(val) {
    if (!isTruthy(val) || length(val) == 0) "" else val
  }

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
          r_value <- reactiveVal(value)
          r_name <- reactiveVal(normalize_text(name))
          r_min <- reactiveVal(min)
          r_max <- reactiveVal(max)
          r_title <- reactiveVal(normalize_text(title))
          r_theme <- reactiveVal(theme)

          # Sync with board theme option
          r_board_theme <- setup_board_theme_sync(session)

          # Observe input changes
          observeEvent(input$value, r_value(input$value))
          observeEvent(input$name, r_name(normalize_text(input$name)))
          observeEvent(input$min, r_min(input$min))
          observeEvent(input$max, r_max(input$max))
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column-dependent inputs
          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "value",
                choices = numeric_cols(),
                selected = r_value()
              )
            }
          )

          list(
            expr = reactive({
              # Get current values with safe defaults
              value_val <- r_value()
              name_val <- r_name()
              min_val <- r_min()
              max_val <- r_max()
              title_val <- r_title()
              theme_val <- r_theme()

              # Validate required fields
              if (!isTruthy(value_val) || length(value_val) == 0) {
                return(quote(NULL))
              }

              value_col <- value_val

              # Build title part
              has_title <- isTruthy(title_val) && nchar(title_val) > 0
              title_part <- if (has_title) {
                glue::glue(" |>\n    echarts4r::e_title(\"{title_val}\")")
              } else {
                ""
              }

              # Determine effective theme: block setting takes priority, then board option
              if (isTRUE(theme_val == "default")) {
                theme_val <- r_board_theme()
              }

              theme_part <- if (isTruthy(theme_val) && !isTRUE(theme_val == "default")) {
                glue::glue(" |>\n    echarts4r::e_theme(\"{theme_val}\")")
              } else {
                ""
              }

              # Build name string - escape quotes
              name_str <- gsub('"', '\\\\"', name_val)

              # Gauge needs value extracted from data and passed to e_gauge
              expr_text <- glue::glue("
local({{
  .val <- data[['{value_col}']][1]
  echarts4r::e_charts() |>
    echarts4r::e_gauge(.val, \"{name_str}\", min = {min_val}, max = {max_val}){title_part}{theme_part} |>
    echarts4r::e_text_style(fontFamily = 'Open Sans') |>
    echarts4r::e_tooltip()
}})
")

              parse(text = expr_text)[[1]]
            }),
            state = list(
              value = r_value,
              name = r_name,
              min = r_min,
              max = r_max,
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

            # Gauge Mappings Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Gauge Mappings",
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
                # Value
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("value"),
                    label = tags$span(
                      tags$strong("Value Column"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = value,
                    selected = value,
                    width = "100%"
                  )
                ),
                # Name (label)
                div(
                  class = "block-input-wrapper",
                  textInput(
                    inputId = ns("name"),
                    label = "Gauge Label",
                    value = normalize_text(name),
                    width = "100%"
                  )
                )
              )
            ),

            # Scale Section
            div(
              class = "block-section",
              tags$h4("Scale"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper",
                  numericInput(
                    inputId = ns("min"),
                    label = "Minimum",
                    value = min,
                    width = "100%"
                  )
                ),
                div(
                  class = "block-input-wrapper",
                  numericInput(
                    inputId = ns("max"),
                    label = "Maximum",
                    value = max,
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
    class = "echart_gauge_block",
    allow_empty_state = c("name", "title"),
    ...
  )
}

#' @rdname new_echart_gauge_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_gauge_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_gauge_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_gauge_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_gauge_block
#' @export
board_options.echart_gauge_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}

#' @rdname new_echart_gauge_block
#' @export
block_render_trigger.echart_gauge_block <- function(x, session = blockr.core::get_session()) {
  blockr.core::get_board_option_or_null("echart_theme", session)
}
