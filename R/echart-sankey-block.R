#' ECharts Sankey Block
#'
#' A specialized block for creating Sankey diagram visualizations with ECharts.
#' Shows flow relationships between nodes with values indicating flow magnitude.
#'
#' @param source Column for source nodes
#' @param target Column for target nodes
#' @param value Column for flow values (numeric)
#' @param title Chart title
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_sankey_block`.
#'
#' @examples
#' # Create a Sankey block
#' new_echart_sankey_block(source = "from", target = "to", value = "count")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   flow_data <- data.frame(
#'     from = c("A", "A", "B", "B"),
#'     to = c("X", "Y", "X", "Y"),
#'     count = c(10, 20, 15, 25)
#'   )
#'   serve(new_echart_sankey_block(), list(data = flow_data))
#' }
#'
#' @export
new_echart_sankey_block <- function(
    source = character(),
    target = character(),
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
          r_source <- reactiveVal(source)
          r_target <- reactiveVal(normalize_aes(target))
          r_value <- reactiveVal(normalize_aes(value))
          r_title <- reactiveVal(normalize_text(title))
          r_theme <- reactiveVal(theme)

          # Sync with board theme option
          r_board_theme <- setup_board_theme_sync(session)

          # Observe input changes
          observeEvent(input$source, r_source(input$source))
          observeEvent(input$target, r_target(normalize_aes(input$target)))
          observeEvent(input$value, r_value(normalize_aes(input$value)))
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column-dependent inputs
          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "source",
                choices = cols(),
                selected = r_source()
              )
              updateSelectInput(
                session,
                inputId = "target",
                choices = c("(none)", cols()),
                selected = r_target()
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
              source_val <- r_source()
              target_val <- r_target()
              value_val <- r_value()
              title_val <- r_title()
              theme_val <- r_theme()

              # Validate required fields
              if (!isTruthy(source_val) || length(source_val) == 0) {
                return(quote(NULL))
              }
              if (!isTruthy(target_val) || isTRUE(target_val == "(none)")) {
                return(quote(NULL))
              }
              if (!isTruthy(value_val) || isTRUE(value_val == "(none)")) {
                return(quote(NULL))
              }

              source_col <- backtick_if_needed(source_val)
              target_col <- backtick_if_needed(target_val)
              value_col <- backtick_if_needed(value_val)

              # Build title part
              has_title <- isTruthy(title_val) && nchar(title_val) > 0
              title_part <- if (has_title) {
                glue::glue(" |>\n  echarts4r::e_title(\"{title_val}\")")
              } else {
                ""
              }

              # Determine effective theme: block setting takes priority, then board option
              if (isTRUE(theme_val == "default")) {
                theme_val <- r_board_theme()
              }

              theme_part <- if (isTruthy(theme_val) && !isTRUE(theme_val == "default")) {
                glue::glue(" |>\n  echarts4r::e_theme(\"{theme_val}\")")
              } else {
                ""
              }

              expr_text <- glue::glue("
data |>
  echarts4r::e_charts() |>
  echarts4r::e_sankey({source_col}, {target_col}, {value_col}){title_part}{theme_part} |>
  echarts4r::e_text_style(fontFamily = 'Open Sans') |>
  echarts4r::e_tooltip()
")

              parse(text = expr_text)[[1]]
            }),
            state = list(
              source = r_source,
              target = r_target,
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

            # Sankey Mappings Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Sankey Mappings",
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
                # Source
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("source"),
                    label = tags$span(
                      tags$strong("Source"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = source,
                    selected = source,
                    width = "100%"
                  )
                ),
                # Target
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("target"),
                    label = tags$span(
                      tags$strong("Target"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", target),
                    selected = normalize_aes(target),
                    width = "100%"
                  )
                ),
                # Value
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
    class = "echart_sankey_block",
    allow_empty_state = c("target", "value", "title"),
    ...
  )
}

#' @rdname new_echart_sankey_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_sankey_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_sankey_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_sankey_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_sankey_block
#' @export
board_options.echart_sankey_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}

#' @rdname new_echart_sankey_block
#' @export
block_render_trigger.echart_sankey_block <- function(x, session = blockr.core::get_session()) {
  blockr.core::get_board_option_or_null("echart_theme", session)
}
