#' ECharts Candlestick Block
#'
#' A specialized block for creating candlestick chart visualizations with ECharts.
#' Used for displaying financial OHLC (Open, High, Low, Close) data.
#'
#' @param date Column for date/time x-axis
#' @param open Column for opening price
#' @param close Column for closing price
#' @param low Column for low price
#' @param high Column for high price
#' @param title Chart title
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_candlestick_block`.
#'
#' @examples
#' # Create a candlestick block
#' new_echart_candlestick_block(
#'   date = "date", open = "open", close = "close",
#'   low = "low", high = "high"
#' )
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   stock_data <- data.frame(
#'     date = as.Date("2024-01-01") + 0:9,
#'     open = c(100, 102, 101, 103, 102, 104, 103, 105, 104, 106),
#'     close = c(102, 101, 103, 102, 104, 103, 105, 104, 106, 105),
#'     low = c(99, 100, 100, 101, 101, 102, 102, 103, 103, 104),
#'     high = c(103, 103, 104, 104, 105, 105, 106, 106, 107, 107)
#'   )
#'   serve(new_echart_candlestick_block(), list(data = stock_data))
#' }
#'
#' @export
new_echart_candlestick_block <- function(
    date = character(),
    open = character(),
    close = character(),
    low = character(),
    high = character(),
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
          r_date <- reactiveVal(date)
          r_open <- reactiveVal(normalize_aes(open))
          r_close <- reactiveVal(normalize_aes(close))
          r_low <- reactiveVal(normalize_aes(low))
          r_high <- reactiveVal(normalize_aes(high))
          r_title <- reactiveVal(normalize_text(title))
          r_theme <- reactiveVal(theme)

          # Sync with board theme option
          r_board_theme <- setup_board_theme_sync(session)

          # Observe input changes
          observeEvent(input$date, r_date(input$date))
          observeEvent(input$open, r_open(normalize_aes(input$open)))
          observeEvent(input$close, r_close(normalize_aes(input$close)))
          observeEvent(input$low, r_low(normalize_aes(input$low)))
          observeEvent(input$high, r_high(normalize_aes(input$high)))
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column-dependent inputs
          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "date",
                choices = cols(),
                selected = r_date()
              )
              updateSelectInput(
                session,
                inputId = "open",
                choices = c("(none)", numeric_cols()),
                selected = r_open()
              )
              updateSelectInput(
                session,
                inputId = "close",
                choices = c("(none)", numeric_cols()),
                selected = r_close()
              )
              updateSelectInput(
                session,
                inputId = "low",
                choices = c("(none)", numeric_cols()),
                selected = r_low()
              )
              updateSelectInput(
                session,
                inputId = "high",
                choices = c("(none)", numeric_cols()),
                selected = r_high()
              )
            }
          )

          list(
            expr = reactive({
              # Get current values with safe defaults
              date_val <- r_date()
              open_val <- r_open()
              close_val <- r_close()
              low_val <- r_low()
              high_val <- r_high()
              title_val <- r_title()
              theme_val <- r_theme()

              # Validate required fields
              if (!isTruthy(date_val) || length(date_val) == 0) {
                return(quote(NULL))
              }
              if (!isTruthy(open_val) || isTRUE(open_val == "(none)")) {
                return(quote(NULL))
              }
              if (!isTruthy(close_val) || isTRUE(close_val == "(none)")) {
                return(quote(NULL))
              }
              if (!isTruthy(low_val) || isTRUE(low_val == "(none)")) {
                return(quote(NULL))
              }
              if (!isTruthy(high_val) || isTRUE(high_val == "(none)")) {
                return(quote(NULL))
              }

              date_col <- backtick_if_needed(date_val)
              open_col <- backtick_if_needed(open_val)
              close_col <- backtick_if_needed(close_val)
              low_col <- backtick_if_needed(low_val)
              high_col <- backtick_if_needed(high_val)

              # Build title part
              has_title <- isTruthy(title_val) && nchar(title_val) > 0
              grid_top <- if (has_title) 60 else 40
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
  echarts4r::e_charts({date_col}) |>
  echarts4r::e_candle({open_col}, {close_col}, {low_col}, {high_col}) |>
  echarts4r::e_grid(left = 60, right = 25, top = {grid_top}, bottom = 60) |>
  echarts4r::e_y_axis(axisLine = list(show = FALSE), axisTick = list(show = FALSE), axisLabel = list(color = '#666')) |>
  echarts4r::e_x_axis(axisLine = list(lineStyle = list(color = '#ccc')), axisLabel = list(color = '#666')) |>
  echarts4r::e_legend(show = FALSE){title_part}{theme_part} |>
  echarts4r::e_text_style(fontFamily = 'Open Sans') |>
  echarts4r::e_tooltip(trigger = 'axis')
")

              parse(text = expr_text)[[1]]
            }),
            state = list(
              date = r_date,
              open = r_open,
              close = r_close,
              low = r_low,
              high = r_high,
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

            # Candlestick Mappings Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Candlestick Mappings",
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
                # Date
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("date"),
                    label = tags$span(
                      tags$strong("Date"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = date,
                    selected = date,
                    width = "100%"
                  )
                ),
                # Open
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("open"),
                    label = tags$span(
                      tags$strong("Open"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", open),
                    selected = normalize_aes(open),
                    width = "100%"
                  )
                ),
                # Close
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("close"),
                    label = tags$span(
                      tags$strong("Close"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", close),
                    selected = normalize_aes(close),
                    width = "100%"
                  )
                ),
                # Low
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("low"),
                    label = tags$span(
                      tags$strong("Low"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", low),
                    selected = normalize_aes(low),
                    width = "100%"
                  )
                ),
                # High
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("high"),
                    label = tags$span(
                      tags$strong("High"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", high),
                    selected = normalize_aes(high),
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
    class = "echart_candlestick_block",
    allow_empty_state = c("open", "close", "low", "high", "title"),
    ...
  )
}

#' @rdname new_echart_candlestick_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_candlestick_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_candlestick_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_candlestick_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_candlestick_block
#' @export
board_options.echart_candlestick_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}

#' @rdname new_echart_candlestick_block
#' @export
block_render_trigger.echart_candlestick_block <- function(x, session = blockr.core::get_session()) {
  blockr.core::get_board_option_or_null("echart_theme", session)
}
