#' ECharts Calendar Block
#'
#' A specialized block for creating calendar heatmap visualizations with ECharts.
#' Displays values over time in a calendar grid layout.
#'
#' @param date Column for date values
#' @param value Column for numeric values (determines color intensity)
#' @param year Year for calendar range (auto-detected from data if empty)
#' @param title Chart title
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_calendar_block`.
#'
#' @examples
#' # Create a calendar heatmap block
#' new_echart_calendar_block(date = "date", value = "commits")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   activity_data <- data.frame(
#'     date = seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
#'     commits = sample(0:10, 366, replace = TRUE)
#'   )
#'   serve(new_echart_calendar_block(), list(data = activity_data))
#' }
#'
#' @export
new_echart_calendar_block <- function(
    date = character(),
    value = character(),
    year = character(),
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
          date_cols <- reactive({
            d <- data()
            names(d)[sapply(d, function(x) inherits(x, c("Date", "POSIXt")))]
          })

          # Initialize reactive values
          r_date <- reactiveVal(date)
          r_value <- reactiveVal(normalize_aes(value))
          r_year <- reactiveVal(normalize_text(year))
          r_title <- reactiveVal(normalize_text(title))
          r_theme <- reactiveVal(theme)

          # Sync with board theme option
          r_board_theme <- setup_board_theme_sync(session)

          # Observe input changes
          observeEvent(input$date, r_date(input$date))
          observeEvent(input$value, r_value(normalize_aes(input$value)))
          observeEvent(input$year, r_year(normalize_text(input$year)))
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column-dependent inputs
          observeEvent(
            cols(),
            {
              # Prefer date columns, but allow any column
              date_choices <- if (length(date_cols()) > 0) {
                c(date_cols(), setdiff(cols(), date_cols()))
              } else {
                cols()
              }
              updateSelectInput(
                session,
                inputId = "date",
                choices = date_choices,
                selected = r_date()
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
              date_val <- r_date()
              value_val <- r_value()
              year_val <- r_year()
              title_val <- r_title()
              theme_val <- r_theme()

              # Validate required fields
              if (!isTruthy(date_val) || length(date_val) == 0) {
                return(quote(NULL))
              }
              if (!isTruthy(value_val) || isTRUE(value_val == "(none)")) {
                return(quote(NULL))
              }

              date_col <- backtick_if_needed(date_val)
              value_col <- backtick_if_needed(value_val)

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

              # Year range - auto-detect if not specified
              year_part <- if (isTruthy(year_val) && nchar(year_val) > 0) {
                glue::glue("range = \"{year_val}\"")
              } else {
                glue::glue("range = .year")
              }

              # Calendar heatmap using local() to extract year from data
              expr_text <- glue::glue("
local({{
  .dates <- as.Date(data[['{date_val}']])
  .year <- as.character(lubridate::year(.dates[1]))
  data |>
    dplyr::mutate(.date_chr = as.character(as.Date({date_col}))) |>
    echarts4r::e_charts(.date_chr) |>
    echarts4r::e_calendar({year_part}) |>
    echarts4r::e_heatmap({value_col}, coord_system = 'calendar') |>
    echarts4r::e_visual_map({value_col}, orient = 'horizontal', bottom = 20, left = 'center'){title_part}{theme_part} |>
    echarts4r::e_text_style(fontFamily = 'Open Sans') |>
    echarts4r::e_tooltip()
}})
")

              parse(text = expr_text)[[1]]
            }),
            state = list(
              date = r_date,
              value = r_value,
              year = r_year,
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

            # Calendar Mappings Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Calendar Mappings",
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
                ),
                # Year
                div(
                  class = "block-input-wrapper",
                  textInput(
                    inputId = ns("year"),
                    label = "Year (auto-detect if empty)",
                    value = normalize_text(year),
                    placeholder = "e.g., 2024",
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
    class = "echart_calendar_block",
    allow_empty_state = c("value", "year", "title"),
    ...
  )
}

#' @rdname new_echart_calendar_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_calendar_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_calendar_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_calendar_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_calendar_block
#' @export
board_options.echart_calendar_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}

#' @rdname new_echart_calendar_block
#' @export
block_render_trigger.echart_calendar_block <- function(x, session = blockr.core::get_session()) {
  blockr.core::get_board_option_or_null("echart_theme", session)
}
