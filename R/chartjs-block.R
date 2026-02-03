#' Universal Chart.js block with selectable visualization types
#'
#' A flexible block that allows users to select from various Chart.js chart
#' types and dynamically shows relevant aesthetics for the selected visualization.
#'
#' @param type Initial chart type (default "bar"). Options: "bar", "horizontalBar",
#'   "line", "pie", "doughnut", "radar", "polar"
#' @param x Column for labels (x-axis/categories)
#' @param y Column for values (y-axis/data)
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `chartjs_block`.
#'
#' @examples
#' # Create a bar chart block
#' new_chartjs_block(type = "bar", x = "cyl", y = "mpg")
#'
#' # Create a pie chart block
#' new_chartjs_block(type = "pie", x = "cyl", y = "mpg")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_chartjs_block(), list(data = mtcars))
#' }
#'
#' @export
new_chartjs_block <- function(
    type = "bar",
    x = character(),
    y = character(),
    ...
) {
  # Normalize aesthetic values - empty/NULL/NA becomes "(none)"
  normalize_aes <- function(val) {
    if (!isTruthy(val)) "(none)" else val
  }

  # Define which aesthetics are valid for each chart type
  chart_aesthetics <- list(
    bar = list(
      required = c("x", "y"),
      optional = character()
    ),
    horizontalBar = list(
      required = c("x", "y"),
      optional = character()
    ),
    line = list(
      required = c("x", "y"),
      optional = character()
    ),
    pie = list(
      required = c("x", "y"),
      optional = character()
    ),
    doughnut = list(
      required = c("x", "y"),
      optional = character()
    ),
    radar = list(
      required = c("x", "y"),
      optional = character()
    ),
    polar = list(
      required = c("x", "y"),
      optional = character()
    )
  )

  blockr.core::new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          cols <- reactive(colnames(data()))

          # Initialize reactive values
          r_type <- reactiveVal(type)
          r_x <- reactiveVal(x)
          r_y <- reactiveVal(normalize_aes(y))

          # Observe input changes
          observeEvent(input$type, r_type(input$type))
          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(normalize_aes(input$y)))

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
            }
          )

          list(
            expr = reactive({
              current_type <- r_type()
              chart_config <- chart_aesthetics[[current_type]]

              # Validate required fields
              if (!isTruthy(r_x()) || length(r_x()) == 0) {
                return(quote(NULL))
              }

              # Check if y is required and missing
              if (
                "y" %in% chart_config$required &&
                  (r_y() == "(none)" || !isTruthy(r_y()))
              ) {
                return(quote(NULL))
              }

              x_col <- r_x()
              y_col <- r_y()

              # Build chartjs pipeline
              # Chart.js API: chartjs() |> cjsType(labels) |> cjsSeries(data, label)
              # The chart type function determines the visualization
              chart_func <- switch(
                current_type,
                bar = "cjsBar",
                horizontalBar = "cjsHorizontalBar",
                line = "cjsLine",
                pie = "cjsPie",
                doughnut = "cjsDoughnut",
                radar = "cjsRadar",
                polar = "cjsPolar"
              )

              # Build final expression
              # Column names need to be quoted strings for [[ accessor
              # Add cjsOptions to disable maintainAspectRatio for better sizing
              text <- glue::glue(
                "chartjs::chartjs() |> ",
                "chartjs::cjsOptions(maintainAspectRatio = FALSE) |> ",
                "chartjs::{chart_func}(labels = data[[\"{x_col}\"]]) |> ",
                "chartjs::cjsSeries(data = data[[\"{y_col}\"]], label = \"{y_col}\")"
              )

              parse(text = text)[[1]]
            }),
            state = list(
              type = r_type,
              x = r_x,
              y = r_y
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

          # Add custom CSS for chart type selector
          tags$style(HTML(
            "
            .chart-type-selector {
              margin-top: 0 !important;
              padding-top: 0 !important;
              width: 100%;
            }
            .chart-type-selector .btn-group-toggle,
            .chart-type-selector .btn-group {
              display: grid !important;
              grid-template-columns: repeat(auto-fit, minmax(80px, 1fr));
              gap: 5px;
              margin: 0;
              width: 100% !important;
              max-width: 100%;
            }
            .chart-type-selector .btn {
              display: flex;
              flex-direction: column;
              align-items: center;
              padding: 8px 12px;
              white-space: nowrap;
              width: 100%;
            }
            .chart-type-selector .btn i {
              font-size: 1.2em;
              margin-bottom: 4px;
            }
            .chart-type-selector .btn span {
              font-size: 0.85em;
              white-space: nowrap;
            }
            "
          )),

          # Set container query context
          block_container_script(),

          # Form inputs
          div(
            class = "block-form-grid",

            # Chart Type Selection Section
            div(
              class = "block-section",
              tags$h4("Chart Type"),
              div(
                class = "block-section-grid",
                div(
                  class = "block-input-wrapper chart-type-selector",
                  style = "grid-column: 1 / -1;",
                  shinyWidgets::radioGroupButtons(
                    inputId = ns("type"),
                    label = NULL,
                    choiceNames = list(
                      tags$div(icon("chart-bar"), tags$span("Bar")),
                      tags$div(
                        tags$i(
                          class = "fa fa-chart-bar",
                          style = "transform: rotate(90deg);"
                        ),
                        tags$span("H-Bar")
                      ),
                      tags$div(icon("chart-line"), tags$span("Line")),
                      tags$div(icon("chart-pie"), tags$span("Pie")),
                      tags$div(icon("circle-notch"), tags$span("Doughnut")),
                      tags$div(icon("diagram-project"), tags$span("Radar")),
                      tags$div(icon("bullseye"), tags$span("Polar"))
                    ),
                    choiceValues = c(
                      "bar",
                      "horizontalBar",
                      "line",
                      "pie",
                      "doughnut",
                      "radar",
                      "polar"
                    ),
                    selected = type,
                    status = "light",
                    size = "sm",
                    justified = FALSE,
                    individual = FALSE,
                    checkIcon = list(
                      yes = tags$i(
                        class = "fa fa-check",
                        style = "display: none;"
                      ),
                      no = tags$i(style = "display: none;")
                    )
                  )
                ),
                div(
                  class = "block-help-text",
                  style = "margin-top: -8px;",
                  p("Click an icon to change the visualization type")
                )
              )
            ),

            # Aesthetic Mapping Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Mappings",
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
                # X-axis (Labels)
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("x"),
                    label = tags$span(
                      tags$strong("Labels"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = x,
                    selected = x,
                    width = "100%"
                  )
                ),
                # Y-axis (Values)
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("y"),
                    label = tags$span(
                      tags$strong("Values"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", y),
                    selected = normalize_aes(y),
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
    class = "chartjs_block",
    allow_empty_state = c("y"),
    ...
  )
}

#' @rdname new_chartjs_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.chartjs_block <- function(id, x, ...) {
  tagList(
    chartjs::chartjsOutput(NS(id, "result"), width = "100%", height = "300px")
  )
}

#' @rdname new_chartjs_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.chartjs_block <- function(x, result, session) {
  chartjs::renderChartjs({
    if (!inherits(result, "chartjs")) {
      return(NULL)
    }
    result
  })
}
