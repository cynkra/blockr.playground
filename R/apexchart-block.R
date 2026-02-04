#' Universal ApexCharts block with selectable visualization types
#'
#' A flexible block that allows users to select from various ApexCharts chart
#' types and dynamically shows relevant aesthetics for the selected visualization.
#'
#' @param type Initial chart type (default "scatter"). Options: "scatter", "line",
#'   "bar", "area", "pie", "donut", "radar"
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color/group aesthetic (uses group_by)
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `apexchart_block`.
#'
#' @examples
#' # Create a scatter plot block
#' new_apexchart_block(type = "scatter", x = "wt", y = "mpg")
#'
#' # Create a bar chart block
#' new_apexchart_block(type = "bar", x = "cyl", y = "avg_mpg")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_apexchart_block(), list(data = mtcars))
#' }
#'
#' @export
new_apexchart_block <- function(
    type = "scatter",
    x = character(),
    y = character(),
    color = character(),
    ...
) {
  # Normalize aesthetic values - empty/NULL/NA becomes "(none)"
  normalize_aes <- function(val) {
    if (!isTruthy(val)) "(none)" else val
  }

  # Define which aesthetics are valid for each chart type
  chart_aesthetics <- list(
    scatter = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    line = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    bar = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    area = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    pie = list(
      required = c("x", "y"),
      optional = character()
    ),
    donut = list(
      required = c("x", "y"),
      optional = character()
    ),
    radar = list(
      required = c("x", "y"),
      optional = c("color")
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
          r_color <- reactiveVal(normalize_aes(color))

          # Observe input changes
          observeEvent(input$type, r_type(input$type))
          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(normalize_aes(input$y)))
          observeEvent(input$color, r_color(normalize_aes(input$color)))

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
                inputId = "color",
                choices = c("(none)", cols()),
                selected = r_color()
              )
            }
          )

          # Dynamic UI visibility based on chart type
          observe({
            current_type <- r_type()
            chart_config <- chart_aesthetics[[current_type]]

            if (!is.null(chart_config)) {
              all_aesthetics <- c("y", "color")
              valid_aesthetics <- c(
                chart_config$required,
                chart_config$optional
              )
              # x is always shown
              valid_aesthetics <- valid_aesthetics[valid_aesthetics != "x"]

              # Hide/show aesthetic inputs based on validity
              for (aes in all_aesthetics) {
                if (aes %in% valid_aesthetics) {
                  shinyjs::show(aes)
                } else {
                  shinyjs::hide(aes)
                }
              }

              # Update x label based on chart type
              x_label <- if (current_type %in% c("pie", "donut")) {
                "Labels"
              } else {
                "X-axis"
              }
              updateSelectInput(
                session,
                inputId = "x",
                label = tags$span(
                  tags$strong(x_label),
                  tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                )
              )

              # Y label
              if ("y" %in% valid_aesthetics) {
                y_label <- if (current_type %in% c("pie", "donut")) {
                  "Values"
                } else {
                  "Y-axis"
                }
                updateSelectInput(
                  session,
                  inputId = "y",
                  label = if ("y" %in% chart_config$required) {
                    tags$span(
                      tags$strong(y_label),
                      tags$span(
                        "*",
                        style = "color: #dc3545; margin-left: 2px;"
                      )
                    )
                  } else {
                    y_label
                  }
                )
              }
            }
          })

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

              x_col <- backtick_if_needed(r_x())
              y_col <- backtick_if_needed(r_y())
              color_col <- r_color()

              # Build apexcharter expression
              # ApexCharts uses: apex(data, type = "type",
              #                       mapping = aes(x = col, y = col))
              # Grouping is done via dplyr::group_by() before apex()

              parts <- list()

              # Add grouping if color is specified (except for pie/donut)
              has_group <- color_col != "(none)" &&
                "color" %in% chart_config$optional &&
                !current_type %in% c("pie", "donut")

              if (has_group) {
                group_col <- backtick_if_needed(color_col)
                parts <- c(parts, glue::glue("dplyr::group_by({group_col})"))
              }

              # Build the apex() call with mapping
              aes_call <- glue::glue(
                "apexcharter::aes(x = {x_col}, y = {y_col})"
              )

              apex_call <- glue::glue(
                "apexcharter::apex(type = \"{current_type}\", ",
                "mapping = {aes_call})"
              )
              parts <- c(parts, apex_call)

              # Build final expression with data pipe
              text <- paste("data", paste(parts, collapse = " |> "), sep = " |> ")

              parse(text = text)[[1]]
            }),
            state = list(
              type = r_type,
              x = r_x,
              y = r_y,
              color = r_color
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
                      tags$div(icon("braille"), tags$span("Scatter")),
                      tags$div(icon("chart-line"), tags$span("Line")),
                      tags$div(icon("chart-bar"), tags$span("Bar")),
                      tags$div(icon("chart-area"), tags$span("Area")),
                      tags$div(icon("chart-pie"), tags$span("Pie")),
                      tags$div(icon("circle-notch"), tags$span("Donut")),
                      tags$div(icon("diagram-project"), tags$span("Radar"))
                    ),
                    choiceValues = c(
                      "scatter",
                      "line",
                      "bar",
                      "area",
                      "pie",
                      "donut",
                      "radar"
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
                # X-axis
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("x"),
                    label = tags$span(
                      tags$strong("X-axis"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = x,
                    selected = x,
                    width = "100%"
                  )
                ),
                # Y-axis
                div(
                  id = ns("y"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("y"),
                    label = tags$span(
                      tags$strong("Y-axis"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = c("(none)", y),
                    selected = normalize_aes(y),
                    width = "100%"
                  )
                ),
                # Color/Group
                div(
                  id = ns("color"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("color"),
                    label = "Group By",
                    choices = c("(none)", color),
                    selected = normalize_aes(color),
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
    class = "apexchart_block",
    allow_empty_state = c("y", "color"),
    ...
  )
}

#' @rdname new_apexchart_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.apexchart_block <- function(id, x, ...) {
  tagList(
    apexcharter::apexchartOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_apexchart_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.apexchart_block <- function(x, result, session) {
  apexcharter::renderApexchart({
    if (!inherits(result, "apexcharter")) {
      return(NULL)
    }
    result
  })
}
