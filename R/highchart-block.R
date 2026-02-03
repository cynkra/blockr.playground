#' Universal Highchart block with selectable visualization types
#'
#' A flexible block that allows users to select from various Highcharts chart
#' types and dynamically shows relevant aesthetics for the selected visualization.
#'
#' @param type Initial chart type (default "scatter"). Options: "scatter", "line",
#'   "column", "area", "pie"
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color/group aesthetic
#' @param size Column for size aesthetic (scatter only)
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `highchart_block`.
#'
#' @examples
#' # Create a scatter plot block
#' new_highchart_block(type = "scatter", x = "wt", y = "mpg")
#'
#' # Create a column chart block
#' new_highchart_block(type = "column", x = "cyl")
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_highchart_block(), list(data = mtcars))
#' }
#'
#' @export
new_highchart_block <- function(
    type = "scatter",
    x = character(),
    y = character(),
    color = character(),
    size = character(),
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
      optional = c("color", "size")
    ),
    line = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    column = list(
      required = c("x"),
      optional = c("y", "color")
    ),
    area = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    pie = list(
      required = c("x"),
      optional = c("y")
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
          r_size <- reactiveVal(normalize_aes(size))

          # Observe input changes
          observeEvent(input$type, r_type(input$type))
          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(normalize_aes(input$y)))
          observeEvent(input$color, r_color(normalize_aes(input$color)))
          observeEvent(input$size, r_size(normalize_aes(input$size)))

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
              updateSelectInput(
                session,
                inputId = "size",
                choices = c("(none)", cols()),
                selected = r_size()
              )
            }
          )

          # Dynamic UI visibility based on chart type
          observe({
            current_type <- r_type()
            chart_config <- chart_aesthetics[[current_type]]

            if (!is.null(chart_config)) {
              all_aesthetics <- c("y", "color", "size")
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

              # Update labels to show required indicators
              updateSelectInput(
                session,
                inputId = "x",
                label = if ("x" %in% chart_config$required) {
                  tags$span(
                    tags$strong("X-axis"),
                    tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                  )
                } else {
                  "X-axis"
                }
              )

              # Y label
              if ("y" %in% valid_aesthetics) {
                updateSelectInput(
                  session,
                  inputId = "y",
                  label = if ("y" %in% chart_config$required) {
                    tags$span(
                      tags$strong("Y-axis"),
                      tags$span(
                        "*",
                        style = "color: #dc3545; margin-left: 2px;"
                      )
                    )
                  } else {
                    "Y-axis"
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

              # Build hcaes parts
              aes_parts <- c(glue::glue("x = {backtick_if_needed(r_x())}"))

              # Add y if not "(none)" and valid for this chart
              if (
                r_y() != "(none)" &&
                  "y" %in% c(chart_config$required, chart_config$optional)
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("y = {backtick_if_needed(r_y())}")
                )
              }

              # Add color/group if not "(none)" and valid
              if (
                r_color() != "(none)" &&
                  "color" %in% chart_config$optional
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("group = {backtick_if_needed(r_color())}")
                )
              }

              # Add size if not "(none)" and valid (scatter only)
              if (
                r_size() != "(none)" &&
                  "size" %in% chart_config$optional
              ) {
                aes_parts <- c(
                  aes_parts,
                  glue::glue("size = {backtick_if_needed(r_size())}")
                )
              }

              aes_text <- paste(aes_parts, collapse = ", ")

              # Build the hchart call
              text <- glue::glue(
                "highcharter::hchart(data, \"{current_type}\", ",
                "highcharter::hcaes({aes_text}))"
              )

              parse(text = text)[[1]]
            }),
            state = list(
              type = r_type,
              x = r_x,
              y = r_y,
              color = r_color,
              size = r_size
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
                      tags$div(icon("chart-bar"), tags$span("Column")),
                      tags$div(icon("chart-area"), tags$span("Area")),
                      tags$div(icon("chart-pie"), tags$span("Pie"))
                    ),
                    choiceValues = c(
                      "scatter",
                      "line",
                      "column",
                      "area",
                      "pie"
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
                    label = "Color By",
                    choices = c("(none)", color),
                    selected = normalize_aes(color),
                    width = "100%"
                  )
                ),
                # Size (scatter only)
                div(
                  id = ns("size"),
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("size"),
                    label = "Size By",
                    choices = c("(none)", size),
                    selected = normalize_aes(size),
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
    class = "highchart_block",
    allow_empty_state = c("y", "color", "size"),
    ...
  )
}

#' @rdname new_highchart_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.highchart_block <- function(id, x, ...) {
  tagList(
    highcharter::highchartOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_highchart_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.highchart_block <- function(x, result, session) {
  highcharter::renderHighchart({
    if (!inherits(result, "highchart")) {
      return(NULL)
    }
    result
  })
}
