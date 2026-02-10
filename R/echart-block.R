#' Universal EChart block with selectable visualization types
#'
#' A flexible block that allows users to select from various ECharts chart
#' types and dynamically shows relevant aesthetics for the selected visualization.
#'
#' @param type Initial chart type (default "scatter"). Options: "scatter", "line",
#'   "bar", "area", "pie", "doughnut", "boxplot", "violin", "histogram",
#'   "funnel", "density", "step", "effect_scatter"
#' @param x Column for x-axis
#' @param y Column for y-axis
#' @param color Column for color/group aesthetic
#' @param size Column for size aesthetic (scatter only)
#' @param bins Number of bins for histogram (default 30)
#' @param title Chart title
#' @param subtitle Chart subtitle
#' @param y_scale Y-axis scaling: "auto", "data", or "zero"
#' @param x_scale X-axis scaling: "auto", "data", or "zero"
#' @param axis_label_rotate Rotation angle for axis labels: 0, 45, or 90
#' @param legend_position Legend position: "bottom", "top", "left", "right", "none"
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_block`.
#'
#' @examples
#' # Create a scatter plot block
#' new_echart_block(type = "scatter", x = "wt", y = "mpg")
#'
#' # Create a bar chart block
#' new_echart_block(type = "bar", x = "cyl")
#'
#' # Create a histogram
#' new_echart_block(type = "histogram", x = "mpg", bins = 20)
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_echart_block(), list(data = mtcars))
#' }
#'
#' @export
new_echart_block <- function(
    type = "scatter",
    x = character(),
    y = character(),
    color = character(),
    size = character(),
    bins = 30,
    ripple = FALSE,
    title = character(),
    subtitle = character(),
    y_scale = "auto",
    x_scale = "auto",
    axis_label_rotate = 0,
    legend_position = "bottom",
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
    bar = list(
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
    ),
    doughnut = list(
      required = c("x"),
      optional = c("y")
    ),
    boxplot = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    violin = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    histogram = list(
      required = c("x"),
      optional = c("bins")
    ),
    funnel = list(
      required = c("x", "y"),
      optional = c()
    ),
    density = list(
      required = c("x"),
      optional = c("color")
    ),
    step = list(
      required = c("x", "y"),
      optional = c("color")
    ),
    effect_scatter = list(
      required = c("x", "y"),
      optional = c("color", "size")
    )
  )

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
          r_type <- reactiveVal(type)
          r_x <- reactiveVal(x)
          r_y <- reactiveVal(normalize_aes(y))
          r_color <- reactiveVal(normalize_aes(color))
          r_size <- reactiveVal(normalize_aes(size))
          r_bins <- reactiveVal(bins)
          r_ripple <- reactiveVal(ripple)
          r_title <- reactiveVal(normalize_text(title))
          r_subtitle <- reactiveVal(normalize_text(subtitle))
          r_y_scale <- reactiveVal(y_scale)
          r_x_scale <- reactiveVal(x_scale)
          r_axis_label_rotate <- reactiveVal(axis_label_rotate)
          r_legend_position <- reactiveVal(legend_position)
          r_theme <- reactiveVal(theme)

          # Sync with board theme option
          r_board_theme <- setup_board_theme_sync(session)

          # Observe input changes
          observeEvent(input$type, r_type(input$type))
          observeEvent(input$x, r_x(input$x))
          observeEvent(input$y, r_y(normalize_aes(input$y)))
          observeEvent(input$color, r_color(normalize_aes(input$color)))
          observeEvent(input$size, r_size(normalize_aes(input$size)))
          observeEvent(input$bins, r_bins(input$bins))
          observeEvent(input$ripple, r_ripple(input$ripple))
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$subtitle, r_subtitle(normalize_text(input$subtitle)))
          observeEvent(input$y_scale, r_y_scale(input$y_scale))
          observeEvent(input$x_scale, r_x_scale(input$x_scale))
          observeEvent(input$axis_label_rotate, r_axis_label_rotate(as.numeric(input$axis_label_rotate)))
          observeEvent(input$legend_position, r_legend_position(input$legend_position))
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
              all_aesthetics <- c("y", "color", "size", "bins")
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

              # Show ripple option only for effect_scatter
              if (current_type == "effect_scatter") {
                shinyjs::show("ripple")
              } else {
                shinyjs::hide("ripple")
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

              # Build echarts4r pipeline
              x_col <- backtick_if_needed(r_x())

              # Start building the expression parts
              parts <- list()

              # Add grouping if color is specified (except for pie, histogram, boxplot, funnel)
              has_group <- r_color() != "(none)" &&
                "color" %in% chart_config$optional &&
                !current_type %in% c("pie", "doughnut", "histogram", "boxplot", "violin", "funnel")

              if (has_group) {
                color_col <- backtick_if_needed(r_color())
                parts <- c(parts, glue::glue("dplyr::group_by({color_col})"))
              }

              # Boxplots and violins always need grouping by x
              if (current_type %in% c("boxplot", "violin")) {
                parts <- c(parts, glue::glue("dplyr::group_by({x_col})"))
              }

              # Add e_charts() call - histogram, boxplot, funnel, density need empty e_charts()
              if (current_type %in% c("histogram", "boxplot", "violin", "funnel", "density")) {
                parts <- c(parts, "echarts4r::e_charts()")
              } else {
                parts <- c(parts, glue::glue("echarts4r::e_charts({x_col})"))
              }

              # Add the chart type function
              chart_call <- switch(
                current_type,
                scatter = {
                  y_col <- backtick_if_needed(r_y())
                  if (r_size() != "(none)" && "size" %in% chart_config$optional) {
                    size_col <- backtick_if_needed(r_size())
                    glue::glue("echarts4r::e_scatter({y_col}, size = {size_col})")
                  } else {
                    glue::glue("echarts4r::e_scatter({y_col}, symbol_size = 10)")
                  }
                },
                line = {
                  y_col <- backtick_if_needed(r_y())
                  glue::glue("echarts4r::e_line({y_col})")
                },
                bar = {
                  if (r_y() != "(none)" && isTruthy(r_y())) {
                    y_col <- backtick_if_needed(r_y())
                    glue::glue("echarts4r::e_bar({y_col})")
                  } else {
                    "echarts4r::e_bar()"
                  }
                },
                area = {
                  y_col <- backtick_if_needed(r_y())
                  glue::glue("echarts4r::e_line({y_col}, areaStyle = list(opacity = 0.4))")
                },
                pie = {
                  if (r_y() != "(none)" && isTruthy(r_y())) {
                    y_col <- backtick_if_needed(r_y())
                    glue::glue("echarts4r::e_pie({y_col})")
                  } else {
                    "echarts4r::e_pie()"
                  }
                },
                doughnut = {
                  if (r_y() != "(none)" && isTruthy(r_y())) {
                    y_col <- backtick_if_needed(r_y())
                    glue::glue("echarts4r::e_doughnut({y_col})")
                  } else {
                    "echarts4r::e_doughnut()"
                  }
                },
                boxplot = {
                  y_col <- backtick_if_needed(r_y())
                  glue::glue("echarts4r::e_boxplot({y_col})")
                },
                violin = {
                  y_col <- backtick_if_needed(r_y())
                  glue::glue("echarts4r::e_violin({y_col})")
                },
                histogram = {
                  bin_count <- r_bins()
                  glue::glue("echarts4r::e_histogram({x_col}, breaks = {bin_count})")
                },
                funnel = {
                  y_col <- backtick_if_needed(r_y())
                  glue::glue("echarts4r::e_funnel({y_col}, {x_col})")
                },
                density = {
                  glue::glue("echarts4r::e_density({x_col})")
                },
                step = {
                  y_col <- backtick_if_needed(r_y())
                  glue::glue("echarts4r::e_step({y_col})")
                },
                effect_scatter = {
                  y_col <- backtick_if_needed(r_y())
                  ripple_opts <- if (r_ripple()) {
                    ", rippleEffect = list(brushType = \"stroke\", scale = 2.5, period = 4)"
                  } else {
                    ""
                  }
                  if (r_size() != "(none)" && "size" %in% chart_config$optional) {
                    size_col <- backtick_if_needed(r_size())
                    glue::glue("echarts4r::e_effect_scatter({y_col}, size = {size_col}{ripple_opts})")
                  } else {
                    glue::glue("echarts4r::e_effect_scatter({y_col}, symbol_size = 10{ripple_opts})")
                  }
                }
              )
              parts <- c(parts, chart_call)

              # Add title if specified
              if (nchar(r_title()) > 0 || nchar(r_subtitle()) > 0) {
                title_text <- r_title()
                subtitle_text <- r_subtitle()
                if (nchar(subtitle_text) > 0) {
                  parts <- c(
                    parts,
                    glue::glue(
                      "echarts4r::e_title(\"{title_text}\", \"{subtitle_text}\")"
                    )
                  )
                } else {
                  parts <- c(
                    parts,
                    glue::glue("echarts4r::e_title(\"{title_text}\")")
                  )
                }
              }

              # Grid/padding - adjusted for title
              grid_top <- if (nchar(r_title()) > 0) 60 else 40
              grid_bottom <- if (r_legend_position() == "bottom" && (has_group || current_type %in% c("pie", "doughnut"))) 60 else 40

              # Axis styling only for non-pie and non-funnel charts
              if (!current_type %in% c("pie", "doughnut", "funnel")) {
                # Build y-axis options
                y_axis_opts <- list(
                  "axisLine = list(show = FALSE)",
                  "axisTick = list(show = FALSE)",
                  "axisLabel = list(color = \"#666\")"
                )

                # Add y-scale settings
                if (r_y_scale() == "data") {
                  y_axis_opts <- c(y_axis_opts, "min = \"dataMin\"", "max = \"dataMax\"")
                } else if (r_y_scale() == "zero") {
                  y_axis_opts <- c(y_axis_opts, "min = 0")
                }

                y_axis_call <- paste0(
                  "echarts4r::e_y_axis(",
                  paste(y_axis_opts, collapse = ", "),
                  ")"
                )

                # Build x-axis options
                x_axis_opts <- list(
                  "axisLine = list(lineStyle = list(color = \"#ccc\"))",
                  "splitLine = list(show = FALSE)",
                  "axisLabel = list(color = \"#666\")"
                )

                # Add x-scale settings (for numeric x)
                if (r_x_scale() == "data") {
                  x_axis_opts <- c(x_axis_opts, "min = \"dataMin\"", "max = \"dataMax\"")
                } else if (r_x_scale() == "zero") {
                  x_axis_opts <- c(x_axis_opts, "min = 0")
                }

                # Add label rotation
                if (r_axis_label_rotate() != 0) {
                  x_axis_opts <- c(
                    x_axis_opts,
                    glue::glue("axisLabel = list(color = \"#666\", rotate = {r_axis_label_rotate()})")
                  )
                  # Remove the duplicate axisLabel
                  x_axis_opts <- x_axis_opts[!x_axis_opts == "axisLabel = list(color = \"#666\")"]
                }

                x_axis_call <- paste0(
                  "echarts4r::e_x_axis(",
                  paste(x_axis_opts, collapse = ", "),
                  ")"
                )

                parts <- c(
                  parts,
                  glue::glue("echarts4r::e_grid(left = 40, right = 25, top = {grid_top}, bottom = {grid_bottom})"),
                  y_axis_call,
                  x_axis_call
                )
              }

              # Legend positioning based on setting and number of series
              legend_pos <- r_legend_position()

              if (legend_pos == "none") {
                parts <- c(parts, "echarts4r::e_legend(show = FALSE)")
              } else if (current_type %in% c("pie", "doughnut")) {
                # Pie always shows legend
                legend_call <- switch(
                  legend_pos,
                  bottom = "echarts4r::e_legend(bottom = 0)",
                  top = "echarts4r::e_legend(top = 0)",
                  left = "echarts4r::e_legend(left = 10, orient = \"vertical\")",
                  right = "echarts4r::e_legend(right = 10, orient = \"vertical\")"
                )
                parts <- c(parts, legend_call)
              } else if (has_group) {
                # Only show legend when color grouping creates multiple series
                legend_call <- switch(
                  legend_pos,
                  bottom = "echarts4r::e_legend(bottom = 0)",
                  top = "echarts4r::e_legend(top = 0)",
                  left = "echarts4r::e_legend(left = 10, orient = \"vertical\")",
                  right = "echarts4r::e_legend(right = 10, orient = \"vertical\")"
                )
                parts <- c(parts, legend_call)
              } else {
                # Single series - hide legend
                parts <- c(parts, "echarts4r::e_legend(show = FALSE)")
              }

              # Determine effective theme: block setting takes priority, then board option
              theme_val <- r_theme()
              if (theme_val == "default") {
                theme_val <- r_board_theme()
              }

              # Apply theme if not default
              if (theme_val != "default") {
                parts <- c(
                  parts,
                  glue::glue("echarts4r::e_theme(\"{theme_val}\")")
                )
              }

              # Open Sans font (consistent with blockr universe)
              parts <- c(
                parts,
                "echarts4r::e_text_style(fontFamily = \"Open Sans\")"
              )

              # Add tooltip
              parts <- c(parts, "echarts4r::e_tooltip()")

              # Build final expression
              text <- paste("data", paste(parts, collapse = " |> "), sep = " |> ")

              parse(text = text)[[1]]
            }),
            state = list(
              type = r_type,
              x = r_x,
              y = r_y,
              color = r_color,
              size = r_size,
              bins = r_bins,
              ripple = r_ripple,
              title = r_title,
              subtitle = r_subtitle,
              y_scale = r_y_scale,
              x_scale = r_x_scale,
              axis_label_rotate = r_axis_label_rotate,
              legend_position = r_legend_position,
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

        # CSS for collapsible section (scoped to this block instance)
        tags$style(HTML(sprintf(
          "
          #%s-advanced-options {
            max-height: 0;
            overflow: hidden;
            transition: max-height 0.3s ease-out;
            grid-column: 1 / -1;
            display: grid;
            grid-template-columns: subgrid;
            gap: 15px;
          }
          #%s-advanced-options.expanded {
            max-height: 2000px;
            overflow: visible;
            transition: max-height 0.5s ease-in;
          }
          .block-advanced-toggle {
            cursor: pointer;
            user-select: none;
            padding: 8px 0;
            display: flex;
            align-items: center;
            gap: 6px;
            grid-column: 1 / -1;
            color: #6c757d;
            font-size: 0.875rem;
          }
          .block-advanced-toggle .block-chevron {
            transition: transform 0.2s;
            display: inline-block;
            font-size: 14px;
            font-weight: bold;
          }
          .block-advanced-toggle .block-chevron.rotated {
            transform: rotate(90deg);
          }
          ",
          id,
          id
        ))),

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
              grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
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
                      tags$div(icon("circle"), tags$span("Doughnut")),
                      tags$div(icon("box"), tags$span("Boxplot")),
                      tags$div(icon("guitar"), tags$span("Violin")),
                      tags$div(icon("chart-column"), tags$span("Histogram")),
                      tags$div(icon("filter"), tags$span("Funnel")),
                      tags$div(icon("wave-square"), tags$span("Density")),
                      tags$div(icon("stairs"), tags$span("Step")),
                      tags$div(icon("circle-dot"), tags$span("Effect"))
                    ),
                    choiceValues = c(
                      "scatter",
                      "line",
                      "bar",
                      "area",
                      "pie",
                      "doughnut",
                      "boxplot",
                      "violin",
                      "histogram",
                      "funnel",
                      "density",
                      "step",
                      "effect_scatter"
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
                ),
                # Bins (histogram only)
                div(
                  id = ns("bins"),
                  class = "block-input-wrapper",
                  numericInput(
                    inputId = ns("bins"),
                    label = "Number of Bins",
                    value = bins,
                    min = 5,
                    max = 100,
                    step = 5,
                    width = "100%"
                  )
                )
              )
            ),

            # Advanced Options Toggle
            div(
              class = "block-section",
              div(
                class = "block-advanced-toggle text-muted",
                id = ns("advanced-toggle"),
                onclick = sprintf(
                  "
                  const section = document.getElementById('%s');
                  const chevron = document.querySelector('#%s .block-chevron');
                  section.classList.toggle('expanded');
                  chevron.classList.toggle('rotated');
                  ",
                  ns("advanced-options"),
                  ns("advanced-toggle")
                ),
                tags$span(class = "block-chevron", "\u203A"),
                "Advanced Options"
              )
            ),

            # Advanced Options Section (Collapsible)
            div(
              id = ns("advanced-options"),

              # Titles
              div(
                class = "block-section",
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
                    textInput(
                      inputId = ns("subtitle"),
                      label = "Subtitle",
                      value = normalize_text(subtitle),
                      width = "100%"
                    )
                  )
                )
              ),

              # Animation (effect_scatter only)
              div(
                id = ns("ripple"),
                class = "block-section",
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    checkboxInput(
                      inputId = ns("ripple"),
                      label = "Enable ripple animation",
                      value = ripple,
                      width = "100%"
                    )
                  )
                )
              ),

              # Axes
              div(
                class = "block-section",
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = ns("y_scale"),
                      label = "Y-axis Scale",
                      choices = c(
                        "Auto" = "auto",
                        "Fit to Data" = "data",
                        "Start at Zero" = "zero"
                      ),
                      selected = y_scale,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = ns("x_scale"),
                      label = "X-axis Scale",
                      choices = c(
                        "Auto" = "auto",
                        "Fit to Data" = "data",
                        "Start at Zero" = "zero"
                      ),
                      selected = x_scale,
                      width = "100%"
                    )
                  ),
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = ns("axis_label_rotate"),
                      label = "Label Rotation",
                      choices = c(
                        "0\u00b0" = "0",
                        "45\u00b0" = "45",
                        "90\u00b0" = "90"
                      ),
                      selected = as.character(axis_label_rotate),
                      width = "100%"
                    )
                  )
                )
              ),

              # Legend & Theme
              div(
                class = "block-section",
                div(
                  class = "block-section-grid",
                  div(
                    class = "block-input-wrapper",
                    selectInput(
                      inputId = ns("legend_position"),
                      label = "Legend Position",
                      choices = c(
                        "Bottom" = "bottom",
                        "Top" = "top",
                        "Left" = "left",
                        "Right" = "right",
                        "None" = "none"
                      ),
                      selected = legend_position,
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
      )
    },
    dat_valid = function(data) {
      if (!is.data.frame(data)) {
        stop("Input must be a data frame")
      }
    },
    class = "echart_block",
    allow_empty_state = c("y", "color", "size", "title", "subtitle"),
    ...
  )
}

#' @rdname new_echart_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_block
#' @export
board_options.echart_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}

#' @rdname new_echart_block
#' @export
block_render_trigger.echart_block <- function(x, session = blockr.core::get_session()) {
  blockr.core::get_board_option_or_null("echart_theme", session)
}
