#' ECharts Radar Block
#'
#' A specialized block for creating radar chart visualizations with ECharts.
#' Compares multiple entities across multiple numeric metrics.
#'
#' @param group Column to group by (creates separate radar shapes per group)
#' @param metrics Character vector of numeric columns to use as radar axes
#' @param title Chart title
#' @param theme ECharts theme name
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A block object of class `echart_radar_block`.
#'
#' @examples
#' # Create a radar block comparing species across iris measurements
#' new_echart_radar_block(group = "Species", metrics = c("Sepal.Length", "Sepal.Width"))
#'
#' if (interactive()) {
#'   library(blockr.core)
#'   serve(new_echart_radar_block(), list(data = iris))
#' }
#'
#' @export
new_echart_radar_block <- function(
    group = character(),
    metrics = character(),
    title = character(),
    theme = "default",
    ...
) {
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
          r_group <- reactiveVal(group)
          r_metrics <- reactiveVal(if (length(metrics) > 0) metrics else character())
          r_title <- reactiveVal(normalize_text(title))
          r_theme <- reactiveVal(theme)

          # Sync with board theme option
          r_board_theme <- setup_board_theme_sync(session)

          # Observe input changes
          observeEvent(input$group, r_group(input$group))
          observeEvent(input$metrics, r_metrics(input$metrics), ignoreNULL = FALSE)
          observeEvent(input$title, r_title(normalize_text(input$title)))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column-dependent inputs
          observeEvent(
            cols(),
            {
              updateSelectInput(
                session,
                inputId = "group",
                choices = cols(),
                selected = r_group()
              )
              # Update multi-select for metrics
              updateSelectInput(
                session,
                inputId = "metrics",
                choices = numeric_cols(),
                selected = r_metrics()
              )
            }
          )

          list(
            expr = reactive({
              # Get current values with safe defaults
              group_val <- r_group()
              metrics_val <- r_metrics()
              title_val <- r_title()
              theme_val <- r_theme()

              # Validate required fields
              if (!isTruthy(group_val) || length(group_val) == 0) {
                return(quote(NULL))
              }
              if (!isTruthy(metrics_val) || length(metrics_val) < 2) {
                return(quote(NULL))
              }

              group_col <- group_val
              metric_cols <- metrics_val

              # Build title and theme parts
              has_title <- isTruthy(title_val) && nchar(title_val) > 0
              title_part <- if (has_title) {
                glue::glue(" |>\n    echarts4r::e_title(\"{title_val}\")")
              } else {
                ""
              }

              # Determine effective theme: block setting takes priority, then board option
              if (!isTruthy(theme_val) || isTRUE(theme_val == "default")) {
                theme_val <- r_board_theme()
              }

              theme_part <- if (isTruthy(theme_val) && !isTRUE(theme_val == "default")) {
                glue::glue(" |>\n    echarts4r::e_theme(\"{theme_val}\")")
              } else {
                ""
              }

              # Build aggregation expressions
              agg_exprs <- sapply(metric_cols, function(m) {
                m_bt <- backtick_if_needed(m)
                paste0("`", m, "` = mean(", m_bt, ", na.rm = TRUE)")
              })
              agg_str <- paste(agg_exprs, collapse = ", ")

              # Metrics for pivot
              metrics_quoted <- paste0("\"", metric_cols, "\"", collapse = ", ")

              # Build radar expression using local() to handle dynamic group names
              expr_text <- glue::glue("
local({{
  # Aggregate and pivot data
  .agg <- data |>
    dplyr::summarize({agg_str}, .by = {backtick_if_needed(group_col)}) |>
    tidyr::pivot_longer(cols = c({metrics_quoted}), names_to = 'indicator', values_to = 'value') |>
    tidyr::pivot_wider(names_from = {backtick_if_needed(group_col)}, values_from = value)

  # Compute max for scaling
  .max_val <- ceiling(max(.agg[, -1], na.rm = TRUE))


  # Get group names dynamically
  .groups <- setdiff(names(.agg), 'indicator')

  # Build radar chart - start with e_charts
  .chart <- .agg |>
    echarts4r::e_charts(indicator)

  # Add e_radar for each group
  for (.g in .groups) {{
    .chart <- .chart |>
      echarts4r::e_radar_(.g, max = .max_val)
  }}

  # Add remaining options
  .chart |>
    echarts4r::e_legend(bottom = 0){title_part}{theme_part} |>
    echarts4r::e_text_style(fontFamily = 'Open Sans') |>
    echarts4r::e_tooltip()
}})
")

              parse(text = expr_text)[[1]]
            }),
            state = list(
              group = r_group,
              metrics = r_metrics,
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

            # Radar Mappings Section
            div(
              class = "block-section",
              tags$h4(
                style = paste(
                  "display: flex; align-items: center;",
                  "justify-content: space-between;"
                ),
                "Radar Mappings",
                tags$small(
                  tags$span("*", style = "color: #dc3545; font-weight: bold;"),
                  " Required",
                  style = paste(
                    "font-size: 0.7em; color: #6c757d;",
                    "font-weight: normal;"
                  )
                )
              ),
              div(
                class = "block-section-grid",
                # Group by
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("group"),
                    label = tags$span(
                      tags$strong("Group By"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = group,
                    selected = group,
                    width = "100%"
                  )
                ),
                # Metrics (multi-select dropdown)
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    inputId = ns("metrics"),
                    label = tags$span(
                      tags$strong("Metrics"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;"),
                      tags$small(" (select at least 2)", style = "color: #6c757d; font-weight: normal;")
                    ),
                    choices = metrics,
                    selected = metrics,
                    multiple = TRUE,
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
    class = "echart_radar_block",
    allow_empty_state = c("metrics", "title"),
    ...
  )
}

#' @rdname new_echart_radar_block
#' @param id Module ID
#' @param x Block object
#' @export
block_ui.echart_radar_block <- function(id, x, ...) {
  tagList(
    echart_theme_blockr(),
    echarts4r::echarts4rOutput(NS(id, "result"), height = "400px")
  )
}

#' @rdname new_echart_radar_block
#' @param result Evaluation result
#' @param session Shiny session object
#' @export
block_output.echart_radar_block <- function(x, result, session) {
  echarts4r::renderEcharts4r({
    if (!inherits(result, "echarts4r")) {
      return(NULL)
    }
    result
  })
}

#' @rdname new_echart_radar_block
#' @export
board_options.echart_radar_block <- function(x, ...) {
  blockr.core::combine_board_options(
    new_echart_theme_option(...),
    NextMethod()
  )
}

#' @rdname new_echart_radar_block
#' @export
block_render_trigger.echart_radar_block <- function(x, session = blockr.core::get_session()) {
  blockr.core::get_board_option_or_null("echart_theme", session)
}
