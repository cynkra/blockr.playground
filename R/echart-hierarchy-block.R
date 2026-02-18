#' ECharts Hierarchy Filter Block
#'
#' A hierarchical filter block that renders sunburst, treemap, or tree charts.
#' Clicking a node filters the data and passes the filtered data frame
#' downstream — behaves like a filter block, not a terminal visualization.
#'
#' @param levels Character vector. Multi-select for hierarchy columns (order = depth).
#' @param value Character. Numeric column for sizing leaf nodes.
#' @param chart_type Character. One of "sunburst", "treemap", or "tree".
#' @param title Character. Chart title.
#' @param theme Character. ECharts theme name.
#' @param ... Forwarded to \code{\link[blockr.core]{new_transform_block}}
#'
#' @return A blockr transform block that returns filtered data.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   accounts <- data.frame(
#'     asset_class = c("Liquid", "Liquid", "Illiquid", "Illiquid"),
#'     account = c("Checking", "Savings", "Property A", "Fund X"),
#'     value = c(50000, 120000, 450000, 200000)
#'   )
#'   serve(
#'     new_echart_hierarchy_block(
#'       levels = c("asset_class", "account"),
#'       value = "value"
#'     ),
#'     data = list(data = accounts)
#'   )
#' }
#'
#' @export
new_echart_hierarchy_block <- function(
    levels = character(),
    value = character(),
    chart_type = "sunburst",
    title = character(),
    theme = "default",
    ...) {
  blockr.core::new_transform_block(
    server = function(id, data) {
      moduleServer(
        id,
        function(input, output, session) {
          ns <- session$ns

          cols <- reactive(colnames(data()))
          numeric_cols <- reactive({
            d <- data()
            names(d)[vapply(d, is.numeric, logical(1))]
          })

          # Reactive state
          r_levels <- reactiveVal(levels)
          r_value <- reactiveVal(if (!length(value)) "(none)" else value)
          r_chart_type <- reactiveVal(chart_type)
          r_title <- reactiveVal(if (!length(title)) "" else title)
          r_theme <- reactiveVal(theme)
          r_clicked_path <- reactiveVal(NULL)

          # Board theme sync
          r_board_theme <- setup_board_theme_sync(session)

          # Sync inputs -> state
          observeEvent(input$levels, r_levels(input$levels), ignoreNULL = FALSE)
          observeEvent(input$value, r_value(input$value))
          observeEvent(input$chart_type, r_chart_type(input$chart_type))
          observeEvent(input$title, r_title(input$title))
          observeEvent(input$theme, r_theme(input$theme))

          # Update column dropdowns when data changes
          observeEvent(cols(), {
            current_lvl <- r_levels()
            if (!length(current_lvl) || !all(current_lvl %in% cols())) {
              # Auto-detect: non-numeric columns
              non_num <- setdiff(cols(), numeric_cols())
              current_lvl <- head(non_num, 3)
              r_levels(current_lvl)
            }
            updateSelectizeInput(
              session, "levels",
              choices = cols(),
              selected = current_lvl
            )

            current_val <- r_value()
            if (!length(current_val) || (!current_val %in% c("(none)", numeric_cols()))) {
              current_val <- if (length(numeric_cols())) numeric_cols()[1] else "(none)"
              r_value(current_val)
            }
            updateSelectInput(
              session, "value",
              choices = c("(none)", numeric_cols()),
              selected = current_val
            )
          })

          # Reset filter when levels or data change
          observeEvent(list(r_levels(), data()), {
            r_clicked_path(NULL)
          })

          # Clear button
          observeEvent(input$clear_filter, {
            r_clicked_path(NULL)
          })

          # Build nested tree data for echarts
          tree_data <- reactive({
            df <- data()
            req(is.data.frame(df), nrow(df) > 0)
            lvls <- r_levels()
            val_col <- r_value()
            req(length(lvls) > 0, all(lvls %in% names(df)))

            has_value <- !is.null(val_col) && val_col != "(none)" && val_col %in% names(df)
            wide_to_nested(df, lvls, if (has_value) val_col else NA)
          })

          # Render chart
          output$hierarchy_chart <- echarts4r::renderEcharts4r({
            tree <- tree_data()
            req(length(tree) > 0)

            ct <- r_chart_type()
            title_val <- r_title()
            theme_val <- r_theme()

            # Determine effective theme
            if (isTRUE(theme_val == "default")) {
              theme_val <- r_board_theme()
            }

            # Read clicked path — triggers re-render on filter change
            clicked <- r_clicked_path()

            # Apply visual highlight: dim non-matching branches
            display_tree <- if (!is.null(clicked) && length(clicked) > 0) {
              style_tree_by_path(tree, clicked)
            } else {
              tree
            }

            # Build series config based on chart type
            series <- if (ct == "sunburst") {
              list(
                type = "sunburst",
                data = display_tree,
                radius = list("10%", "90%"),
                label = list(rotate = "radial", fontSize = 11),
                itemStyle = list(borderWidth = 1),
                levels = list(
                  list(),
                  list(
                    r0 = "15%", r = "45%",
                    itemStyle = list(borderWidth = 2),
                    label = list(rotate = "tangential", fontSize = 12)
                  ),
                  list(
                    r0 = "45%", r = "70%",
                    label = list(align = "right")
                  ),
                  list(
                    r0 = "70%", r = "90%",
                    label = list(position = "outside", padding = 3, silent = FALSE),
                    itemStyle = list(borderWidth = 1)
                  )
                )
              )
            } else if (ct == "treemap") {
              list(
                type = "treemap",
                data = display_tree,
                roam = FALSE,
                label = list(show = TRUE, fontSize = 12),
                upperLabel = list(show = TRUE, height = 30),
                levels = list(
                  list(
                    itemStyle = list(
                      borderColor = "#555",
                      borderWidth = 2,
                      gapWidth = 2
                    )
                  ),
                  list(
                    colorSaturation = c(0.3, 0.6),
                    itemStyle = list(
                      borderColorSaturation = 0.7,
                      gapWidth = 1,
                      borderWidth = 1
                    )
                  )
                )
              )
            } else {
              # tree: wrap in single root node
              list(
                type = "tree",
                data = list(list(name = "All", children = display_tree)),
                layout = "orthogonal",
                orient = "LR",
                label = list(
                  position = "left",
                  verticalAlign = "middle",
                  align = "right",
                  fontSize = 12
                ),
                leaves = list(
                  label = list(
                    position = "right",
                    verticalAlign = "middle",
                    align = "left"
                  )
                ),
                expandAndCollapse = FALSE,
                animationDuration = 400,
                animationDurationUpdate = 400
              )
            }

            # Build chart using e_list to pass nested data directly
            chart <- echarts4r::e_charts() |>
              echarts4r::e_list(list(
                series = list(series),
                tooltip = list(trigger = "item")
              ))

            # Add title
            if (nchar(title_val) > 0) {
              chart <- chart |>
                echarts4r::e_title(title_val, left = "center", top = 5)
            }

            # Apply theme
            if (!is.null(theme_val) && theme_val != "default") {
              chart <- chart |> echarts4r::e_theme(theme_val)
            }

            # Register click handler — sends treePathInfo to Shiny
            click_js <- sprintf(
              "function(params) {
                 var path = [];
                 if (params.data && params.data._path) {
                   path = params.data._path;
                 } else if (params.treePathInfo) {
                   for (var i = 1; i < params.treePathInfo.length; i++) {
                     path.push(params.treePathInfo[i].name);
                   }
                 }
                 if (path.length > 0) {
                   Shiny.setInputValue('%s', {path: path, nonce: Math.random()}, {priority: 'event'});
                 }
               }",
              ns("chart_clicked")
            )
            chart <- chart |>
              htmlwidgets::onRender(sprintf(
                "function(el, x) {
                   var instance = echarts.getInstanceByDom(el);
                   if (instance) { instance.on('click', %s); }
                 }",
                click_js
              ))

            chart
          })

          # Process click events
          observeEvent(input$chart_clicked, {
            clicked <- input$chart_clicked
            new_path <- clicked$path

            # Toggle: clicking same path again clears filter
            current <- r_clicked_path()
            if (!is.null(current) && identical(as.character(current), as.character(new_path))) {
              r_clicked_path(NULL)
            } else {
              r_clicked_path(new_path)
            }
          })

          # Filter status text
          output$filter_status <- renderText({
            path <- r_clicked_path()
            lvls <- r_levels()
            if (is.null(path) || length(path) == 0) {
              "Click a node to filter"
            } else {
              parts <- vapply(seq_along(path), function(i) {
                if (i <= length(lvls)) {
                  paste0(lvls[i], " = ", path[[i]])
                } else {
                  as.character(path[[i]])
                }
              }, character(1))
              paste0("Filter: ", paste(parts, collapse = " > "))
            }
          })

          # Expression: filter or identity
          list(
            expr = reactive({
              path <- r_clicked_path()
              level_vals <- r_levels()

              if (is.null(path) || length(path) == 0) {
                return(quote(identity(data)))
              }

              # Only use as many conditions as we have path parts AND level columns
              n <- min(length(path), length(level_vals))
              conditions <- lapply(seq_len(n), function(i) {
                call("==", as.name(level_vals[i]), path[[i]])
              })

              combined <- Reduce(function(a, b) call("&", a, b), conditions)
              as.call(list(quote(dplyr::filter), quote(data), combined))
            }),
            state = list(
              levels = r_levels,
              value = r_value,
              chart_type = r_chart_type,
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
        div(
          class = "block-container",
          block_responsive_css(),
          block_container_script(),

          # Chart output
          div(
            style = "margin-bottom: 8px;",
            echarts4r::echarts4rOutput(ns("hierarchy_chart"), height = "400px")
          ),

          # Filter status + clear button
          div(
            style = paste(
              "display: flex; align-items: center;",
              "justify-content: space-between; margin-bottom: 8px; padding: 0 4px;"
            ),
            div(
              class = "text-muted",
              style = "font-size: 0.8rem;",
              textOutput(ns("filter_status"), inline = TRUE)
            ),
            actionButton(
              ns("clear_filter"),
              "Clear",
              class = "btn-sm btn-outline-secondary",
              style = "font-size: 0.75rem; padding: 2px 8px;"
            )
          ),

          # Controls
          div(
            class = "block-form-grid",
            div(
              class = "block-section",
              div(
                class = "block-section-grid",
                # Chart type
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    ns("chart_type"), "Chart Type",
                    choices = c(
                      "Sunburst" = "sunburst",
                      "Treemap" = "treemap",
                      "Tree" = "tree"
                    ),
                    selected = chart_type,
                    width = "100%"
                  )
                ),
                # Levels (hierarchy columns)
                div(
                  class = "block-input-wrapper",
                  selectizeInput(
                    ns("levels"),
                    label = tags$span(
                      tags$strong("Hierarchy Levels"),
                      tags$span("*", style = "color: #dc3545; margin-left: 2px;")
                    ),
                    choices = levels,
                    selected = levels,
                    multiple = TRUE,
                    width = "100%",
                    options = list(
                      plugins = list("remove_button", "drag_drop"),
                      placeholder = "Select columns (order = depth)"
                    )
                  )
                ),
                # Value column
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    ns("value"), "Value (size)",
                    choices = c("(none)", value),
                    selected = if (!length(value)) "(none)" else value,
                    width = "100%"
                  )
                ),
                # Title
                div(
                  class = "block-input-wrapper",
                  textInput(
                    ns("title"), "Title",
                    value = if (!length(title)) "" else title,
                    width = "100%"
                  )
                ),
                # Theme
                div(
                  class = "block-input-wrapper",
                  selectInput(
                    ns("theme"), "Theme",
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
    allow_empty_state = c("levels", "value", "title"),
    class = "echart_hierarchy_block",
    ...
  )
}

# Helper: convert wide data to nested list structure for echarts
# @param data Data frame
# @param levels Character vector of column names (order = hierarchy depth)
# @param value_col Column name for sizing, or NA for tree mode
# @return List suitable for e_sunburst / e_treemap / e_tree
wide_to_nested <- function(data, levels, value_col) {
  build_level <- function(df, depth, ancestor_path) {
    if (depth > length(levels)) {
      return(list())
    }

    col <- levels[depth]
    groups <- split(df, df[[col]], drop = TRUE)

    lapply(unname(groups), function(grp) {
      nm <- as.character(grp[[col]][1])
      node_path <- c(ancestor_path, nm)

      if (depth == length(levels)) {
        # Leaf level
        node <- list(name = nm, `_path` = as.list(node_path))
        if (!is.na(value_col) && value_col %in% names(grp)) {
          node$value <- sum(grp[[value_col]], na.rm = TRUE)
        }
        node
      } else {
        # Intermediate level
        children <- build_level(grp, depth + 1, node_path)
        node <- list(name = nm, `_path` = as.list(node_path), children = children)
        if (!is.na(value_col) && value_col %in% names(grp)) {
          node$value <- sum(grp[[value_col]], na.rm = TRUE)
        }
        node
      }
    })
  }

  build_level(data, 1, character(0))
}

# Apply visual styling to tree nodes based on the active filter path.
# Nodes on the selected path keep full opacity; others are dimmed.
# @param tree Nested list from wide_to_nested
# @param path Character vector of selected node names (depth order)
# @return Modified tree with itemStyle applied
style_tree_by_path <- function(tree, path) {
  # Use explicit grey color + low opacity for dimmed nodes.
  # opacity alone is not enough for treemap/tree chart types.
  dim_style <- list(
    itemStyle = list(color = "#d0d0d0", opacity = 0.3, borderColor = "#bbb"),
    label = list(color = "#999")
  )

  walk <- function(nodes, depth) {
    lapply(nodes, function(node) {
      if (depth > length(path)) {
        # Past the filter depth — inside selected branch, keep original style
        node
      } else if (identical(node$name, path[[depth]])) {
        # On the active path — keep original style, recurse into children
        if (!is.null(node$children)) {
          node$children <- walk(node$children, depth + 1)
        }
        node
      } else {
        # Not on the active path — dim this entire subtree
        node <- c(node, dim_style)
        if (!is.null(node$children)) {
          node$children <- dim_subtree(node$children)
        }
        node
      }
    })
  }

  dim_subtree <- function(nodes) {
    lapply(nodes, function(node) {
      node <- c(node, dim_style)
      if (!is.null(node$children)) {
        node$children <- dim_subtree(node$children)
      }
      node
    })
  }

  walk(tree, 1)
}
