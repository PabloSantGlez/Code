# ============================================================
#  Módulo: Spider Charts por Universidad (mod_spider_university.R)
# ============================================================

spider_categories <- list(
  advanced = list(
    label   = "Advanced",
    metrics = c("win_shares", "win_shares_per_48_minutes", "value_over_replacement", "box_plus_minus")
  ),
  totals = list(
    label   = "Career Totals",
    metrics = c("games", "minutes_played", "points", "assists", "total_rebounds", "years_active")
  ),
  averages = list(
    label   = "Per Game",
    metrics = c("points_per_game", "average_assists", "average_total_rebounds", "average_minutes_played")
  ),
  percentages = list(
    label   = "Shooting %",
    metrics = c("field_goal_percentage", "free_throw_percentage", "3_point_percentage")
  )
)

invert_metrics <- c("overall_pick")

label_map <- c(
  overall_performance       = "Overall",
  overall_pick              = "Avg Pick",
  pick_perf_difference      = "Pick Diff",
  win_shares                = "Win Shares",
  win_shares_per_48_minutes = "WS/48",
  value_over_replacement    = "VORP",
  box_plus_minus            = "BPM",
  games                     = "Games",
  minutes_played            = "Minutes",
  points                    = "Points",
  assists                   = "Assists",
  total_rebounds            = "Rebounds",
  years_active              = "Yrs Active",
  points_per_game           = "PTS/G",
  average_assists           = "AST/G",
  average_total_rebounds    = "REB/G",
  average_minutes_played    = "MIN/G",
  field_goal_percentage     = "FG%",
  free_throw_percentage     = "FT%",
  `3_point_percentage`      = "3PT%"
)

uni_colors      <- c("#C8102E", "#1D428A", "#FFD700")
uni_text_colors <- c("#FF6B6B", "#7EB3FF", "#FFD700")

# ── 1. UI ─────────────────────────────────────────────────────
mod_spider_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    
    # ── Columna izquierda: panel + tabla ──────────────────────
    column(width = 3,
           
           div(
             style = "background-color: var(--navy-deeper);
                      border: 1px solid var(--border);
                      border-radius: var(--radius-lg);
                      padding: 20px;
                      box-shadow: var(--shadow-md);
                      display: flex;
                      flex-direction: column;",
             
             h4("University Profiles",
                style = "color: #FFFFFF; font-weight: 700; margin-top: 0;"),
             
             hr(style = "border-top: 1px solid rgba(255,255,255,0.1);"),
             
             h5("UNIVERSITIES",
                style = "color: var(--bg-surface); font-family: var(--font-display);
                         font-weight: 700; font-size: 12px; letter-spacing: 1px;"),
             
             tags$style(HTML(paste0("
                #", ns("unis"), " ~ .selectize-control .selectize-input {
                  background-color: var(--navy-dark) !important;
                  border: 1px solid var(--navy) !important;
                  color: var(--bg-surface) !important;
                  border-radius: var(--radius-sm) !important;
                  box-shadow: var(--shadow-sm) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-input input {
                  color: var(--bg-surface) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-input input::placeholder {
                  color: var(--text-muted) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-dropdown {
                  background-color: var(--navy-deeper) !important;
                  border: 1px solid var(--navy) !important;
                  border-radius: var(--radius-sm) !important;
                  box-shadow: var(--shadow-md) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-dropdown .option {
                  color: var(--text-muted) !important;
                  background-color: var(--navy-deeper) !important;
                  transition: background-color var(--ease) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-dropdown .option.active {
                  background-color: var(--navy) !important;
                  color: var(--bg-surface) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-input .item {
                  background-color: var(--red) !important;
                  color: var(--bg-surface) !important;
                  border-radius: var(--radius-sm) !important;
                  border: none !important;
                  font-size: 11px !important;
                  font-family: var(--font-display) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-input .item .remove {
                  border-left: 1px solid var(--red-dark) !important;
                  color: var(--bg-surface) !important;
                }
                #", ns("unis"), " ~ .selectize-control .selectize-input .item .remove:hover {
                  background-color: var(--red-dark) !important;
                }
             "))),
             
             selectizeInput(ns("unis"),
                            label    = NULL,
                            choices  = NULL,
                            multiple = TRUE,
                            options  = list(
                              maxItems    = 3,
                              placeholder = "Type or select...",
                              searchField = "label"
                            )
             )
           ),
           
           div(
             style = "margin-top: 16px;",
             uiOutput(ns("values_table"))
           )
    ),
    
    column(width = 9,
           fluidRow(
             column(width = 6,
                    box(title = "Advanced", width = 12, solidHeader = TRUE,
                        status = "primary", 
                        highchartOutput(ns("spider_advanced"), height = "320px"))
             ),
             column(width = 6,
                    box(title = "Career Totals", width = 12, solidHeader = TRUE,
                        status = "primary", 
                        highchartOutput(ns("spider_totals"), height = "320px"))
             )
           ),
           fluidRow(
             column(width = 6,
                    box(title = "Per Game", width = 12, solidHeader = TRUE,
                        status = "primary", 
                        highchartOutput(ns("spider_averages"), height = "320px"))
             ),
             column(width = 6,
                    box(title = "Shooting %", width = 12, solidHeader = TRUE,
                        status = "primary", 
                        highchartOutput(ns("spider_percentages"), height = "320px"))
             )
           )
    )
  )
}
# ── 2. Server ─────────────────────────────────────────────────
mod_spider_server <- function(id, data, nba_title, nba_subtitle, nba_tooltip) {
  moduleServer(id, function(input, output, session) {
    
    min_players <- 20
    
    unis_available <- reactive({
      data %>%
        dplyr::distinct(player, college) %>%
        dplyr::group_by(college) %>%
        dplyr::summarise(n_players = dplyr::n_distinct(player), .groups = "drop") %>%
        dplyr::filter(n_players >= min_players, college != "Didn't play College") %>%
        dplyr::arrange(desc(n_players)) %>%
        dplyr::pull(college)
    })
    
    observe({
      unis <- unis_available()
      updateSelectizeInput(session, "unis",
                           choices  = unis,
                           selected = unis[1:3],
                           server   = TRUE
      )
    })
    
    # ── Percentiles ───────────────────────────────────────────
    percentile_data <- reactive({
      req(input$unis)
      unis_sel     <- input$unis
      all_metrics  <- unlist(lapply(spider_categories, `[[`, "metrics"))
      tidy_metrics <- setdiff(all_metrics, "overall_pick")
      
      valid_unis <- data %>%
        dplyr::distinct(player, college) %>%
        dplyr::group_by(college) %>%
        dplyr::summarise(n = dplyr::n_distinct(player), .groups = "drop") %>%
        dplyr::filter(n >= min_players) %>%
        dplyr::pull(college)
      
      tidy_means <- data %>%
        dplyr::filter(college %in% valid_unis, indicator %in% tidy_metrics) %>%
        dplyr::group_by(college, indicator) %>%
        dplyr::summarise(avg = mean(as.numeric(value), na.rm = TRUE), .groups = "drop")
      
      pick_means <- data %>%
        dplyr::filter(college %in% valid_unis) %>%
        dplyr::distinct(player, college, overall_pick) %>%
        dplyr::group_by(college) %>%
        dplyr::summarise(avg = mean(overall_pick, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(indicator = "overall_pick")
      
      dplyr::bind_rows(tidy_means, pick_means) %>%
        dplyr::group_by(indicator) %>%
        dplyr::mutate(
          n_unis   = dplyr::n(),
          rnk      = if (dplyr::first(indicator) %in% invert_metrics) {
            rank(avg,  ties.method = "min")
          } else {
            rank(-avg, ties.method = "min")
          },
          draw_val = round((n_unis - rnk) / (n_unis - 1) * 100, 1)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(college %in% unis_sel)
    })
    
    # ── Helper: construye un spider para una categoría ────────
    make_spider <- function(df, cat_key, unis_sel, title_style, subtitle_style) {
      metrics    <- spider_categories[[cat_key]]$metrics
      cat_df     <- df %>% dplyr::filter(indicator %in% metrics)
      categories <- unname(sapply(metrics, function(m) {
        if (m %in% names(label_map)) label_map[[m]] else m
      }))
      
      hc <- highchart() %>%
        hc_chart(
          polar           = TRUE,
          type            = "area",
          backgroundColor = "transparent",
          style           = list(fontFamily = "Trebuchet MS")
        ) %>%
        hc_xAxis(
          categories        = categories,
          tickmarkPlacement = "on",
          lineWidth         = 0,
          labels            = list(
            style = list(fontSize = "10px", color = "#4A5568", fontWeight = "600")
          )
        ) %>%
        hc_yAxis(
          gridLineInterpolation = "polygon",
          lineWidth             = 0,
          min                   = 0,
          max                   = 100,
          tickInterval          = 25,
          gridLineColor         = "#E2E8F0",
          labels                = list(enabled = FALSE),
          plotBands = list(
            list(from = 75, to = 100, color = "rgba(29,66,138,0.06)"),
            list(from = 50, to = 75,  color = "rgba(29,66,138,0.03)"),
            list(from = 25, to = 50,  color = "rgba(200,16,46,0.02)"),
            list(from = 0,  to = 25,  color = "rgba(200,16,46,0.05)")
          ),
          plotLines = list(list(
            value     = 50,
            color     = "#8A93A6",
            dashStyle = "ShortDash",
            width     = 1,
            label     = list(
              text  = "50",
              align = "right",
              style = list(color = "#8A93A6", fontSize = "9px")
            )
          ))
        ) %>%
        hc_tooltip(
          shared          = TRUE,
          useHTML         = TRUE,
          backgroundColor = "#091840",
          borderColor     = "#1D428A",
          borderRadius    = 8,
          borderWidth     = 1,
          formatter       = JS("function() {
            var cat = this.points[0].key;
            var rows = this.points.map(function(p) {
              var real = p.point.real !== undefined ? p.point.real : p.y.toFixed(1);
              var rnk  = p.point.rnk  !== undefined ? p.point.rnk  : '—';
              return '<div style=\"display:flex;align-items:center;gap:6px;padding:2px 0;\">'
                + '<span style=\"color:' + p.series.color + ';font-size:12px\">&#9679;</span>'
                + '<span style=\"color:#C8D6E5;flex:1;font-size:11px\">' + p.series.name + '</span>'
                + '<span style=\"color:#fff;font-weight:700;font-size:12px\">' + real + '</span>'
                + '<span style=\"color:#8A93A6;font-size:10px;margin-left:3px\">(#' + rnk + ')</span>'
                + '</div>';
            }).join('');
            return '<div style=\"padding:6px 10px 4px;font-family:Trebuchet MS;\">'
              + '<div style=\"font-size:10px;color:#8A93A6;border-bottom:1px solid rgba(255,255,255,0.1);'
              + 'padding-bottom:4px;margin-bottom:4px;\">' + cat + '</div>'
              + rows + '</div>';
          }")
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_plotOptions(
          area = list(
            lineWidth      = 2,
            pointPlacement = "on",
            marker         = list(
              enabled = TRUE,
              radius  = 3,
              states  = list(hover = list(enabled = TRUE, radius = 5))
            )
          )
        ) %>%
        hc_title(
          text  = spider_categories[[cat_key]]$label,
          style = title_style$style
        ) %>%
        hc_add_theme(hc_theme_smpl())
      
      for (i in seq_along(unis_sel)) {
        uni <- unis_sel[i]
        pts <- lapply(metrics, function(m) {
          v        <- cat_df %>% dplyr::filter(college == uni, indicator == m)
          draw     <- if (nrow(v) == 0) NA_real_ else v$draw_val[1]
          rnk_val  <- if (nrow(v) == 0) NA_real_ else round(v$rnk[1], 0)
          real_val <- if (nrow(v) == 0) NA_real_ else round(v$avg[1], 2)
          real_fmt <- if (m == "overall_pick" && !is.na(real_val)) {
            paste0("#", real_val)
          } else {
            as.character(real_val)
          }
          list(y = draw, real = real_fmt, rnk = rnk_val)
        })
        hc <- hc %>%
          hc_add_series(
            name      = uni,
            data      = pts,
            color     = uni_colors[i],
            fillColor = list(
              linearGradient = list(x1 = 0, y1 = 0, x2 = 0, y2 = 1),
              stops = list(
                list(0, paste0(uni_colors[i], "55")),
                list(1, paste0(uni_colors[i], "08"))
              )
            )
          )
      }
      hc
    }
    
    # ── Cuatro renders, uno por categoría ─────────────────────
    output$spider_advanced    <- renderHighchart({
      df <- percentile_data(); req(nrow(df) > 0)
      make_spider(df, "advanced",    input$unis, nba_title, nba_subtitle)
    })
    output$spider_totals      <- renderHighchart({
      df <- percentile_data(); req(nrow(df) > 0)
      make_spider(df, "totals",      input$unis, nba_title, nba_subtitle)
    })
    output$spider_averages    <- renderHighchart({
      df <- percentile_data(); req(nrow(df) > 0)
      make_spider(df, "averages",    input$unis, nba_title, nba_subtitle)
    })
    output$spider_percentages <- renderHighchart({
      df <- percentile_data(); req(nrow(df) > 0)
      make_spider(df, "percentages", input$unis, nba_title, nba_subtitle)
    })
    
    # ── Tabla: valor / #rank sin colores de percentil ─────────
    output$values_table <- renderUI({
      df       <- percentile_data()
      req(nrow(df) > 0)
      unis_sel <- input$unis
      uni_hex  <- uni_colors[seq_along(unis_sel)]
      uni_tcols <- uni_text_colors[seq_along(unis_sel)]
      
      # Todas las métricas de todas las categorías
      all_metrics <- unlist(lapply(spider_categories, `[[`, "metrics"))
      
      # Cabecera
      header_cells <- paste(
        "<th style='width:90px'>Metric</th>",
        paste(sapply(seq_along(unis_sel), function(i) {
          paste0(
            "<th style='text-align:center'>",
            "<span style='display:inline-block;width:8px;height:8px;",
            "border-radius:50%;background:", uni_hex[i], ";margin-right:5px;",
            "vertical-align:middle;'></span>",
            "<span style='color:", uni_tcols[i], ";font-weight:700;font-size:11px;",
            "letter-spacing:0.5px;'>", htmltools::htmlEscape(unis_sel[i]), "</span>",
            "</th>"
          )
        }), collapse = "")
      )
      
      # Separador de sección
      make_section_header <- function(label, n_cols) {
        paste0(
          "<tr style='background:linear-gradient(90deg,var(--navy-deeper),var(--navy-dark));'>",
          "<td colspan='", n_cols + 1, "' style='padding:6px 10px;'>",
          "<span style='font-size:10px;font-weight:700;color:var(--red);",
          "text-transform:uppercase;letter-spacing:1.2px;'>", label, "</span>",
          "</td></tr>"
        )
      }
      
      # Filas de una categoría
      make_rows <- function(cat_key, df, unis_sel, uni_hex, start_idx) {
        metrics <- spider_categories[[cat_key]]$metrics
        rows <- sapply(seq_along(metrics), function(mi) {
          m            <- metrics[mi]
          metric_label <- if (m %in% names(label_map)) label_map[[m]] else m
          row_bg       <- if ((start_idx + mi) %% 2 == 0) "background:rgba(255,255,255,0.02);" else ""
          
          cells <- paste(sapply(seq_along(unis_sel), function(i) {
            uni      <- unis_sel[i]
            v        <- df %>% dplyr::filter(college == uni, indicator == m)
            has_data <- nrow(v) > 0
            real_val <- if (!has_data) NA_real_ else round(v$avg[1], 2)
            rnk_val  <- if (!has_data) NA_real_ else round(v$rnk[1], 0)
            val_fmt  <- if (is.na(real_val)) "—" else {
              if (m == "overall_pick") paste0("#", real_val) else as.character(real_val)
            }
            rnk_fmt  <- if (is.na(rnk_val)) "—" else paste0("#", rnk_val)
            
            paste0(
              "<td style='text-align:center;padding:7px 6px;'>",
              "<span style='font-size:13px;font-weight:700;color:#ffffff;",
              "font-family:var(--font-display);'>", val_fmt, "</span>",
              "<span style='display:block;font-size:10px;color:var(--text-muted);",
              "margin-top:2px;'>", rnk_fmt, "</span>",
              "</td>"
            )
          }), collapse = "")
          
          paste0(
            "<tr style='border-bottom:1px solid rgba(29,66,138,0.2);", row_bg, "'>",
            "<td style='padding:7px 10px;'>",
            "<span style='font-size:11px;font-weight:600;color:var(--text-muted);",
            "text-transform:uppercase;letter-spacing:0.8px;'>", metric_label, "</span>",
            "</td>",
            cells, "</tr>"
          )
        })
        paste(rows, collapse = "")
      }
      
      # Construir tbody con secciones
      n_cols   <- length(unis_sel)
      row_idx  <- 0
      tbody_html <- paste(sapply(names(spider_categories), function(cat_key) {
        section_rows <- make_rows(cat_key, df, unis_sel, uni_hex, row_idx)
        row_idx <<- row_idx + length(spider_categories[[cat_key]]$metrics)
        paste0(
          make_section_header(spider_categories[[cat_key]]$label, n_cols),
          section_rows
        )
      }), collapse = "")
      
      tagList(
        tags$style(HTML("
          .spider-metric-table {
            width: 100%;
            border-collapse: collapse;
            font-family: var(--font-body);
          }
          .spider-metric-table thead tr {
            background: linear-gradient(90deg, var(--navy-deeper), var(--navy-dark));
            border-bottom: 2px solid var(--red);
          }
          .spider-metric-table thead th {
            padding: 10px 8px;
            font-size: 10px;
            font-weight: 700;
            color: var(--text-muted);
            text-transform: uppercase;
            letter-spacing: 1px;
            text-align: left;
          }
          .spider-metric-table tbody tr:hover {
            background: rgba(29,66,138,0.18) !important;
            transition: background var(--ease);
          }
        ")),
        div(
          style = "background:var(--navy-deeper);border:1px solid var(--navy);
                   border-radius:var(--radius-lg);overflow:hidden;box-shadow:var(--shadow-md);",
          # Título
          div(
            style = "padding:12px 16px 10px;
                     background:linear-gradient(90deg,var(--navy-deeper),var(--navy-dark));
                     border-bottom:1px solid var(--navy);
                     display:flex;align-items:center;gap:8px;",
            tags$span(style = "color:var(--red);font-size:13px;",
                      tags$i(class = "fa fa-table")),
            tags$span(
              style = "color:#fff;font-weight:700;font-size:13px;
                       font-family:var(--font-display);letter-spacing:0.5px;",
              "Metric Breakdown"
            ),
            tags$span(
              style = "margin-left:auto;font-size:10px;color:var(--text-muted);",
              paste0(length(unis_sel), " universities")
            )
          ),
          div(
            style = "overflow-x:auto;max-height:620px;overflow-y:auto;",
            HTML(paste0(
              "<table class='spider-metric-table'>",
              "<thead><tr>", header_cells, "</tr></thead>",
              "<tbody>", tbody_html, "</tbody>",
              "</table>"
            ))
          )
        )
      )
    })
    
  })
}