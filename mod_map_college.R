# ============================================================
#  Módulo: Mapa de Talento por Estado (mod_map_college.R)
# ============================================================

mod_map_college_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("

        /* ── Modal general ─────────────────────────────── */
        .modal-content {
          background: #0a1628 !important;
          border: 1px solid #1D428A !important;
          border-radius: 12px !important;
          color: #e8edf5 !important;
        }
        .modal-header {
          background: linear-gradient(135deg, #091840 0%, #0F2355 100%) !important;
          border-bottom: 1px solid #1D428A !important;
          border-radius: 12px 12px 0 0 !important;
          padding: 16px 20px !important;
        }
        .modal-footer {
          background: #091840 !important;
          border-top: 1px solid #1D428A !important;
          border-radius: 0 0 12px 12px !important;
        }
        .modal-body {
          padding: 20px !important;
          background: #0a1628 !important;
        }
        .modal-title { color: #ffffff !important; }
        .close { color: #8A93A6 !important; opacity: 1 !important; }
        .close:hover { color: #ffffff !important; }

        /* ── KPI cards ─────────────────────────────────── */
        .nba-kpi-row {
          display: flex;
          gap: 12px;
          margin-bottom: 20px;
        }
        .nba-kpi {
          flex: 1;
          background: linear-gradient(135deg, #0F2355 0%, #1a3568 100%);
          border: 1px solid #1D428A;
          border-radius: 10px;
          padding: 14px 16px;
          text-align: center;
          position: relative;
          overflow: hidden;
        }
        .nba-kpi::before {
          content: '';
          position: absolute;
          top: 0; left: 0; right: 0;
          height: 3px;
          background: linear-gradient(90deg, #C8102E, #1D428A);
        }
        .nba-kpi-value {
          font-size: 26px;
          font-weight: 800;
          color: #ffffff;
          line-height: 1;
          font-family: 'Georgia', serif;
          letter-spacing: -0.5px;
        }
        .nba-kpi-value.highlight { color: #F0C040; }
        .nba-kpi-label {
          font-size: 10px;
          font-weight: 600;
          color: #8A93A6;
          text-transform: uppercase;
          letter-spacing: 1.2px;
          margin-top: 6px;
        }
        .nba-kpi-icon {
          font-size: 11px;
          color: #C8102E;
          margin-bottom: 4px;
        }

        /* ── Divider ────────────────────────────────────── */
        .nba-divider {
          border: none;
          border-top: 1px solid #1D428A;
          margin: 16px 0;
        }
        .nba-section-title {
          font-size: 11px;
          font-weight: 700;
          color: #8A93A6;
          text-transform: uppercase;
          letter-spacing: 1.5px;
          margin-bottom: 12px;
        }

        /* ── Tabla NBA ──────────────────────────────────── */
        .nba-table-wrap {
          overflow-x: auto;
          border-radius: 8px;
          border: 1px solid #1D428A;
        }
        .nba-table {
          width: 100%;
          border-collapse: collapse;
          font-size: 12px;
          table-layout: fixed;
        }
        .nba-table thead tr {
          background: linear-gradient(90deg, #091840, #0F2355);
        }
        .nba-table thead th {
          padding: 10px 12px;
          text-align: center;
          font-size: 10px;
          font-weight: 700;
          color: #8A93A6;
          text-transform: uppercase;
          letter-spacing: 1px;
          border-bottom: 2px solid #C8102E;
          white-space: nowrap;
        }
        .nba-table thead th:first-child {
          text-align: left;
          width: 160px;
        }
        .nba-table tbody tr {
          border-bottom: 1px solid rgba(29,66,138,0.3);
          transition: background 0.15s;
        }
        .nba-table tbody tr:hover {
          background: rgba(29,66,138,0.25) !important;
        }
        .nba-table tbody tr:nth-child(even) {
          background: rgba(255,255,255,0.02);
        }
        .nba-table tbody td {
          padding: 9px 12px;
          color: #c8d4e8;
          text-align: center;
          white-space: nowrap;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .nba-table tbody td:first-child {
          text-align: left;
          color: #ffffff;
          font-weight: 600;
          font-size: 12px;
          max-width: 160px;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        /* badge de jugadores */
        .badge-players {
          display: inline-block;
          background: #C8102E;
          color: #fff;
          border-radius: 20px;
          padding: 2px 9px;
          font-size: 11px;
          font-weight: 700;
          min-width: 28px;
        }
        /* colores condicionales (Aplicado ahora a todas las columnas) */
        .val-high  { color: #4ade80 !important; font-weight: 700; }
        .val-mid   { color: #FFFFFF !important; }
        .val-low   { color: #f87171 !important; }

        /* botón close del modal */
        .btn-modal-close {
          background: linear-gradient(90deg, #C8102E, #a00d24) !important;
          color: #fff !important;
          border: none !important;
          border-radius: 6px !important;
          padding: 6px 18px !important;
          font-size: 12px !important;
          font-weight: 600 !important;
          letter-spacing: 0.5px !important;
        }
      "))
    ),
    div(
      style = "width: 100%; height: calc(100vh - 220px); background-color: var(--bg-body);",
      highchartOutput(ns("college_map"), height = "100%")
    ),
    
    uiOutput(ns("state_panel"))
  )
}

mod_map_college_server <- function(id, data, raw_data, nba_title, nba_subtitle) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    us_map_data <- readRDS("us_map_data.rds")
    
    # ── Mapping universidad -> estado ─────────────────────────
    colleges_list  <- unique(data$college)
    states_id_list <- c(
      "us-ky", "us-nc", "us-az", "us-mi", "us-nc", "us-ok", "us-fl", "us-la", "us-ga", "us-ca",
      "us-il", "us-ut", "us-tx", "us-ca", "us-ma", "us-tx", "us-ia", "us-ga", "us-nc", "us-mo",
      "us-nj", NA,      "us-ny", "us-tn", "us-tn", "us-ky", "us-il", "us-in", "us-co", "us-ct",
      "us-al", "us-pa", "us-la", "us-nc", "us-hi", "us-in", "us-la", "us-al", "us-al", "us-ok",
      "us-wa", "us-md", "us-nj", "us-ga", "us-or", "us-pa", "us-ca", "us-mn", "us-oh", "us-tx",
      "us-oh", "us-md", "us-pa", "us-fl", "us-ms", "us-ny", "us-mo", "us-fl", "us-sc", "us-tx",
      "us-oh", "us-nh", "us-ks", "us-va", "us-in", "us-va", "us-ks", "us-nc", "us-ca", "us-tn",
      "us-wi", "us-ri", "us-nv", "us-dc", "us-mi", "us-nm", "us-or", "us-ne", "us-va", "us-ia",
      "us-la", "us-ok", "us-co", "us-nm", "us-ne", "us-pa", "us-pa", "us-tn", "us-fl", "us-ny",
      "us-va", "us-va", "us-la", "us-ok", "us-nj", "us-az", "us-wi", "us-mi", "us-oh", "us-in",
      "us-ar", "us-ms", "us-ca", "us-va", "us-ca", "us-tx", "us-wi", "us-al", "us-nc", "us-nc",
      "us-pa", "us-nc", "us-va", "us-ky", "us-ky", "us-ct", "us-ms", "us-la", "us-oh", "us-ca",
      "us-va", "us-ms", "us-ky", "us-wa", "us-la", "us-ut", "us-tx", "us-de", "us-ca", "us-tn",
      "us-dc", "us-mo", "us-al", "us-pa", "us-ok", "us-ok", "us-sc", "us-sc", "us-wy", "us-wi",
      "us-il", "us-ma", "us-nc", "us-ca", "us-tx", "us-pa", "us-ks", "us-il", "us-ms", "us-oh",
      "us-ca", "us-ar", "us-oh", "us-fl", "us-wv", "us-pa", "us-oh", "us-ny", "us-ca", "us-tn",
      "us-il", "us-tn", "us-ca", "us-sc", "us-ks", "us-wv", "us-ia", "us-wa", "us-ca", "us-in",
      "us-in", "us-fl", "us-oh", "us-ri", "us-oh", "us-ks", "us-tx", "us-mn", "us-il", "us-il",
      "us-pa", "us-va", "us-id", "us-ny", "us-la", "us-ca", "us-id", "us-az", "us-fl", "us-tx",
      "us-il", "us-ms", "us-nc", "us-ar", "us-wv", "us-ca", "us-ct", "us-mi", "us-nd", "us-mi",
      "us-nv", "us-pa", "us-il", "us-nc", "us-ny", "us-tx", "us-oh", "us-fl", "us-wa", "us-tx",
      "us-ny", "us-nj", "us-in", "us-hi", "us-nc", "us-fl", "us-ca", "us-tn", "us-in", "us-oh",
      "us-mi", "us-ut", "us-ny", "us-ca", "us-va", "us-pa", "us-tx", "us-sd", "us-pa", "us-ks",
      "us-ga", "us-va", "us-mo", "us-tx", "us-tn", "us-ct", "us-il", "us-ut"
    )
    
    mapping <- tibble::tibble(
      college  = colleges_list,
      state_id = states_id_list
    ) %>% dplyr::filter(!is.na(state_id))
    
    count_players <- function(df, state_filter = NULL) {
      base <- df %>%
        dplyr::distinct(player, college) %>%
        dplyr::left_join(mapping, by = "college")
      if (!is.null(state_filter)) base <- base %>% dplyr::filter(state_id == state_filter)
      else                        base <- base %>% dplyr::filter(!is.na(state_id))
      base %>%
        dplyr::group_by(college) %>%
        dplyr::summarise(players = dplyr::n_distinct(player), .groups = "drop")
    }
    
    avg_pick_by_college <- raw_data %>%
      dplyr::distinct(player, college, overall_pick) %>%
      dplyr::group_by(college) %>%
      dplyr::summarise(avg_overall_pick = round(mean(overall_pick, na.rm = TRUE), 1), .groups = "drop")
    
    state_names <- c(
      "us-al"="Alabama","us-ak"="Alaska","us-az"="Arizona","us-ar"="Arkansas",
      "us-ca"="California","us-co"="Colorado","us-ct"="Connecticut","us-de"="Delaware",
      "us-fl"="Florida","us-ga"="Georgia","us-hi"="Hawaii","us-id"="Idaho",
      "us-il"="Illinois","us-in"="Indiana","us-ia"="Iowa","us-ks"="Kansas",
      "us-ky"="Kentucky","us-la"="Louisiana","us-me"="Maine","us-md"="Maryland",
      "us-ma"="Massachusetts","us-mi"="Michigan","us-mn"="Minnesota","us-ms"="Mississippi",
      "us-mo"="Missouri","us-mt"="Montana","us-ne"="Nebraska","us-nv"="Nevada",
      "us-nh"="New Hampshire","us-nj"="New Jersey","us-nm"="New Mexico","us-ny"="New York",
      "us-nc"="North Carolina","us-nd"="North Dakota","us-oh"="Ohio","us-ok"="Oklahoma",
      "us-or"="Oregon","us-pa"="Pennsylvania","us-ri"="Rhode Island","us-sc"="South Carolina",
      "us-sd"="South Dakota","us-tn"="Tennessee","us-tx"="Texas","us-ut"="Utah",
      "us-vt"="Vermont","us-va"="Virginia","us-wa"="Washington","us-wv"="West Virginia",
      "us-wi"="Wisconsin","us-wy"="Wyoming","us-dc"="Washington D.C."
    )
    
    # ── Mapa ──────────────────────────────────────────────────
    output$college_map <- renderHighchart({
      
      players_by_state <- data %>%
        dplyr::distinct(player, college) %>%
        dplyr::left_join(mapping, by = "college") %>%
        dplyr::filter(!is.na(state_id)) %>%
        dplyr::group_by(state_id) %>%
        dplyr::summarise(total_players = dplyr::n_distinct(player), .groups = "drop")
      
      perf_by_state <- data %>%
        dplyr::filter(indicator %in% c("win_shares_per_48_minutes", "overall_performance")) %>%
        dplyr::left_join(mapping, by = "college") %>%
        dplyr::filter(!is.na(state_id)) %>%
        dplyr::group_by(state_id, indicator) %>%
        dplyr::summarise(avg_val = round(mean(as.numeric(value), na.rm = TRUE), 3), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = indicator, values_from = avg_val) %>%
        dplyr::rename(avg_ws48 = win_shares_per_48_minutes, avg_ov_perf = overall_performance)
      
      avg_pick_state <- raw_data %>%
        dplyr::distinct(player, college, overall_pick) %>%
        dplyr::left_join(mapping, by = "college") %>%
        dplyr::filter(!is.na(state_id)) %>%
        dplyr::group_by(state_id) %>%
        dplyr::summarise(avg_pick = round(mean(overall_pick, na.rm = TRUE), 1), .groups = "drop")
      
      colleges_by_state <- data %>%
        dplyr::distinct(college) %>%
        dplyr::left_join(mapping, by = "college") %>%
        dplyr::filter(!is.na(state_id)) %>%
        dplyr::group_by(state_id) %>%
        dplyr::summarise(
          college_names = paste(sort(unique(college)), collapse = "<br/>• "),
          n_colleges    = dplyr::n_distinct(college),
          .groups       = "drop"
        )
      
      geo_df <- players_by_state %>%
        dplyr::left_join(perf_by_state,     by = "state_id") %>%
        dplyr::left_join(avg_pick_state,    by = "state_id") %>%
        dplyr::left_join(colleges_by_state, by = "state_id")
      
      if (nrow(geo_df) == 0) {
        return(highchart() %>% hc_title(text = "No data") %>% hc_add_theme(hc_theme_smpl()))
      }
      
      click_input_id <- ns("map_click")
      
      highchart(type = "map") %>%
        hc_add_series_map(
          map = us_map_data, df = geo_df,
          value = "total_players", joinBy = c("hc-key", "state_id"),
          name = "Total Players", borderColor = "#FFFFFF", borderWidth = 0.5
        ) %>%
        hc_colorAxis(
          minColor = "#CED4DA",
          maxColor = "#C8102E",
          showInLegend = FALSE,
          stops = color_stops(3, c("#CED4DA", "#1D428A", "#C8102E"))
        ) %>%
        hc_chart(backgroundColor = "transparent") %>%
        hc_plotOptions(series = list(
          cursor = "pointer",
          point  = list(events = list(
            click = JS(paste0("function(){Shiny.setInputValue('", click_input_id, "',this['hc-key'],{priority:'event'});}")
            )
          ))
        )) %>%
        hc_title(text = "USA Talent Map: College Impact by State", style = nba_title$style) %>%
        hc_subtitle(text = "Click on a state to see university breakdown", style = nba_subtitle$style) %>%
        hc_tooltip(
          useHTML = TRUE, backgroundColor = "#091840", borderColor = "#1D428A",
          style   = list(color = "#FFFFFF", fontSize = "12px"),
          formatter = JS("function() {
            var p = this.point;
            var colleges = p.college_names ? '• ' + p.college_names : '<i>No data</i>';
            return '<span style=\"font-size:13px;font-weight:700;color:#F0C040\">' + p.name + '</span><br/>'
              + '<hr style=\"border-color:#1D428A;margin:4px 0\"/>'
              + '<b>Total Players:</b> '    + (p.value || 'N/A') + '<br/>'
              + '<b style=\"color:#8A93A6\">Universities (' + (p.n_colleges || 0) + '):</b><br/>'
              + '<span style=\"color:#A0B0C8;font-size:11px;line-height:1.8\">' + colleges + '</span><br/><br/>'
              + '<i style=\"color:#F0C040\">&#9658; Click to expand details</i>';
          }")
        ) %>%
        hc_mapNavigation(enabled = TRUE) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
    # ── Helper: genera HTML de la tabla NBA con múltiples colores ───
    make_nba_table <- function(df) {
      
      # Cabecera
      header <- paste0(
        "<thead><tr>",
        "<th>University</th>",
        "<th>Players</th>",
        "<th>Avg Pick</th>",
        "<th>Yrs</th>",
        "<th>MIN</th>",
        "<th>PTS</th>",
        "<th>AST</th>",
        "<th>REB</th>",
        "<th>WS/48</th>",
        "<th>VORP</th>",
        "<th>Ov.Perf</th>",
        "</tr></thead>"
      )
      
      # Filas
      rows <- apply(df, 1, function(r) {
        
        # Helper para redondear y formatear texto
        fmt  <- function(x, d = 2) {
          v <- suppressWarnings(as.numeric(x))
          if (is.na(v)) "—" else format(round(v, d), nsmall = d)
        }
        
        # Helper para la lógica de colores de todas las columnas
        get_cls <- function(val_str, col_name, invert = FALSE) {
          v <- suppressWarnings(as.numeric(val_str))
          m <- mean(as.numeric(df[[col_name]]), na.rm = TRUE)
          
          if (is.na(v) || is.na(m) || m == 0) return("")
          
          if (invert) { 
            # Lógica invertida para Draft Pick (menor es mejor)
            if (v <= m * 0.8) return("val-high")
            if (v <= m * 1.2) return("val-mid")
            return("val-low")
          } else {
            # Lógica normal para el resto de estadísticas
            if (m > 0) {
              if (v >= m * 1.2) return("val-high")
              if (v >= m * 0.8) return("val-mid")
              return("val-low")
            } else { 
              # Si la media es negativa (ocurre a veces con VORP)
              if (v >= m * 0.8) return("val-high")
              if (v >= m * 1.2) return("val-mid")
              return("val-low")
            }
          }
        }
        
        full_name  <- r["college"]
        short_name <- if (nchar(full_name) > 28) paste0(substr(full_name, 1, 26), "…") else full_name
        
        paste0(
          "<tr>",
          "<td title='", htmltools::htmlEscape(full_name), "'>", htmltools::htmlEscape(short_name), "</td>",
          "<td><span class='badge-players'>", r["players"], "</span></td>",
          "<td class='", get_cls(r["avg_overall_pick"], "avg_overall_pick", TRUE), "'>#", fmt(r["avg_overall_pick"], 1), "</td>",
          "<td class='", get_cls(r["years_active"], "years_active"), "'>",  fmt(r["years_active"],  1), "</td>",
          "<td class='", get_cls(r["average_minutes_played"], "average_minutes_played"), "'>",  fmt(r["average_minutes_played"], 1), "</td>",
          "<td class='", get_cls(r["points_per_game"], "points_per_game"), "'>",  fmt(r["points_per_game"],   1), "</td>",
          "<td class='", get_cls(r["average_assists"], "average_assists"), "'>",  fmt(r["average_assists"],   1), "</td>",
          "<td class='", get_cls(r["average_total_rebounds"], "average_total_rebounds"), "'>",  fmt(r["average_total_rebounds"], 1), "</td>",
          "<td class='", get_cls(r["win_shares_per_48_minutes"], "win_shares_per_48_minutes"), "'>", fmt(r["win_shares_per_48_minutes"], 3), "</td>",
          "<td class='", get_cls(r["value_over_replacement"], "value_over_replacement"), "'>",  fmt(r["value_over_replacement"], 2), "</td>",
          "<td class='", get_cls(r["overall_performance"], "overall_performance"), "'>",  fmt(r["overall_performance"], 2), "</td>",
          "</tr>"
        )
      })
      
      paste0(
        "<table class='nba-table'>", header,
        "<tbody>", paste(rows, collapse = ""), "</tbody>",
        "</table>"
      )
    }
    
    # ── Modal ─────────────────────────────────────────────────
    observeEvent(input$map_click, {
      state      <- input$map_click
      state_name <- ifelse(state %in% names(state_names), state_names[state], state)
      
      college_stats <- data %>%
        dplyr::filter(indicator %in% c(
          "years_active", "average_minutes_played", "points_per_game",
          "average_assists", "average_total_rebounds",
          "win_shares_per_48_minutes", "value_over_replacement", "overall_performance"
        )) %>%
        dplyr::left_join(mapping, by = "college") %>%
        dplyr::filter(state_id == state) %>%
        dplyr::group_by(college, indicator) %>%
        dplyr::summarise(avg_val = round(mean(as.numeric(value), na.rm = TRUE), 2), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = indicator, values_from = avg_val) %>%
        dplyr::left_join(count_players(data, state_filter = state), by = "college") %>%
        dplyr::left_join(avg_pick_by_college, by = "college") %>%
        dplyr::arrange(desc(players))
      
      if (nrow(college_stats) == 0) {
        showModal(modalDialog(
          title = paste("No data —", state_name),
          "No NBA draftees found for this state.",
          easyClose = TRUE, footer = modalButton("Close")
        ))
        return()
      }
      
      total_players  <- sum(college_stats$players, na.rm = TRUE)
      avg_pick_val   <- round(mean(college_stats$avg_overall_pick, na.rm = TRUE), 1)
      avg_ws48_val   <- round(mean(college_stats$win_shares_per_48_minutes, na.rm = TRUE), 3)
      
      # Generar la tabla pasando únicamente el DataFrame (las medias se calculan solas dentro)
      tabla_html <- make_nba_table(college_stats)
      
      showModal(modalDialog(
        title = tagList(
          tags$span(
            style = "color:#ffffff; font-weight:800; font-size:17px; letter-spacing:0.5px;",
            tags$i(class = "fa fa-graduation-cap", style = "color:#C8102E; margin-right:8px;"),
            state_name
          )
        ),
        size      = "l",
        easyClose = TRUE,
        footer    = tags$button(
          class            = "btn btn-modal-close",
          `data-dismiss`   = "modal",
          "Close"
        ),
        
        # ── KPI cards ────────────────────────────────────────
        tags$div(class = "nba-kpi-row",
                 tags$div(class = "nba-kpi",
                          tags$div(class = "nba-kpi-icon", tags$i(class = "fa fa-university")),
                          tags$div(class = "nba-kpi-value", nrow(college_stats)),
                          tags$div(class = "nba-kpi-label", "Universities")
                 ),
                 tags$div(class = "nba-kpi",
                          tags$div(class = "nba-kpi-icon", tags$i(class = "fa fa-user")),
                          tags$div(class = "nba-kpi-value highlight", total_players),
                          tags$div(class = "nba-kpi-label", "Players Drafted")
                 ),
                 tags$div(class = "nba-kpi",
                          tags$div(class = "nba-kpi-icon", tags$i(class = "fa fa-hashtag")),
                          tags$div(class = "nba-kpi-value", paste0("#", avg_pick_val)),
                          tags$div(class = "nba-kpi-label", "Avg Draft Pick")
                 ),
                 tags$div(class = "nba-kpi",
                          tags$div(class = "nba-kpi-icon", tags$i(class = "fa fa-trophy")),
                          tags$div(class = "nba-kpi-value", avg_ws48_val),
                          tags$div(class = "nba-kpi-label", "Avg WS / 48")
                 )
        ),
        
        tags$hr(class = "nba-divider"),
        tags$div(class = "nba-section-title", "University Breakdown"),
        
        # ── Tabla HTML custom ────────────────────────────────
        tags$div(
          class = "nba-table-wrap",
          HTML(tabla_html)
        )
      ))
    })
    
  })
}