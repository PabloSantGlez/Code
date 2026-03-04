# ============================================================
#  Module: Quality vs Volume — Scatter/Bubble Chart
#  (mod_scatter_university.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_scatter_ui <- function(id) {
    ns <- NS(id)
    box(
        title = NULL,
        width = 12,
        solidHeader = FALSE,
        status = "primary",
        highchartOutput(ns("scatter_chart"), height = "460px")
    )
}

# ── 2. Server ────────────────────────────────────────────────
mod_scatter_server <- function(id, data, metric_sel) {
    moduleServer(id, function(input, output, session) {
        scatter_data <- reactive({
            met <- metric_sel()

            data %>%
                filter(indicator == met, college != "Didn't play College") %>%
                group_by(college) %>%
                summarise(
                    avg_metric   = round(mean(as.numeric(value), na.rm = TRUE), 3),
                    total_metric = round(sum(as.numeric(value), na.rm = TRUE), 1),
                    n_players    = n_distinct(player),
                    .groups      = "drop"
                ) %>%
                filter(n_players >= 15) %>%
                arrange(desc(avg_metric))
        })

        output$scatter_chart <- renderHighchart({
            df <- scatter_data()
            req(nrow(df) > 0)

            # Assign colors: top 5 highlighted in red, rest in navy
            top5 <- head(df$college, 5)

            point_data <- lapply(seq_len(nrow(df)), function(i) {
                is_top <- df$college[i] %in% top5
                list(
                    x = df$n_players[i],
                    y = df$avg_metric[i],
                    z = abs(df$total_metric[i]),
                    name = df$college[i],
                    college = df$college[i],
                    total = df$total_metric[i],
                    color = if (is_top) "rgba(200,16,46,0.75)" else "rgba(29,66,138,0.50)",
                    dataLabels = if (is_top) {
                        list(
                            enabled = TRUE,
                            format = "{point.college}",
                            style = list(
                                fontSize    = "9px",
                                color       = "#0F2355",
                                fontWeight  = "600",
                                textOutline = "1.5px #FFFFFF",
                                fontFamily  = "Trebuchet MS"
                            ),
                            y = -12
                        )
                    } else {
                        list(enabled = FALSE)
                    }
                )
            })

            highchart() %>%
                hc_nba_base("bubble", extra = list(zoomType = "xy")) %>%
                hc_plotOptions(
                    bubble = list(
                        minSize       = "4%",
                        maxSize       = "14%",
                        softThreshold = TRUE
                    )
                ) %>%
                hc_xAxis(
                    title = list(text = "Players Drafted", style = nba_axis_title_style),
                    gridLineColor = "rgba(226,232,240,0.4)",
                    gridLineDashStyle = "Dot",
                    gridLineWidth = 1,
                    lineColor = "#E2E8F0",
                    labels = list(style = nba_axis_label_style)
                ) %>%
                hc_nba_yaxis(title_text = paste0("Avg ", metric_sel())) %>%
                hc_add_series(
                    name         = "Universities",
                    data         = point_data,
                    showInLegend = FALSE
                ) %>%
                hc_nba_tooltip_dark(
                    formatter = JS("function() {
            var p = this.point;
            return '<div style=\"padding:10px 14px;font-family:Trebuchet MS,sans-serif;min-width:200px;\">'
              + '<div style=\"font-size:13px;font-weight:700;color:#FFFFFF;'
              + 'border-bottom:1px solid rgba(29,66,138,0.5);padding-bottom:7px;margin-bottom:7px;\">'
              + p.college + '</div>'
              + '<div style=\"display:flex;justify-content:space-between;padding:3px 0;\">'
              + '<span style=\"color:#A0AEC0;font-size:11px;\">Avg Value</span>'
              + '<span style=\"color:#FFFFFF;font-weight:700;font-size:12px;\">'
              + p.y.toFixed(3) + '</span></div>'
              + '<div style=\"display:flex;justify-content:space-between;padding:3px 0;\">'
              + '<span style=\"color:#A0AEC0;font-size:11px;\">Players</span>'
              + '<span style=\"color:#FFD700;font-weight:600;font-size:12px;\">'
              + p.x + '</span></div>'
              + '<div style=\"display:flex;justify-content:space-between;padding:3px 0;\">'
              + '<span style=\"color:#A0AEC0;font-size:11px;\">Total</span>'
              + '<span style=\"color:#98ABC8;font-weight:600;font-size:12px;\">'
              + p.total.toFixed(1) + '</span></div>'
              + '</div>';
          }")
                ) %>%
                hc_nba_title("Quality vs Volume") %>%
                hc_nba_subtitle(paste0("Bubble size = total cumulative ", metric_sel(), " | Top 5 labeled")) %>%
                hc_legend(enabled = FALSE)
        })
    })
}
