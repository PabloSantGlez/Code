# ============================================================
#  Módulo: Quality vs Volume — Scatter/Bubble Chart
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
mod_scatter_server <- function(id, data, metric_sel, nba_title, nba_subtitle, nba_tooltip) {
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
                filter(n_players >= 3) %>%
                arrange(desc(avg_metric))
        })

        output$scatter_chart <- renderHighchart({
            df <- scatter_data()
            req(nrow(df) > 0)

            # Normalize bubble size (z) for display
            z_max <- max(abs(df$total_metric), na.rm = TRUE)
            if (z_max == 0) z_max <- 1

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
                                fontSize        = "9px",
                                color           = "#0F2355",
                                fontWeight      = "600",
                                textOutline     = "1.5px #FFFFFF",
                                fontFamily      = "Trebuchet MS"
                            ),
                            y = -12
                        )
                    } else {
                        list(enabled = FALSE)
                    }
                )
            })

            highchart() %>%
                hc_chart(
                    type            = "bubble",
                    backgroundColor = "transparent",
                    animation       = list(duration = 600),
                    style           = list(fontFamily = "Trebuchet MS"),
                    zoomType        = "xy"
                ) %>%
                hc_plotOptions(
                    bubble = list(
                        minSize = "4%",
                        maxSize = "14%",
                        softThreshold = TRUE
                    ),
                    series = list(
                        animation = list(duration = 600)
                    )
                ) %>%
                hc_xAxis(
                    title = list(
                        text  = "Players Drafted",
                        style = list(color = "#718096", fontSize = "11px", fontWeight = "600")
                    ),
                    gridLineColor = "rgba(226,232,240,0.4)",
                    gridLineDashStyle = "Dot",
                    gridLineWidth = 1,
                    lineColor = "#E2E8F0",
                    labels = list(
                        style = list(color = "#A0AEC0", fontSize = "10px")
                    )
                ) %>%
                hc_yAxis(
                    title = list(
                        text  = paste0("Avg ", metric_sel()),
                        style = list(color = "#718096", fontSize = "11px", fontWeight = "600")
                    ),
                    gridLineColor = "rgba(226,232,240,0.6)",
                    gridLineDashStyle = "Dot",
                    labels = list(
                        style = list(color = "#A0AEC0", fontSize = "10px")
                    )
                ) %>%
                hc_add_series(
                    name         = "Universities",
                    data         = point_data,
                    showInLegend = FALSE
                ) %>%
                hc_tooltip(
                    useHTML         = TRUE,
                    backgroundColor = "rgba(9, 24, 64, 0.97)",
                    borderColor     = "#1D428A",
                    borderRadius    = 8,
                    borderWidth     = 1,
                    shadow          = TRUE,
                    formatter       = JS("function() {
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
                hc_title(
                    text = "Quality vs Volume",
                    style = list(
                        color      = "#0F2355",
                        fontFamily = "Trebuchet MS",
                        fontWeight = "700",
                        fontSize   = "16px"
                    )
                ) %>%
                hc_subtitle(
                    text  = paste0("Bubble size = total cumulative ", metric_sel(), " | Top 5 labeled"),
                    style = list(color = "#8A93A6", fontSize = "11px")
                ) %>%
                hc_legend(enabled = FALSE) %>%
                hc_add_theme(hc_theme_smpl())
        })
    })
}
