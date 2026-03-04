# ============================================================
#  Módulo: University Rankings — Horizontal Bar Chart
#  (mod_rankings_universities.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_rankings_ui <- function(id) {
  ns <- NS(id)
  box(
    title = NULL,
    width = 12,
    solidHeader = FALSE,
    status = "primary",
    highchartOutput(ns("rankings_chart"), height = "460px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_rankings_server <- function(id, data, metric_sel, nba_title, nba_subtitle, nba_tooltip) {
  moduleServer(id, function(input, output, session) {
    rankings_data <- reactive({
      data %>%
        filter(indicator == metric_sel()) %>%
        group_by(college) %>%
        summarise(
          avg_metric = round(mean(as.numeric(value), na.rm = TRUE), 3),
          n_players  = n_distinct(player),
          .groups    = "drop"
        ) %>%
        filter(college != "Didn't play College") %>%
        arrange(desc(avg_metric)) %>%
        head(15)
    })

    output$rankings_chart <- renderHighchart({
      df <- rankings_data()
      req(nrow(df) > 0)
      df <- df %>% arrange(avg_metric)

      # Gradient palette: top positions in red, lower in navy
      n <- nrow(df)
      colors <- colorRampPalette(c("#1D428A", "#4A6FA5", "#C8102E"))(n)

      highchart() %>%
        hc_chart(
          type            = "bar",
          backgroundColor = "transparent",
          animation       = list(duration = 600),
          style           = list(fontFamily = "Trebuchet MS")
        ) %>%
        hc_plotOptions(
          bar = list(
            borderWidth  = 0,
            borderRadius = 3,
            pointPadding = 0.08,
            groupPadding = 0.08,
            colorByPoint = TRUE
          ),
          series = list(
            animation = list(duration = 600)
          )
        ) %>%
        hc_xAxis(
          categories = df$college,
          labels = list(
            style = list(
              fontSize   = "11px",
              color      = "#2D3748",
              fontWeight = "500",
              fontFamily = "Trebuchet MS"
            )
          ),
          lineWidth = 0,
          tickWidth = 0
        ) %>%
        hc_yAxis(
          title = list(
            text  = metric_sel(),
            style = list(color = "#718096", fontSize = "11px", fontWeight = "600")
          ),
          gridLineColor = "rgba(226,232,240,0.6)",
          gridLineDashStyle = "Dot",
          labels = list(
            style = list(color = "#A0AEC0", fontSize = "10px")
          )
        ) %>%
        hc_add_series(
          name = "Avg Value",
          data = lapply(seq_len(n), function(i) {
            list(
              y         = df$avg_metric[i],
              color     = colors[i],
              n_players = df$n_players[i],
              college   = df$college[i]
            )
          }),
          showInLegend = FALSE
        ) %>%
        hc_tooltip(
          useHTML         = TRUE,
          backgroundColor = "rgba(9, 24, 64, 0.97)",
          borderColor     = "#1D428A",
          borderRadius    = 8,
          borderWidth     = 1,
          shadow          = TRUE,
          outside         = FALSE,
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
              + '<span style=\"color:#A0AEC0;font-size:11px;\">Players Drafted</span>'
              + '<span style=\"color:#FFD700;font-weight:600;font-size:12px;\">'
              + p.n_players + '</span></div>'
              + '</div>';
          }")
        ) %>%
        hc_title(
          text = "University Rankings",
          style = list(
            color      = "#0F2355",
            fontFamily = "Trebuchet MS",
            fontWeight = "700",
            fontSize   = "16px"
          )
        ) %>%
        hc_subtitle(
          text  = paste0("Top 15 by avg ", metric_sel()),
          style = list(color = "#8A93A6", fontSize = "11px")
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_add_theme(hc_theme_smpl())
    })
  })
}
