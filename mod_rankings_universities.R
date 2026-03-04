# ============================================================
#  Module: University Rankings — Horizontal Bar Chart
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
mod_rankings_server <- function(id, data, metric_sel) {
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
        filter(n_players >= 15, college != "Didn't play College") %>%
        arrange(desc(avg_metric)) %>%
        head(15)
    })

    output$rankings_chart <- renderHighchart({
      df <- rankings_data()
      req(nrow(df) > 0)
      df <- df %>% arrange(avg_metric)

      n <- nrow(df)
      colors <- colorRampPalette(c("#1D428A", "#4A6FA5", "#C8102E"))(n)

      highchart() %>%
        hc_nba_base("bar") %>%
        hc_plotOptions(
          bar = list(
            borderWidth  = 0,
            borderRadius = 3,
            pointPadding = 0.08,
            groupPadding = 0.08,
            colorByPoint = TRUE
          )
        ) %>%
        hc_nba_xaxis_categories(df$college) %>%
        hc_nba_yaxis(title_text = metric_sel()) %>%
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
              + '<span style=\"color:#A0AEC0;font-size:11px;\">Players Drafted</span>'
              + '<span style=\"color:#FFD700;font-weight:600;font-size:12px;\">'
              + p.n_players + '</span></div>'
              + '</div>';
          }")
        ) %>%
        hc_nba_title("University Rankings") %>%
        hc_nba_subtitle(paste0("Top 15 by avg ", metric_sel())) %>%
        hc_legend(enabled = FALSE)
    })
  })
}
