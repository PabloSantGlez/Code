# ============================================================
#  Module: Attribute Heatmap (mod_heatmap.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  box(
    title = NULL,
    width = 6,
    height = 580,
    solidHeader = FALSE,
    highchartOutput(ns("chart_heatmap"), height = "540px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_heatmap_server <- function(id, data, year_reactivo) {
  moduleServer(id, function(input, output, session) {
    output$chart_heatmap <- renderHighchart({
      current_year <- year_reactivo()

      lottery_picks <- data %>%
        filter(year == current_year, unit == "Player") %>%
        distinct(player, overall_pick) %>%
        arrange(overall_pick) %>%
        head(14) %>%
        pull(player)

      df_heat <- data %>%
        filter(year == current_year, unit == "Player", player %in% lottery_picks) %>%
        group_by(indicator) %>%
        mutate(
          raw_value = round(value, 2),
          normValue = (value - min(value, na.rm = TRUE)) /
            (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
        ) %>%
        ungroup() %>%
        arrange(desc(overall_pick))

      hchart(
        df_heat, "heatmap",
        hcaes(x = indicator, y = player, value = normValue)
      ) %>%
        hc_chart(
          backgroundColor = "#FFFFFF",
          marginLeft      = 160,
          marginBottom    = 110,
          marginRight     = 60,
          animation       = list(duration = 0)
        ) %>%
        hc_nba_title(paste0("Heatmap — Lottery Picks ", current_year)) %>%
        hc_xAxis(
          title = list(text = NULL),
          labels = list(
            rotation = -45,
            style = list(fontSize = "10px", color = "#4A5568")
          )
        ) %>%
        hc_yAxis(
          title = list(
            text  = "Players",
            style = list(color = "#0F2355", fontWeight = "600", fontSize = "13px")
          ),
          labels = list(style = list(fontSize = "11px", color = "#4A5568"))
        ) %>%
        hc_plotOptions(
          heatmap = list(
            borderWidth = 2,
            borderColor = "#FFFFFF",
            nullColor   = "#F2F4F8",
            states      = list(hover = list(enabled = TRUE, brightness = 0))
          )
        ) %>%
        hc_nba_tooltip_dark(
          formatter = JS("function() {
            return '<b>' + this.point.player + '</b><br/>' +
                   'Atributo: <b>' + this.point.indicator + '</b><br/>' +
                   'Valor: <b>' + this.point.raw_value + '</b>';
          }")
        ) %>%
        hc_add_theme(hc_theme_smpl()) %>%
        hc_legend(enabled = FALSE) %>%
        hc_colorAxis(
          min = 0,
          max = 1,
          showInLegend = FALSE,
          stops = list(
            list(0, "#0F2355"),
            list(0.25, "#1D428A"),
            list(0.5, "#FFFFFF"),
            list(0.75, "#C8102E"),
            list(1, "#7A0018")
          )
        )
    })
  })
}
