# ============================================================
#  Módulo: Rendimiento por Pick (mod_pick_performance.R)
#
#  Muestra 3 líneas para el año seleccionado:
#    · overall_performance  — rendimiento real del jugador drafteado
#    · observed_perf        — media histórica por posición de pick
#    · expected_perf        — curva spline (valor esperado teórico)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_pick_performance_ui <- function(id) {
  ns <- NS(id)
  box(
    title       = NULL,
    width       = 12,
    height      = 520,
    solidHeader = FALSE,
    highchartOutput(ns("chart_pick_perf"), height = "480px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_pick_performance_server <- function(id, data, year_reactivo,
                                        nba_title, nba_subtitle, nba_tooltip) {
  moduleServer(id, function(input, output, session) {

    output$chart_pick_perf <- renderHighchart({
      current_year <- year_reactivo()

      # Una fila por pick: rendimiento real del jugador drafteado ese año
      # observed_perf y expected_perf son iguales para todos los años
      # (son la curva histórica), así que las tomamos del mismo df.
      df <- data %>%
        filter(year == current_year) %>%
        distinct(overall_pick, player, overall_performance,
                 observed_perf, expected_perf) %>%
        mutate(overall_pick = as.integer(overall_pick)) %>%
        arrange(overall_pick)

      highchart() %>%
        hc_chart(backgroundColor = "#FFFFFF") %>%

        hc_title(
          text  = paste0("Pick Performance — Draft ", current_year),
          style = nba_title$style
        ) %>%
        hc_subtitle(
          text  = "Player Prformance vs. Historical Pick Performance & Expected Performance",
          style = nba_subtitle$style
        ) %>%

        # ── Series ─────────────────────────────────────────────
        hc_add_series(
          data = purrr::pmap(
            df %>% select(overall_pick, overall_performance, player),
            function(overall_pick, overall_performance, player) {
              list(
                x = overall_pick,
                y = overall_performance,
                custom = list(player = player)
              )
            }
          ),
          type      = "line",
          name      = "Player Performance",
          color     = "#1D428A",
          lineWidth = 2,
          marker    = list(enabled = TRUE, radius = 4),
          zIndex    = 3
        ) %>%
        hc_add_series(
          data        = list_parse2(
            df %>% select(x = overall_pick, y = observed_perf)
          ),
          type        = "line",
          name        = "Historical Pick Performance",
          color       = "#C8102E",
          lineWidth   = 2,
          marker      = list(enabled = FALSE),
          zIndex      = 2
        ) %>%
        hc_add_series(
          data        = list_parse2(
            df %>% select(x = overall_pick, y = expected_perf)
          ),
          type        = "line",
          name        = "Expected Performance",
          color       = "#8A93A6",
          lineWidth   = 1.5,
          dashStyle   = "ShortDash",
          marker      = list(enabled = FALSE),
          zIndex      = 1
        ) %>%

        # ── Ejes ───────────────────────────────────────────────
        hc_xAxis(
          title = list(
            text  = "Pick Number",
            style = list(color = "#0F2355", fontWeight = "600")
          ),
          min           = 1,
          max           = 60,
          tickInterval  = 5,
          gridLineColor = "#E2E8F0",
          lineColor     = "#E2E8F0",
          labels        = list(style = list(color = "#4A5568", fontSize = "11px")),
          plotLines = list(
            list(
              value     = 14.5,
              color     = "#E2E8F0",
              dashStyle = "Dash",
              width     = 1,
              label     = list(
                text  = "Lottery",
                style = list(color = "#8A93A6", fontSize = "10px")
              )
            ),
            list(
              value     = 30.5,
              color     = "#E2E8F0",
              dashStyle = "Dash",
              width     = 1,
              label     = list(
                text  = "1ª Round",
                style = list(color = "#8A93A6", fontSize = "10px")
              )
            )
          )
        ) %>%
        hc_yAxis(
          title         = list(
            text  = "Performance Score",
            style = list(color = "#0F2355", fontWeight = "600")
          ),
          gridLineColor = "#E2E8F0",
          lineColor     = "#E2E8F0",
          labels        = list(style = list(color = "#4A5568", fontSize = "11px"))
        ) %>%

        # ── Tooltip ────────────────────────────────────────────
        hc_tooltip(
          useHTML = TRUE,
          shared  = TRUE,
          formatter = JS("
    function () {
      var s = '<b>Pick #' + this.x + '</b><br/>';
      
      if (this.points[0].point.custom && 
          this.points[0].point.custom.player) {
        s += '<span style=\"font-size:13px; opacity:1\">' 
             + this.points[0].point.custom.player + 
             '</span><br/><br/>';
      }
      
      this.points.forEach(function(point) {
        s += point.series.name + ': <b>' 
             + Math.round(point.y * 100) / 100 + 
             '</b><br/>';
      });
      
      return s;
    }
  ")
        ) %>%

        # ── Leyenda ────────────────────────────────────────────
        hc_legend(
          align          = "center",
          verticalAlign  = "bottom",
          layout         = "horizontal",
          itemStyle      = list(color = "#4A5568", fontSize = "11px", fontWeight = "500"),
          itemHoverStyle = list(color = "#0F2355")
        ) %>%

        hc_add_theme(hc_theme_smpl())
    })
  })
}
