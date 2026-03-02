# ============================================================
#  Módulo: Talento por Equipo (mod_talent.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_talent_ui <- function(id) {
  ns <- NS(id)
  box(
    title       = NULL,
    width       = 12,
    height      = 480,
    solidHeader = FALSE,
    highchartOutput(ns("chart_talent"), height = "440px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_talent_server <- function(id, data, year_reactivo,
                              rank_levels, rank_colors,
                              nba_title, nba_subtitle, nba_tooltip,
                              nba_axis, nba_legend) {
  moduleServer(id, function(input, output, session) {

    output$chart_talent <- renderHighchart({
      current_year <- year_reactivo()

      team_rank_data <- data %>%
        filter(year == current_year) %>%
        distinct(player, .keep_all = TRUE) %>%
        group_by(team, rank) %>%
        summarise(
          n       = n(),
          players = paste(player, collapse = ", "),
          .groups = "drop"
        ) %>%
        arrange(team) %>%
        mutate(rank = factor(rank, levels = rank_levels))

      all_teams <- sort(unique(team_rank_data$team))

      series_data <- lapply(rank_levels, function(r) {
        counts    <- team_rank_data %>% filter(rank == r)
        data_list <- lapply(all_teams, function(t) {
          row <- counts %>% filter(team == t)
          if (nrow(row) == 0) list(y = 0, players = "") else list(y = row$n, players = row$players)
        })
        list(name = r, data = data_list, color = rank_colors[[r]])
      })

      highchart() %>%
        hc_chart(type = "bar", backgroundColor = "#FFFFFF") %>%
        hc_title(text  = paste0("Selección de Talento por Equipo — ", current_year),
                 style = nba_title$style) %>%
        hc_subtitle(text  = "Distribución de categorías de rendimiento por franquicia",
                    style = nba_subtitle$style) %>%
        hc_xAxis(categories    = all_teams,
                 title         = list(text = NULL),
                 gridLineColor = nba_axis$gridLineColor,
                 lineColor     = nba_axis$lineColor,
                 labels        = list(style = list(fontSize = "11px", color = "#4A5568"))) %>%
        hc_yAxis(gridLineColor = nba_axis$gridLineColor,
                 lineColor     = nba_axis$lineColor) %>%
        hc_plotOptions(bar = list(stacking     = "normal",
                                  borderWidth  = 0,
                                  pointPadding = 0.05,
                                  groupPadding = 0.03)) %>%
        hc_add_series_list(series_data) %>%
        hc_legend(align          = nba_legend$align,
                  verticalAlign  = nba_legend$verticalAlign,
                  layout         = nba_legend$layout,
                  title          = nba_legend$title,
                  itemStyle      = nba_legend$itemStyle,
                  itemHoverStyle = nba_legend$itemHoverStyle) %>%
        hc_tooltip(
          useHTML         = TRUE,
          backgroundColor = nba_tooltip$backgroundColor,
          borderColor     = nba_tooltip$borderColor,
          borderRadius    = nba_tooltip$borderRadius,
          borderWidth     = nba_tooltip$borderWidth,
          style           = nba_tooltip$style,
          formatter = JS("function() {
            if (this.point.y === 0) return false;
            return '<b>' + this.key + '</b><br/>' +
                   '<span style=\"color:' + this.series.color + '\">●</span> ' +
                   this.series.name + ': <b>' + this.point.y + '</b> jugadores<br/>' +
                   '<hr style=\"margin:4px 0;border-color:rgba(255,255,255,0.15)\"/>' +
                   '<span style=\"font-size:11px\">' + this.point.players + '</span>';
          }")) %>%
        hc_add_theme(hc_theme_smpl())
    })
  })
}
