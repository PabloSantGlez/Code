# ============================================================
#  Módulo: Re-Draft (mod_redraft.R)
# ============================================================

# ── 0. Definición de la Paleta de Colores (Fuera del server para acceso global o puedes importarla) ──


# ── 1. UI ────────────────────────────────────────────────────
mod_redraft_ui <- function(id) {
  ns <- NS(id)
  box(
    title       = NULL,
    width       = 6,
    height      = 580,
    solidHeader = FALSE,
    highchartOutput(ns("chart_redraft"), height = "540px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_redraft_server <- function(id, data, year_reactivo,
                               nba_title, nba_subtitle, nba_tooltip, nba_legend) {
  moduleServer(id, function(input, output, session) {
    
    output$chart_redraft <- renderHighchart({
      rank_colors <- c(
        "Generational" = "#FFD700",
        "SuperStar"    = "#C8102E",
        "Star"         = "#A00D24",
        "Elite"        = "#091840",
        "Starter"      = "#0F2355",
        "Solid"        = "#1D428A",
        "Role"         = "#5C7EB0",
        "Rotation"     = "#98ABC8",
        "Deep"         = "#CED4DA"
      )
      
      
      current_year <- year_reactivo()
      
      top_redraft <- data %>%
        filter(year == current_year) %>%
        distinct(player, .keep_all = TRUE) %>%
        mutate(
          rank = factor(rank, levels = names(rank_colors), ordered = TRUE),
          color = rank_colors[as.character(rank)]
        ) %>%
        arrange(newPick)
    
      
      hchart(top_redraft, "scatter",
             hcaes(x = as.numeric(overall_pick),
                   y = newPick,
                   color = color)) %>%
        
        hc_chart(backgroundColor = "#FFFFFF") %>%
        hc_title(text  = paste0("Re-Draft ", current_year),
                 style = nba_title$style) %>%
        hc_subtitle(text  = "Original pick vs. Re-calculate pick",
                    style = nba_subtitle$style) %>%
        hc_xAxis(
          title = list(text  = "Original pick",
                       style = list(color = "#0F2355", fontWeight = "600")),
          min           = 1,
          max           = 61,
          gridLineColor = "#E2E8F0",
          lineColor     = "#E2E8F0",
          labels        = list(style = list(color = "#4A5568")),
          plotLines     = list(list(value = 30, color = "#CBD5E0",
                                    dashStyle = "Dash", width = 1))
        ) %>%
        hc_yAxis(
          title = list(text  = "New Pick",
                       style = list(color = "#0F2355", fontWeight = "600")),
          reversed      = TRUE,
          min           = 1,
          max           = 61,
          gridLineColor = "#E2E8F0",
          lineColor     = "#E2E8F0",
          labels        = list(style = list(color = "#4A5568")),
          plotLines     = list(list(value = 30, color = "#CBD5E0",
                                    dashStyle = "Dash", width = 1))
        ) %>%
        hc_tooltip(
          useHTML         = TRUE,
          backgroundColor = nba_tooltip$backgroundColor,
          borderColor     = nba_tooltip$borderColor,
          borderRadius    = nba_tooltip$borderRadius,
          borderWidth     = nba_tooltip$borderWidth,
          style           = nba_tooltip$style,
          pointFormat     = "
            <b>{point.player}</b><br/>
            Rank: <b>{point.rank}</b><br/>
            Original pick: <b>{point.x}</b><br/>
            New Pick: <b>{point.y}</b><br/>
          ") %>%
        hc_plotOptions(
          scatter = list(marker = list(radius = 6, symbol = "circle"),
                         states = list(hover = list(enabled = TRUE)))) %>%
        hc_legend(align          = nba_legend$align,
                  verticalAlign  = nba_legend$verticalAlign,
                  layout         = nba_legend$layout,
                  itemStyle      = nba_legend$itemStyle,
                  itemHoverStyle = nba_legend$itemHoverStyle) %>%
        hc_add_theme(hc_theme_smpl())
    })
  })
}