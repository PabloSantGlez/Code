# ============================================================
#  Module: Re-Draft (mod_redraft.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_redraft_ui <- function(id) {
  ns <- NS(id)
  box(
    title = NULL,
    width = 6,
    height = 580,
    solidHeader = FALSE,
    highchartOutput(ns("chart_redraft"), height = "540px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_redraft_server <- function(id, data, year_reactivo) {
  moduleServer(id, function(input, output, session) {
    output$chart_redraft <- renderHighchart({
      current_year <- year_reactivo()

      top_redraft <- data %>%
        filter(year == current_year) %>%
        distinct(player, .keep_all = TRUE) %>%
        mutate(
          rank  = factor(rank, levels = names(rank_colors), ordered = TRUE),
          color = rank_colors[as.character(rank)]
        ) %>%
        arrange(newPick)

      hchart(
        top_redraft, "scatter",
        hcaes(
          x = as.numeric(overall_pick),
          y = newPick,
          color = color
        )
      ) %>%
        hc_chart(backgroundColor = "#FFFFFF") %>%
        hc_nba_title(paste0("Re-Draft ", current_year)) %>%
        hc_nba_subtitle("Original pick vs. Re-calculate pick") %>%
        hc_xAxis(
          title = list(
            text = "Original pick",
            style = list(color = "#0F2355", fontWeight = "600")
          ),
          min = 1,
          max = 61,
          gridLineColor = "#E2E8F0",
          lineColor = "#E2E8F0",
          labels = list(style = list(color = "#4A5568")),
          plotLines = list(list(
            value = 30, color = "#CBD5E0",
            dashStyle = "Dash", width = 1
          ))
        ) %>%
        hc_yAxis(
          title = list(
            text = "New Pick",
            style = list(color = "#0F2355", fontWeight = "600")
          ),
          reversed = TRUE,
          min = 1,
          max = 61,
          gridLineColor = "#E2E8F0",
          lineColor = "#E2E8F0",
          labels = list(style = list(color = "#4A5568")),
          plotLines = list(list(
            value = 30, color = "#CBD5E0",
            dashStyle = "Dash", width = 1
          ))
        ) %>%
        hc_nba_tooltip_dark(
          pointFormat = "
            <b>{point.player}</b><br/>
            Rank: <b>{point.rank}</b><br/>
            Original pick: <b>{point.x}</b><br/>
            New Pick: <b>{point.y}</b><br/>
          "
        ) %>%
        hc_plotOptions(
          scatter = list(
            marker = list(radius = 6, symbol = "circle"),
            states = list(hover = list(enabled = TRUE))
          )
        ) %>%
        hc_legend(
          align          = "right",
          verticalAlign  = "middle",
          layout         = "vertical",
          itemStyle      = list(color = "#4A5568", fontSize = "11px", fontWeight = "500"),
          itemHoverStyle = list(color = "#0F2355")
        ) %>%
        hc_add_theme(hc_theme_smpl())
    })
  })
}
