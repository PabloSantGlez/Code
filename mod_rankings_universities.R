# ============================================================
#  Módulo: University Rankings (mod_rankings_universities.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_rankings_ui <- function(id) {
  ns <- NS(id)
  box(
    title       = NULL,
    width       = 12,
    solidHeader = FALSE,
    status      = "primary",
    highchartOutput(ns("rankings_chart"), height = "500px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_rankings_server <- function(id, data, metric_sel, nba_title, nba_subtitle, nba_tooltip) {
  moduleServer(id, function(input, output, session) {
    min_players <- 20
    
    rankings_data <- reactive({
      data %>%
        filter(indicator == metric_sel()) %>%   # ← usa el reactivo externo
        group_by(college) %>%
        summarise(
          avg_metric = round(mean(as.numeric(value), na.rm = TRUE), 3),
          n_players  = n_distinct(player),
          .groups    = "drop"
        ) %>%
        filter(n_players >= min_players, college != "Didn't play College") %>%
        arrange(desc(avg_metric)) %>%
        head(10)
    })
    
    output$rankings_chart <- renderHighchart({
      df <- rankings_data()
      req(nrow(df) > 0)
      df <- df %>% arrange(avg_metric)
      
      highchart() %>%
        hc_chart(type = "bar", backgroundColor = "transparent") %>%
        hc_plotOptions(bar = list(pointPadding = 0.05, groupPadding = 0.1,
                                  borderWidth = 0, borderRadius = 2)) %>%
        hc_xAxis(categories = df$college,
                 labels = list(style = list(fontSize = "11px", color = "#4A5568"))) %>%
        hc_yAxis(title = list(text = metric_sel())) %>%
        hc_add_series(name = "Avg Value", data = df$avg_metric,
                      colorByPoint = TRUE,
                      colors = colorRampPalette(c("#CED4DA", "#1D428A", "#C8102E"))(nrow(df))) %>%
        hc_tooltip(backgroundColor = nba_tooltip$backgroundColor,
                   borderColor     = nba_tooltip$borderColor,
                   style           = nba_tooltip$style,
                   pointFormat     = "<b>{point.category}</b><br/>Value: {point.y:.3f}") %>%
        hc_title(text = "Top 10 NBA Draft Universities", style = nba_title$style) %>%
        hc_subtitle(text = paste("Ranked by average", metric_sel(), "| Min.", min_players, "players drafted"),
                    style = nba_subtitle$style) %>%
        hc_legend(enabled = FALSE) %>%
        hc_add_theme(hc_theme_smpl())
    })
  })
}