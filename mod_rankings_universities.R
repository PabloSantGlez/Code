# ============================================================
#  Módulo: University Rankings (mod_rankings.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_rankings_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # ── Panel Filtros ──
    column(width = 2,
           div(
             style = "background-color: var(--navy-deeper); 
                      border: 1px solid var(--border); 
                      border-radius: var(--radius-lg); 
                      padding: 20px; 
                      box-shadow: var(--shadow-md);
                      height: fit-content;
                      display: flex;
                      flex-direction: column;",
             
             h4("Rankings", style = "color: #FFFFFF; font-weight: 700; margin-top: 0;"),
             hr(style = "border-top: 1px solid rgba(255,255,255,0.1);"),
             
             h5("METRIC", style = "color: var(--bg-surface); font-family: var(--font-display); 
                                    font-weight: 700; font-size: 12px; letter-spacing: 1px;"),
             selectInput(ns("metric"),
                         label   = NULL,
                         choices = c(
                           "Overall Performance"  = "overall_performance",
                           "Games"                = "games",
                           "Minutes Played"       = "minutes_played",
                           "Years Active"         = "years_active",
                           "Avg Minutes Played"   = "average_minutes_played",
                           "Points per Game"      = "points_per_game",
                           "Avg Assists"          = "average_assists",
                           "Avg Total Rebounds"   = "average_total_rebounds",
                           "Points"               = "points",
                           "Assists"              = "assists",
                           "Total Rebounds"       = "total_rebounds",
                           "3PT Percentage"       = "3_point_percentage",
                           "FG Percentage"        = "field_goal_percentage",
                           "FT Percentage"        = "free_throw_percentage",
                           "Win Shares / 48"      = "win_shares_per_48_minutes",
                           "Win Shares"           = "win_shares",
                           "VORP"                 = "value_over_replacement",
                           "Box Plus/Minus"       = "box_plus_minus"
                         ),
                         selected = "overall_performance"
             ),
             
             hr(style = "border-top: 1px solid rgba(255,255,255,0.1); margin: 15px 0;"),
             
             p("Select the performance metric.",
               style = "color: var(--bg-surface); font-size: 15px; margin-top: 10px;")
           )
    ),
    
    # ── Panel Gráfico ──
    column(width = 10,
           box(
             title       = NULL,
             width       = 6,
             solidHeader = FALSE,
             status      = "primary",
             highchartOutput(ns("rankings_chart"), height = "500px")
           )
    )
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_rankings_server <- function(id, data, nba_title, nba_subtitle, nba_tooltip) {
  moduleServer(id, function(input, output, session) {
    
    min_players <- 20
    
    # ── Datos procesados ──
    rankings_data <- reactive({
      data %>%
        filter(indicator == input$metric) %>%
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
    
    # ── Gráfico de barras horizontal ──
    output$rankings_chart <- renderHighchart({
      df <- rankings_data()
      req(nrow(df) > 0)
      
      df <- df %>% arrange(avg_metric)
      
      highchart() %>%
        hc_chart(type = "bar", backgroundColor = "transparent") %>%
        
        # ── AQUÍ CONTROLAMOS EL GROSOR DE LAS BARRAS ──
        hc_plotOptions(
          bar = list(
            pointPadding = 0.05,
            groupPadding = 0.1,
            borderWidth  = 0,
            borderRadius = 2
          )
        ) %>%
        # ──────────────────────────────────────────────
        
        hc_xAxis(
          categories = df$college,
          labels     = list(style = list(fontSize = "11px", color = "#4A5568"))
        ) %>%
        hc_yAxis(
          title = list(text = input$metric)
        ) %>%
        hc_add_series(
          name         = "Avg Value",
          data         = df$avg_metric,
          colorByPoint = TRUE,
          colors       = colorRampPalette(c("#CED4DA", "#1D428A", "#C8102E"))(nrow(df))
        ) %>%
        hc_tooltip(
          backgroundColor = nba_tooltip$backgroundColor,
          borderColor     = nba_tooltip$borderColor,
          style           = nba_tooltip$style,
          pointFormat     = "<b>{point.category}</b><br/>Value: {point.y:.3f}"
        ) %>%
        hc_title(text = "Top 10 NBA Draft Universities", style = nba_title$style) %>%
        hc_subtitle(
          text  = paste("Ranked by average", input$metric, "| Min.", min_players, "players drafted"),
          style = nba_subtitle$style
        ) %>%
        hc_legend(enabled = FALSE) %>%
        hc_add_theme(hc_theme_smpl())
    })
    
  })
}