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
           ),
           box(
             title       = NULL,
             width       = 6,
             solidHeader = FALSE,
             status      = "primary",
             highchartOutput(ns("area_rank_chart"), height = "500px")
           )
    )
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_rankings_server <- function(id, data, raw_data, nba_title, nba_subtitle, nba_tooltip,
                                rank_levels, rank_colors) {
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
    
    # ── Datos porcentuales por año y rank ──
    area_data <- reactive({
      raw_data %>%
        distinct(year, player, .keep_all = TRUE) %>%
        count(year, rank) %>%
        mutate(rank = factor(rank, levels = rank_levels, ordered = TRUE)) %>%
        tidyr::complete(year, rank, fill = list(n = 0)) %>%
        group_by(year) %>%
        mutate(
          total      = sum(n),
          percentage = round(n / total * 100, 1)
        ) %>%
        ungroup()
    })
    
    # ── Gráfico de áreas apiladas (100 %) ──
    output$area_rank_chart <- renderHighchart({
      df <- area_data()
      req(nrow(df) > 0)
      
      years <- sort(unique(df$year))
      
      hc <- highchart() %>%
        hc_chart(type = "area", backgroundColor = "transparent") %>%
        hc_plotOptions(
          area = list(
            stacking  = "percent",
            lineWidth = 1,
            lineColor = "#FFFFFF30",
            marker    = list(enabled = FALSE,
                             states  = list(hover = list(enabled = TRUE, radius = 3))),
            fillOpacity = 0.85
          )
        ) %>%
        hc_xAxis(
          categories = as.character(years),
          labels     = list(style = list(fontSize = "10px", color = "#4A5568"),
                            rotation = -45),
          tickInterval = 2,
          lineColor    = "#E2E8F0"
        ) %>%
        hc_yAxis(
          title         = list(text = "% of Draft Class"),
          max           = 100,
          gridLineColor = "#E2E8F0",
          labels        = list(format = "{value}%")
        ) %>%
        hc_title(
          text  = "Draft Class Composition by Rank",
          style = nba_title$style
        ) %>%
        hc_subtitle(
          text  = "Year-by-year percentage of players in each rank tier",
          style = nba_subtitle$style
        ) %>%
        hc_tooltip(
          backgroundColor = "rgba(15, 35, 85, 0.96)",
          borderColor     = "#1D428A",
          borderRadius    = 10,
          borderWidth     = 1,
          shadow          = TRUE,
          useHTML         = TRUE,
          shared          = TRUE,
          headerFormat    = paste0(
            "<div style='padding:10px 14px 8px; min-width:220px; font-family:Trebuchet MS,sans-serif;'>",
            "<div style='font-size:15px; font-weight:800; color:#FFFFFF; ",
            "border-bottom:2px solid rgba(200,16,46,0.6); padding-bottom:7px; margin-bottom:8px;'>",
            "\U0001F3C0 Draft Class {point.key}</div>"
          ),
          pointFormat     = paste0(
            "<div style='display:flex; align-items:center; gap:8px; padding:3px 0; font-size:12px;'>",
            "<span style='color:{series.color}; font-size:14px;'>\u25cf</span>",
            "<span style='color:#C8D6E5; flex:1;'>{series.name}</span>",
            "<span style='color:#FFFFFF; font-weight:700; min-width:48px; text-align:right;'>",
            "{point.percentage:.1f}%</span>",
            "<span style='color:#8A93A6; font-size:11px; min-width:30px; text-align:right;'>",
            "({point.y})</span>",
            "</div>"
          ),
          footerFormat    = paste0(
            "<div style='border-top:1px solid rgba(255,255,255,0.15); margin-top:6px; ",
            "padding-top:6px; font-size:11px; color:#8A93A6; text-align:right;'>",
            "Total players shown above</div></div>"
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
      
      for (lvl in rev(rank_levels)) {
        lvl_data <- df %>% filter(rank == lvl)
        if (nrow(lvl_data) > 0) {
          full <- data.frame(year = years) %>%
            left_join(lvl_data %>% select(year, n), by = "year") %>%
            mutate(n = ifelse(is.na(n), 0, n))
          
          hc <- hc %>%
            hc_add_series(
              name  = lvl,
              data  = full$n,
              color = rank_colors[[lvl]]
            )
        }
      }
      
      hc
    })
    
  })
}