# ============================================================
#  Módulo: Area Rank Chart (mod_area_rank.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_area_rank_ui <- function(id) {
  ns <- NS(id)
  
  box(
    title       = NULL,
    width       = 12,
    solidHeader = FALSE,
    status      = "primary",
    highchartOutput(ns("area_rank_chart"), height = "500px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_area_rank_server <- function(id, data, nba_title, nba_subtitle, nba_tooltip,
                                 rank_levels, rank_colors) {
  moduleServer(id, function(input, output, session) {
    
    # ── Datos porcentuales por año y rank ──
    area_data <- reactive({
      data %>%
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
    
    # ── Gráfico de áreas apiladas (100%) ──
    output$area_rank_chart <- renderHighchart({
      df <- area_data()
      req(nrow(df) > 0)
      
      years <- sort(unique(df$year))
      
      hc <- highchart() %>%
        hc_chart(type = "area", backgroundColor = "transparent") %>%
        hc_plotOptions(
          area = list(
            stacking    = "percent",
            lineWidth   = 1,
            lineColor   = "#FFFFFF30",
            marker      = list(enabled = FALSE,
                               states  = list(hover = list(enabled = TRUE, radius = 3))),
            fillOpacity = 0.85
          )
        ) %>%
        hc_xAxis(
          categories = as.character(years),
          labels     = list(style    = list(fontSize = "10px", color = "#4A5568"),
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