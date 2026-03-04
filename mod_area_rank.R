# ============================================================
#  Módulo: Rank Tier Composition — Stacked Bar
#  (mod_area_rank.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_area_rank_ui <- function(id) {
  ns <- NS(id)
  box(
    title = NULL,
    width = 12,
    solidHeader = FALSE,
    status = "primary",
    highchartOutput(ns("composition_chart"), height = "460px")
  )
}

# ── 2. Server ────────────────────────────────────────────────
mod_area_rank_server <- function(id, data, metric_sel, nba_title, nba_subtitle, nba_tooltip,
                                 rank_levels, rank_colors) {
  moduleServer(id, function(input, output, session) {
    comp_data <- reactive({
      met <- metric_sel()

      # Top 15 universities by average metric
      top_unis <- data %>%
        filter(indicator == met, college != "Didn't play College") %>%
        group_by(college) %>%
        summarise(
          avg_metric = mean(as.numeric(value), na.rm = TRUE),
          .groups    = "drop"
        ) %>%
        arrange(desc(avg_metric)) %>%
        head(15) %>%
        pull(college)

      # Count players per rank tier for each top university
      data %>%
        filter(college %in% top_unis) %>%
        distinct(player, college, rank) %>%
        mutate(rank = factor(rank, levels = rank_levels, ordered = TRUE)) %>%
        count(college, rank, .drop = FALSE) %>%
        group_by(college) %>%
        mutate(
          total      = sum(n),
          percentage = round(n / total * 100, 1)
        ) %>%
        ungroup()
    })

    output$composition_chart <- renderHighchart({
      df <- comp_data()
      req(nrow(df) > 0)

      # Order colleges by total players (descending) for y-axis
      college_order <- df %>%
        group_by(college) %>%
        summarise(total = sum(n), .groups = "drop") %>%
        arrange(total) %>%
        pull(college)

      hc <- highchart() %>%
        hc_chart(
          type            = "bar",
          backgroundColor = "transparent",
          animation       = list(duration = 600),
          style           = list(fontFamily = "Trebuchet MS")
        ) %>%
        hc_plotOptions(
          bar = list(
            stacking     = "percent",
            borderWidth  = 0,
            borderRadius = 0,
            pointPadding = 0.05,
            groupPadding = 0.08
          ),
          series = list(
            animation = list(duration = 600)
          )
        ) %>%
        hc_xAxis(
          categories = college_order,
          labels = list(
            style = list(
              fontSize   = "11px",
              color      = "#2D3748",
              fontWeight = "500",
              fontFamily = "Trebuchet MS"
            )
          ),
          lineWidth = 0,
          tickWidth = 0
        ) %>%
        hc_yAxis(
          title = list(
            text  = "% of Drafted Players",
            style = list(color = "#718096", fontSize = "11px", fontWeight = "600")
          ),
          max = 100,
          gridLineColor = "rgba(226,232,240,0.6)",
          gridLineDashStyle = "Dot",
          labels = list(
            format = "{value}%",
            style  = list(color = "#A0AEC0", fontSize = "10px")
          )
        ) %>%
        hc_tooltip(
          useHTML = TRUE,
          shared = TRUE,
          backgroundColor = "rgba(9, 24, 64, 0.97)",
          borderColor = "#1D428A",
          borderRadius = 8,
          borderWidth = 1,
          shadow = TRUE,
          headerFormat = paste0(
            "<div style='padding:10px 14px 6px;font-family:Trebuchet MS,sans-serif;min-width:200px;'>",
            "<div style='font-size:13px;font-weight:700;color:#FFFFFF;",
            "border-bottom:1px solid rgba(29,66,138,0.5);padding-bottom:7px;margin-bottom:6px;'>",
            "{point.key}</div>"
          ),
          pointFormat = paste0(
            "<div style='display:flex;align-items:center;gap:6px;padding:2px 0;font-size:11px;'>",
            "<span style='color:{series.color};font-size:12px;'>&#9679;</span>",
            "<span style='color:#A0AEC0;flex:1;'>{series.name}</span>",
            "<span style='color:#FFFFFF;font-weight:700;min-width:36px;text-align:right;'>",
            "{point.percentage:.1f}%</span>",
            "<span style='color:#718096;font-size:10px;min-width:28px;text-align:right;'>",
            "({point.y})</span>",
            "</div>"
          ),
          footerFormat = "</div>"
        ) %>%
        hc_title(
          text = "Player Quality by University",
          style = list(
            color      = "#0F2355",
            fontFamily = "Trebuchet MS",
            fontWeight = "700",
            fontSize   = "16px"
          )
        ) %>%
        hc_subtitle(
          text  = paste0("Rank-tier composition of top 15 universities (by ", metric_sel(), ")"),
          style = list(color = "#8A93A6", fontSize = "11px")
        ) %>%
        hc_legend(
          align          = "right",
          verticalAlign  = "middle",
          layout         = "vertical",
          itemStyle      = list(color = "#4A5568", fontSize = "10px", fontWeight = "500"),
          itemHoverStyle = list(color = "#0F2355"),
          symbolRadius   = 3,
          symbolHeight   = 10,
          symbolWidth    = 10
        ) %>%
        hc_add_theme(hc_theme_smpl())

      # Add one series per rank tier (in reverse so legend order = top-to-bottom)
      for (lvl in rev(rank_levels)) {
        lvl_df <- df %>% filter(rank == lvl)
        if (nrow(lvl_df) > 0) {
          # Align data to college_order
          vals <- sapply(college_order, function(c) {
            row <- lvl_df %>% filter(college == c)
            if (nrow(row) == 0) 0L else row$n[1]
          })

          hc <- hc %>%
            hc_add_series(
              name  = lvl,
              data  = as.numeric(vals),
              color = rank_colors[[lvl]]
            )
        }
      }

      hc
    })
  })
}
