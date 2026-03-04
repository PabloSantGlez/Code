# ============================================================
#  Module: Rank Tier Composition — Stacked Bar
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
mod_area_rank_server <- function(id, data, metric_sel) {
  moduleServer(id, function(input, output, session) {
    comp_data <- reactive({
      met <- metric_sel()

      # Top 15 universities by average metric
      top_unis <- data %>%
        filter(indicator == met, college != "Didn't play College") %>%
        group_by(college) %>%
        summarise(
          avg_metric = mean(as.numeric(value), na.rm = TRUE),
          n_players  = n_distinct(player),
          .groups    = "drop"
        ) %>%
        filter(n_players >= 15) %>%
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
        hc_nba_base("bar") %>%
        hc_plotOptions(
          bar = list(
            stacking     = "percent",
            borderWidth  = 0,
            borderRadius = 0,
            pointPadding = 0.05,
            groupPadding = 0.08
          )
        ) %>%
        hc_nba_xaxis_categories(college_order) %>%
        hc_nba_yaxis(
          title_text = "% of Drafted Players",
          extra = list(
            max = 100,
            labels = list(
              format = "{value}%",
              style  = nba_axis_label_style
            )
          )
        ) %>%
        hc_nba_tooltip_dark(
          shared = TRUE,
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
        hc_nba_title("Player Quality by University") %>%
        hc_nba_subtitle(paste0("Rank-tier composition of top 15 universities (by ", metric_sel(), ")")) %>%
        hc_nba_legend_right()

      # Add one series per rank tier (in reverse so legend order = top-to-bottom)
      for (lvl in rev(rank_levels)) {
        lvl_df <- df %>% filter(rank == lvl)
        if (nrow(lvl_df) > 0) {
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
