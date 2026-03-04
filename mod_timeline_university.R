# ============================================================
#  Module: Draft Production Timeline — Column Chart
#  (mod_timeline_university.R)
# ============================================================

# ── 1. UI ────────────────────────────────────────────────────
mod_timeline_ui <- function(id) {
    ns <- NS(id)
    box(
        title = NULL,
        width = 12,
        solidHeader = FALSE,
        status = "primary",
        highchartOutput(ns("timeline_chart"), height = "460px")
    )
}

# ── 2. Server ────────────────────────────────────────────────
mod_timeline_server <- function(id, data, metric_sel) {
    moduleServer(id, function(input, output, session) {
        timeline_data <- reactive({
            met <- metric_sel()

            # Top 8 universities by average metric (all-time)
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
                head(8) %>%
                pull(college)

            # Count distinct players per era per university
            data %>%
                filter(college %in% top_unis) %>%
                distinct(player, college, year) %>%
                mutate(era = paste0(floor(year / 5) * 5, "s")) %>%
                count(era, college) %>%
                tidyr::complete(era, college, fill = list(n = 0))
        })

        output$timeline_chart <- renderHighchart({
            df <- timeline_data()
            req(nrow(df) > 0)

            eras <- sort(unique(df$era))
            colleges <- unique(df$college)

            # Palette for 8 schools
            timeline_palette <- c(
                "#C8102E", "#1D428A", "#0F2355", "#5C7EB0",
                "#A00D24", "#2E8B57", "#DAA520", "#708090"
            )

            hc <- highchart() %>%
                hc_nba_base("column") %>%
                hc_plotOptions(
                    column = list(
                        borderWidth  = 0,
                        borderRadius = 2,
                        pointPadding = 0.05,
                        groupPadding = 0.15
                    )
                ) %>%
                hc_nba_xaxis_categories(eras, extra = list(lineColor = "#E2E8F0")) %>%
                hc_nba_yaxis(
                    title_text = "Players Drafted",
                    extra = list(allowDecimals = FALSE)
                ) %>%
                hc_nba_tooltip_shared() %>%
                hc_nba_title("Draft Production Over Time") %>%
                hc_nba_subtitle(paste0("Top 8 universities (by ", metric_sel(), ") across 5-year eras")) %>%
                hc_nba_legend_right()

            for (i in seq_along(colleges)) {
                uni <- colleges[i]
                uni_df <- df %>% filter(college == uni)

                vals <- sapply(eras, function(e) {
                    row <- uni_df %>% filter(era == e)
                    if (nrow(row) == 0) 0L else row$n[1]
                })

                hc <- hc %>%
                    hc_add_series(
                        name  = uni,
                        data  = as.numeric(vals),
                        color = timeline_palette[((i - 1) %% length(timeline_palette)) + 1]
                    )
            }

            hc
        })
    })
}
