# ============================================================
#  Módulo: Draft Production Timeline — Column Chart
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
mod_timeline_server <- function(id, data, metric_sel, nba_title, nba_subtitle, nba_tooltip) {
    moduleServer(id, function(input, output, session) {
        timeline_data <- reactive({
            met <- metric_sel()

            # Top 8 universities by average metric (all-time)
            top_unis <- data %>%
                filter(indicator == met, college != "Didn't play College") %>%
                group_by(college) %>%
                summarise(
                    avg_metric = mean(as.numeric(value), na.rm = TRUE),
                    .groups    = "drop"
                ) %>%
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
            timeline_colors <- c(
                "#C8102E", "#1D428A", "#0F2355", "#5C7EB0",
                "#A00D24", "#2E8B57", "#DAA520", "#708090"
            )

            hc <- highchart() %>%
                hc_chart(
                    type            = "column",
                    backgroundColor = "transparent",
                    animation       = list(duration = 600),
                    style           = list(fontFamily = "Trebuchet MS")
                ) %>%
                hc_plotOptions(
                    column = list(
                        borderWidth  = 0,
                        borderRadius = 2,
                        pointPadding = 0.05,
                        groupPadding = 0.15
                    ),
                    series = list(
                        animation = list(duration = 600)
                    )
                ) %>%
                hc_xAxis(
                    categories = eras,
                    labels = list(
                        style = list(
                            fontSize   = "11px",
                            color      = "#2D3748",
                            fontWeight = "500",
                            fontFamily = "Trebuchet MS"
                        )
                    ),
                    lineColor = "#E2E8F0",
                    tickWidth = 0
                ) %>%
                hc_yAxis(
                    title = list(
                        text  = "Players Drafted",
                        style = list(color = "#718096", fontSize = "11px", fontWeight = "600")
                    ),
                    gridLineColor = "rgba(226,232,240,0.6)",
                    gridLineDashStyle = "Dot",
                    allowDecimals = FALSE,
                    labels = list(
                        style = list(color = "#A0AEC0", fontSize = "10px")
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
                        "<span style='color:#FFFFFF;font-weight:700;min-width:28px;text-align:right;'>",
                        "{point.y}</span>",
                        "</div>"
                    ),
                    footerFormat = "</div>"
                ) %>%
                hc_title(
                    text = "Draft Production Over Time",
                    style = list(
                        color      = "#0F2355",
                        fontFamily = "Trebuchet MS",
                        fontWeight = "700",
                        fontSize   = "16px"
                    )
                ) %>%
                hc_subtitle(
                    text  = paste0("Top 8 universities (by ", metric_sel(), ") across 5-year eras"),
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
                        color = timeline_colors[((i - 1) %% length(timeline_colors)) + 1]
                    )
            }

            hc
        })
    })
}
