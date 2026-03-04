# ============================================================
#  Shared Chart Styles & Constants — utils_chart_styles.R
#
#  Centralizes Highcharts styling helpers and palette constants
#  used across all modules. Source this file once in app.R.
# ============================================================

# ── Rank tiers ───────────────────────────────────────────────
rank_levels <- c(
    "Generational", "SuperStar", "Star", "Elite",
    "Starter", "Solid", "Role", "Rotation", "Deep"
)

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

# ── Highcharts style primitives ──────────────────────────────
#    Reusable inline-style lists for hc_title, hc_subtitle, etc.

nba_title_style <- list(
    color      = "#0F2355",
    fontFamily = "Trebuchet MS",
    fontWeight = "700",
    fontSize   = "16px"
)

nba_subtitle_style <- list(
    color    = "#8A93A6",
    fontSize = "11px"
)

nba_axis_title_style <- list(
    color      = "#718096",
    fontSize   = "11px",
    fontWeight = "600"
)

nba_axis_label_style <- list(
    color    = "#A0AEC0",
    fontSize = "10px"
)

nba_xaxis_category_style <- list(
    fontSize   = "11px",
    color      = "#2D3748",
    fontWeight = "500",
    fontFamily = "Trebuchet MS"
)

# ── Pipe-friendly Highcharts helpers ────────────────────────
#    Each function returns the modified hc object, so they can
#    be chained with %>%.

#' Apply common chart-level defaults (transparent bg, font, theme)
hc_nba_base <- function(hc, chart_type = "bar", extra = list()) {
    defaults <- list(
        type            = chart_type,
        backgroundColor = "transparent",
        animation       = list(duration = 600),
        style           = list(fontFamily = "Trebuchet MS")
    )
    hc %>%
        hc_chart(!!!modifyList(defaults, extra)) %>%
        hc_add_theme(hc_theme_smpl())
}

#' NBA-styled title
hc_nba_title <- function(hc, text) {
    hc %>% hc_title(text = text, style = nba_title_style)
}

#' NBA-styled subtitle
hc_nba_subtitle <- function(hc, text) {
    hc %>% hc_subtitle(text = text, style = nba_subtitle_style)
}

#' Dark tooltip shared by the university-ranking modules
hc_nba_tooltip_dark <- function(hc, ...) {
    hc %>% hc_tooltip(
        useHTML         = TRUE,
        backgroundColor = "rgba(9, 24, 64, 0.97)",
        borderColor     = "#1D428A",
        borderRadius    = 8,
        borderWidth     = 1,
        shadow          = TRUE,
        ...
    )
}

#' Standard shared tooltip (header + point rows) for multi-series charts
hc_nba_tooltip_shared <- function(hc) {
    hc %>% hc_nba_tooltip_dark(
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
            "<span style='color:#FFFFFF;font-weight:700;min-width:28px;text-align:right;'>",
            "{point.y}</span>",
            "</div>"
        ),
        footerFormat = "</div>"
    )
}

#' Right-aligned vertical legend (used in stacked/multi-series charts)
hc_nba_legend_right <- function(hc) {
    hc %>% hc_legend(
        align          = "right",
        verticalAlign  = "middle",
        layout         = "vertical",
        itemStyle      = list(color = "#4A5568", fontSize = "10px", fontWeight = "500"),
        itemHoverStyle = list(color = "#0F2355"),
        symbolRadius   = 3,
        symbolHeight   = 10,
        symbolWidth    = 10
    )
}

#' Clean y-axis with dotted grid lines
hc_nba_yaxis <- function(hc, title_text = NULL, extra = list()) {
    defaults <- list(
        title = list(text = title_text, style = nba_axis_title_style),
        gridLineColor = "rgba(226,232,240,0.6)",
        gridLineDashStyle = "Dot",
        labels = list(style = nba_axis_label_style)
    )
    hc %>% hc_yAxis(!!!modifyList(defaults, extra))
}

#' Clean x-axis with category labels
hc_nba_xaxis_categories <- function(hc, categories, extra = list()) {
    defaults <- list(
        categories = categories,
        labels     = list(style = nba_xaxis_category_style),
        lineWidth  = 0,
        tickWidth  = 0
    )
    hc %>% hc_xAxis(!!!modifyList(defaults, extra))
}
