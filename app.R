# ============================================================
#  NBA Draft Analysis — Shiny App
#  Convertido desde flexdashboard
# ============================================================

library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(readr)

# Evitar conflictos entre librerías
select    <- dplyr::select
mutate    <- dplyr::mutate
filter    <- dplyr::filter
group_by  <- dplyr::group_by
summarise <- dplyr::summarise

# ── Helpers ─────────────────────────────────────────────────

yeo.johnson <- function(y, lambda) {
  y_t     <- numeric(length(y))
  pos_idx <- which(y >= 0)
  if (lambda == 0) {
    y_t[pos_idx] <- log(y[pos_idx] + 1)
  } else {
    y_t[pos_idx] <- ((y[pos_idx] + 1)^lambda - 1) / lambda
  }
  neg_idx <- which(y < 0)
  if (lambda == 2) {
    y_t[neg_idx] <- -log(-y[neg_idx] + 1)
  } else {
    y_t[neg_idx] <- -(( (-y[neg_idx] + 1)^(2 - lambda) - 1) / (2 - lambda))
  }
  return(y_t)
}

optimize.yeojohnson.R2 <- function(x, y, lambda_range = c(-1, 1.9)) {
  r2_neg <- function(lambda) {
    y_t    <- yeo.johnson(y, lambda)
    modelo <- lm(y_t ~ x)
    -summary(modelo)$r.squared
  }
  optimize(r2_neg, interval = lambda_range)$minimum
}

ParetoValue <- function(v, ParetoSignificancia) {
  x <- sort(v, decreasing = TRUE)
  x[which(cumsum(x) >= ParetoSignificancia * sum(x))[1]]
}

rank_levels <- c("Generational", "SuperStar", "Star", "Elite",
                 "Starter", "Solid", "Role", "Rotation", "Deep")

# ── Estilos NBA reutilizables ────────────────────────────────
nba_font   <- "Segoe UI"
nba_title  <- list(
  style = list(color = "#0F2355", fontFamily = "Trebuchet MS",
               fontWeight = "700", fontSize = "17px")
)
nba_subtitle <- list(
  style = list(color = "#8A93A6", fontSize = "12px")
)
nba_tooltip <- list(
  backgroundColor = "#0F2355",
  borderColor     = "#1D428A",
  borderRadius    = 6,
  borderWidth     = 0,
  style           = list(color = "#FFFFFF", fontSize = "12px")
)
nba_axis <- list(
  gridLineColor = "#E2E8F0",
  lineColor     = "#E2E8F0",
  tickColor     = "#E2E8F0",
  labels = list(style = list(color = "#4A5568", fontSize = "11px")),
  title  = list(style = list(color = "#0F2355", fontWeight = "600"))
)
nba_legend <- list(
  align = "right", verticalAlign = "middle", layout = "vertical",
  title = list(text = "Categoría"),
  itemStyle      = list(color = "#4A5568", fontSize = "11px", fontWeight = "500"),
  itemHoverStyle = list(color = "#0F2355")
)

rank_colors <- c(
  "Generational" = "#FFD700",
  "SuperStar"    = "#FF6B35",
  "Star"         = "#E63946",
  "Elite"        = "#457B9D",
  "Starter"      = "#1D3557",
  "Solid"        = "#2D6A4F",
  "Role"         = "#74C69D",
  "Rotation"     = "#A8DADC",
  "Deep"         = "#CED4DA"
)

# ── Datos ────────────────────────────────────────────────────

untidyData <- readr::read_csv("untidyData.csv")
tidyData   <- readr::read_csv("tidyData.csv")

# ── UI ───────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "NBA Draft Analysis"),

  dashboardSidebar(
    width = 220,

    br(),
    div(style = "padding: 0 15px;",
      selectInput(
        "year",
        label    = "Draft Year:",
        choices  = sort(unique(tidyData$year)),
        selected = 2003
      )
    ),
    hr(),
    div(style = "padding: 10px 15px; color: #ccc; font-size: 12px; line-height: 1.6;",
      p(strong("NBA Draft Analysis"))
      )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    # ── Fila 1: Talento por equipo ───────────────────────────
    fluidRow(
      box(
        title  = NULL,
        width  = 12,
        height = 480,
        solidHeader = FALSE,
        highchartOutput("chart_talent", height = "440px")
      )
    ),

    # ── Fila 2: Re-Draft y Heatmap ───────────────────────────
    fluidRow(
      box(
        title  = NULL,
        width  = 6,
        height = 580,
        solidHeader = FALSE,
        highchartOutput("chart_redraft", height = "540px")
      ),
      box(
        title  = NULL,
        width  = 6,
        height = 580,
        solidHeader = FALSE,
        highchartOutput("chart_heatmap", height = "540px")
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── 1. Talento por equipo ─────────────────────────────────
  output$chart_talent <- renderHighchart({

    team_rank_data <- tidyData %>%
      filter(year == input$year) %>%
      distinct(player, .keep_all = TRUE) %>%
      group_by(team, rank) %>%
      summarise(
        n       = n(),
        players = paste(player, collapse = ", "),
        .groups = "drop"
      ) %>%
      arrange(team) %>%
      mutate(rank = factor(rank, levels = rank_levels))

    all_teams <- sort(unique(team_rank_data$team))

    series_data <- lapply(rank_levels, function(r) {
      counts    <- team_rank_data %>% filter(rank == r)
      data_list <- lapply(all_teams, function(t) {
        row <- counts %>% filter(team == t)
        if (nrow(row) == 0) list(y = 0, players = "") else list(y = row$n, players = row$players)
      })
      list(name = r, data = data_list, color = rank_colors[[r]])
    })

    highchart() %>%
      hc_chart(type = "bar", backgroundColor = "#FFFFFF"
               ) %>%
      hc_title(text = paste0("Selección de Talento por Equipo — ", input$year),
               style = nba_title$style) %>%
      hc_subtitle(text = "Distribución de categorías de rendimiento por franquicia",
                  style = nba_subtitle$style) %>%
      hc_xAxis(categories = all_teams, title = list(text = NULL),
               gridLineColor = nba_axis$gridLineColor,
               lineColor     = nba_axis$lineColor,
               labels = list(style = list(fontSize = "11px", color = "#4A5568"))) %>%
      hc_yAxis(gridLineColor = nba_axis$gridLineColor,
               lineColor     = nba_axis$lineColor) %>%
      hc_plotOptions(bar = list(stacking = "normal", borderWidth = 0,
                                pointPadding = 0.05, groupPadding = 0.03)) %>%
      hc_add_series_list(series_data) %>%
      hc_legend(align = nba_legend$align, verticalAlign = nba_legend$verticalAlign,
                layout = nba_legend$layout, title = nba_legend$title,
                itemStyle = nba_legend$itemStyle,
                itemHoverStyle = nba_legend$itemHoverStyle) %>%
      hc_tooltip(
        useHTML         = TRUE,
        backgroundColor = nba_tooltip$backgroundColor,
        borderColor     = nba_tooltip$borderColor,
        borderRadius    = nba_tooltip$borderRadius,
        borderWidth     = nba_tooltip$borderWidth,
        style           = nba_tooltip$style,
        formatter = JS("function() {
          if (this.point.y === 0) return false;
          return '<b>' + this.key + '</b><br/>' +
                 '<span style=\"color:' + this.series.color + '\">●</span> ' +
                 this.series.name + ': <b>' + this.point.y + '</b> jugadores<br/>' +
                 '<hr style=\"margin:4px 0;border-color:rgba(255,255,255,0.15)\"/>' +
                 '<span style=\"font-size:11px\">' + this.point.players + '</span>';
        }") ) %>%
      hc_add_theme(hc_theme_smpl())
  })

  # ── 2. Re-Draft ───────────────────────────────────────────
  output$chart_redraft <- renderHighchart({

    top_redraft <- untidyData %>%
      filter(year == input$year) %>%
      distinct(player, .keep_all = TRUE) %>%
      arrange(newPick)

    hchart(top_redraft, "scatter",
           hcaes(x = as.numeric(overall_pick), y = newPick, group = rank)) %>%
      hc_chart(backgroundColor = "#FFFFFF") %>%
      hc_title(text = paste0("Re-Draft ", input$year), style = nba_title$style) %>%
      hc_subtitle(text = "Pick original vs. pick basado en rendimiento real",
                  style = nba_subtitle$style) %>%
      hc_xAxis(title = list(text = "Pick Original",
                            style = list(color = "#0F2355", fontWeight = "600")),
               min = 1, max = 60,
               gridLineColor = "#E2E8F0", lineColor = "#E2E8F0",
               labels = list(style = list(color = "#4A5568")),
               plotLines = list(list(value = 30, color = "#CBD5E0",
                                     dashStyle = "Dash", width = 1))) %>%
      hc_yAxis(title = list(text = "Nuevo Pick (Performance Real)",
                            style = list(color = "#0F2355", fontWeight = "600")),
               reversed = TRUE, min = 1, max = 60,
               gridLineColor = "#E2E8F0", lineColor = "#E2E8F0",
               labels = list(style = list(color = "#4A5568")),
               plotLines = list(list(value = 30, color = "#CBD5E0",
                                     dashStyle = "Dash", width = 1))) %>%
      hc_tooltip(
        useHTML         = TRUE,
        backgroundColor = nba_tooltip$backgroundColor,
        borderColor     = nba_tooltip$borderColor,
        borderRadius    = nba_tooltip$borderRadius,
        borderWidth     = nba_tooltip$borderWidth,
        style           = nba_tooltip$style,
        pointFormat     = "
          <b>{point.player}</b><br/>
          Pick Original: <b>{point.x}</b><br/>
          Nuevo Pick: <b>{point.y}</b><br/>
          Diferencia: <b>{point.pickDifference}</b>
        ") %>%
      hc_plotOptions(
        scatter = list(marker = list(radius = 6, symbol = "circle"),
                       states = list(hover = list(enabled = TRUE)))) %>%
      hc_legend(align = nba_legend$align, verticalAlign = nba_legend$verticalAlign,
                layout = nba_legend$layout, title = nba_legend$title,
                itemStyle = nba_legend$itemStyle,
                itemHoverStyle = nba_legend$itemHoverStyle) %>%
      hc_add_theme(hc_theme_smpl())
  })

  # ── 3. Heatmap Lottery Picks ──────────────────────────────
  output$chart_heatmap <- renderHighchart({

    lottery_picks <- tidyData %>%
      filter(year == input$year, unit == "Player") %>%
      distinct(player, overall_pick) %>%
      arrange(overall_pick) %>%
      head(14) %>%
      pull(player)

    df_heat <- tidyData %>%
      filter(year == input$year, unit == "Player", player %in% lottery_picks) %>%
      group_by(indicator) %>%
      mutate(normValue = (value - min(value, na.rm = TRUE)) /
                         (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))) %>%
      ungroup() %>%
      arrange(desc(overall_pick))

    hchart(df_heat, "heatmap", hcaes(x = indicator, y = player, value = normValue)) %>%
      hc_chart(backgroundColor = "#FFFFFF",
               marginLeft = 160, marginBottom = 110, marginRight = 60,
               animation = list(duration = 0)
               ) %>%
      hc_colorAxis(min = 0, max = 1,
                   stops = color_stops(7, c("#0F2355","#1D428A","#FFFFFF","#C8102E","#7A0018"))) %>%
      hc_title(text = paste0("Heatmap de Atributos — Elite del Draft ", input$year),
               style = nba_title$style) %>%
      hc_subtitle(text = "Picks de Lotería — valores normalizados [0, 1]",
                  style = nba_subtitle$style) %>%
      hc_xAxis(title = list(text = NULL),
               labels = list(rotation = -45,
                             style = list(fontSize = "10px", color = "#4A5568"))) %>%
      hc_yAxis(title = list(text = NULL),
               labels = list(style = list(fontSize = "11px", color = "#4A5568"))) %>%
      hc_plotOptions(
        heatmap = list(borderWidth = 2, borderColor = "#FFFFFF",
                       nullColor = "#F2F4F8",
                       states = list(hover = list(enabled = TRUE, brightness = 0)))) %>%
      hc_tooltip(
        useHTML         = TRUE,
        backgroundColor = nba_tooltip$backgroundColor,
        borderColor     = nba_tooltip$borderColor,
        borderRadius    = nba_tooltip$borderRadius,
        borderWidth     = nba_tooltip$borderWidth,
        style           = nba_tooltip$style,
        formatter = JS("function() {
          return '<b>' + this.point.player + '</b><br/>' +
                 'Atributo: <b>' + this.point.indicator + '</b><br/>' +
                 'Valor norm.: <b>' + Math.round(this.point.value * 100) / 100 + '</b>';
        }")) %>%
      hc_add_theme(hc_theme_smpl())
  })
}

# ── Run ──────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)
