# ============================================================
#  NBA Draft Analysis — app.R
# ============================================================

library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(readr)

# ── Conflictos ───────────────────────────────────────────────
select    <- dplyr::select
mutate    <- dplyr::mutate
filter    <- dplyr::filter
group_by  <- dplyr::group_by
summarise <- dplyr::summarise

# ── Módulos ──────────────────────────────────────────────────
source("mod_talent.R")
source("mod_redraft.R")
source("mod_heatmap.R")
source("mod_overall_performance.R")

# ── Helpers ──────────────────────────────────────────────────
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

# ── Constantes ───────────────────────────────────────────────
rank_levels <- c("Generational", "SuperStar", "Star", "Elite",
                 "Starter", "Solid", "Role", "Rotation", "Deep")

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

# ── Estilos NBA ───────────────────────────────────────────────
nba_title <- list(
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
  lineColor     = "#E2E8F0"
)
nba_legend <- list(
  align          = "right",
  verticalAlign  = "middle",
  layout         = "vertical",
  title          = list(text = "Categoria"),
  itemStyle      = list(color = "#4A5568", fontSize = "11px", fontWeight = "500"),
  itemHoverStyle = list(color = "#0F2355")
)

# ── Datos ─────────────────────────────────────────────────────
untidyData <- readr::read_csv("untidyData.csv")
tidyData   <- readr::read_csv("tidyData.csv")

# ── UI ────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Draft Analysis"),

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
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    # Fila 1: Pick Performance
    fluidRow(mod_pick_performance_ui("pick_perf")),
    # Fila 2: Re-Draft + Heatmap
    fluidRow(mod_redraft_ui("redraft"), mod_heatmap_ui("heatmap"))
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  year_sel <- reactive(input$year)

  mod_redraft_server("redraft",
    data          = untidyData,
    year_reactivo = year_sel,
    nba_title     = nba_title,
    nba_subtitle  = nba_subtitle,
    nba_tooltip   = nba_tooltip,
    nba_legend    = nba_legend
  )

  mod_heatmap_server("heatmap",
    data          = tidyData,
    year_reactivo = year_sel,
    nba_title     = nba_title,
    nba_subtitle  = nba_subtitle,
    nba_tooltip   = nba_tooltip
  )

  mod_pick_performance_server("pick_perf",
    data          = untidyData,
    year_reactivo = year_sel,
    nba_title     = nba_title,
    nba_subtitle  = nba_subtitle,
    nba_tooltip   = nba_tooltip
  )
}

# ── Run ───────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
