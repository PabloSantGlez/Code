# ============================================================
#  NBA Draft Analysis — app.R
# ============================================================

library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(readr)
options(shiny.autoreload = TRUE)

# ── Conflictos ───────────────────────────────────────────────
select    <- dplyr::select
mutate    <- dplyr::mutate
filter    <- dplyr::filter
group_by  <- dplyr::group_by
summarise <- dplyr::summarise

# ── Módulos ──────────────────────────────────────────────────
source("mod_redraft.R")
source("mod_heatmap.R")
source("mod_overall_performance.R")
source("mod_map_college.R")
source("mod_rankings_universities.R")

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
  
  # ── 1. Sidebar (Solo navegación) ──
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      id = "tabs",
      menuItem("Overall Performance", tabName = "overall", icon = icon("dashboard")),
      menuItem("College & Scouting",  tabName = "college", icon = icon("graduation-cap")),
      menuItem("Positional Value",    tabName = "position", icon = icon("users")),
      menuItem("Busts & Steals",      tabName = "outliers", icon = icon("search"))
    )
  ),
  
  # ── 2. Body con el filtro dentro de la pestaña ──
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      # Dentro de dashboardBody( tags$head( ... ) )
      tags$style(HTML("
        #draft_table { 
          color: var(--bg-surface); 
          font-family: var(--font-body);
        }
        #draft_table th { 
          color: var(--bg-surface); /* NBA Red oficial */
          font-family: var(--font-display);
          font-size: 11px;
          letter-spacing: 0.1em;
          text-transform: uppercase;
          border-bottom: 2px solid var(--navy) !important;
          padding: 8px 4px !important;
        }
        #draft_table td { 
          border-bottom: 1px solid var(--red-dark); 
          padding: 6px 4px !important;
          vertical-align: middle;
        }
        #draft_table tr:hover { 
          background-color: var(--red-dark) !important; /* Navy Blue con opacidad */
        }
        
        /* Scrollbar personalizado para que no rompa la estética */
        .draft-scroll::-webkit-scrollbar { width: 4px; }
        .draft-scroll::-webkit-scrollbar-track { background: var(--navy-dark); }
        .draft-scroll::-webkit-scrollbar-thumb { background: var(--red); border-radius: 10px; }
      "))
    ),
    
    tabItems(
      
      # --- PESTAÑA 1: Overall Performance ---
      tabItem(tabName = "overall",
              fluidRow(
                column(width = 2,
                       div(
                         style = "background-color: var(--navy-deeper); 
                                  border: 1px solid var(--border); 
                                  border-radius: var(--radius-lg); 
                                  padding: 20px; 
                                  box-shadow: var(--shadow-md);
                                  height: calc(100vh - 100px); 
                                  display: flex; 
                                  flex-direction: column;",
                         h4("Draft Year", style = "color: #FFFFFF; font-weight: 700; margin-top: 0;"),
                         hr(style = "border-top: 1px solid rgba(255,255,255,0.1);"),
                         
                         selectInput(
                           "year",
                           label    = "Select Year:",
                           choices  = sort(unique(tidyData$year)),
                           selected = 2003
                         ),
                         
                         p("Filter to view specific draft class performance.", 
                           style = "color: var(--bg-surface); font-size: 11px; margin-top: 10px;"),
                         
                         hr(style = "border-top: 1px solid rgba(255,255,255,0.1); margin: 15px 0;"),
                         
                         h5("DRAFT CLASS",
                            style = "color: var(--bg-surface); font-family: var(--font-display); font-weight: 700; font-size: 12px; letter-spacing: 1px;"),
                         
                         # 2. Cambiamos max-height por flex-grow: 1
                         div(class = "draft-scroll",
                             style = "flex-grow: 1; overflow-y: auto; margin-right: -5px; padding-right: 5px;",
                             tableOutput("draft_table")
                         )
                       )
                ),
                # ── Panel Derecho (Gráficos) ──
                column(width = 10,
                       fluidRow(mod_pick_performance_ui("pick_perf")),
                       fluidRow(mod_redraft_ui("redraft"), mod_heatmap_ui("heatmap"))
                )
              )
      ),
      
      # --- PESTAÑA 2: College & Scouting ---
      tabItem(tabName = "college",
              
              # Título principal de la sección
              h2("College Talent & Scouting", 
                 style = "color: #0F2355; font-weight: bold; margin-bottom: 20px;"),
              
              # Creamos el contenedor de pestañas (sub-navegación)
              tabsetPanel(
                id = "college_tabs",
                type = "pills",
                
                # ── Sub-pestaña 1: Mapa Interactivo ──
                tabPanel(
                  title = "National Talent Map", 
                  icon = icon("map"),
                  
                  
                  div(style = "margin-top: 15px; margin-left: -15px; margin-right: -15px;",
                      mod_map_college_ui("map_col")
                  )
                ),
                
                # ── Sub-pestaña 2: Factory Rankings (Espacio para el Treemap o Scatter) ──
                tabPanel(
                  title = "University Rankings",
                  icon  = icon("list-ol"),
                  div(style = "margin-top: 20px;",
                      mod_rankings_ui("rankings")
                  )
                ),
                
                # ── Sub-pestaña 3: Non College Players ──
                tabPanel(
                  title = "Non College Players",
                  icon = icon("earth-europe"),
                  
                  div(style = "margin-top: 20px;",
                      box(
                        title = "Analysis of Players tha Didn´t Play College", status = "danger", solidHeader = TRUE, width = 12,
                        p("Aquí colocaremos el Lollipop Chart para ver qué universidades sobrevaloran a sus jugadores.", 
                          style = "color: #8A93A6; font-style: italic;")
                      )
                  )
                )
              )
              
      ),
      # --- PESTAÑA 3: Positional Value ---
      tabItem(tabName = "position",
              h2("Positional Draft Strategy", style = "color: #0F2355; font-weight: bold;"),
              p("Análisis de riesgo/recompensa por posición.")
      ),
      
      # --- PESTAÑA 4: Busts & Steals ---
      tabItem(tabName = "outliers",
              h2("Busts & Steals Deep Dive", style = "color: #0F2355; font-weight: bold;"),
              p("Jugadores atípicos del Draft.")
      )
    )
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
  
  output$draft_table <- renderTable({
    untidyData %>%
      filter(year == year_sel()) %>%
      arrange(overall_pick) %>%
      mutate(overall_pick = as.character(overall_pick)) %>%
      select(
        `PICK` = overall_pick, 
        `PLAYER` = player, 
        `TEAM` = team
      )
  }, 
  striped = FALSE, 
  hover = TRUE, 
  width = "100%", 
  align = "cll")
  
  mod_map_college_server("map_col",
                         data = tidyData,
                         raw_data = untidyData,
                         nba_title = nba_title,
                         nba_subtitle = nba_subtitle
                         )
  mod_rankings_server("rankings",
                      data         = tidyData,
                      raw_data     = untidyData,
                      nba_title    = nba_title,
                      nba_subtitle = nba_subtitle,
                      nba_tooltip  = nba_tooltip,
                      rank_levels  = rank_levels,
                      rank_colors  = rank_colors
  )
}

# ── Run ───────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
